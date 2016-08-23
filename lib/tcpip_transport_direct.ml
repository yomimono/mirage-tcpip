(*
 * Copyright (c) 2016 Mindy Preston <mindy.preston@docker.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix

let src = Logs.Src.create "tcpip-transport-direct" ~doc:"Transport layer implementations in pure OCaml"
module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (Time    : V1_LWT.TIME)
    (Random  : V1.RANDOM)
    (Ip      : V1_LWT.IP)
    (Icmp    : V1_LWT.ICMP)
    (Udp     : V1_LWT.UDP with type ip = Ip.ipaddr)
    (Tcp     : V1_LWT.TCP with type ip = Ip.ipaddr) =
struct

  type +'a io = 'a Lwt.t
  type ('a,'b) config = ('a,'b) V1_LWT.stack_config
  type mode = V1_LWT.direct_stack_config
  type buffer = Cstruct.t
  type ipaddr = Ip.ipaddr

  type tcp_action = [
    | `Reject
    | `Accept of (Tcp.flow -> unit Lwt.t)
  ]

  type direct_ip_input = src:Ipaddr.t -> dst:Ipaddr.t -> Cstruct.t -> unit Lwt.t

  type tcp_on_flow_arrival_callback = src:(ipaddr * int) -> dst:(ipaddr * int) -> tcp_action Lwt.t

  type t = {
    ip    : Ip.t;
    icmp  : Icmp.t;
    udp   : Udp.t;
    tcp   : Tcp.t;
    udp_listeners: (int, Udp.callback) Hashtbl.t;
    tcp_listeners: (int, (Tcp.flow -> unit Lwt.t)) Hashtbl.t;
    mutable tcp_on_flow_arrival: tcp_on_flow_arrival_callback;
  }

  type error = [
      `Unknown of string
  ]

  let uprinter fmt addr = match Ip.to_uipaddr addr with
  | Ipaddr.V4 addr -> Ipaddr.V4.pp_hum fmt addr
  | Ipaddr.V6 addr -> Ipaddr.V6.pp_hum fmt addr

  let tcp { tcp; _ } = tcp
  let udp { udp; _ } = udp

  let pp_netconfig fmt t =
    let ips = Ip.get_ip t.ip in
    let netmasks = Ip.get_ip_netmasks t.ip in
    let gateways = Ip.get_ip_gateways t.ip in

    let ip_print_list fmt l = Format.pp_print_list uprinter fmt l in
    Format.fprintf fmt "ips: %a, gateways %a"
      ip_print_list ips ip_print_list gateways

  let pp fmt t =
    Format.fprintf fmt "tcpip direct transport: network %a"
    pp_netconfig t

  let pp_pair fmt (ipaddr, int) = Format.fprintf fmt "%a,%d" Ipaddr.pp_hum ipaddr int

  let err_invalid_port p = Printf.sprintf "invalid port number (%d)" p

  let listen_udp t ~port callback =
    if port < 0 || port > 65535
    then raise (Invalid_argument (err_invalid_port port))
    else begin
      Log.debug (fun f -> f "establishing listener on %a for UDP port %d" pp t port);
      Hashtbl.replace t.udp_listeners port callback
    end

  let listen_tcp t ~port callback =
    if port < 0 || port > 65535
    then raise (Invalid_argument (err_invalid_port port))
    else begin
      Log.debug (fun f -> f "establishing listener on %a for TCP port %d" pp t port);
      Hashtbl.replace t.tcp_listeners port callback
    end

  let listen_tcp_flow t ~on_flow_arrival =
    (* Wrap the callback to check the registered listeners first, treating
       the [on_flow_arrival] callback as a default *)
    t.tcp_on_flow_arrival <-
      (fun ~src ~dst:(ip, port) ->
        (if Hashtbl.mem t.tcp_listeners port
         then Lwt.return (`Accept (Hashtbl.find t.tcp_listeners port))
         else on_flow_arrival ~src ~dst:(ip, port)))

  let pp_opt pp f = function
    | None -> Format.pp_print_string f "None"
    | Some x -> pp f x

  let udp_listeners t ~dst_port =
    try Some (Hashtbl.find t.udp_listeners dst_port)
    with Not_found -> None

  let listen t =
    (* NB: this function will be called more than once, including at initialisation
       time. Be careful not to capture the state of `t` before the program can
       customise it, see `tcp_on_flow_arrival` below. *)
    Netif.listen t.netif (
      Ethif.input
        ~arpv4:(Arpv4.input t.arpv4)
        ~ipv4:(
          Ipv4.input
            ~tcp:(Tcpv4.input_flow t.tcp
                    ~on_flow_arrival:(fun ~src ~dst -> t.tcp_on_flow_arrival ~src ~dst))
            ~udp:(Udpv4.input t.udp
                    ~listeners:(udp_listeners t))
            ~default:(fun ~proto ~src ~dst buf -> 
                match proto with
                | 1 -> Icmpv4.input t.icmpv4 ~src ~dst buf
                | _ -> Lwt.return_unit)
            t.ipv4)
        ~ipv6:(fun _ -> Lwt.return_unit)
        t.ethif)

  let connect ?on_flow_arrival ip icmp udp tcp =
    let { V1_LWT.interface = netif; mode; _ } = id in
    Log.info (fun f -> f "Manager: connect");
    let udp_listeners = Hashtbl.create 7 in
    let tcp_listeners = Hashtbl.create 7 in
    (* the default behavior for an incoming flow should be rejection --
     * we should only do something else when our rules specify that the
     * traffic should be forwarded on to either a port mapping or an
     * existing connection. *)
    let tcp_on_flow_arrival = match on_flow_arrival with
    | None -> (fun ~src ~dst ->
                    Log.debug (fun f -> f "Traffic from %a to %a dropped" pp_pair src pp_pair dst);
                    Lwt.return `Reject)
    | Some fn -> fn
    in
    let t = { ip; icmp; tcp; udp;
              udp_listeners; tcp_listeners; tcp_on_flow_arrival } in
    Log.info (fun f -> f "Manager: configuring");
    Lwt.async (fun () -> listen t);
    (* TODO: this is fine for now, because the DHCP state machine isn't fully
       implemented and its thread will terminate after one successful lease
       transaction.  For a DHCP thread that runs forever, `configure` will need
       to spawn a background thread, but we need to consider how to inform the
       application stack that the IP address has changed (perhaps via a control
       Lwt_stream that the application can ignore if it doesn't care). *)
    Log.info (fun f -> f "Manager: configuration done");
    Lwt.return (`Ok t)

  let disconnect _t =
    (* TODO: kill the listening thread *)
    Log.info (fun f -> f "Manager: disconnect");
    Lwt.return_unit
end
