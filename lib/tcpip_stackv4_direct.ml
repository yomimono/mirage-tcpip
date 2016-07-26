(*
 * Copyright (c) 2011-2014 Anil Madhavapeddy <anil@recoil.org>
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

let src = Logs.Src.create "tcpip-stack-direct" ~doc:"Pure OCaml TCP/IP stack"
module Log = (val Logs.src_log src : Logs.LOG)

type direct_ipv4_input = src:Ipaddr.V4.t -> dst:Ipaddr.V4.t -> Cstruct.t -> unit Lwt.t
module type UDPV4_DIRECT = V1_LWT.UDPV4
  with type ipinput = direct_ipv4_input

module type TCPV4_DIRECT = V1_LWT.TCPV4
  with type ipinput = direct_ipv4_input

module Make
    (Time    : V1_LWT.TIME)
    (Random  : V1.RANDOM)
    (Netif   : V1_LWT.NETWORK)
    (Ethif   : V1_LWT.ETHIF with type netif = Netif.t)
    (Arpv4   : V1_LWT.ARP)
    (Ipv4    : V1_LWT.IPV4 with type ethif = Ethif.t)
    (Icmpv4  : V1_LWT.ICMPV4)
    (Udpv4   : UDPV4_DIRECT with type ip = Ipv4.t)
    (Tcpv4   : TCPV4_DIRECT with type ip = Ipv4.t) =
struct

  type +'a io = 'a Lwt.t
  type ('a,'b) config = ('a,'b) V1_LWT.stackv4_config
  type netif = Netif.t
  type mode = V1_LWT.direct_stack_config
  type id = (netif, mode) config
  type buffer = Cstruct.t
  type tcp = Tcpv4.t
  type udp = Udpv4.t
  type ipaddr = Ipaddr.V4.t

  module UDP = Udpv4
  module TCP = Tcpv4
  module Dhcp  = Dhcp_clientv4.Make(Time)(Random)(Udpv4)

  type tcp_action = [
    | `Reject
    | `Accept of (Tcpv4.flow -> unit Lwt.t)
  ]

  type tcp_on_flow_arrival_callback = src:(ipaddr * int) -> dst:(ipaddr * int) -> tcp_action Lwt.t

  type t = {
    id    : id;
    mode  : mode;
    netif : Netif.t;
    ethif : Ethif.t;
    arpv4 : Arpv4.t;
    ipv4  : Ipv4.t;
    icmpv4: Icmpv4.t;
    udp : Udpv4.t;
    tcp : Tcpv4.t;
    udp_listeners: (int, Udpv4.callback) Hashtbl.t;
    tcp_listeners: (int, (Tcpv4.flow -> unit Lwt.t)) Hashtbl.t;
    mutable tcp_on_flow_arrival: tcp_on_flow_arrival_callback;
  }

  type error = [
      `Unknown of string
  ]

  let tcp { tcp; _ } = tcp
  let udp { udp; _ } = udp

  let pp_netconfig fmt t =
    let ips = Ipv4.get_ip t.ipv4 in
    let netmasks = Ipv4.get_ip_netmasks t.ipv4 in
    let gateways = Ipv4.get_ip_gateways t.ipv4 in
    let ip_print_list fmt l = Format.pp_print_list Ipaddr.V4.pp_hum fmt l in
    Format.fprintf fmt "ips: %a, netmasks %a, gateways %a"
      ip_print_list ips ip_print_list netmasks ip_print_list gateways

  let pp fmt t =
    Format.fprintf fmt "tcpip direct stack: interface %s, network %a"
    (Macaddr.to_string (Ethif.mac t.ethif)) pp_netconfig t

  let pp_pair fmt (ipaddr, int) = Format.fprintf fmt "%a,%d" Ipaddr.V4.pp_hum ipaddr int

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

  let configure_dhcp t info =
    Ipv4.set_ip t.ipv4 info.Dhcp.ip_addr
    >>= fun () ->
    (match info.Dhcp.netmask with
     | Some nm -> Ipv4.set_ip_netmask t.ipv4 nm
     | None    -> Lwt.return_unit)
    >>= fun () ->
    Ipv4.set_ip_gateways t.ipv4 info.Dhcp.gateways
    >|= fun () ->
    Log.info (fun f -> f "DHCP offer received and bound to %a nm %a gw [%s]"
      Ipaddr.V4.pp_hum info.Dhcp.ip_addr
      (pp_opt Ipaddr.V4.pp_hum) info.Dhcp.netmask
      (String.concat ", " (List.map Ipaddr.V4.to_string info.Dhcp.gateways))
    )

  let configure t config =
    match config with
    | `DHCP -> begin
        (* TODO: spawn a background thread to reconfigure the interface
           when future offers are received. *)
        let dhcp, offers = Dhcp.create (Ethif.mac t.ethif) t.udp in
        listen_udp t ~port:68 (Dhcp.input dhcp);
        (* TODO: stop listening to this port when done with DHCP. *)
        Lwt_stream.get offers >>= function
        | None -> Log.info (fun f -> f "No DHCP offer received"); Lwt.return ()
        | Some offer -> configure_dhcp t offer
      end
    | `IPv4 (addr, netmask, gateways) ->
      Log.info (fun f -> f "Manager: Interface to %a nm %a gw [%s]"
                           Ipaddr.V4.pp_hum addr
                           Ipaddr.V4.pp_hum netmask
                           (String.concat ", " (List.map Ipaddr.V4.to_string gateways)));
      Ipv4.set_ip t.ipv4 addr
      >>= fun () ->
      Ipv4.set_ip_netmask t.ipv4 netmask
      >>= fun () ->
      Ipv4.set_ip_gateways t.ipv4 gateways

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

  let connect ?on_flow_arrival id ethif arpv4 ipv4 icmpv4 udp tcp =
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
    let t = { id; mode; netif; ethif; arpv4; ipv4; icmpv4; tcp; udp;
              udp_listeners; tcp_listeners; tcp_on_flow_arrival } in
    Log.info (fun f -> f "Manager: configuring");
    let _ = listen t in
    configure t t.mode
    >>= fun () ->
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
