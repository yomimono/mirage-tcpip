(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

let src = Logs.Src.create "tcpip-stack-socket" ~doc:"Platform's native TCP/IP stack"
module Log = (val Logs.src_log src : Logs.LOG)

type socket_ipv4_input = unit Lwt.t

module type UDPV4_SOCKET = V1_LWT.UDP
  with type ipinput = socket_ipv4_input
   and type ip = Ipaddr.V4.t option

module type TCPV4_SOCKET = V1_LWT.TCP
  with type ipinput = socket_ipv4_input
   and type ip = Ipaddr.V4.t option

module Tcpv4 = Tcpv4_socket
module Udpv4 = Udpv4_socket

type +'a io = 'a Lwt.t
type ('a,'b) config = ('a,'b) V1_LWT.stackv4_config
type netif = Ipaddr.V4.t list
type mode = unit
type id = (netif, mode) config
type buffer = Cstruct.t
type ipv4addr = Ipaddr.V4.t

module TCP = Tcpv4_socket
module UDP = Udpv4_socket

type udp = Udpv4_socket.t
type tcp = Tcpv4_socket.t
type ipaddr  = Ipaddr.V4.t

type t = {
  id    : id;
  udp : Udpv4.t;
  tcp : Tcpv4.t;
  udp_listeners: (int, Udpv4.callback) Hashtbl.t;
  tcp_listeners: (int, (Tcpv4.flow -> unit Lwt.t)) Hashtbl.t;
}

type error = [
    `Unknown of string
]

type tcp_action = [
  | `Reject
  | `Accept of Tcpv4.flow -> unit Lwt.t
]

type tcp_on_flow_arrival_callback = src:(ipv4addr * int) -> dst:(ipv4addr * int) -> tcp_action io

let udp { udp; _ } = udp
let tcp { tcp; _ } = tcp

(* List of IP addresses to bind to *)
let configure _t addrs =
  match addrs with
  | [] -> Lwt.return_unit
  | [ip] when (Ipaddr.V4.compare Ipaddr.V4.any ip) = 0 -> Lwt.return_unit
  | l ->
    let pp_iplist fmt l = Format.pp_print_list Ipaddr.V4.pp_hum fmt l in
    Log.warn (fun f -> f
              "Manager: sockets currently bind to all available IPs. IPs %a were specified, but this will be ignored" pp_iplist l);
    Lwt.return_unit

let err_invalid_port p = Printf.sprintf "invalid port number (%d)" p

let listen_udp t ~port callback =
  if port < 0 || port > 65535 then
    raise (Invalid_argument (err_invalid_port port))
  else
    let fd = Udpv4.get_udpv4_listening_fd t.udp port in
    let buf = Cstruct.create 4096 in
    let rec loop () =
      let continue () =
        (* TODO cancellation *)
        if true then loop () else Lwt.return_unit in
      Lwt_cstruct.recvfrom fd buf []
      >>= fun (len, sa) ->
      let buf = Cstruct.sub buf 0 len in
      begin match sa with
            | Lwt_unix.ADDR_INET (addr, src_port) ->
               let src = Ipaddr_unix.V4.of_inet_addr_exn addr in
               let dst = Ipaddr.V4.any in (* TODO *)
               callback ~src ~dst ~src_port buf
            | _ -> Lwt.return_unit
      end >>= fun () ->
      continue ()
    in
    (* FIXME: we should not ignore the result *)
    Lwt.ignore_result (loop ())

let listen_tcp _t ~port callback =
  if port < 0 || port > 65535 then
    raise (Invalid_argument (err_invalid_port port))
  else
    let open Lwt_unix in
    let fd = socket PF_INET SOCK_STREAM 0 in
    setsockopt fd SO_REUSEADDR true;
    (* TODO: we should attempt to bind the interface relevant to the adddress set in [t] *)
    let interface = Ipaddr_unix.V4.to_inet_addr Ipaddr.V4.any in
    bind fd (ADDR_INET (interface, port));
    listen fd 10;
    let rec loop () =
      let continue () =
        (* TODO cancellation *)
        if true then loop () else Lwt.return_unit in
      Lwt_unix.accept fd
      >>= fun (afd, _) ->
      Lwt.async (fun () ->
                 Lwt.catch
                   (fun () -> callback afd)
                   (fun _ -> Lwt.return_unit)
                );
      Lwt.return_unit
      >>= fun () ->
      continue ();
    in
    (* FIXME: we should not ignore the result *)
    Lwt.ignore_result (loop ())

let listen_tcp_flow _t ~on_flow_arrival:_ =
    failwith "there is no socket api for on_flow_arrival"

let listen _t =
  let t, _ = Lwt.task () in
  t (* TODO cancellation *)

let connect id udp tcp =
  let { V1_LWT.interface; _ } = id in
  Log.info (fun f -> f "Manager: connect");
  let udp_listeners = Hashtbl.create 7 in
  let tcp_listeners = Hashtbl.create 7 in
  let t = { id; tcp; udp; udp_listeners; tcp_listeners } in
  Log.info (fun f -> f "Manager: configuring");
  configure t interface
  >>= fun () ->
  Lwt.return (`Ok t)

let disconnect _ = Lwt.return_unit
