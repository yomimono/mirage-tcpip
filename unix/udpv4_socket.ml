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

open Lwt

type buffer = Cstruct.t
type ipaddr = Ipaddr.V4.t
type flow = Lwt_unix.file_descr
type +'a io = 'a Lwt.t
type ip = Ipaddr.V4.t option (* source ip and port *)
type ipinput = unit Lwt.t
type callback = src:ipaddr -> dst:ipaddr -> src_port:int -> buffer -> unit io

type t = {
  interface: Unix.inet_addr; (* source ip to bind to *)
  listen_fds: ((Unix.inet_addr * int),Lwt_unix.file_descr) Hashtbl.t; (* UDPv4 fds bound to a particular source ip/port *)
}

let get_udpv4_listening_fd {listen_fds;interface} port =
  try
    Hashtbl.find listen_fds (interface,port)
  with Not_found ->
    let fd = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
    Lwt_unix.bind fd (Lwt_unix.ADDR_INET (interface, port));
    Hashtbl.add listen_fds (interface, port) fd;
    fd

(** IO operation errors *)
type error = [
  | `Unknown of string (** an undiagnosed error *)
]

let connect (id:ip) =
  let t =
    let listen_fds = Hashtbl.create 7 in
    let interface =
      match id with
      | None -> Ipaddr_unix.V4.to_inet_addr Ipaddr.V4.any
      | Some ip -> Ipaddr_unix.V4.to_inet_addr ip
    in { interface; listen_fds }
  in return (`Ok t)

let disconnect _ =
  return_unit

let id { interface; _ } =
  Some (Ipaddr_unix.V4.of_inet_addr_exn interface)

(* FIXME: how does this work at all ?? *)
 let input ~listeners:_ _ =
  (* TODO terminate when signalled by disconnect *)
  let t, _ = Lwt.task () in
  t

let writev ?src ~src_port ~dst ~dst_port t bufs =
  let open Lwt_unix in
  (* TODO: currently we silently discard a given ~src, which isn't the right thing to do *)
  let fd = get_udpv4_listening_fd t src_port in
  Lwt_cstruct.sendto fd (Cstruct.concat bufs) [] (ADDR_INET ((Ipaddr_unix.V4.to_inet_addr dst), dst_port))
  >>= fun _ ->
  return_unit

let write ?src ~src_port ~dst ~dst_port t buf = writev ?src ~src_port ~dst ~dst_port t [buf]
