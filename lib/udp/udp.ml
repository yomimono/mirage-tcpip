(*
 * Copyright (c) 2010-2014 Anil Madhavapeddy <anil@recoil.org>
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
open Result

let src = Logs.Src.create "udp" ~doc:"Mirage UDP"
module Log = (val Logs.src_log src : Logs.LOG)

let pp_ips = Format.pp_print_list Ipaddr.pp_hum

module Make(Ip: V1_LWT.IP) = struct

  type 'a io = 'a Lwt.t
  type buffer = Cstruct.t
  type ip = Ip.t
  type ipaddr = Ip.ipaddr
  type ipinput = src:ipaddr -> dst:ipaddr -> buffer -> unit io
  type callback = src:ipaddr -> dst:ipaddr -> src_port:int -> Cstruct.t -> unit Lwt.t

  (** IO operation errors *)
  type error = [
    | `Unknown of string (** an undiagnosed error *)
  ]

  type t = {
    ip : Ip.t;
  }

  (* TODO: ought we to check to make sure the destination is relevant here?  Currently we process all incoming packets without making sure they're either unicast for us or otherwise interesting. *)
  let input ~listeners _t ~src ~dst buf =
    match Udp_packet.Unmarshal.of_cstruct buf with
    | Error s ->
      Log.debug (fun f -> f "Discarding received UDP message: error parsing: %s" s); Lwt.return_unit
    | Ok ({ Udp_packet.src_port; dst_port}, payload) ->
      match listeners ~dst_port with
      | None    -> Lwt.return_unit
      | Some fn ->
        fn ~src ~dst ~src_port payload

  let writev ?src ~src_port ~dst ~dst_port t bufs =
    begin match src_port with
      | n when n < 0 || n > 65535 -> Lwt.fail (Failure "source port not a 16-bit unsigned int")
      | n -> Lwt.return src_port
    end >>= fun src_port ->
    let payload_size = Cstruct.lenv bufs in
    let frame, header_len = Ip.allocate_frame ?src ~dst ~proto:`UDP t.ip in
    let frame = Cstruct.set_len frame header_len in
    let ph = Ip.pseudoheader t.ip ~dst ~proto:`UDP (payload_size + Udp_wire.sizeof_udp) in
    let udp_header = Udp_packet.({ src_port; dst_port; }) in
    let udp_buf = Udp_packet.Marshal.make_cstruct udp_header ~pseudoheader:ph
            ~payload:(Cstruct.concat bufs) in
    Ip.writev t.ip frame (udp_buf :: bufs)

  let write ?src ~src_port ~dst ~dst_port t buf =
    writev ?src ~src_port ~dst ~dst_port t [buf]

  let connect ip =
    let ips = List.map Ip.to_uipaddr @@ Ip.get_ip ip in
    Log.info (fun f -> f "UDP interface connected on %a" pp_ips ips);
    Lwt.return (`Ok { ip })

  let disconnect t =
    let ips = List.map Ip.to_uipaddr @@ Ip.get_ip t.ip in
    Log.info (fun f -> f "UDP interface disconnected on %a" pp_ips ips);
    Lwt.return_unit

end
