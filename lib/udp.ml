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

  let id {ip} = ip

  (* FIXME: [t] is not taken into account at all? *)
  let input ~listeners _t ~src ~dst buf =
    let dst_port = Wire_structs.get_udp_dest_port buf in
    let data =
      Cstruct.sub buf Wire_structs.sizeof_udp
        (Wire_structs.get_udp_length buf - Wire_structs.sizeof_udp)
    in
    match listeners ~dst_port with
    | None    -> Lwt.return_unit
    | Some fn ->
      let src_port = Wire_structs.get_udp_source_port buf in
      fn ~src ~dst ~src_port data

  let writev ~source_port ~dest_ip ~dest_port t bufs =
    let open Wire_structs in
    let udp_header = Io_page.(get 1 |> to_cstruct) in (*TODO: does this need
                                                        page-alignment? *)
    let udp_header = Cstruct.set_len udp_header sizeof_udp in
    set_udp_source_port udp_header source_port;
    set_udp_dest_port udp_header dest_port;
    set_udp_length udp_header (sizeof_udp + Cstruct.lenv bufs);
    let pseudoheader = Ip.pseudoheader t.ip ~dst:dest_ip ~proto:`UDP sizeof_udp in
    let csum = Ip.checksum pseudoheader (udp_header :: bufs) in
    set_udp_checksum udp_header csum;
    Ip.output t.ip ~dst:dest_ip ~proto:`UDP (udp_header :: bufs)

  let write ~source_port ~dest_ip ~dest_port t buf =
    writev ~source_port ~dest_ip ~dest_port t [buf]

  let connect ip = Lwt.return (`Ok { ip })

  let disconnect _ = Lwt.return_unit
end
