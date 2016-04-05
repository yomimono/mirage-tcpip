(*
 * Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS l SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Ipv6_wire = Wire_structs.Ipv6_wire

module I = Ipaddr

open Lwt.Infix

module Make (E : V1_LWT.ETHIF) (T : V1_LWT.TIME) (C : V1.CLOCK) = struct
  type ethif    = E.t
  type 'a io    = 'a Lwt.t
  type buffer   = Cstruct.t
  type ipaddr   = Ipaddr.V6.t
  type callback = src:ipaddr -> dst:ipaddr -> buffer -> unit Lwt.t
  type prefix   = Ipaddr.V6.Prefix.t

  type t =
    { ethif : E.t;
      mutable ctx : Ndpv6.context }

  type error =
    [ `Unimplemented
    | `Unknown of string ]

  let id { ethif } = ethif

  let start_ticking t =
    let rec loop () =
      let now = C.time () in
      let ctx, bufs = Ndpv6.tick ~now t.ctx in
      t.ctx <- ctx;
      Lwt_list.iter_s (E.writev t.ethif) bufs >>= fun () ->
      T.sleep 1.0 >>= loop
    in
    loop ()

  let allocate_frame t ~dst ~proto =
    Ndpv6.allocate_frame t.ctx dst proto

  let allocate t ~src ~dst ~proto =
    let proto = Ipv6_wire.protocol_to_int proto in
    Allocate.frame ~mac:t.state.mac ~src ~hlim:t.state.cur_hop_limit ~dst ~proto

  let writev t frame bufs =
    let now = C.time () in
    let dst =
      Ndpv6.ipaddr_of_cstruct
        (Ipv6_wire.get_ipv6_dst (Cstruct.shift frame Wire_structs.sizeof_ethernet))
    in
    let ctx, bufs = Ndpv6.send ~now t.ctx dst frame bufs in
    t.ctx <- ctx;
    Lwt_list.iter_s (E.writev t.ethif) bufs

  let write t frame buf =
    writev t frame [buf]

  let input t ~tcp ~udp ~default buf =
    let now = C.time () in
    let ctx, bufs, actions = Ndpv6.handle ~now t.ctx buf in
    Lwt_list.iter_s (function
        | `Tcp (src, dst, buf) -> tcp ~src ~dst buf
        | `Udp (src, dst, buf) -> udp ~src ~dst buf
        | `Default (proto, src, dst, buf) -> default ~proto ~src ~dst buf
      ) actions >>= fun () ->
    Lwt_list.iter_s (E.writev t.ethif) bufs

  let disconnect _ = (* TODO *)
    Lwt.return_unit

  let checksum = Ndpv6.checksum

  let get_source t ~dst = Ndpv6.select_source t.ctx dst

  let set_ip t ip =
    let now = C.time () in
    let ctx, bufs = Ndpv6.add_ip ~now t.ctx ip in
    t.ctx <- ctx;
    Lwt_list.iter_s (E.writev t.ethif) bufs

  let get_ip t =
    Ndpv6.get_ip t.ctx

  let set_ip_gateways t ips =
    let now = C.time () in
    let ctx = Ndpv6.add_routers ~now t.ctx ips in
    t.ctx <- ctx;
    Lwt.return_unit

  let get_ip_gateways t =
    Ndpv6.get_routers t.ctx

  let get_ip_netmasks t =
    Ndpv6.get_prefix t.ctx

  let set_ip_netmask t pfx =
    let now = C.time () in
    let ctx = Ndpv6.add_prefix ~now t.ctx pfx in
    t.ctx <- ctx;
    Lwt.return_unit

  type uipaddr = I.t
  let to_uipaddr ip = I.V6 ip
  let of_uipaddr ip = Some (I.to_v6 ip)

  let (>>=?) (x,f) g = match x with
    | Some x -> f x >>= g
    | None -> g ()

  let connect ?ip ?netmask ?gateways ethif =
    Printf.printf "IP6: Starting\n%!";
    let now = C.time () in
    let ctx, bufs = Ndpv6.local ~now (E.mac ethif) in
    let t = {ctx; ethif} in
    Lwt_list.iter_s (E.writev t.ethif) bufs >>= fun () ->
    (ip, set_ip t) >>=? fun () ->
    (netmask, Lwt_list.iter_s (set_ip_netmask t)) >>=? fun () ->
    (gateways, set_ip_gateways t) >>=? fun () ->
    Lwt.async (fun () -> start_ticking t);
    Lwt.return (`Ok t)

end
