(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2011 Richard Mortier <richard.mortier@nottingham.ac.uk>
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
 *
 *)
open Lwt.Infix

module Raw(Netif : V1_LWT.NETWORK) = struct

  type 'a io = 'a Lwt.t
  type buffer = Cstruct.t
  type macaddr = Macaddr.t
  type netif = Netif.t

  type error = [
    | `Unknown of string
    | `Unimplemented
    | `Disconnected
  ]

  type t = {
    netif: Netif.t;
  }

  let id t = t.netif
  let mac t = Netif.mac t.netif

  (* some default logic -- an ethernet frame is interesting if
   * it looks like the sender intended for us to receive it *)
  let for_us t dest =
    Macaddr.compare dest (mac t) = 0 || not (Macaddr.is_unicast dest)

  let input ~arpv4 ~ipv4 ~ipv6 t frame =
    match Wire_structs.parse_ethernet_frame frame with
    | None -> Lwt.return_unit
    | Some (typ, _destination, payload) ->
        match typ with
        | Some Wire_structs.ARP -> arpv4 payload
        | Some Wire_structs.IPv4 -> ipv4 payload
        | Some Wire_structs.IPv6 -> ipv6 payload
        | None -> Lwt.return_unit (* TODO: default ethertype payload handler *)

  let write t frame =
    MProf.Trace.label "ethif.write";
    Netif.write t.netif frame

  let writev t bufs =
    MProf.Trace.label "ethif.writev";
    Netif.writev t.netif bufs

  let output t ~dst ~proto payload =
    let proto = Wire_structs.(match proto with
    | `ARP -> ARP
    | `IPv4 -> IPv4
    | `IPv6 -> IPv6
    ) in
    let proto_num = Wire_structs.ethertype_to_int proto in
    let ethernet_frame = Io_page.to_cstruct (Io_page.get 1) in
    let smac = Macaddr.to_bytes (mac t) in
    Wire_structs.set_ethernet_src smac 0 ethernet_frame;
    Wire_structs.set_ethernet_ethertype ethernet_frame proto_num;
    Wire_structs.set_ethernet_dst (Macaddr.to_bytes dst) 0 ethernet_frame;
    Netif.writev t.netif (ethernet_frame :: payload)

  let connect netif =
    MProf.Trace.label "ethif.connect";
    Lwt.return (`Ok { netif })

  let disconnect _ = Lwt.return_unit
end

module Make(Netif : V1_LWT.NETWORK) = struct
  include Raw(Netif)

  let input ~arpv4 ~ipv4 ~ipv6 t frame =
    MProf.Trace.label "ethif.input";
    let of_interest dest =
      Macaddr.compare dest (mac t) = 0 || not (Macaddr.is_unicast dest) in
    match Macaddr.of_bytes (Wire_structs.copy_ethernet_dst frame) with
    | Some dst when of_interest dst -> input ~arpv4 ~ipv4 ~ipv6 t frame
    | None -> Lwt.return_unit         (* Can't parse MAC address *)
    | exception _ -> Lwt.return_unit  (* e.g. frame too short - could maybe log at debug level here *)
end
