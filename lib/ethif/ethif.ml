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
open Result

let src = Logs.Src.create "ethif" ~doc:"Mirage Ethernet"
module Log = (val Logs.src_log src : Logs.LOG)

module Common(Netif : V1_LWT.NETWORK) = struct

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

  let mac t = Netif.mac t.netif

  let input ~arpv4 ~ipv4 ~ipv6 t frame =
    let open Ethif_packet in
    MProf.Trace.label "ethif.input";
    let of_interest dest =
      Macaddr.compare dest (mac t) = 0 || not (Macaddr.is_unicast dest)
    in
    match Unmarshal.of_cstruct frame with
    | Ok (header, payload) when of_interest header.destination ->
      begin
        let open Ethif_wire in
        match header.ethertype with
        | ARP -> arpv4 payload
        | IPv4 -> ipv4 payload
        | IPv6 -> ipv6 payload
      end
    | Ok _ -> Lwt.return_unit
    | Error s -> Log.debug (fun f -> f "Dropping Ethernet frame: %s" s);
      Lwt.return_unit

  let write t frame =
    MProf.Trace.label "ethif.write";
    Netif.write t.netif frame

  let writev t bufs =
    MProf.Trace.label "ethif.writev";
    Netif.writev t.netif bufs

  let connect netif =
    MProf.Trace.label "ethif.connect";
    let t = { netif } in
    Log.info (fun f -> f "Connected Ethernet interface %s" (Macaddr.to_string (mac t)));
    Lwt.return (`Ok t)

  let disconnect t =
    Log.info (fun f -> f "Disconnected Ethernet interface %s" (Macaddr.to_string (mac t)));
    Lwt.return_unit
end

module Make(Netif : V1_LWT.NETWORK) = struct
  module C = Common(Netif)
  include C

  let input ~arpv4 ~ipv4 ~ipv6 t frame =
    let open Ethif_packet in
    MProf.Trace.label "ethif.input";
    let of_interest dest =
      Macaddr.compare dest (mac t) = 0 || not (Macaddr.is_unicast dest)
    in
    match Unmarshal.of_cstruct frame with
    | Ok (header, payload) when of_interest header.destination ->
      begin
        let open Ethif_wire in
        match header.ethertype with
        | ARP -> arpv4 payload
        | IPv4 -> ipv4 payload
        | IPv6 -> ipv6 payload
      end
    | Ok _ -> Lwt.return_unit
    | Error s -> Log.debug (fun f -> f "Dropping Ethernet frame: %s" s);
      Lwt.return_unit

end

module Raw(Netif : V1_LWT.NETWORK) = struct
  module C = Common(Netif)
  include C
end
