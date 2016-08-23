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

type direct_ip_input = src:Ipaddr.t -> dst:Ipaddr.t -> Cstruct.t -> unit Lwt.t
module Make
    (Ip      : V1_LWT.IP)
    (Icmp    : V1_LWT.ICMP)
    (Udp     : V1_LWT.UDP with type ip = Ip.ipaddr)
    (Tcp     : V1_LWT.TCP with type ip = Ip.ipaddr) : sig
  include V1_LWT.STACK
     with type mode   = V1_LWT.direct_stack_config
     and type ipaddr  = Ip.ipaddr
  val connect :
    (* TODO: on_flow_arrival would more appropriately be a thing
     * that already had been passed to Tcp.t; it's not a property
     * of the whole stack *)
    ?on_flow_arrival:tcp_on_flow_arrival_callback ->
    Ip.t -> Icmp.t -> Udp.t -> Tcp.t ->
    [> `Ok of t | `Error of error ] Lwt.t
end
