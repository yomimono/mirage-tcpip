(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

module type IP = sig
  include V1_LWT.IP
  val allocate: t -> src:ipaddr -> dst:ipaddr -> proto:[`ICMP | `TCP | `UDP] -> buffer * int
end

module Make(Ip:IP) : sig
  type id = {
    dst_port: int;               (* Remote TCP port *)
    dst: Ip.ipaddr;         (* Remote IP address *)
    src_port: int;              (* Local TCP port *)
    src: Ip.ipaddr;        (* Local IP address *)
  }

  val pp_id : Format.formatter -> id -> unit

  val xmit : ip:Ip.t -> id:id ->
    ?rst:bool -> ?syn:bool -> ?fin:bool -> ?psh:bool ->
    rx_ack:Sequence.t option -> seq:Sequence.t -> window:int ->
    options:Options.t list ->
    Cstruct.t -> unit Lwt.t
end
