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

include V1_LWT.STACK
  with type netif   = Ipaddr.t list
   and type ipaddr  = Ipaddr.t
   and type mode    = unit
   and type tcp   = Tcp_socket.t
   and type udp   = Udp_socket.t
   and module UDP = Udp_socket
   and module TCP = Tcp_socket
val connect : (netif, mode) V1_LWT.stack_config ->
  Udp_socket.t -> Tcp_socket.t -> [> `Ok of t | `Error of error ] Lwt.t
