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

let src = Logs.Src.create "Wire" ~doc:"Mirage TCP Wire module"
module Log = (val Logs.src_log src : Logs.LOG)

let count_tcp_to_ip = MProf.Counter.make ~name:"tcp-to-ip"

module Make (Ip:V1_LWT.IP) = struct
  type id = {
    remote_port: int;             (* Remote TCP port *)
    remote_address: Ip.ipaddr;    (* Remote IP address *)
    local_port: int;              (* Local TCP port *)
    local_address: Ip.ipaddr;     (* Local IP address *)
  }

  let pp_id fmt id =
    let uip = Ip.to_uipaddr in
    Format.fprintf fmt "remote %a,%d to local %a, %d"
      Ipaddr.pp_hum (uip id.remote_address) id.remote_port
      Ipaddr.pp_hum (uip id.local_address) id.local_port

  let xmit ~ip ~id:{ local_port; remote_port; remote_address; _ }
      ?(rst=false) ?(syn=false) ?(fin=false) ?(psh=false)
      ~rx_ack ~seq ~window ~options payload =
    let (ack, ack_number) = match rx_ack with
      | None -> (false, Sequence.zero)
      | Some n -> (true, n)
    in
    let header = {
        sequence = seq; Tcp_packet.ack_number; window;
        urg = false; ack; psh; rst; syn; fin;
        options;
        src_port = local_port; dst_port = remote_port;
      } in
    (* Make a TCP/IP header frame *)
    let frame, header_len = Ip.allocate_frame ip ~dst:remote_address ~proto:`TCP in
    (* Shift this out by the combined ethernet + IP header sizes *)
    let tcp_buf = Cstruct.shift frame header_len in
    let pseudoheader = Ip.pseudoheader ip ~dst:remote_address ~proto:`TCP
      (Tcp_wire.sizeof_tcp + Options.lenv options + Cstruct.len payload) in
    match Tcp_packet.Marshal.into_cstruct header tcp_buf ~pseudoheader ~payload with
    | Result.Error s ->
      Log.info (fun fmt -> fmt "Error transmitting TCP packet: %s" s);
      Lwt.return_unit
    | Result.Ok len ->
      let frame = Cstruct.set_len frame (header_len + len) in
      MProf.Counter.increase count_tcp_to_ip (Cstruct.len payload + (if syn then 1 else 0));
      Ip.write ip frame payload

end
