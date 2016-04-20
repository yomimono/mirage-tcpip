let echo_request ?payload id seq =
  let open Icmpv4_wire in
  let header = Cstruct.create sizeof_icmpv4 in
  set_icmpv4_ty header 0x08;
  set_icmpv4_code header 0x00;
  set_icmpv4_csum header 0x0000;
  set_icmpv4_seq header seq;
  set_icmpv4_id header id;
  let packet = match payload with
    | Some payload -> Cstruct.append header payload
    | None -> header
  in
  set_icmpv4_csum header (Tcpip_checksum.ones_complement_list [ packet ]);
  packet
