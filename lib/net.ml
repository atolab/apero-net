open Apero
open Lwt


let read_all sock buf = 
    let rec r_read_all_r tlen sock buf offset len  =
      let%lwt _ = Logs_lwt.debug (fun m -> m "r_read_all off: %d len: %d" offset len) in
      let%lwt n = Lwt_bytes.read sock buf offset len in   
      if n < len then r_read_all_r tlen sock buf (offset + n) (len - n) 
      else Lwt.return tlen 
    in
      let pos = IOBuf.position buf in 
      let len = (IOBuf.limit buf) - pos in 
      r_read_all_r len sock (IOBuf.to_bytes buf) pos len

  

(* let read sock buf = Lwt_bytes.read sock (IOBuf.to_bytes buf) (IOBuf.position buf) (IOBuf.limit buf) *)
let read = read_all

let write_all sock buf = 
    let rec r_write_all_r tlen sock buf offset len  =
      let%lwt _ = Logs_lwt.debug (fun m -> m "r_write_all off: %d len: %d" offset len) in
      let%lwt n = Lwt_bytes.write sock buf offset len in   
      if n < len then r_write_all_r tlen sock buf (offset + n) (len - n) 
      else Lwt.return tlen 
    in
      let pos = IOBuf.position buf in 
      let len = (IOBuf.limit buf) - pos in 
      r_write_all_r len sock (IOBuf.to_bytes buf) pos len

let write sock buf = Lwt_bytes.write sock (IOBuf.to_bytes buf) (IOBuf.position buf) (IOBuf.limit buf)

let recv ?(flags=[]) sock buf =
  match%lwt Lwt_bytes.recv sock (IOBuf.to_bytes buf) (IOBuf.position buf) (IOBuf.limit buf) flags with
  | 0 -> fail @@ Exception (`ClosedSession `NoMsg)
  | n -> return n


let send ?(flags=[]) sock buf = 
  Lwt_bytes.send sock (IOBuf.to_bytes buf) (IOBuf.position buf) (IOBuf.limit buf) flags

let recvfrom ?(flags=[]) sock buf =
  match%lwt Lwt_bytes.recvfrom sock (IOBuf.to_bytes buf) (IOBuf.position buf) (IOBuf.limit buf) flags with
  | (0, _) -> fail @@ Exception (`ClosedSession `NoMsg)
  | _ as r -> return r

let sendto ?(flags=[]) sock buf addr = 
  Lwt_bytes.sendto sock (IOBuf.to_bytes buf) (IOBuf.position buf) (IOBuf.limit buf) flags addr


let recv_vec sock bs =
  let iovec = List.map (fun buf -> IOBuf.to_io_vector buf) bs in
  match%lwt (Lwt_bytes.recv_msg ~socket:sock ~io_vectors:iovec) with
  | (0, _) -> fail @@ Exception (`ClosedSession `NoMsg)
  | _ as r -> return r

let send_vec sock bs =
  let iovec = List.map (fun buf -> IOBuf.to_io_vector buf) bs in
  (Lwt_bytes.send_msg ~socket:sock ~io_vectors:iovec ~fds:[])

type net_send_vec_ctx = {total_bytes: int; mutable sent_bytes: int; mutable bs: IOBuf.t list; mutable idx: int}
let send_vec_all sock bs = 
  
  let rec recompute_iovec ctx bs = 
    match bs with 
    | h::tl ->
      let blen = (IOBuf.limit h) in 
      if ctx.sent_bytes >= blen + ctx.idx then  
        begin
          ctx.idx <- ctx.idx + blen;
          recompute_iovec ctx tl
        end 
      else 
        let pos = ctx.sent_bytes - ctx.idx in 
        let b = IOBuf.set_position_unsafe pos h in 
        let rs = b::tl  in         
        rs
    | [] -> []
  in  
  
  let rec send_more ctx = 
    if ctx.sent_bytes < ctx.total_bytes then 
      begin 
        let bs = recompute_iovec ctx ctx.bs in 
        ctx.bs <- bs ; 
        let iovec = List.map (fun buf -> IOBuf.to_io_vector buf) bs in
        let%lwt sent_bytes = Lwt_bytes.send_msg ~socket:sock ~io_vectors:iovec ~fds:[] in 
        ctx.sent_bytes <- ctx.sent_bytes + sent_bytes ;
        send_more ctx
      end
    else 
      begin
        Lwt.return ctx.sent_bytes 
      end 

  in 
  
  let total_bytes = List.fold_left (fun a b -> a + IOBuf.limit b) 0 bs in
  let iovec = List.map (fun buf -> IOBuf.to_io_vector buf) bs in
  let%lwt sent_bytes = Lwt_bytes.send_msg ~socket:sock ~io_vectors:iovec ~fds:[] in 
  if sent_bytes < total_bytes then 
    begin
      let ctx = {total_bytes; sent_bytes; idx = 0; bs} in 
      send_more ctx
    end
  else 
    Lwt.return sent_bytes


let safe_close fd =
  Lwt.catch
    (fun () -> 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Closing socket...") in 
      Lwt_unix.close fd)
    (fun _ -> Lwt.return_unit)

let read_vle sock buf = 
  let rec extract_length (v:Vle.t) (bc:int) (buf: IOBuf.t) =       
    Result.try_get 
      ~on:(IOBuf.reset_with 0 1 buf)
      ~run:(fun buf ->
          match%lwt recv sock buf with
          | 0 -> Lwt.fail @@ Exception (`ClosedSession (`Msg "Peer closed the session unexpectedly"))
          | _ ->
            Result.try_get 
              ~on:(IOBuf.get_char buf)
              ~run:(fun (b, buf) -> 
                  match Vle.of_char b with
                  | c when c <= 0x7fL -> Lwt.return (Vle.logor v (Vle.shift_left c (bc * 7)))
                  | c  -> 
                    let x : Vle.t = Vle.shift_left (Vle.logand c  0x7fL)  (bc * 7) in 
                    extract_length  (Vle.logor v x) (bc + 1) buf)
              ~fail_with:(fun e -> Lwt.fail @@ Exception e))            
      ~fail_with:(fun e -> Lwt.fail @@ Exception e)        
  in extract_length Vle.zero 0 buf 

  let write_vle sock buf vle =
    match encode_vle vle buf with 
    | Ok buf -> send sock buf
    | Error e -> Lwt.fail (Apero.Exception e)

let connect socket locator = 
  let saddr = match locator with 
    | Locator.Locator.UdpLocator ul -> 
      Endpoint.IpEndpoint.to_sockaddr @@ Iplocator.UdpLocator.endpoint ul   
    | Locator.Locator.TcpLocator tl -> 
      Endpoint.IpEndpoint.to_sockaddr @@ Iplocator.TcpLocator.endpoint tl
  in Lwt_unix.connect socket saddr >>= fun () -> Lwt.return socket
