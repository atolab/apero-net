open Apero
open Lwt


let rec to_io_vecs bufs offset = 
   match bufs with 
   | [] -> []
   | hd :: tl -> 
     if IOBuf.available hd >= offset
     then 
       Lwt_bytes.{ iov_buffer = IOBuf.to_bytes hd; iov_offset = IOBuf.position hd + offset; iov_length = IOBuf.available hd - offset; }
       :: to_io_vecs tl 0
     else 
       to_io_vecs tl (offset - IOBuf.available hd)


let read_all sock buf = 
    let rec r_read_all_r sock buf offset len read =
      let%lwt _ = Logs_lwt.debug (fun m -> m "r_read_all off: %d len: %d" offset len) in
      let%lwt n = Lwt_bytes.read sock buf offset len in   
      if n <> 0 && n < len then r_read_all_r sock buf (offset + n) (len - n) (read + n)
      else Lwt.return (read + n)
    in
      let pos = IOBuf.position buf in 
      let len = (IOBuf.limit buf) - pos in 
      r_read_all_r sock (IOBuf.to_bytes buf) pos len 0

  

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
  let iovec = to_io_vecs bs 0 in
  match%lwt (Lwt_bytes.recv_msg ~socket:sock ~io_vectors:iovec) with
  | (0, _) -> fail @@ Exception (`ClosedSession `NoMsg)
  | _ as r -> return r

let send_vec sock bs =
  let iovec = to_io_vecs bs 0 in
  (Lwt_bytes.send_msg ~socket:sock ~io_vectors:iovec ~fds:[])

let send_vec_all sock bs = 
  let rec send_vec_all_from sock bs offset = 
    let total_bytes = (List.fold_left (fun a b -> a + (IOBuf.available b)) 0 bs) - offset in
    let iovec = to_io_vecs bs offset in
    let%lwt sent_bytes = Lwt_bytes.send_msg ~socket:sock ~io_vectors:iovec ~fds:[] in 
    if sent_bytes < total_bytes 
    then 
      let%lwt sent_bytes' = send_vec_all_from sock bs sent_bytes in
      Lwt.return (sent_bytes + sent_bytes')
    else 
      Lwt.return sent_bytes
  in 
  send_vec_all_from sock bs 0

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
