open Apero
open Lwt

let read_all sock buf = 
  let open Lwt.Infix in
  let opos = IOBuf.position buf in 
  let tlen = (IOBuf.limit buf) - opos in   
  let rec do_read alen buf =     
    (try 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Buffer: %s" (IOBuf.to_string buf)) in 
      Lwt_bytes.read sock (IOBuf.to_bytes buf) (IOBuf.position buf) (IOBuf.limit buf)
    with 
    | e -> 
      let%lwt _ = Logs_lwt.warn (fun m -> m "Read failed on socket with %s" (Printexc.to_string e)) in
      Lwt.fail e
    )
    >>= fun n ->       
      let alen' = alen + n in 
      let%lwt _ = Logs_lwt.debug (fun m -> m "Net.read_all: Read %d out of %d bytes" alen' tlen) in
      if alen' = tlen then Lwt.return tlen 
      else 
        begin
          let b = IOBuf.set_position_unsafe (opos + alen') buf  in
          do_read alen' b
        end
  in 
    do_read 0 buf

(* let read sock buf = Lwt_bytes.read sock (IOBuf.to_bytes buf) (IOBuf.position buf) (IOBuf.limit buf) *)
let read = read_all

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

let safe_close fd =
  Lwt.catch
    (fun () -> Lwt_unix.close fd)
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
                    let x : Vle.t = Vle.shift_left (Vle.logand c  0x7fL)  bc in 
                    extract_length  (Vle.logor v x) (bc + 1) buf)
              ~fail_with:(fun e -> Lwt.fail @@ Exception e))            
      ~fail_with:(fun e -> Lwt.fail @@ Exception e)        
  in extract_length Vle.zero 0 buf 

  let write_vle sock buf vle =
    match encode_vle vle buf with 
    | Ok buf -> send sock buf
    | Error e -> Lwt.fail (Apero.Exception e)