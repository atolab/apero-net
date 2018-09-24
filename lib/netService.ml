(* open Iobuf
open Atypes
open Common *)

type mtu = Unlimited | Limited of int

module Id = Id.Make (Int64)

module TxSession = struct
  type t = 
    { sock : Lwt_unix.file_descr
    ; close : unit -> unit Lwt.t
    ; mtu : mtu 
    ; sid : Id.t 
    ; wait_on_close : bool Lwt.t}
  let make ~close ~wait_on_close ~mtu sid sock = { sock; close; mtu; sid; wait_on_close }  
  let mtu s = s.mtu 
  let socket s = s.sock
  let close s = s.close ()
  let id s = s.sid
  let when_closed s = s.wait_on_close
end

module type S = sig         
  type io_service = TxSession.t -> unit -> unit Lwt.t
  type config 
  type t                      
  val make : config -> t
  val mtu : mtu                         
  val start : t -> io_service -> unit Lwt.t 
  val stop : t -> unit Lwt.t 
  val config : t -> config 
  val socket : t -> Lwt_unix.file_descr
  val establish_session : t -> Locator.Locator.t -> TxSession.t Lwt.t 
end
(* 

module TxSessionF = struct
  module type CoDec = sig 
    type t
    type msg
    val encode : t -> msg -> IOBuf.t -> (IOBuf.t, error) Result.t
    val decode : t -> IOBuf.t -> (msg * IOBuf.t, error) Result.t
  end
  module type S = sig 
    type t 
    type msg
    val mtu : t -> mtu
    val send : t -> msg -> unit Lwt.t
    val recv : t ->  msg Lwt.t
    val close : t -> unit Lwt.t 
  end
end *)