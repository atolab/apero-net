
type mtu = Unlimited | Limited of int

module Id = Id.Make (Int64)

module Session = struct
  type t = 
    { sock : Lwt_unix.file_descr
    ; close : unit -> unit Lwt.t
    ; mtu : mtu 
    ; sid : Id.t }

  let make ~close ~mtu sid sock = { sock; close; mtu; sid }  
  let mtu s = s.mtu 
  let socket s = s.sock
  let close s = s.close ()
end

module type S = sig         
  type io_service = Session.t -> unit -> unit Lwt.t
  type config 
  type t                      
  val make : config -> t
  val mtu : mtu                         
  val start : t -> io_service -> unit Lwt.t 
  val stop : t -> unit Lwt.t 
  val config : t -> config 
  val socket : t -> Lwt_unix.file_descr
  val open_session : t -> io_service -> Locator.Locator.t -> Session.t Lwt.t 
end
