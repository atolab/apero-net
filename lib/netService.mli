
type mtu = Unlimited | Limited of int

module Id : Id.S

module Session : sig                 
  type t     
  val make : close:(unit -> unit Lwt.t) -> mtu:mtu -> Id.t -> Lwt_unix.file_descr -> t
  val mtu : t -> mtu                         
  val socket : t -> Lwt_unix.file_descr
  val close : t -> unit Lwt.t
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


