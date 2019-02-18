open Apero

(** I/O related functions *)


val read : Lwt_unix.file_descr -> MIOBuf.t -> int Lwt.t
(** [read] at most (limit -pos) bytes out of the file descriptior in to
    the MIOBuf. Returns the  actual number of bytes read. *)

val read_all : Lwt_unix.file_descr -> MIOBuf.t -> int Lwt.t
(** [read] (limit -pos) bytes out of the file descriptior in to
    the MIOBuf. Returns the  actual number of bytes read. *)

val write : Lwt_unix.file_descr -> MIOBuf.t -> int Lwt.t
(** [write] at most the bytes between {e pos} and {e limit}. Returns the number
    of bytes actually written. *)

val write_all : Lwt_unix.file_descr -> MIOBuf.t -> int Lwt.t
(** [write] the bytes between {e pos} and {e limit}. Returns the number
    of bytes actually written. *)

val recv : ?flags:Unix.msg_flag list -> Lwt_unix.file_descr -> MIOBuf.t -> int Lwt.t
(** [recv] receives at most (limit -pos) bytes out of the file descriptior
    in to the MIOBuf. Returns the  actual number of bytes received.  *)

val send : ?flags:Unix.msg_flag list -> Lwt_unix.file_descr -> MIOBuf.t -> int Lwt.t
(** [send] send the bytes between {e pos} and {e limit}. Returns the number
    of bytes actually sent. *)

val recvfrom : ?flags:Unix.msg_flag list -> Lwt_unix.file_descr -> MIOBuf.t -> (int * Unix.sockaddr) Lwt.t

val sendto : ?flags:Unix.msg_flag list -> Lwt_unix.file_descr -> MIOBuf.t -> Unix.sockaddr -> int Lwt.t

val recv_vec : Lwt_unix.file_descr -> MIOBuf.t list -> (int * Unix.file_descr list) Lwt.t

val send_vec : Lwt_unix.file_descr -> MIOBuf.t list -> int Lwt.t

val send_vec_all : Lwt_unix.file_descr -> MIOBuf.t list -> int Lwt.t

val safe_close : Lwt_unix.file_descr -> unit Lwt.t

val read_vle : Lwt_unix.file_descr -> MIOBuf.t -> Vle.t Lwt.t 

val write_vle : Lwt_unix.file_descr -> MIOBuf.t -> Vle.t -> int Lwt.t 

val connect : Lwt_unix.file_descr -> Locator.Locator.t -> Lwt_unix.file_descr Lwt.t