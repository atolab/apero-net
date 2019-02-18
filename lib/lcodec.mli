open Apero
open Locator


val decode_locator : MIOBuf.t -> Locator.t option
val encode_locator : Locator.t -> MIOBuf.t -> unit

val decode_locators : MIOBuf.t -> Locators.t
val encode_locators : Locators.t -> MIOBuf.t -> unit
