open Apero
open Locator


val decode_locator : IOBuf.t -> (((Locator.t option) * IOBuf.t), error) result
val encode_locator : Locator.t -> IOBuf.t -> (IOBuf.t, error) result

val decode_locators : IOBuf.t -> ((Locators.t * IOBuf.t), error) result
val encode_locators : Locators.t -> IOBuf.t -> (IOBuf.t, error) result
