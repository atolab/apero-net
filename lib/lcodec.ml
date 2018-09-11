open Locator
open Apero
open Apero.Result
open Apero.Result.Infix



let decode_locator buf = 
  decode_string buf 
  >>= (fun (s, buf) ->  return (Locator.of_string s, buf))

let encode_locator l = 
  Logs.debug (fun m -> m "Encoding Locator");
  encode_string (Locator.to_string l) 

let decode_locators buf =   
  decode_seq decode_locator buf
  >>= (fun (ols, buf) -> 
    let ls = Option.get @@ Option.flatten ols in 
    return (Locators.of_list ls,buf)
  )
  
let encode_locators ls buf = 
  Logs.debug (fun m -> m "Encoding Locators");
  let locs = Locators.to_list ls in
  encode_seq encode_locator locs buf


