open Monad
open Population
open Sequential



let rec applyN n f x =
  if n == 0 then
    x
  else
    applyN (n-1) f (f x)

let smc k n model =
  let open Population in
  let open Sequential in
  finish (applyN k (fun m -> advance (resample m)) (spawn n model))
