open Monad
open Sampler
open Population
open SMC

module type Model = sig
  type 'a m
  type output
  val model : output m
end

module Sprinkler (M : MonadInfer) : Model
  with type 'a m = 'a M.t with type output = bool =
struct
  type 'a m = 'a M.t
  type output = bool


  open M

  let bernoulli p = random >>= fun x -> return (x < p)

  let model =
    bernoulli 0.2 >>= fun rain ->
    bernoulli 0.1 >>= fun sprinkler ->
    let prob_lawn_wet =
      match (rain, sprinkler) with
        | (true , true ) -> 0.99
        | (true , false) -> 0.70
        | (false, true ) -> 0.90
        | (false, false) -> 0.01
    in
    score prob_lawn_wet >>= fun () ->
    return rain
end

module Alg = SMC(Sampler)
module Mod = Sprinkler(Alg.In)
module Out = Population(Sampler)

let sampler = Out.run (Alg.apply {steps = 1; particles = 3} Mod.model)
let results : (bool * float) list = sampler ()

let print_pair (b,w) = String.concat "" ["("; string_of_bool b; ", "; string_of_float w; ")"]
let main () = print_string (String.concat " " (List.map print_pair results)) ;;

main ()
