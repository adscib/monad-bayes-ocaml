open Monad
open Sampler
open Population
open Sequential
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

module IR = Seq(Pop(Sam))
module Prog = Sprinkler(IR)

let results : (bool * float) list = SMC.smc {steps = 1; particles = 3} (IR.inj Prog.model)

let print_pair (b,w) = String.concat "" ["("; string_of_bool b; ", "; string_of_float w; ")"]
let main () = print_string (String.concat " " (List.map print_pair results)) ;;

main ()
