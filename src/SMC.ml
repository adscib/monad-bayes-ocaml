open Monad
open Sampler
open Population
open Sequential

type smcparam = {steps : int; particles : int}

module SMCGen (M : MonadSample) =
struct
  module P = Pop(M)
  module S = Seq(P)

  let smc param =
    hoistS (P.spawn param.particles) >>
    repeat param.steps (hoistS P.resample >> S.advance) >>
    S.finish
end

module SMC =
struct
  module Alg = SMCGen (Sam)
  module P = Pop(Sam)

  let smc param model =
    Sam.run (P.run (apply (Alg.smc param) model))
end
