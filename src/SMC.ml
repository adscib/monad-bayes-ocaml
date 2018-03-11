open Monad
open Population
open Sequential

module type InferenceT = sig
 module In : MonadInfer
 module Out : MonadInfer
 type param
 val apply : param -> 'a In.t -> 'a Out.t
end

type smcparam = {steps : int; particles : int}

module SMC (M : MonadInfer) : InferenceT with type param = smcparam
  with module Out = Population(M).Pop =
struct
  module P = Population(M)
  module S = Sequential(P.Pop)
  module In = S.Seq
  module Out = P.Pop

  type param = smcparam

  let rec applyN n f x =
    if n == 0 then
      x
    else
      applyN (n-1) f (f x)

  let apply {steps = k; particles = n} (model) =
    S.finish (applyN k (fun x -> S.Seq.apply S.advance
                                (S.Seq.apply (S.hoist P.resample) x))
                       (S.Seq.apply (S.hoist (P.spawn n)) model))
end
