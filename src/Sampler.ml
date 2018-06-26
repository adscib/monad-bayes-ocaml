open Higher
open Monad

module Sampler : sig
  type 'a sampler
  type m
  val inj : 'a sampler -> ('a, m) app
  val prj : ('a, m) app -> 'a sampler
  val run : ('a, m) app -> 'a
end =
struct
  type 'a sampler = unit -> 'a

  module S = Newtype1(struct type 'a t = 'a sampler end)

  type m = S.t
  let inj = S.inj
  let prj = S.prj

  let run s = S.prj s ()
end


module Sam : MonadSample =
struct
  type 'a t = 'a Sampler.sampler
  let return x = fun () -> x
  let (>>=) s f = fun () -> f (s()) ()
  let random = fun () -> Random.float 1.0
end
