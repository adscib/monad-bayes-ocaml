open Higher
open Monad

type 'a sampler = unit -> 'a

module Sampler = Newtype1(struct type 'a t = 'a sampler end)

module Sam : sig
  include MonadSample
  val run : ('a, m) app -> 'a
end with type m = Sampler.t =
struct
  type 'a t = 'a sampler
  let return x = fun () -> x
  let (>>=) s f = fun () -> f (s()) ()
  type m = Sampler.t
  let inj = Sampler.inj
  let prj = Sampler.prj
  let random = fun () -> Random.float 1.0
  let run s = Sampler.prj s ()
end
