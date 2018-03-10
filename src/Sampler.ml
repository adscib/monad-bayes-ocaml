open Monad

module Sampler : MonadSample =
struct
  type 'a t = unit -> 'a
  let return x = fun () -> x
  let (>>=) s f = fun () -> f (s()) ()
  let random = fun () -> Random.float 1.0
end
