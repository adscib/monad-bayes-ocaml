open Higher
open Monad

type ('a,'m) weighted = ('a * float, 'm) app

module Weighted = Newtype2(struct type ('a, 'm) t = ('a, 'm) weighted end)

let hoistW f = {tau = fun x -> Weighted.inj (apply f (Weighted.prj x))}

module W (M : MonadSample) : sig
  include MonadInfer
  val lift : 'a M.t -> 'a t
  val run : ('a, m) app -> ('a * float, M.m) app
end with type m = (M.m, Weighted.t) app =
struct
  type 'a t = ('a * float) M.t

  let return x = M.return (x, 1.0)

  let (>>=) c k =
    let open M in
    c >>= fun (x,p) ->
    k x >>= fun (y,q) ->
    return (y, p *. q)

  let lift c =
    let open M in
    c >>= fun x ->
    return (x, 1.0)

  type m = (M.m, Weighted.t) app

  let inj x = Weighted.inj (M.inj x)

  let prj x = M.prj (Weighted.prj x)

  let random =
    lift M.random

  let score w =
    M.return ((), w)

  let run' x = x

  let run x = M.inj (run' (prj x))
end
