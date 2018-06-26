open Higher
open Monad

module Weighted : sig
  type ('m,'a) weighted

  type t
  val inj : ('m, 'a) weighted -> ('a, ('m, t) app) app
  val prj : ('a, ('m, t) app) app -> ('m, 'a) weighted

  val hoist : ('m, 'n) nt -> (('m, t) app, ('n, t) app) nt

  val run : ('a, ('m, t) app) app -> ('a * float, 'm) app
end =
struct
  type ('a,'m) weighted = ('a * float, 'm) app

  module W = Newtype2(struct type ('m, 'a) t = ('m, 'a) weighted end)

  type t = W.t
  let inj = W.inj
  let prj = W.prj

  let hoist f = {tau = fun x -> S.inj (apply f (S.prj x))}

  let run x = S.prj x
end

module W (M : MonadSample) : MonadInfer =
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

  let random =
    lift M.random

  let score w =
    M.return ((), w)
end
