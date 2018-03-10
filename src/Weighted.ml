open Monad

module Weighted (M : MonadSample) : MonadInfer =
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
