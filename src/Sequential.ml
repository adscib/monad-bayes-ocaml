open Higher
open Monad

type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'm) sequential = Sequential of (('a, unit -> ('a, 'm) sequential) either, 'm) app

module Sequential = Newtype2(struct type ('a, 'm) t = ('a, 'm) sequential end)

let hoistS (f : ('m, 'm) nt) : (('m, Sequential.t) app, ('m, Sequential.t) app) nt =
  {tau = fun x -> Sequential.inj ((fun (Sequential x) -> Sequential (apply f x)) (Sequential.prj x))}

module Seq (M : MonadInfer) : sig
  include MonadInfer
  val advance : (m, m) nt
  val finish :  (m, M.m) nt
end with type m = (M.m, Sequential.t) app =
struct
  type 'a t = Seq of ('a, unit -> 'a t) either M.t
  let unseq (Seq c) = c

  let return x =
    Seq (M.return (Left x))

  let rec (>>=) c k = Seq (
    M.(>>=) (unseq c) (fun t ->
      match t with
      | Left x -> unseq (k x)
      | Right r -> M.return (Right (fun () -> (r () >>= k)))
      )
    )

  let lift c = Seq (
    let open M in
    c >>= fun x ->
    return (Left x)
    )

  type m = (M.m, Sequential.t) app

  let inj x =
    let rec inj_seq (Seq c) = Sequential (
      M.inj (let open M in
             c >>= fun y ->
             return (match y with
                     | Left a -> Left a
                     | Right z -> Right (fun () -> inj_seq (z ()))))
     )
    in
      Sequential.inj (inj_seq x)

  let prj x =
    let rec prj_seq (Sequential c) = Seq (
      let open M in
         M.prj c >>= fun y ->
         return (match y with
                 | Left a -> Left a
                 | Right z -> Right (fun () -> prj_seq (z ())))
      )
    in
      prj_seq (Sequential.prj x)

  let random =
    lift M.random

  let suspend =
    Seq (M.return (Right return))

  let score w =
    lift (M.score w) >>= fun () ->
    suspend

  let advance' (Seq c) =
    Seq (
      let open M in
      c >>= fun t ->
      match t with
      | Left x -> M.return (Left x)
      | Right r -> unseq (r ())
      )

  let advance = {tau = fun x -> inj (advance' (prj x))}

  let rec finish' (Seq c) =
    let open M in
    c >>= fun t ->
    match t with
    | Left x -> M.return x
    | Right r -> finish' (r ())

  let finish = {tau = fun x ->  M.inj (finish' (prj x))}
end
