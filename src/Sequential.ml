open Monad

module Sequential (M : MonadInfer) : MonadInfer =
struct
  type ('a, 'b) either = Left of 'a | Right of 'b
  type 'a seq = Seq of ('a, unit -> 'a seq) either M.t
  let unseq (Seq c) = c

  type 'a t = 'a seq

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

  let random =
    lift M.random

  let suspend =
    Seq (M.return (Right return))

  let score w =
    lift (M.score w) >>= fun () ->
    suspend

  let hoist f c = Seq (f (unseq c))

  let advance c = Seq (
    let open M in
    unseq c >>= fun t ->
    match t with
    | Left x -> M.return (Left x)
    | Right r -> unseq (r ())
    )

  let rec finish c =
    let open M in
    unseq c >>= fun t ->
    match t with
    | Left x -> M.return x
    | Right r -> finish (r ())
end
