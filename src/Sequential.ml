open Monad

module Seq (M : MonadInfer) : MonadInfer =
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

  let apply tau c = tau.f c

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
end
(*
module type MonadSeq = sig
  module Seq : MonadInfer
  type 'a m
  type h
  val lift : 'a m -> 'a Seq.t
  val hoist : h -> Seq.nt
  val advance : Seq.nt
  val finish : 'a Seq.t -> 'a m
end

module Sequential (M : MonadInfer) : MonadSeq with type h = M.nt with type 'a m = 'a M.t =
struct
  type ('a, 'b) either = Left of 'a | Right of 'b
  type 'a seq = Seq of ('a, unit -> 'a seq) either M.t
  let unseq (Seq c) = c

  type ns = {f : 'a. 'a seq -> 'a seq}

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

  let apply tau c = tau.f c

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

  module Seq : MonadInfer with type 'a t = 'a seq with type nt = ns =
  struct
    type 'a t = 'a seq
    let return = return
    let (>>=) = (>>=)
    type nt = ns
    let apply = apply
    let random = random
    let score = score
  end

  type 'a m = 'a M.t
  type h = M.nt

  let hoist tau : ns = {f = fun c -> Seq (M.apply tau (unseq c))}

  let advance =
    {f = fun c ->
      Seq (
        let open M in
        unseq c >>= fun t ->
        match t with
        | Left x -> M.return (Left x)
        | Right r -> unseq (r ())
        )
    }

  let rec finish c =
    let open M in
    unseq c >>= fun t ->
    match t with
    | Left x -> M.return x
    | Right r -> finish (r ())
end *)
