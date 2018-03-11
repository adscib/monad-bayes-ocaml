module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  type nt
  val apply : nt -> 'a t -> 'a t
end

module type MonadSample = sig
  include Monad
  val random : float t
end

module type MonadInfer = sig
  include MonadSample
  val score : float -> unit t
end

module type Model = sig
  type 'a m
  type output
  val model : output m
end

module Example (M : MonadInfer) : Model
  with type 'a m = 'a M.t with type output = bool =
struct
  type 'a m = 'a M.t
  type output = bool

  open M
  let model =
    random >>= fun x ->
    score 0.5 >>= fun _ ->
    return (x < 0.5)
end
