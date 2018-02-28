module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MonadSample = sig
  include Monad
  val random : float t
end

module type MonadInfer = sig
  include MonadSample
  val score : float -> unit t
end

module Model (M : MonadInfer) =
struct
  open M
  let model =
    random >>= fun x ->
    score 0.5 >>= fun _ ->
    return (x < 0.5)
end
