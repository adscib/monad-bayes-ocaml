module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type Sample = sig
  type 'a t
  val random : float t
end

module type Score = sig
  type 'a t
  val score : float -> unit t
end

module type MonadBayes = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val random : float t
  val score : float -> unit t
end

module Model (M : MonadBayes) =
struct
  open M
  let model =
    random >>= fun x ->
    score 0.5 >>= fun _ ->
    return (x < 0.5)
end
