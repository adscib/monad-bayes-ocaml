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
