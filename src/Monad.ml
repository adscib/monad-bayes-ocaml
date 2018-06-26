open Higher

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

type ('m,'n) nt = {tau : 'a. ('a, 'm) app -> ('a, 'n) app}
let apply f x = f.tau x
