open Distribution

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MonadSample = sig
  include Monad
  module Dist : Distribution
  val sample : Dist.dist -> Dist.Num.real t
end

module type MonadInfer = sig
  include MonadSample
  val score : Dist.Num.log_real -> unit t
end
