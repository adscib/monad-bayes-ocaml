open Higher

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  type m
  val inj : 'a t -> ('a, m) app
  val prj : ('a, m) app -> 'a t
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
let (>>) f g = {tau = fun x -> g.tau (f.tau x)}
let repeat n f =
  let rec r k g x =
    if k == 0 then
      x
    else
      r (k-1) g (g x)
  in
      {tau = fun x -> r n f.tau x}
