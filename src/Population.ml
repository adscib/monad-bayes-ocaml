open Monad

module type MonadPop = sig
  module Pop : MonadInfer
  type 'a m
  type h
  val lift : 'a m -> 'a Pop.t
  val hoist : h -> Pop.nt
  val run : 'a Pop.t -> ('a * float) list m
  val spawn : int -> Pop.nt
  val resample : Pop.nt
end

module Population (M : MonadSample) : MonadPop with type h = M.nt with type 'a m = 'a M.t =
struct

  type 'a p = ('a * float) list M.t

  type np = {f : 'a. 'a p -> 'a p}

  let return x = M.return [(x, 1.0)]

  let rec mapM f zs =
    let open M in
    match zs with
    | [] -> return []
    | x::xs ->
        f x >>= fun y ->
        mapM f xs >>= fun ys ->
        return (y::ys)

  let (>>=) c k =
    let open M in
    c >>= fun xs ->
    let f (x,p) =
      k x >>= fun ys ->
      return (List.map (fun (y,q) -> (y, p *. q)) ys)
    in
    mapM f xs >>= fun yss ->
    return (List.concat yss)

  let random =
    let open M in
    random >>= fun x ->
    return [(x, 1.0)]

  let score w =
    M.return [((), w)]

  module Pop : MonadInfer with type 'a t = 'a p with type nt = np =
  struct
    type 'a t = 'a p
    let return = return
    let (>>=) = (>>=)
    type nt = np
    let apply tau m = tau.f m
    let random = random
    let score = score
  end

  type 'a m = 'a M.t

  type h = M.nt

  let lift c =
    let open M in
    c >>= fun x ->
    return [(x, 1.0)]

  let hoist tau = {f = fun x -> M.apply tau x}

  let run c = c

  let spawn n =
    { f = fun c ->
      let w = 1.0 /. float n in
      let rec replicate n x = if n == 0 then [] else x :: replicate (n-1) x in
      M.return (replicate n ((), w)) >>= fun () ->
      c
    }

  let resample = {f = fun c -> c}

end
