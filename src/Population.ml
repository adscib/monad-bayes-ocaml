open Monad

module type MonadPop = sig
  module Pop : MonadInfer
  type 'a m
  val lift : 'a m -> 'a Pop.t
  val hoist : (('a * float) list m -> ('b * float) list m) -> 'a Pop.t -> 'b Pop.t
  val spawn : int -> 'a Pop.t -> 'a Pop.t
  val resample : 'a Pop.t -> 'a Pop.t
end

module Population (M : MonadInfer) : MonadPop =
struct

  type 'a p = ('a * float) list M.t

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

  module Pop : MonadInfer with type 'a t = 'a p =
  struct
    type 'a t = 'a p
    let return = return
    let (>>=) = (>>=)
    let random = random
    let score = score
  end

  type 'a m = 'a M.t

  let lift c =
    let open M in
    c >>= fun x ->
    return [(x, 1.0)]

  let hoist f c = f c

  let spawn n c =
    let w = 1.0 /. float n in
    let rec replicate n x = if n == 0 then [] else x :: replicate (n-1) x in
    M.return (replicate n ((), w)) >>= fun () ->
    c

  let resample c = c

end
