open Higher
open Monad

type ('a, 'm) population = (('a * float) list, 'm) app

module Population = Newtype2(struct type ('a, 'm) t = ('a, 'm) population end)

let hoistP f = {tau = fun x -> Population.inj (apply f (Population.prj x))}

module Pop (M : MonadSample) : sig
  include MonadInfer
  val spawn : int -> (m, m) nt
  val resample : (m, m) nt
  val run : ('a, m) app -> (('a * float) list,  M.m) app
end with type m = (M.m, Population.t) app =
struct
  type 'a t = ('a * float) list M.t

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

  type m = (M.m, Population.t) app

  let inj x = Population.inj (M.inj x)

  let prj x = M.prj (Population.prj x)

  let random =
    let open M in
    random >>= fun x ->
    return [(x, 1.0)]

  let score w =
    M.return [((), w)]

  let spawn' n c =
    let w = 1.0 /. float n in
    let rec replicate n x = if n == 0 then [] else x :: replicate (n-1) x in
    M.return (replicate n ((), w)) >>= fun () ->
    c

  let spawn n = {tau = fun x -> inj (spawn' n (prj x))}

  let resample' c = c

  let resample = {tau = fun x -> inj (resample' (prj x))}

  let run' x = x

  let run x = M.inj (run' (prj x))

end
