open Owl

open Numeric

module type Distribution = sig
  module Num : Numeric

  (* Distribution type. We probably also want to have it specify
  the domain type. It should come with functions for sampling
  and computing PDFs. *)
  type dist

  (* Various constructors for popular distributions *)
  val bernoulli : Num.real -> dist
end

module MakeDistribution (N : Numeric) : Distribution with module Num = N = struct

  module Num = N

  type dist = unit

  let bernoulli p = ()

end
