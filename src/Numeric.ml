open Owl

(* Type-checked operations in log-domain to prevent underflow *)
module type LogFloat = sig
  type real
  type log_real

  val to_log_real : real -> log_real
  val from_log_real : log_real -> real
  val log_to_log_real : real -> log_real
  val log_from_log_real : log_real -> real

  module LogMath : sig
    val add : log_real -> log_real -> log_real
    val mul : log_real -> log_real -> log_real
  end

end

(* Types and functions for real number arithmetic *)
module type Numeric = sig
  (* Owl already defined many useful operations so
  we just include these here *)
  include Owl_algodiff_generic_sig.Sig
  include LogFloat with type real = t
end

module MakeNumeric (AD : Owl_algodiff_generic_sig.Sig) : Numeric = struct
  include AD

  type real = t
  type log_real = Log of real

  let to_log_real r = Log (Maths.log r)
  let from_log_real (Log l) = Maths.exp l
  let log_to_log_real r = Log r
  let log_from_log_real (Log l) = l

  module LogMath = struct
    let add a b = to_log_real (Maths.add (from_log_real a) (from_log_real b))
    let mul a b = log_to_log_real (Maths.add (log_from_log_real a) (log_from_log_real b))
  end
end
