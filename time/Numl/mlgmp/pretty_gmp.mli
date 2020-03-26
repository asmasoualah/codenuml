(*****************************************************
GNU MP interface for Objective CAML

v0.14
David.Monniaux@ens.fr
*****************************************************)

val base : int ref
val precision : int ref
val z : Format.formatter -> Gmp.Z.t -> unit
val q : Format.formatter -> Gmp.Q.t -> unit
val f : Format.formatter -> Gmp.F.t -> unit
val fr : Format.formatter -> Gmp.FR.t -> unit
