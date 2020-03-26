
(* This is a hack to install the pretty-printers in the customized toplevel. *)

(* Caml longidents. *)
type t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t

let _ = Topdirs.dir_install_printer Format.std_formatter 
	  (Obj.magic (Ldot (Lident "Creal_pp", "pp")) : 'a)
