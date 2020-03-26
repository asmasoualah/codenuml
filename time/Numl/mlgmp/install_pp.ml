(*
 * ML GMP - Interface between Objective Caml and GNU MP
 * Copyright (C) 2001 David MONNIAUX
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2 published by the Free Software Foundation,
 * or any more recent version published by the Free Software
 * Foundation, at your choice.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *
 * As a special exception to the GNU Library General Public License, you
 * may link, statically or dynamically, a "work that uses the Library"
 * with a publicly distributed version of the Library to produce an
 * executable file containing portions of the Library, and distribute
 * that executable file under terms of your choice, without any of the
 * additional requirements listed in clause 6 of the GNU Library General
 * Public License.  By "a publicly distributed version of the Library",
 * we mean either the unmodified Library as distributed by INRIA, or a
 * modified version of the Library that is distributed under the
 * conditions defined in clause 3 of the GNU Library General Public
 * License.  This exception does not however invalidate any other reasons
 * why the executable file might be covered by the GNU Library General
 * Public License.
 *)

(* The following code is a hack! *)
type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t;;

List.iter
  (fun f -> Topdirs.dir_install_printer Format.std_formatter
             (Obj.magic (Ldot((Lident "Pretty_gmp"), f)))
  ) ["z"; "q"; "f"; "fr"];;
