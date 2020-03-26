
open Format

let precision = ref 20;;

let pp x = print_string (Creal.to_string x !precision);;

