let print_block(ptr) =
  let n = size(ptr) in
  print_string "block: [";
  let rec w(i) =
    if i > n then () else
    (print_int (ram[ptr+i-1]); print_string "|"; w(i+1))
  in
  w(0);
  print_string "]";
  print_newline () ;;



let print_stack(sp) =
  print_string "stack: [";
  let rec w(i) =
    if i >= sp then () else
    (print_int (ram[i]); print_string "|"; w(i+1))
  in
  w(stack_start);
  print_string "]";
  print_newline () ;;

