open CustomStdlib ;;


let rec sum acc n = 
  print_int n;
  if n < 1 then acc else sum (acc + n) (n-1) ;;

print_int (sum 0 6);;
