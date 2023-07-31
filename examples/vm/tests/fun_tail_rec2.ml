open CustomStdlib ;;


let rec fact acc n = 
  print_int n;
  if n < 2 then acc else fact (acc*n) (n-1) ;;

print_int (fact 1 6);;
