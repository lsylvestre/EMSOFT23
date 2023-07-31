open CustomStdlib ;;



let rec fact x = 
  print_int x;
  if x < 2 then 1 else (x * fact (x-1)) ;;

print_int (fact 6);;
