
open Hashtbl

type helper = string * string * int ref * (string, int) t * int ref

let get_event_fulltype (t1,t2,_,_,_) = 
  t1 ^ "< " ^ t2 ^" >"

let get_event_type (t1,t2,_,_,_) =
  t2

let get_proposition_hashtbl (_,_,_,tbl,_) =
  tbl

let get_proposition_counter (_,_,count,_,_) =
  count := !count + 1;
  !count

let get_inc_counter_test_cases (_,_,_,_,count) =
  count := !count + 1;
  !count

let get_counter_test_cases (_,_,_,_,count) =
  !count
