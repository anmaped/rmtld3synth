(*pp camlp4o `ocamlfind query type_conv`/pa_type_conv.cma  `ocamlfind query pa_sexp_conv`/pa_sexp_conv.cma  -I `ocamlfind query sexplib` -I `ocamlfind query pa_sexp_conv` *)


open List
open Batteries

open Sexplib
open Sexplib.Conv

open Rmtld3
open Rmtld3synth_helper

(* parsing latex equations *)

let matches s =
  let chars = String.explode s in
  fun c -> List.mem c chars;;

let space = matches " \t\n\r"
and punctuation = matches "()[]{},"
and symbolic = matches "\\^_"
and numeric = matches "0123456789"
and alphanumeric = matches "abcdefghijklmnopqrstuvwxyz'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";;

let rec lexwhile prop inp =
  match inp with
    c::cs when prop c -> let tok,rest = lexwhile prop cs in (Char.escaped c)^tok,rest
  | _ -> "",inp;;

let rec lex inp =
  match snd (lexwhile space inp) with
    [] -> []
  | c::cs -> let prop = if alphanumeric(c) then alphanumeric
               else if symbolic(c) then symbolic
               else fun c -> false in
    let toktl,rest = lexwhile prop cs in
    ((Char.escaped c)^toktl)::lex rest;;


type tokens = string list with sexp

(* intermediate representation *)
type op = Leq of unit | Geq of unit | Eq of unit | Less of unit | Greater of unit | N of unit with sexp
type intermediate_ltx_pm =
    PEmpty of unit
  | POp of op * intermediate_ltx_tm list
  | PList of intermediate_ltx_pm list
  | Pvar of string
and intermediate_ltx_tm = 
    TTimes of intermediate_ltx_tm list
  | TPlus of intermediate_ltx_tm list
  | TMinus of intermediate_ltx_tm list
  | TFrac of intermediate_ltx_tm * intermediate_ltx_tm
  | TInt of intermediate_ltx_tm * intermediate_ltx_fm
  | TVal of int
  | TVar of string * intermediate_ltx_tm
  | TFun of string * intermediate_ltx_tm list * intermediate_ltx_tm list
  | TEmpty of unit
and intermediate_ltx_fm =
    FIneq of  (intermediate_ltx_fm * op) list
  | Fland of intermediate_ltx_fm list
  | Flor of intermediate_ltx_fm list
  | FNot of intermediate_ltx_fm
  | FExists of intermediate_ltx_tm * intermediate_ltx_fm
  | Always of intermediate_ltx_pm * intermediate_ltx_fm
  | Eventually of intermediate_ltx_pm * intermediate_ltx_fm
  | ULess of intermediate_ltx_pm * intermediate_ltx_fm * intermediate_ltx_fm
  | FImplies of intermediate_ltx_fm * intermediate_ltx_fm
  | FIsol of intermediate_ltx_fm
  | FTerm of intermediate_ltx_tm list
  | Strr of string list
  | FBreak of intermediate_ltx_fm list
  | FVar of string
with sexp

type intermediate_ltx_tm_list = intermediate_ltx_tm list with sexp
type intermediate_ltx_fm_list = intermediate_ltx_fm list with sexp

let chk_alphanum a = List.for_all alphanumeric (String.explode a)
let chk_num a = List.for_all numeric (String.explode a)

(*
let rec parse_latexeq_pm (l: string list) (feed: intermediate_ltx_pm) : intermediate_ltx_pm * string list =
  match l with
  | "{" :: r    -> parse_latexeq_pm r feed
  | "}" :: r    -> (feed,r)
  | "(" :: r    -> parse_latexeq_pm r feed
  | ")" :: r    -> (feed,r)
  | "<"  :: r   -> let pm,rlst = parse_latexeq_tm' r []
    in (POp(Less(), pm),rlst)
  | "leq" :: r  -> let pm,rlst = parse_latexeq_tm' r []
    in (POp(Leq(), pm),rlst)
  | "="  :: r   -> let pm,rlst = parse_latexeq_tm' r []
    in (POp(Eq(), pm),rlst)
  (*| x :: r      -> (
                     match feed with
                       PList(a) -> parse_latexeq_pm r (PList(a@[Pvar(x)])) (* get back here... *)
                     | PEmpty() -> parse_latexeq_pm r (PList([Pvar(x)]))

                   )*)
  | _ -> raise (Failure ("bad expression pm: "^(Sexp.to_string_hum (sexp_of_tokens l))))

and parse_latexeq_tm' (l: string list) (feed: intermediate_ltx_tm list) : intermediate_ltx_tm list * string list =
  match l with
    [] 				   -> (feed,[])
  | "\\\\" :: r            -> begin
      match r with 
        "int" :: r         -> let pf,rlst = parse_latexeq_tm' r [] in
        let pf2,rlst2 = parse_latexeq_tm' rlst []
        in ([TInt(List.hd pf,List.hd pf2)], rlst2) 

      | "times" :: r       -> parse_latexeq_tm' r feed
      | "frac"  :: r       -> let pf,rlst = parse_latexeq_tm' r feed in
        let pf2,rlst2 = parse_latexeq_tm' rlst feed
        in parse_latexeq_tm' rlst2 ([TFrac(List.hd pf,List.hd pf2)])

      | a :: r when chk_alphanum a -> parse_latexeq_tm' (a::r) feed

      | _ -> raise (Failure ("bad term var: " ^(Sexp.to_string_hum (sexp_of_tokens l))))
    end

  | "+" :: r             -> let pf,rlst = parse_latexeq_tm' r [] in ([TPlus([List.hd feed; List.hd pf])], rlst)
  | "-" :: r             -> let pf,rlst = parse_latexeq_tm' r [] in ([TMinus([List.hd feed; List.hd pf])], rlst)

  | "^" :: r             -> parse_latexeq_tm' r feed (* TODO *)
  | "_" :: r             -> parse_latexeq_tm' r feed (* TODO *)
  | "{" :: r             -> let pf,rlst = parse_latexeq_tm' r feed in (pf,rlst)
  | "}" :: r             -> (feed, r)
  | "(" :: r             -> let pf,rlst = parse_latexeq_tm' r feed in parse_latexeq_tm' rlst pf
  | ")" :: r             -> (feed, r)

  | a :: ("_" :: ("{" :: r)) when chk_alphanum a
    -> let tm,rlst = parse_latexeq_tm' ("{"::r) []
    in
    if (List.hd rlst) = "(" then
      let pm2,rlst2 = parse_latexeq_tm' rlst [] in
      parse_latexeq_tm' rlst2 ([TFun(a, tm, pm2 )]) 
    else
      parse_latexeq_tm' rlst ([TVar(a, List.hd tm )]) (* feed is discarded *)
  | a :: ("_" :: (b :: r))   when chk_alphanum a && chk_alphanum b
    ->  parse_latexeq_tm' r  (feed@[TVar(a^b, TEmpty() )]) (* feed is discarded *)
  | a :: r when chk_alphanum a -> parse_latexeq_tm' r (try [TVal(int_of_string a)] with | _ -> feed@[TVar(a,TEmpty())])

  (* term skip keyword *)
  | "---" :: r            ->  parse_latexeq_tm' r feed

  | _        -> raise (Failure ("bad term: "^(Sexp.to_string_hum (sexp_of_tokens l))))
*)

let propagate feed =
    match feed with
    | (FBreak(x))::[] -> (x, [FBreak([])])
    | _ -> (feed,[])

let rec match_feed feed : string list = match feed with Strr(a) -> a | FIsol(x) -> ["("]@(match_feed x)@[")"]  | _ -> raise (Failure ("bad expression for feed: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm feed))))

type parenthesis = LF | LT
let parenthesis_stack = Stack.create ()

type level = FM | TM
let nested_stack = Stack.create ()

let rec parse_latexeq_pm (l: string list) (feed: intermediate_ltx_pm list) : intermediate_ltx_pm list * string list =
  match l with
  | "{" :: r    -> parse_latexeq_pm r feed
  | "}" :: r    -> (feed,r)
  | "(" :: r    -> parse_latexeq_pm r feed
  | ")" :: r    -> (feed,r)
  | "<"  :: r   -> let pm,rlst = parse_latexeq_tm' r []
    in ([POp(Less(), pm)],rlst)
  | "leq" :: r  -> let pm,rlst = parse_latexeq_tm' r []
    in ([POp(Leq(), pm)],rlst)
  | "="  :: r   -> let pm,rlst = parse_latexeq_tm' r []
    in ([POp(Eq(), pm)],rlst)
  (*| x :: r      -> (
                     match feed with
                       PList(a) -> parse_latexeq_pm r (PList(a@[Pvar(x)])) (* get back here... *)
                     | PEmpty() -> parse_latexeq_pm r (PList([Pvar(x)]))

                   )*)
  | _ -> raise (Failure ("bad expression pm: "^(Sexp.to_string_hum (sexp_of_tokens l))))

and parse_latexeq_tm' (l: string list) (feed: intermediate_ltx_tm list) : intermediate_ltx_tm list * string list = (* empty out: ([],[]) *)
  verb_m 3 (fun _ -> print_endline ("#tm_feed -> "^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_tm_list feed )) ) );
  verb_m 3 (fun _ -> print_endline ("#tm_l -> "^ (Sexp.to_string_hum (sexp_of_tokens l)) ) );
  match l with
    []           -> (feed,[])
  | "\\\\" :: r                   -> begin
      match r with 
      | "int" :: "^" :: r ->
        Stack.push FM nested_stack;
        let pf,rlst = if (hd r) = "{" then parse_latexeq_tm' (tl r) [] else ([TVal(int_of_string (hd r))], tl (tl r)) in (* TODO: this can be tvar as well *)
        let pf2,rlst2 = parse_latexeq_eq' rlst [FBreak([])]
        (* cleanup fbreak *)
        in let pf2,_ = propagate pf2
        in let _ = Stack.pop nested_stack
        in ([TInt(hd pf, hd pf2)], rlst2)

      (* skip keywords *)
      | "left" :: r       -> parse_latexeq_tm' r feed
      | "right" :: r      -> parse_latexeq_tm' r feed

      | _                 ->
        verb_m 3 (fun _ -> print_endline ("\\var -> "^(Sexp.to_string_hum (sexp_of_tokens l))) );
        (feed,l)
    end

  | "+" :: r                      ->
    let pf,rlst = parse_latexeq_tm' r [] in ([TPlus([List.hd feed; List.hd pf])], rlst)
  
  | "-" :: r                      ->
    let pf,rlst = parse_latexeq_tm' r [] in ([TMinus([List.hd feed; List.hd pf])], rlst)

  | "*" :: r                      ->
    let pf,rlst = parse_latexeq_tm' r [] in ([TTimes([List.hd feed; List.hd pf])], rlst)

  | "{" :: r                      -> let pf,rlst = parse_latexeq_tm' r feed in (pf,rlst)
  | "}" :: r                      -> (feed, r)
  | "(" :: r                      ->
    Stack.push LT parenthesis_stack;
    let pf,rlst = parse_latexeq_tm' r feed in parse_latexeq_tm' rlst pf
  | ")" :: r                      ->
    if Stack.top parenthesis_stack = LT then let _ = Stack.pop parenthesis_stack in (feed, r) else (feed, l)
    (* we need to know if ")" is from equations or terms then return (feed, l) *)

  | x :: r when chk_num x      -> (* feed is discarded *)
    parse_latexeq_tm' r [TVal(int_of_string x)]

  | x :: r when chk_alphanum x ->
    parse_latexeq_tm' r [TVar((x,TEmpty()))]

  | _                             ->
    (feed,l)

and parse_latexeq_eq' (l: string list) (feed: intermediate_ltx_fm list) : intermediate_ltx_fm list * string list = (* empty out: ([],[]) *)
  verb_m 3 (fun _ -> print_endline ("#eq_feed -> "^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm_list feed )) ) );
  verb_m 3 (fun _ -> print_endline ("#eq_l -> "^ (Sexp.to_string_hum (sexp_of_tokens l)) ) );
  match l with
    []                   -> (feed,[])
  | "\\\\" :: r          -> begin
      match r with
        "leq"  :: r        -> (*let prefix,rlst = parse_latexeq_eq' r termlabel in
        (ineq_parse prefix feed (Leq()),rlst) *)
        (feed,[])
      | "geq"  :: r        -> (*let prefix,rlst = parse_latexeq_eq' r termlabel in
        (ineq_parse prefix feed (Geq()),rlst)*)
        (feed,[])

      | "land" :: r        ->
        let feed,feed_to_break = propagate feed
        in
        let prefix,rlst = parse_latexeq_eq' r feed_to_break
        in let itt =
          match (hd prefix) with
            Fland(a)         -> Fland(feed@a)

          | Strr(a)          -> Fland(feed @[ Strr(a)])
          | FIneq(a)         -> Fland(feed @[ FIneq(a)])
          | Always (a,b)     -> Fland(feed @[ Always(a,b)])
          | Eventually (a,b) -> Fland(feed @[ Eventually(a,b)])
          | FVar(a)          -> Fland(feed @[ FVar(a)])

          | Flor(a) -> Flor( (Fland(feed@[List.hd a]) )::(List.tl a))
          | _ -> raise (Failure ("bad expression for and: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm (hd prefix) ))))
        in
        ((if feed_to_break <> [] then [FBreak([itt])] else [itt]),rlst)

      | "lor"  :: r        ->
        let feed,feed_to_break = propagate feed
        in
        let prefix,rlst = parse_latexeq_eq' r feed_to_break
        in let itt =
          match hd prefix with
            Flor(a)          -> Flor(feed@a)

          | Strr(a)          -> Flor(feed@[ Strr(a)])
          | FIneq(a)         -> Flor(feed@[ FIneq(a)])
          | Always (a,b)     -> Flor(feed@[ Always(a,b)])
          | Eventually (a,b) -> Flor(feed@[ Eventually(a,b)])
          | ULess (a,b,c)    -> Flor(feed@[ ULess(a,b,c)])
          | FExists (a,b)    -> Flor(feed@[ FExists(a,b)])
          | Fland(a)         -> Flor(feed@[ Fland(a)])
          | FIsol(a)         -> Flor(feed@[ FIsol(a)])
          | FVar(a)          -> Flor(feed@[ FVar(a)])
          | FBreak(a)        -> Flor(feed@a)

          | _ -> raise (Failure ("bad expression for or: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm (hd prefix) ))))
        in
        ((if feed_to_break <> [] then [FBreak([itt])] else [itt]),rlst)

      | "rightarrow" :: r  ->
        let feed,feed_to_break = propagate feed
        in let prefix,rlst = parse_latexeq_eq' r feed_to_break in
        ([FImplies(hd feed, hd prefix)],rlst)

      | "always" :: r      -> (* feed is discarded *)
        let feed,feed_to_break = propagate feed
        in if (List.hd r) = "_" then 
          let pm, rlst = parse_latexeq_pm (List.tl r) []
          in let prefix,rlst = parse_latexeq_eq' rlst feed_to_break
          in ([Always(hd pm, hd prefix)],rlst)
        else raise (Failure ("malformed always"))

      | "eventually" :: r  -> (* feed is discarded *)
        let feed,feed_to_break = propagate feed
        in if (List.hd r) = "_" then
          let pm, rlst = parse_latexeq_pm (List.tl r) []
          in let prefix,rlst = parse_latexeq_eq' rlst feed_to_break
          in ([Eventually(hd pm, hd prefix)],rlst)
        else raise (Failure ("malformed eventually"))

      | "until" :: r ->
        let feed,feed_to_break = propagate feed
        in if (List.hd r) = "_" then
          let pm, rlst = parse_latexeq_pm (List.tl r) []
          in let prefix,rlst = parse_latexeq_eq' rlst feed_to_break
          in ([ULess(hd pm, hd feed, hd prefix)],rlst)
        else raise (Failure ("malformed until"))


      | "int" :: r         -> (* what to do with feed ? *)
        let feed,feed_to_break = propagate feed
        in
        let tm, rlst = parse_latexeq_tm' (["\\\\"; "int"]@r) [] (*(Strr((match_feed feed)@["\\\\"; "int"]))*)
        in
        let rec rep tm rlst =
          match rlst with
          | "<" :: r ->
            let tm2, rlst2 = parse_latexeq_tm' r []
            in parse_latexeq_eq' rlst2 (if feed_to_break <> [] then [FBreak([FIneq([(FTerm(tm), Less());(FTerm(tm2),N())] )])] else [FIneq([(FTerm(tm), Less());(FTerm(tm2),N())] )] )

          | "+" :: r -> 
            let tm2, rlst2 = parse_latexeq_tm' r [] (* TPlus(tm@tm2) *)
            in rep ([TPlus(tm@tm2)]) rlst2

          | _ -> raise (Failure ("issues along durations" ^ (Sexp.to_string_hum (sexp_of_tokens rlst)) ))
        in
        rep tm rlst

      | "exists" :: r      -> (* get the variable and then the formula *)
        let feed,feed_to_break = propagate feed
        in let tm, rlst = parse_latexeq_tm' (List.tl r) []
        in let prefix,rlst = parse_latexeq_eq' rlst feed_to_break
        in ([FExists(hd tm, hd prefix)],rlst)

      | "neg" :: r         ->
        let feed,feed_to_break = propagate feed
        in let prefix,rlst = parse_latexeq_eq' r feed_to_break
        in ([FNot(hd prefix)],rlst)


      (* TODO
      | "frac" :: r        -> parse_latexeq_eq' r (Strr((match_feed feed)@["\\\\"; "frac"]))
      | "times" :: r       -> parse_latexeq_eq' r (Strr((match_feed feed)@["\\\\"; "times"]))
      *)

      | "\\\\" :: r'       -> (* skip space slashes *) parse_latexeq_eq' r feed

      | x :: r'            ->
        verb_m 2 (fun () -> print_endline ("skipping: "^x) ; ) ;
        parse_latexeq_eq' r' feed (* we are skipping every unknown variable *)

      | []                 -> parse_latexeq_eq' r feed
    end

  | "(" :: r             -> (* feed is skipped TODO !!! *)
    Stack.push LF parenthesis_stack;
    let prefix,rlst = parse_latexeq_eq' r []
    in
    let feed,feed_to_break = propagate feed
    in
    (* decision to proceed based on feed and prefix variables *)
    (*let par_fun prefix : bool * intermediate_ltx_fm = match prefix with Strr(a) -> (true,prefix) | _ -> (false,FIsol(prefix))
    in
    let vvv = match feed with
        Strr(a) when a != [] -> let c,prf = par_fun prefix in
        if c then
          (* feed and prf *)
          Strr((match_feed feed)@["("]@(match_feed prf)@[")"] )
        else raise (Failure ("bad expression for parenthesis shape: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm feed))))
      | emptystr -> (snd (par_fun prefix))
    in*)
    parse_latexeq_eq' rlst (if feed_to_break <> [] then [FBreak(prefix)] else prefix)

  | ")" :: r             -> if Stack.top parenthesis_stack = LF then let _ = Stack.pop parenthesis_stack in (feed, r) else (feed, l)

  | "<" :: r             ->
    if feed = [] then raise (Failure ("< operator cannot contain an empty feed"));
    (
    match feed with
    | FBreak(h)::r -> verb_m 3 (fun _ -> print_endline ("bfreak -> "^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm_list feed )) ) );
      (h,l)
    
    | FIneq(x)::lst -> verb_m 3 (fun _ -> print_endline ("fineq -> "^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm_list feed )) ) ); (feed,l)

    | _ ->
      let prefix,rlst = parse_latexeq_tm' r [] (* TODO trouble with several < operators *)
      in
      verb_m 3 (fun _ -> print_endline ("OLDINEQ -> "^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm_list feed )) ) );
      parse_latexeq_eq' rlst ([FIneq([(hd feed, Less());(FTerm(prefix),N())] )])  (*(ineq_parse prefix feed (Less()),rlst)*)
    )

  | "+" :: r | "*" :: r  ->
    if not (Stack.is_empty nested_stack) then (feed,l) else (
      match feed with
      | FVar(a) :: [] ->
        let tm,rlst = parse_latexeq_tm' l [TVar(a,TEmpty())]
        in parse_latexeq_eq' rlst [FTerm(tm)]

      | _ -> raise (Failure ("+ op"))
    )

  (*
  | ">" :: r             -> let prefix,rlst = parse_latexeq_eq' r termlabel in (ineq_parse prefix feed (Greater()),rlst)
  | "=" :: r             -> let prefix,rlst = parse_latexeq_eq' r termlabel in (ineq_parse prefix feed (Eq()),rlst)
  *)


  (* skip keywords *)
  | "\\\\\\\\" :: r      -> (* skip linebreaks *) parse_latexeq_eq' r feed
  | "&" :: r             -> parse_latexeq_eq' r feed
  | "%" :: r             -> parse_latexeq_eq' r feed
  | "$" :: r             -> parse_latexeq_eq' r feed
  | "," :: r             -> parse_latexeq_eq' r feed

  | a :: r when chk_num a -> (* if we get a number then  parse term *)
    let tm,rlst = parse_latexeq_tm' l []
    in parse_latexeq_eq' rlst [FTerm(tm)]

  | a :: r when chk_alphanum a ->
    let feed,feed_to_break = propagate feed in
    parse_latexeq_eq' r (if feed_to_break <> [] then [FBreak([FVar(a)])] else [FVar(a)])

  | a :: r                ->
    let feed,feed_to_break = propagate feed in
    let nfeed = if feed <> [] then [(Strr((match_feed (hd feed) )@[a]))] else [Strr([a])]
    in parse_latexeq_eq' r (if feed_to_break <> [] then [FBreak(nfeed)] else nfeed)

let parse_latexeq_tm l feed = fst (parse_latexeq_tm' l feed)


let rec ineq_parse prefix feed op =
  let feed_prs feed = match feed with Strr(a) -> FTerm(parse_latexeq_tm a [] ) | _ -> feed in
  let feed = feed_prs feed in
  match prefix with
    FIneq(a)          -> FIneq((feed, op)::a)

  | Strr(a)           -> FIneq([(feed, op); ( FTerm(parse_latexeq_tm a [] )  , N())])   (* first was plain copy of Strr(a) *)

  | Always (a,b)      -> FIneq([(feed, op); (Always(a,b),N())]) (* plain copy of Always and Eventually *)
  | Eventually (a,b)  -> FIneq([(feed, op); (Eventually(a,b),N())])

  | Fland(a)          -> Fland( (ineq_parse (List.hd a) feed op)::(List.tl a))
  | Flor(a)           -> Flor( (ineq_parse (List.hd a) feed op)::(List.tl a))
  | FImplies(a,b)     -> FImplies(ineq_parse a feed op, b)
  | _                 -> raise (Failure ("bad expression for ineq: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm prefix))))

(*
let rec parse_latexeq_prop l =
  let rec join_inf l feed =
    match l with
    | "{" :: r -> join_inf l feed
    | "}" :: r -> (feed,r)
    | a :: r when chk_alphanum a -> join_inf l (feed^a)
    | _ -> raise (Failure ("join_inf"))
  in
  match l with
  | a :: ("_" :: ("{" :: r)) when chk_alphanum a
       -> FProp(fst (join_inf ("{"::r) "") ) (* TODO continue here.... *)
  | a :: ("_" :: (b :: r))   when chk_alphanum a && chk_alphanum b
       -> FProp(a^"#"^b)
  | a :: r when chk_alphanum a
       ->  FProp(a)
  | _  -> raise (Failure ("bad prop: "^(Sexp.to_string_hum (sexp_of_tokens l))))

let termlabel = Strr(["---"])
*)

(*let rec parse_latex_eq' l feed : intermediate_ltx_fm * string list =
  let to_prop a = try match a with Strr(ls) -> parse_latexeq_prop ls  | _ -> a with |_ -> a (*try to convert to proposition *)
  in
  match l with
    []                   -> (feed,[])
  | "\\\\" :: r          -> begin
      match r with
        "leq"  :: r        -> let prefix,rlst = parse_latex_eq' r termlabel in
        (ineq_parse prefix feed (Leq()),rlst)
      | "geq"  :: r        -> let prefix,rlst = parse_latex_eq' r termlabel in
        (ineq_parse prefix feed (Geq()),rlst)

      | "land" :: r        -> let prefix,rlst = parse_latex_eq' r emptystr in
        let itt =
          let feed = to_prop feed in
          match prefix with
            Fland(a)         -> Fland(feed::a)

          | Strr(a)          -> Fland([feed; to_prop (Strr(a))])
          | FIneq(a)         -> Fland([feed; FIneq(a)])
          | Always (a,b)     -> Fland([feed; Always(a,b)])
          | Eventually (a,b) -> Fland([feed; Eventually(a,b)])

          | Flor(a) -> Flor( (Fland(feed::[List.hd a]) )::(List.tl a))
          | _ -> raise (Failure ("bad expression for and: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm prefix))))
        in
        (itt,rlst)

      | "lor"  :: r        -> let prefix,rlst = parse_latex_eq' r emptystr in
        let itt =
          let feed = to_prop feed in
          match prefix with
            Flor(a)          -> Flor(feed::a)

          | Strr(a)          -> Flor([feed; to_prop (Strr(a))])
          | Always (a,b)     -> Flor([feed; Always(a,b)])
          | Eventually (a,b) -> Flor([feed; Eventually(a,b)])
          | Fland(a)         -> Flor([feed; Fland(a)])
          | FIsol(a)         -> Flor([feed; FIsol(a)])

          | _ -> raise (Failure ("bad expression for or: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm prefix))))
        in
        (itt,rlst)

      | "rightarrow" :: r  -> let prefix,rlst = parse_latex_eq' r emptystr in
        (FImplies(to_prop feed, to_prop prefix),rlst)

      | "always" :: r      -> (* feed is discarded *)
        if (List.hd r) = "_" then 
          let pm, rlst = parse_latexeq_pm (List.tl r) (PEmpty())
          in let prefix,rlst = parse_latex_eq' rlst emptystr
          in (Always(pm, to_prop prefix),rlst)
        else raise (Failure ("malformed always"))
      | "eventually" :: r  -> (* feed is discarded *)
        if (List.hd r) = "_" then
          let pm, rlst = parse_latexeq_pm (List.tl r) (PEmpty())
          in let prefix,rlst = parse_latex_eq' rlst emptystr
          in (Eventually(pm, to_prop prefix),rlst)
        else raise (Failure ("malformed eventually"))

      | "until" :: r -> (* feed is discarded *)
        if (List.hd r) = "_" then
          let pm, rlst = parse_latexeq_pm (List.tl r) (PEmpty())
          in let prefix,rlst = parse_latex_eq' rlst emptystr
          in (ULess(pm, to_prop feed, to_prop prefix),rlst)
        else raise (Failure ("malformed until"))


      | "int" :: r         -> parse_latex_eq' r (Strr((match_feed feed)@["\\\\"; "int"]))
      | "frac" :: r        -> parse_latex_eq' r (Strr((match_feed feed)@["\\\\"; "frac"]))
      | "times" :: r       -> parse_latex_eq' r (Strr((match_feed feed)@["\\\\"; "times"]))

      (* skip keywords *)
      | "scriptstyle" :: r -> parse_latex_eq' r feed
      | "left" :: r        -> parse_latex_eq' r feed
      | "right" :: r       -> parse_latex_eq' r feed

      | _                  -> parse_latex_eq' r feed
    end

  | "(" :: r             -> let prefix,rlst = parse_latex_eq' r emptystr in
    (* decision to proceed based on feed and prefix variables *)
    let par_fun prefix : bool * intermediate_ltx_fm = match prefix with Strr(a) -> (true,prefix) | _ -> (false,FIsol(prefix))
    in
    let vvv = match feed with
        Strr(a) when a != [] -> let c,prf = par_fun prefix in
        if c then
          (* feed and prf *)
          Strr((match_feed feed)@["("]@(match_feed prf)@[")"] )
        else raise (Failure ("bad expression for parenthesis shape: " ^ ( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm feed))))
      | emptystr -> (snd (par_fun prefix))
    in
    parse_latex_eq' rlst (vvv)

  | ")" :: r             -> (feed,r)

  | "<" :: r             -> let prefix,rlst = parse_latex_eq' r termlabel in (ineq_parse prefix feed (Less()),rlst)
  | ">" :: r             -> let prefix,rlst = parse_latex_eq' r termlabel in (ineq_parse prefix feed (Greater()),rlst)
  | "=" :: r             -> let prefix,rlst = parse_latex_eq' r termlabel in (ineq_parse prefix feed (Eq()),rlst)


  (* skip keywords *)
  | "\\\\\\\\" :: r      -> (* skip linebreaks *) parse_latex_eq' r feed
  | "&" :: r             -> parse_latex_eq' r feed
  | "%" :: r             -> parse_latex_eq' r feed
  | "$" :: r             -> parse_latex_eq' r feed
  | "," :: r             -> parse_latex_eq' r feed

  | a :: r               -> parse_latex_eq' r (Strr((match_feed feed)@[a]))

*)

let parse_latexeq_eq l feed : intermediate_ltx_fm = let x,y = parse_latexeq_eq' l feed in if y = [] then List.hd x else raise (Failure ("bad expression; check parenthesis"^( Sexp.to_string_hum (sexp_of_tokens y)) ))

(*
    Translation to RMTLD3
*)
let rec rmtld3_fm_of_intermediate_ltx_pm ipm : float =
  match ipm with
  | PEmpty() -> 0.
  | POp(op,tmlst) -> 0.
  | PList(pmlst) -> 0.
  | Pvar(id) -> float_of_string id

and rmtld3_fm_of_intermediate_ltx_tm itm : rmtld3_tm =
  match itm with
    TTimes(tmlst) -> Constant(0.)
  | TPlus(tmlst) -> Constant(0.)
  | TMinus(tmlst) -> Constant(0.)
  | TFrac(tm1,tm2) -> Constant(0.)
  | TInt(tm,fm) -> Duration(rmtld3_fm_of_intermediate_ltx_tm tm, rmtld3_fm_of_intermediate_ltx_fm fm)
  | TVal(v) -> Constant(float_of_int v)
  | TVar(id,tm) -> Constant(0.)
  | TFun(id, tmlst1, tmlst2) -> Constant(0.)
  | TEmpty() -> Constant(0.)

and rmtld3_fm_of_intermediate_ltx_fm ifm : rmtld3_fm =
  match ifm with
  | FIneq((FTerm([el1]),Less())::(FTerm([el2]),N())::[]) -> LessThan(rmtld3_fm_of_intermediate_ltx_tm el1, rmtld3_fm_of_intermediate_ltx_tm el2) (* consider this | FTerm(tmlst) -> intermediate_ltx_tm (hd tmlst) *)

  | Fland(el::el2::[]) -> mand (rmtld3_fm_of_intermediate_ltx_fm el) (rmtld3_fm_of_intermediate_ltx_fm el2)
  | Fland(el::fmlst) -> mand (rmtld3_fm_of_intermediate_ltx_fm el) (rmtld3_fm_of_intermediate_ltx_fm (Fland(fmlst)))

  | Flor(el::el2::[]) -> Or(rmtld3_fm_of_intermediate_ltx_fm el,rmtld3_fm_of_intermediate_ltx_fm el2)
  | Flor(el::fmlst) -> Or(rmtld3_fm_of_intermediate_ltx_fm el,rmtld3_fm_of_intermediate_ltx_fm (Flor(fmlst)))

  | FNot(fm) -> Not(rmtld3_fm_of_intermediate_ltx_fm fm)

  | Always(POp(Less(),[TVal(a)]),fm) -> malways (float_of_int a) (rmtld3_fm_of_intermediate_ltx_fm fm)
  | Always(POp(Eq(),[TVal(a)]),fm) -> malways_eq (float_of_int a) (rmtld3_fm_of_intermediate_ltx_fm fm)
  | Always(POp(Leq(),[TVal(a)]),fm) -> malways_leq (float_of_int a) (rmtld3_fm_of_intermediate_ltx_fm fm)

  | Eventually(POp(Less(),[TVal(a)]),fm) -> meventually (float_of_int a) (rmtld3_fm_of_intermediate_ltx_fm fm)
  | Eventually(POp(Eq(),[TVal(a)]),fm) -> meventually_eq (float_of_int a) (rmtld3_fm_of_intermediate_ltx_fm fm)
  | Eventually(POp(Leq(),[TVal(a)]),fm) -> meventually_leq (float_of_int a) (rmtld3_fm_of_intermediate_ltx_fm fm)

  | ULess(POp(Less(),[TVal(a)]),fm1,fm2) -> let gamma = float_of_int a in
      Until(gamma, rmtld3_fm_of_intermediate_ltx_fm fm1, rmtld3_fm_of_intermediate_ltx_fm fm2)

  | FExists(TVar(a,TEmpty()), fm) -> Exists(a, rmtld3_fm_of_intermediate_ltx_fm fm)

  | FImplies(fm1,fm2) -> mimplies (rmtld3_fm_of_intermediate_ltx_fm fm1) (rmtld3_fm_of_intermediate_ltx_fm fm2)
  | FIsol(fm) -> rmtld3_fm_of_intermediate_ltx_fm fm
  | FVar(id)  -> Prop(id)
  | x -> raise (Failure ("rmtld3_fm_of_intermediate_ltx_fm: "^( Sexp.to_string_hum (sexp_of_intermediate_ltx_fm x))))


(*
   move these tests to unittest folder
 *)

(* parsing latex sample
   (\int^{\pi_1} \psi_1=0\land 0\leq \int^{\pi_2} \psi_2<\theta )&\lor \ \ %\\
   &\scriptstyle\left(0<\int^{\pi_1} \psi_1<\frac{\theta }{4}\land 0\leq \int^{\pi_2} \psi_2<\theta -3 \int^{\pi_1} \psi_1\right)&\lor \\
   &\scriptstyle\left(\int^{\pi_1} \psi_1=\frac{\theta }{4}\land 0\leq \int^{\pi_2} \psi_2\leq \frac{\theta }{4}\right)&\lor \ \ %\\
   &\scriptstyle\left(\frac{\theta }{4}<\int^{\pi_1} \psi_1<\frac{\theta }{3}\land 0\leq \int^{\pi_2} \psi_2<\frac{\theta -\int^{\pi_1} \psi_1}{3}\right)&\lor \\
   &\scriptstyle\left(\int^{\pi_1} \psi_1=\frac{\theta }{3}\land \theta -3 \int^{\pi_1} \psi_1\leq \int^{\pi_2} \psi_2<\frac{\theta -\int^{\pi_1} \psi_1}{3}\right)&\lor \ \ %\\
   &\scriptstyle\left(\frac{\theta }{3}<\int^{\pi_1} \psi_1<\theta \land 0\leq \int^{\pi_2} \psi_2<\frac{\theta -\int^{\pi_1} \psi_1}{3}\right)
*)


let s1 = "
(\\int^{\\pi_1} \\psi_1=0\\land 0\\leq \\int^{\\pi_2} \\psi_2<\\theta )&\\lor \\ \\ %\\\\
&\\scriptstyle\\left(0<\\int^{\\pi_1} \\psi_1<\\frac{\\theta }{4}\\land 0\\leq \\int^{\\pi_2} \\psi_2<\\theta -3 \\int^{\\pi_1} \\psi_1\\right)&\\lor \\\\
&\\scriptstyle\\left(\\int^{\\pi_1} \\psi_1=\\frac{\\theta }{4}\\land 0\\leq \\int^{\\pi_2} \\psi_2\\leq \\frac{\\theta }{4}\\right)&\\lor \\ \\ %\\\\
&\\scriptstyle\\left(\\frac{\\theta }{4}<\\int^{\\pi_1} \\psi_1<\\frac{\\theta }{3}\\land 0\\leq \\int^{\\pi_2} \\psi_2<\\frac{\\theta -\\int^{\\pi_1} \\psi_1}{3}\\right)&\\lor \\\\
"

let s2 = "a=\\int^{\\var} \\psi_1 \\land \\alwaysless \\left(
\\left( f_{\\beta,\\alpha}\\left(a\\right) < (\\frac{3}{4} + 10) - 10 \\right) \\rightarrow \\left(\\eventuallyless_{\\var + \\var_2} \\ \\psi_d\\right) \\right)"

let test2_input = lex (String.explode s1)
let test3_input = lex (String.explode s2)

let texeq_unit_tests () =
  print_endline "\nlexer 1:\n"; print_endline (Sexp.to_string_hum ( sexp_of_tokens test2_input ));
  print_endline "\ntest 1:\n"; print_endline (Sexp.to_string_hum (sexp_of_intermediate_ltx_fm (parse_latexeq_eq [ "G"; "\\\\"; "land"; "Y"; "\\\\"; "leq"; "X"; "\\\\"; "land"; "Z"; "\\\\"; "lor"; "H"; "\\\\"; "lor"; "HH"] [])));
  print_endline "\ntest 2:\n"; print_endline (Sexp.to_string_hum (sexp_of_intermediate_ltx_fm (parse_latexeq_eq test2_input [])));
  print_endline "\ntest 3:\n"; print_endline (Sexp.to_string_hum (sexp_of_intermediate_ltx_fm (parse_latexeq_eq test3_input [])));
;;

let texeqparser str =
  begin
    verb_m 1 (fun _ -> print_endline ("Latexeq input: "^str^"\n"););
    let rsl = (parse_latexeq_eq (lex (String.explode str)) [])
    in
    verb_m 2 (fun _ ->
      print_endline (Sexp.to_string_hum (sexp_of_intermediate_ltx_fm rsl) ) ;
      print_endline "" ;
    ) ;

    (* lets convert the intermediate representation into rmtld3 expressions *)
    rmtld3_fm_of_intermediate_ltx_fm rsl

  end
