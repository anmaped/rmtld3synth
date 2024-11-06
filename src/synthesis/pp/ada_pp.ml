(* 
 * Ada/SPARK2014 Pretty Printer
 *)

type identifier = string

type defining_identifier = string

type subtype_mark = Integer | Float | UnsafeType of string

type parameter = {name: identifier; typ: subtype_mark}

type 'a parameter_association = {selector: identifier option; expression: 'a}

type expression =
  | Null
  | IntLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
  | Name of identifier
  | UMinus of expression
  | UPlus of expression
  | Plus of expression * expression
  | Minus of expression * expression
  | Times of expression * expression
  | Div of expression * expression
  | Equal of expression * expression
  | NotEqual of expression * expression
  | LessThan of expression * expression
  | LessOrEqual of expression * expression
  | GreatThan of expression * expression
  | GreatOrEqual of expression * expression
  | And of expression * expression
  | Or of expression * expression
  | Xor of expression * expression
  | Not of expression
  | FunctionCall of identifier * expression parameter_association list

(*| numeric_literal | string_literal | Name of identifier | (expression) |
  (conditional_expression) | (quantified_expression) | (declare_expression)*)

type statement =
  | Return of expression
  | If of expression * statement list * statement list
  | Print of string

type subprogram_specification =
  { name: identifier
  ; parameters: parameter list
  ; return_type: subtype_mark option }

type variable_specification =
  {name: identifier; typ: subtype_mark; expression: expression}

type type_specification =
  {name: identifier; definition: defining_identifier list}

type formal_subprogram_specification =
  {name: identifier; new_name: identifier; formal_specification: identifier}

type formal_package_specification =
  {name: identifier; new_name: identifier; formal_specification: identifier}

type generic_declaration =
  | FormalPackage of formal_package_specification
  | FormalSubprogram of formal_subprogram_specification
  | UsePackage of identifier

type basic_declaration =
  | Variable of variable_specification
  | Type of type_specification
  | Package of package_specification
  | Subprogram of subprogram_specification
  | GenericPackage of generic_declaration list * package_specification
  | GenericSubprogram of generic_declaration list * subprogram_specification
  | Comment of string
  | UnsafeDeclaration of string

and package_specification =
  {name: identifier; specification: basic_declaration list}

type subprogram_body =
  { specification: subprogram_specification
  ; basic_declaration: basic_declaration list
  ; body: statement list }

type package_body = {name: identifier; body: subprogram_body}

type context_clause =
  | Use of identifier
  | With of identifier
  | ContextComment of string

type compilation_unit_body =
  | B_Subprogram of context_clause list * subprogram_body
  | B_Package of context_clause list * package_body

type compilation_unit_declaration =
  | D_Subprogram of subprogram_specification
  | D_Package of context_clause list * package_specification
  | D_GenericSubprogram of
      context_clause list
      * (generic_declaration list * subprogram_specification)
  | D_GenericPackage of
      context_clause list * (generic_declaration list * package_specification)
(* library_unit_declaration *)
(*| Unit_Generic_instantiation of context_clause list * generic_declaration
  (* library_unit_declaration *) *)

type compilation_unit =
  | Unit_Body of compilation_unit_body
  | Unit_Declaration of compilation_unit_declaration

(*
 * PrettyPrint (pp) functions
 *)
let pp_subtype_mark (mark : subtype_mark) : string =
  match mark with Integer -> "Integer" | Float -> "Float" | UnsafeType s -> s

let rec pp_parameter_list (lst : parameter list) : string =
  let parameters =
    match lst with
    | [] -> ""
    | x :: lst ->
        x.name ^ ": " ^ pp_subtype_mark x.typ ^ pp_parameter_list lst
  in
  if parameters = "" then "" else "(" ^ parameters ^ ")"

let rec pp_expression (e : expression) : string =
  match e with
  | UMinus e -> "-" ^ pp_expression e
  | UPlus e -> "+" ^ pp_expression e
  | Plus (e1, e2) -> pp_expression e1 ^ " + " ^ pp_expression e2
  | Minus (e1, e2) -> pp_expression e1 ^ " - " ^ pp_expression e2
  | Times (e1, e2) -> pp_expression e1 ^ " * " ^ pp_expression e2
  | Div (e1, e2) -> pp_expression e1 ^ " / " ^ pp_expression e2
  | Name x -> x
  | IntLiteral i -> string_of_int i
  | FloatLiteral f -> Printf.sprintf "%.324f" f
  | StringLiteral s -> "\"" ^ s ^ "\""
  | Equal (e1, e2) -> pp_expression e1 ^ " = " ^ pp_expression e2
  | NotEqual (e1, e2) -> pp_expression e1 ^ " /= " ^ pp_expression e2
  | LessThan (e1, e2) -> pp_expression e1 ^ " < " ^ pp_expression e2
  | LessOrEqual (e1, e2) -> pp_expression e1 ^ " <= " ^ pp_expression e2
  | GreatThan (e1, e2) -> pp_expression e1 ^ " > " ^ pp_expression e2
  | GreatOrEqual (e1, e2) -> pp_expression e1 ^ " >= " ^ pp_expression e2
  | And (e1, e2) -> pp_expression e1 ^ " and " ^ pp_expression e2
  | Or (e1, e2) -> pp_expression e1 ^ " or " ^ pp_expression e2
  | Xor (e1, e2) -> pp_expression e1 ^ " xor " ^ pp_expression e2
  | Not e -> "not " ^ pp_expression e
  | Null -> "null"
  | FunctionCall (name, lst) ->
      name ^ "("
      ^ List.fold_left
          (fun acc {selector; expression} ->
            acc
            ^ (if acc = "" then "" else ", ")
            ^ (match selector with Some x -> x ^ " => " | None -> "")
            ^ pp_expression expression )
          "" lst
      ^ ")"

let rec pp_statement (s : statement) : string =
  match s with
  | Return e -> "return " ^ pp_expression e ^ ";"
  | If (_, _, _) as stm -> pp_if stm
  | Print msg -> "Ada.Text_IO.Put_Line(\"" ^ msg ^ "\");"

and pp_statements (lst : statement list) =
  List.fold_left (fun acc s -> acc ^ pp_statement s ^ "\n") "" lst

and pp_if stm =
  match stm with
  | If (cnd, stm1, stm2) ->
      "if " ^ pp_expression cnd ^ " then " ^ pp_statements stm1 ^ " else "
      ^ pp_statements stm2 ^ " end if;"
  | _ -> failwith "Not an if statement"

let pp_type_declaration (d : defining_identifier list) : string =
  List.fold_left
    (fun acc d -> acc ^ (if acc = "" then "" else ", ") ^ d)
    "" d

let pp_subprogram_specification (s : subprogram_specification) : string =
  (if s.return_type <> None then "function " else "procedure ")
  ^ s.name
  ^ pp_parameter_list s.parameters
  ^
  match s.return_type with
  | Some x -> "return " ^ pp_subtype_mark x ^ ";"
  | None -> ";"

let pp_generic_declaration (d : generic_declaration) : string =
  match d with
  | FormalPackage f ->
      "   with package " ^ f.name ^ " is new " ^ f.new_name ^ " "
      ^ f.formal_specification ^ ";"
  | FormalSubprogram f ->
      "   with procedure " ^ f.name ^ " is new " ^ f.new_name ^ " "
      ^ f.formal_specification ^ ";"
  | UsePackage p -> "   use " ^ p ^ ";"

let pp_generic_declarations (d : generic_declaration list) : string =
  List.fold_left (fun acc d -> acc ^ pp_generic_declaration d ^ "\n") "" d

let rec pp_basic_declaration (d : basic_declaration) : string =
  match d with
  | Variable v ->
      "type " ^ v.name ^ " is " ^ pp_subtype_mark v.typ ^ " := "
      ^ pp_expression v.expression
      ^ ";"
  | Type t when List.length t.definition > 0 ->
      "type " ^ t.name ^ " is (" ^ pp_type_declaration t.definition ^ ");"
  | Type t -> "type " ^ t.name ^ " is (Empty);"
  | Package p -> pp_package_declaration p
  | Subprogram s -> pp_subprogram_specification s
  | GenericPackage (declaration, specification) ->
      "generic\n"
      ^ pp_generic_declarations declaration
      ^ pp_package_declaration specification
  | GenericSubprogram (declaration, specification) ->
      "generic\n"
      ^ pp_generic_declarations declaration
      ^ pp_subprogram_specification specification
  | Comment c -> "-- " ^ c
  | UnsafeDeclaration c -> c

and pp_basic_declarations (d : basic_declaration list) : string =
  List.fold_left
    (fun acc d ->
      (if acc = "" then "" else acc ^ "\n") ^ pp_basic_declaration d )
    "" d

and pp_package_declaration (package : package_specification) : string =
  "package " ^ package.name ^ " is\n"
  ^ pp_basic_declarations package.specification
  ^ "\nend " ^ package.name ^ ";"

let pp_subprogram (f : subprogram_body) : string =
  (if f.specification.return_type <> None then "function " else "procedure ")
  ^ f.specification.name
  ^ pp_parameter_list f.specification.parameters
  ^
  match f.specification.return_type with
  | Some x -> pp_subtype_mark x
  | None ->
      "" ^ " is\n"
      ^ pp_basic_declarations f.basic_declaration
      ^ "\nbegin\n" ^ pp_statements f.body ^ "end " ^ f.specification.name
      ^ ";"

let pp_package_body (package : package_body) : string =
  "package body " ^ package.name ^ " is\n"
  ^ pp_subprogram package.body
  ^ "\nend " ^ package.name ^ ";"

let pp_context_clause (c : context_clause) : string =
  match c with
  | Use u -> "use " ^ u ^ ";"
  | With w -> "with " ^ w ^ ";"
  | ContextComment c -> "-- " ^ c

let pp_compilation_unit_body (unit : compilation_unit_body) : string =
  match unit with
  | B_Subprogram (context, body) ->
      List.fold_left
        (fun acc c -> acc ^ pp_context_clause c ^ "\n")
        "" context
      ^ pp_subprogram body
  | B_Package (context, body) ->
      List.fold_left
        (fun acc c -> acc ^ pp_context_clause c ^ "\n")
        "" context
      ^ pp_package_body body

let pp_compilation_unit_declaration (unit : compilation_unit_declaration) :
    string =
  match unit with
  | D_Subprogram s -> pp_subprogram_specification s
  | D_Package (context, body) ->
      List.fold_left
        (fun acc c -> acc ^ pp_context_clause c ^ "\n")
        "" context
      ^ pp_package_declaration body
  | D_GenericSubprogram (context, (declaration, specification)) ->
      List.fold_left
        (fun acc c -> acc ^ pp_context_clause c ^ "\n")
        "" context
      ^ "generic\n"
      ^ pp_generic_declarations declaration
      ^ pp_subprogram_specification specification
  | D_GenericPackage (context, (declaration, specification)) ->
      List.fold_left
        (fun acc c -> acc ^ pp_context_clause c ^ "\n")
        "" context
      ^ "generic\n"
      ^ pp_generic_declarations declaration
      ^ pp_package_declaration specification

let pp_compilation_unit (unit : compilation_unit) : string =
  match unit with
  | Unit_Body body -> pp_compilation_unit_body body
  | Unit_Declaration declaration ->
      pp_compilation_unit_declaration declaration

(*
 * Make (mk) functions
 *)

let mk_list a = [a]

let mk_list_append a b = a :: b

let mk_comment str = Comment str

let mk_context_comment str = ContextComment str

let mk_int i : expression = IntLiteral i

let mk_float f : expression = FloatLiteral f

let mk_string s : expression = StringLiteral s

let mk_enumeration_item name : expression = Name name

let ml_unsafe_type s : subtype_mark = UnsafeType s

let mk_name str : expression = Name str

let mk_function_call name lst : expression = FunctionCall (name, lst)

let mk_subprogram name parameters return_type declaration_lst body :
    subprogram_body =
  { specification= {name; parameters; return_type}
  ; basic_declaration= declaration_lst
  ; body }

let mk_subprogram_specification name parameters return_type :
    subprogram_specification =
  {name; parameters; return_type}

let mk_subprogram_declaration specification : basic_declaration =
  Subprogram specification

let mk_parameter name typ = {name; typ}

let mk_if condition stm1 stm2 = If (condition, stm1, stm2)

let mk_print msg = Print msg

let mk_expression_parameter ?selector expression :
    expression parameter_association =
  {selector; expression}

let mk_type_declaration name definition = Type {name; definition}

let mk_package_specification name specification : package_specification =
  {name; specification}

let mk_package_declaration specification = Package specification

let mk_package_body name body = {name; body}

let mk_generic_package declaration_lst specification : basic_declaration =
  GenericPackage (declaration_lst, specification)

let mk_generic_subprogram declaration_lst specification : basic_declaration =
  GenericSubprogram (declaration_lst, specification)

let mk_formal_package_declaration name new_name formal_specification :
    generic_declaration =
  FormalPackage {name; new_name; formal_specification}

let mk_formal_subprogram_declaration name new_name formal_specification :
    generic_declaration =
  FormalSubprogram {name; new_name; formal_specification}

let mk_use_declaration name = UsePackage name

let mk_compilation_unit_package_body context body : compilation_unit_body =
  B_Package (context, body)

let mk_compilation_unit_subprogram_body context body : compilation_unit_body
    =
  B_Subprogram (context, body)

let mk_compilation_unit_declaration_subprogram specification :
    compilation_unit_declaration =
  D_Subprogram specification

let mk_compilation_unit_declaration_package context
    (basic : basic_declaration) : compilation_unit_declaration =
  match basic with
  | Package specification -> D_Package (context, specification)
  | _ -> failwith "Not a package"

let mk_compilation_unit_declaration_generic_package context
    (basic : basic_declaration) : compilation_unit_declaration =
  match basic with
  | GenericPackage (declaration, specification) ->
      D_GenericPackage (context, (declaration, specification))
  | _ -> failwith "Not a generic package"

let mk_compilation_unit_declaration_generic_subprogram context
    (basic : basic_declaration) : compilation_unit_declaration =
  match basic with
  | GenericSubprogram (declaration, specification) ->
      D_GenericSubprogram (context, (declaration, specification))
  | _ -> failwith "Not a generic subprogram"

let mk_compilation_unit_body body : compilation_unit = Unit_Body body

let mk_compilation_unit_declaration declaration : compilation_unit =
  Unit_Declaration declaration
