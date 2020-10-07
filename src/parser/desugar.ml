#require "flow_parser";;

module SMap = Map.Make(String);;


type lexpr = 
    | LId of string
    | LString of string
    | LUndefined 
    | LSet of lexpr * lexpr
    | LAlloc of lexpr
    | LDeref of lexpr
    | LUpdateField of lexpr * lexpr * lexpr
    | LSeq of lexpr * lexpr
    | LNum of float
    | LObject of (string * lexpr) list
    | LLet of (string * lexpr) list * lexpr
;;


let rec to_string (e: lexpr) : string = 
    match e with
    | LString s -> s
    | _ -> raise @@ Failure "Unsupported conversion"

and

desugar_literal (ctx: bool SMap.t) (l: Loc.t Flow_ast.Literal.t): lexpr =
    match l with {value = value} -> 
    match value with
    | Number n -> LNum n
    | String n -> LString n
    | _ -> raise @@ Failure "Unsupported literal"

and

desugar_properties_init_key (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.Property.key): lexpr = 
    match e with
    | Literal l -> desugar_literal ctx @@ snd l
    | _ -> raise @@ Failure "Unsupported literal"

and

desugar_property (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.property): (string * lexpr) = 
    match e with
    | SpreadProperty p -> raise @@ Failure "SpreadProperty is not supported"
    | Property p -> (
        let p' = snd p in 
        match p' with
        | Init {key = key; value = value} -> (to_string @@ desugar_properties_init_key ctx key, desugar_expr ctx value)
        | _ -> raise @@ Failure "Unsupported property type"
    )

and

desugar_properties (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.property list): lexpr = 
    LAlloc (LObject (List.map (desugar_property ctx) e))

and

desugar_object (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.t): lexpr = 
    match e with {properties = properties} ->
        desugar_properties ctx properties

and

desugar_expr (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.t): lexpr =
    let e' = snd e in 
    match e' with
    | Literal l -> desugar_literal ctx l
    | Object obj -> desugar_object ctx obj
    | _ -> raise @@ Failure "Unsupported expression"

and

desugar_declarator_init (ctx: bool SMap.t) (init: (Loc.t, Loc.t) Flow_ast.Expression.t option): lexpr =
    match init with
    | None -> LUndefined
    | Some init -> desugar_expr ctx init

and

desugar_declarator (ctx: bool SMap.t) (decl: (Loc.t, Loc.t) Flow_ast.Statement.VariableDeclaration.Declarator.t): lexpr =
    let decl' = snd decl in
    match decl' with {id = id; init = init} ->
    let id' = snd id in 
    match id' with
    | Identifier {name = name} -> (
        let name' = snd name in
        match SMap.find_opt name'.name ctx with
        | Some true -> raise @@ Failure "local variable is not supported" 
        | Some false -> raise @@ Failure "Not assignable"
        | None -> (* It's global. if it exists, do nothing, else set to undefined. *)
            let e = desugar_declarator_init ctx init in
            LSet (LId "$global", (LUpdateField (LDeref (LId "$global"), LString name'.name, e)))
        )
    | _ -> raise @@ Failure "Only Identifier is supported"

and

desugar_variableDeclaration (ctx: bool SMap.t) (decls: (Loc.t, Loc.t) Flow_ast.Statement.VariableDeclaration.t): lexpr =
    match decls with {declarations = declarations} ->
    List.fold_right (fun l r -> LSeq (l, r)) (List.map (desugar_declarator ctx) declarations) LUndefined

and 

(* statement is the top level element in js *)
desugar_stmt (ctx: bool SMap.t) (stmt: (Loc.t, Loc.t) Flow_ast.Statement.t): lexpr =
    let stmt' = snd stmt in
    match stmt' with
    | VariableDeclaration var ->  desugar_variableDeclaration ctx var
    | Expression expr ->  desugar_expr ctx expr.expression
    | _ -> raise @@ Failure "Only VariableDeclaration is supported"

and

desugar ((prog, _): (Loc.t, Loc.t) Flow_ast.Program.t * 'b): lexpr =
    let prog' = snd prog in 
    let stmts = prog'.statements in
    List.fold_right (fun l r -> LSeq (l, r)) (List.map (desugar_stmt SMap.empty) stmts) LUndefined
;;



let rec parens (cmd: string) (content: string): string =
    " (" ^ cmd ^ " " ^ content ^ " ) "

and

s_expr (e: lexpr): string = 
    match e with
    | LSeq (e1, e2) -> parens "begin" @@ (s_expr e1) ^ (s_expr e2)
    | LSet (e1, e2) -> parens "set!" @@ (s_expr e1) ^ (s_expr e2)
    | LAlloc e1 -> parens "alloc" @@ (s_expr e1)
    | LDeref e1 -> parens "deref" @@ (s_expr e1)
    | LUpdateField (e1, e2, e3) -> parens "update-field" @@ (s_expr e1) ^ (s_expr e2) ^ (s_expr e3)
    | LUndefined -> "undefined"
    | LNum n -> " " ^ string_of_float n
    | LObject obj -> 
        let ptos (p: string * lexpr): string = parens ("\"" ^ (fst p) ^ "\"") @@ (s_expr (snd p)) in
        parens "object" @@ (String.concat "" (List.map ptos obj))
    | LId id -> id
    | LString s -> "\"" ^ s ^ "\""
    | LLet (list, expr) -> 
        let ptos (p: string * lexpr): string = parens (fst p) (s_expr (snd p)) in
        let slist = parens "" (String.concat "" (List.map ptos list)) in
        let sexpr = s_expr expr in
        parens "let" (slist ^ sexpr)
    | _ -> raise @@ Failure "Not supported print" 
;;

let set_env (expr: lexpr) : lexpr =
    LLet ([
        ("$global", LAlloc (LObject []));
        ], expr)
;;

let ast: lexpr = set_env @@ desugar @@ Parser_flow.program "
    var liwei = 0, ocaml = 6;
";;
print_string @@ s_expr ast ^ "\n";;

let ast: lexpr = set_env @@ desugar @@ Parser_flow.program "
    var v = {'name': 'liwei', 'answer': 42}; 
    var b = 6;
";;

print_string @@ s_expr ast ^ "\n";;
