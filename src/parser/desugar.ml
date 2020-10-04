#require "flow_parser";;

module SMap = Map.Make(String);;


type expr = 
    | Id of string
    | String of string
    | Undefined 
    | SetRef of expr * expr
    | Deref of expr
    | UpdateField of expr * expr * expr
    | ESeq of expr * expr
    | Num of float
;;


let desugar_literal (ctx: bool SMap.t) (l: Loc.t Flow_ast.Literal.t): expr =
    match l with {value = value} -> 
    match value with
    | Number n -> Num n
    | _ -> raise @@ Failure "Unsupported literal"
;;


let desugar_expr (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.t): expr =
    let e' = snd e in 
    match e' with
    | Literal l -> desugar_literal ctx l
    | _ -> raise @@ Failure "Unsupported expression"
;;


let desugar_declarator_init (ctx: bool SMap.t) (init: (Loc.t, Loc.t) Flow_ast.Expression.t option): expr =
    match init with
    | None -> Undefined
    | Some init -> desugar_expr ctx init
;;

let desugar_declarator (ctx: bool SMap.t) (decl: (Loc.t, Loc.t) Flow_ast.Statement.VariableDeclaration.Declarator.t): expr =
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
            SetRef (Id "$global", (UpdateField (Deref (Id "$global"), String name'.name, e)))
        )
    | _ -> raise @@ Failure "Only Identifier is supported"
;;


let desugar_variableDeclaration (ctx: bool SMap.t) (decls: (Loc.t, Loc.t) Flow_ast.Statement.VariableDeclaration.t): expr =
    match decls with {declarations = declarations} ->
    List.fold_right (fun l r -> ESeq (l, r)) (List.map (desugar_declarator ctx) declarations) Undefined
;;

(* statement is the top level element in js *)
let desugar_stmt (ctx: bool SMap.t) (stmt: (Loc.t, Loc.t) Flow_ast.Statement.t): expr =
    let stmt' = snd stmt in
    match stmt' with
    | VariableDeclaration var ->  desugar_variableDeclaration ctx var
    | Expression expr ->  desugar_expr ctx expr.expression
    | _ -> raise @@ Failure "Only VariableDeclaration is supported"
;;


let desugar ((prog, _): (Loc.t, Loc.t) Flow_ast.Program.t * 'b): expr =
    let prog' = snd prog in 
    let stmts = prog'.statements in
    List.fold_right (fun l r -> ESeq (l, r)) (List.map (desugar_stmt SMap.empty) stmts) Undefined
;;

let ast: expr = desugar @@ Parser_flow.program "var liwei = 0, ocaml = 6;";;


let parens (cmd: string) (content: string): string =
    " (" ^ cmd ^ " " ^ content ^ " ) "
;;


let rec str (e: expr): string = 
    match e with
    | ESeq (e1, e2) -> parens "begin" @@ (str e1) ^ (str e2)
    | SetRef (e1, e2) -> parens "set!" @@ (str e1) ^ (str e2)
    | Deref e1 -> parens "deref" @@ (str e1)
    | UpdateField (e1, e2, e3) -> parens "update-field" @@ (str e1) ^ (str e2) ^ (str e3)
    | Undefined -> "undefined"
    | Num n -> " " ^ string_of_float n
    | Id id -> id
    | String s -> "\"" ^ s ^ "\""
    | _ -> raise @@ Failure "Not supported print" 
;;


print_string @@ str ast;;