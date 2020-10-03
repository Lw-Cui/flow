#require "flow_parser";;

module Map = Map.Make(String);;

type expr = 
    | Id of string
    | Undefined 
    | SetRef of expr * expr
    | Deref of expr
    | UpdateField of expr * expr * expr
    | Number of float
;;

let desugar_expr (ctx: 'a Map.t) (expr: expr) =
    match expr with
;;

let desugar_declarator_init (ctx: 'a Map.t) (init: ('a, 'a) Flow_ast.Expression.t option) =
    match init with
    | None -> Undefined
    | Some init -> desugar_expr ctx init
;;

let desugar_declarator (ctx: 'a Map.t) (decl: ('a, 'a) Flow_ast.Statement.VariableDeclaration.Declarator.t): expr =
    let decl' = snd decl in
    match decl' with {id = id; init = init} ->
    let id' = snd id in 
    match id' with
    | Identifier {name = name} -> (
        let name' = snd name in
        match Map.find_opt name'.name ctx with
        | Some true -> raise @@ Failure "local variable is not supported" 
        | Some false -> raise @@ Failure "Not assignable"
        | None -> (* It's global. if it exists, do nothing, else set to undefined. *)
            let e = desugar_declarator_init init in
            SetRef (Id "$global") UpdateField (Deref @@ Id "$global") name'.name e)
    | _ -> raise @@ Failure "Only Identifier is supported"
;;


let desugar_variableDeclaration (ctx: 'a Map.t) (decls: ('a, 'a) Flow_ast.Statement.VariableDeclaration.t) =
    match decls with {declarations = declarations} ->
    List.map (desugar_declarator ctx) declarations
;;

(* statement is the top level element in js *)
let desugar_stmt (ctx: 'a Map.t) (stmt: ('a, 'a) Flow_ast.Statement.t) =
    let stmt' = snd stmt in
    match stmt' with
    | VariableDeclaration var ->  desugar_variableDeclaration ctx var
    | _ -> raise @@ Failure "Only VariableDeclaration is supported"
;;


let desugar ((prog, _): ('a, 'a) Flow_ast.Program.t * 'b) =
    let prog' = snd prog in 
    let stmts = prog'.statements in
    List.map (desugar_stmt Map.empty) stmts
;;

desugar @@ Parser_flow.program "var liwei = 0;"