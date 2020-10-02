#require "flow_parser";;

exception NotImplement
module Map = Map.Make(String);;

let desugar_variableDeclaration (ctx: 'a Map.t) (e: ('a, 'a) Flow_ast.Statement.VariableDeclaration.t) =
    match e with {declarations = declarations; kind = kind; comments = commments} ->
    Map.find_opt "a"
;;

let desugar_stmt (ctx: 'a Map.t) (stmt: ('a, 'a) Flow_ast.Statement.t) =
    let stmt' = snd stmt in
    match stmt' with
    | VariableDeclaration var ->  desugar_variableDeclaration ctx var
    | _ -> raise NotImplement
;;


let desugar ((prog, _): ('a, 'a) Flow_ast.Program.t * 'b) =
    let prog' = snd prog in 
    let stmts = prog'.statements in
    List.map (desugar_stmt Map.empty) stmts
;;

desugar @@ Parser_flow.program "var liwei = 0;"