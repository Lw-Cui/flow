#require "flow_parser";;

module SMap = Map.Make(String);;


type expr = 
    | Id of string
    | EString of string
    | Undefined 
    | SetRef of expr * expr
    | Deref of expr
    | UpdateField of expr * expr * expr
    | ESeq of expr * expr
    | Num of float
    | Object of (string * expr) list
;;


let rec to_string (e: expr) : string = 
    match e with
    | EString s -> s
    | _ -> raise @@ Failure "Unsupported conversion"

and

desugar_literal (ctx: bool SMap.t) (l: Loc.t Flow_ast.Literal.t): expr =
    match l with {value = value} -> 
    match value with
    | Number n -> Num n
    | String n -> EString n
    | _ -> raise @@ Failure "Unsupported literal"

and

desugar_properties_init_key (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.Property.key): expr = 
    match e with
    | Literal l -> desugar_literal ctx @@ snd l
    | _ -> raise @@ Failure "Unsupported literal"

and

desugar_property (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.property): (string * expr) = 
    match e with
    | SpreadProperty p -> raise @@ Failure "SpreadProperty is not supported"
    | Property p -> (
        let p' = snd p in 
        match p' with
        | Init {key = key; value = value} -> (to_string @@ desugar_properties_init_key ctx key, desugar_expr ctx value)
        | _ -> raise @@ Failure "Unsupported property type"
    )

and

desugar_properties (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.property list): expr = 
    Object (List.map (desugar_property ctx) e)

and

desugar_object (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.t): expr = 
    match e with {properties = properties} ->
        desugar_properties ctx properties

and

desugar_expr (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.t): expr =
    let e' = snd e in 
    match e' with
    | Literal l -> desugar_literal ctx l
    | Object obj -> desugar_object ctx obj
    | _ -> raise @@ Failure "Unsupported expression"

and

desugar_declarator_init (ctx: bool SMap.t) (init: (Loc.t, Loc.t) Flow_ast.Expression.t option): expr =
    match init with
    | None -> Undefined
    | Some init -> desugar_expr ctx init

and

desugar_declarator (ctx: bool SMap.t) (decl: (Loc.t, Loc.t) Flow_ast.Statement.VariableDeclaration.Declarator.t): expr =
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
            SetRef (Id "$global", (UpdateField (Deref (Id "$global"), EString name'.name, e)))
        )
    | _ -> raise @@ Failure "Only Identifier is supported"

and

desugar_variableDeclaration (ctx: bool SMap.t) (decls: (Loc.t, Loc.t) Flow_ast.Statement.VariableDeclaration.t): expr =
    match decls with {declarations = declarations} ->
    List.fold_right (fun l r -> ESeq (l, r)) (List.map (desugar_declarator ctx) declarations) Undefined

and 

(* statement is the top level element in js *)
desugar_stmt (ctx: bool SMap.t) (stmt: (Loc.t, Loc.t) Flow_ast.Statement.t): expr =
    let stmt' = snd stmt in
    match stmt' with
    | VariableDeclaration var ->  desugar_variableDeclaration ctx var
    | Expression expr ->  desugar_expr ctx expr.expression
    | _ -> raise @@ Failure "Only VariableDeclaration is supported"

and

desugar ((prog, _): (Loc.t, Loc.t) Flow_ast.Program.t * 'b): expr =
    let prog' = snd prog in 
    let stmts = prog'.statements in
    List.fold_right (fun l r -> ESeq (l, r)) (List.map (desugar_stmt SMap.empty) stmts) Undefined
;;



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
    | EString s -> "\"" ^ s ^ "\""
    | _ -> raise @@ Failure "Not supported print" 
;;


let ast: expr = desugar @@ Parser_flow.program "var liwei = 0, ocaml = 6;";;
let c = print_string @@ "(let (($global (alloc (object))))" ^ (str ast) ^ ")\n";;

let ast: expr = desugar @@ Parser_flow.program "var v = {'a': 6, 'answer': 42}";;
