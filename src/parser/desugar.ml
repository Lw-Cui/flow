#require "flow_parser";;

module SMap = Map.Make(String);;


(* 
    After running js code
        var v = {'name': 'liwei', 'answer': 42} 

    The global scope is like 
        {'$global': {'v': {'name': 'liwei', 'answer': 42} } }

    LId '$global' is the reference to global scope.
    LDeref (LId '$global') gives you the concrete structure {'v': {'name': 'liwei', 'answer': 42} }
    Then LGetField (LDeref (LId '$global'), 'v') gives you the *reference* to {'name': 'liwei', 'answer': 42}

    Hence, you can think all things are stored as reference.
    The first lexpr of LUpdateField need to be a concrete structure, hence LDeref is necessary
*)

type lexpr = 
    | LId of string
    | LString of string
    | LUndefined 
    | LSet of lexpr * lexpr
    | LAlloc of lexpr
    | LDeref of lexpr
    | LGetField of lexpr * lexpr
    | LUpdateField of lexpr * lexpr * lexpr
    | LSeq of lexpr * lexpr
    | LNum of float
    | LObject of (string * lexpr) list
    | LLet of (string * lexpr) list * lexpr
    | LApp of lexpr * lexpr list
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
    LAlloc (LObject (("$proto", LId "@Object_prototype") :: ("$class", LString "Object") :: (List.map (desugar_property ctx) e)))

and

desugar_object (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.t): lexpr = 
    match e with {properties = properties} ->
        desugar_properties ctx properties

and

desugar_member (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Member.t): lexpr =
    match e with {_object = _object; property = property} ->
    let obj = desugar_expr ctx _object in
    match property with
    | PropertyExpression pe -> 
        let idx = desugar_expr ctx pe in
        LGetField (LDeref obj, idx)
    | _ -> raise @@ Failure "Unsupported member property"

and

desugar_identifer (ctx: bool SMap.t) (id: (Loc.t, Loc.t) Flow_ast.Identifier.t): lexpr =
    let id' = snd id in
    match id' with {name = name} ->
    LGetField (LDeref (LId "$global"), LString name)

and

desguar_pattern (ctx: bool SMap.t) (p: (Loc.t, Loc.t) Flow_ast.Pattern.t): lexpr =
    let p' = snd p in
    match p' with
    | Expression e -> desugar_expr ctx e
    | _ ->raise @@ Failure "Unsupported pattern" 

and

desugar_assignment (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.Assignment.t): lexpr =
    match e with {left = left; right = right} ->
    let l = desguar_pattern ctx left in
    let r = desugar_expr ctx right in
    match l with 
    | LGetField (LDeref obj, field) -> LSet (obj, LUpdateField (LDeref obj, field, r))
    | _ -> raise @@ Failure "Unsupported assignment"

and

desugar_expression_or_spread (ctx: bool SMap.t) (l: (Loc.t, Loc.t) Flow_ast.Expression.expression_or_spread): lexpr =
    match l with 
    | Expression e -> desugar_expr ctx e
    | _ -> raise @@ Failure "Unsupported spread"

and

desugar_arglist (ctx: bool SMap.t) (l: (Loc.t, Loc.t) Flow_ast.Expression.ArgList.t): lexpr list =
    let l' = snd l in
    match l' with {arguments = arguments} ->
    List.map (desugar_expression_or_spread ctx) arguments

and 

(* Only support print now *)
desugar_call (ctx: bool SMap.t) (c: (Loc.t, Loc.t) Flow_ast.Expression.Call.t): lexpr =
    match c with {arguments = arguments} ->
    let ag = desugar_arglist ctx arguments in
    LApp (LId "print-string", [LApp (LId "prim->string", ag)])

and

desugar_expr (ctx: bool SMap.t) (e: (Loc.t, Loc.t) Flow_ast.Expression.t): lexpr =
    let e' = snd e in 
    match e' with
    | Literal l -> desugar_literal ctx l
    | Object obj -> desugar_object ctx obj
    | Member mem -> desugar_member ctx mem
    | Identifier id -> desugar_identifer ctx id
    | Assignment assign -> desugar_assignment ctx assign
    | Call c -> desugar_call ctx c
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
    | LGetField (obj, idx) -> parens "get-field" @@ (s_expr obj) ^ (s_expr idx)
    | LApp (func, arg) ->
        parens (s_expr func) (String.concat ""  (List.map s_expr arg))
    | _ -> raise @@ Failure "Not supported s_expr" 
;;

let set_env (expr: lexpr) : lexpr =
    LLet ([("$global", LAlloc (LObject []))],
    LLet ([("@Object_prototype", LAlloc(LObject []))], expr
    ))
;;

let ast: lexpr = set_env @@ desugar @@ Parser_flow.program "
    var v = {'name': 'liwei', 'answer': 42}; 
    var c = 5, b = 6;
";;

print_string @@ s_expr ast ^ "\n";;

let ast: lexpr = set_env @@ desugar @@ Parser_flow.program "
    var v = {'name': 'liwei', 'answer': 42}; 
    v['asn'];
    v['name'] = '5';
    print (v['name']);
";;

print_string @@ s_expr ast ^ "\n";;

let ast: lexpr = set_env @@ desugar @@ Parser_flow.program "
    var v = {'name': 'liwei', 'answer': 42}; 
    print (v['answer']);
";;

print_string @@ s_expr ast ^ "\n";;