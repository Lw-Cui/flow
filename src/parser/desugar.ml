(* 
    Reference
    =========

    Reference is the only way to access data in lambda js.

    For example, global variable in javascript code 
        var v = {'name': 'liwei', 'answer': 42} 

    is transformed into a field within '$global'
        {'$global': {'v': {'name': 'liwei', 'answer': 42} } }
    To access variable v, the reference to '$global' (LiD '$global')  used be used first, 
    , then LDeref & GetField, which are the operators to de-reference & retrieve field respectively.

    For example, LId '$global' is the reference to '$global'.
    LDeref (LId '$global') gives you the concrete structure {'v': {'name': 'liwei', 'answer': 42} }
    Then LGetField (LDeref (LId '$global'), 'v') returns the *reference* to {'name': 'liwei', 'answer': 42}
    To obtain the primitive value 'liwei', 
    LGetField (LDeref (LGetField (LDeref (LId '$global'), 'v')), 'name') is used.


    Context
    =======
    The context structure is (string * bool) list:
    [<variable_name_1, assignable_or_not>, <name_2, assignable_or_not>, ... ]

    Since all global variables are field for '$global', if we can't find it in context, then
    we can tell it's a global variable.

    All local variables and arguments are in the context. Then it's easy to manage scope --
        create new context by adding new name to pervious context when entering a function / block;
        continue use previous one when exit the function / block.

    We can also use this way to distinguish them from global ones.

    In fact, arguments are newly allocated reference to the actually passed one.
    So they're reference to reference in most case. 
    We design so for re-binding argument to new values.
    
*)

#require "flow_parser";;

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
    | LDelete of lexpr * lexpr 
    | LLambda of string list * lexpr 
    | LBreak of string * lexpr 
    | LLabel of (string * lexpr)
    | LApp of lexpr * lexpr list
;;



let rec parens1 (cmd: string): string =
    " (" ^ cmd ^ ") "
and 

parens2 (cmd: string) (content: string): string =
    " (" ^ cmd ^ " " ^ content ^ ") "

and 

parens3 (cmd: string) (content: string) (extra: string): string =
    " (" ^ cmd ^ " " ^ content ^ " " ^ extra ^ ") "

and

parens4 (cmd: string) (content: string) (extra1: string) (extra2: string): string =
    " (" ^ cmd ^ " " ^ content ^ " " ^ extra1 ^ " " ^ extra2 ^ ") "

and

s_expr (e: lexpr): string = 
    match e with
    | LSeq (e1, e2) -> parens3 "begin" (s_expr e1) (s_expr e2)
    | LSet (e1, e2) -> parens3 "set!" (s_expr e1) (s_expr e2)
    | LAlloc e1 -> parens2 "alloc" (s_expr e1)
    | LDeref e1 -> parens2 "deref" (s_expr e1)
    | LUpdateField (e1, e2, e3) -> parens4 "update-field" (s_expr e1) (s_expr e2) (s_expr e3)
    | LUndefined -> "undefined"
    | LNum n -> " " ^ string_of_float n
    | LObject obj -> 
        let ptos (p: string * lexpr): string = parens2 ("\"" ^ (fst p) ^ "\"") (s_expr (snd p)) in
        parens2 "object" (String.concat "" (List.map ptos obj))
    | LId id -> id
    | LString s -> "\"" ^ s ^ "\""
    | LLet (list, expr) -> 
        let ptos (p: string * lexpr): string = parens2 (fst p) (s_expr (snd p)) in
        let slist = parens1 (String.concat "" (List.map ptos list)) in
        let sexpr = s_expr expr in
        parens3 "let" slist sexpr
    | LGetField (obj, idx) -> parens3 "get-field" (s_expr obj) (s_expr idx)
    | LApp (func, arg) ->
        parens2 (s_expr func) (String.concat "" (List.map s_expr arg))
    | LDelete (obj, field) -> parens3 "delete-field" (s_expr obj) (s_expr field)
    | LLambda (args, block) -> parens3 "lambda" (parens1 (String.concat " " args)) (s_expr block)
    | LBreak (label, expr) -> parens4 "break" label " " (s_expr expr)
    | LLabel (label, expr) -> parens4 "label" label " " (s_expr expr)
    | _ -> raise @@ Failure "Not supported s_expr" 
;;

let rec to_string (e: lexpr) : string = 
    match e with
    | LString s -> s
    | _ -> raise @@ Failure "Unsupported conversion"

and

desugar_literal (ctx: (string * bool) list) (l: Loc.t Flow_ast.Literal.t): lexpr =
    match l with {value = value} -> 
    match value with
    | Number n -> LNum n
    | String n -> LString n
    | _ -> raise @@ Failure "Unsupported literal"

and

desugar_properties_init_key (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.Property.key): lexpr = 
    match e with
    | Literal l -> desugar_literal ctx @@ snd l
    | _ -> raise @@ Failure "Unsupported literal"

and

desugar_property (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.property): (string * lexpr) = 
    match e with
    | SpreadProperty p -> raise @@ Failure "SpreadProperty is not supported"
    | Property (_, p) -> (
        match p with
        | Init {key = key; value = value} -> (to_string @@ desugar_properties_init_key ctx key, desugar_expr ctx value)
        | _ -> raise @@ Failure "Unsupported property type"
    )
 
and

desugar_properties (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.property list): lexpr = 
    LAlloc (LObject (("$proto", LId "@Object_prototype") 
        :: ("$class", LString "Object") :: (List.map (desugar_property ctx) e)))

and

desugar_object (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.Object.t): lexpr = 
    match e with {properties = properties} ->
        desugar_properties ctx properties

and

desguar_property_expression (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.t): lexpr =
    let idx = desugar_expr ctx e in
    match idx with 
    | LNum num -> LString (string_of_int (int_of_float num))
    | LString _ -> idx
    | _ -> raise @@ Failure "Unsupported member property expression"

and

desugar_member (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.Member.t): lexpr =
    match e with {_object = _object; property = property} ->
    let obj = desugar_expr ctx _object in
    match property with
    | PropertyExpression pe -> 
        let idx = desguar_property_expression ctx pe in
        LGetField (LDeref obj, idx)
    | PropertyIdentifier pd -> 
        let id = desugar_identifer_name ctx pd in
        LGetField (LDeref obj, LString id)
    | _ -> raise @@ Failure "Unsupported member property"

and

desugar_identifer_name (ctx: (string * bool) list) (id: (Loc.t, Loc.t) Flow_ast.Identifier.t): string =
    let id' = snd id in
    match id' with {name = name} ->
    name

and

desugar_identifer (ctx: (string * bool) list) (id: (Loc.t, Loc.t) Flow_ast.Identifier.t): lexpr =
    let name = desugar_identifer_name ctx id in
    match List.find_opt (fun s -> fst s = name) ctx with
        | Some (_, true) -> LDeref (LId name)  (* LDeref is necessary; don't forget it's a reference to reference *)
        | Some (_, false) -> raise @@ Failure "Not assignable"
        | None -> LGetField (LDeref (LId "$global"), LString name)

and

desugar_pattern_identifer (ctx: (string * bool) list) (id: (Loc.t, Loc.t) Flow_ast.Pattern.Identifier.t): lexpr =
    match id with {name = name} ->
    let id' = desugar_identifer_name ctx name in
    LId id'

and

desugar_pattern (ctx: (string * bool) list) (p: (Loc.t, Loc.t) Flow_ast.Pattern.t): lexpr =
    let p' = snd p in
    match p' with
    | Expression e -> desugar_expr ctx e
    | Identifier e -> desugar_pattern_identifer ctx e
    | _ ->raise @@ Failure "Unsupported pattern" 

and


desugar_assignment_var (ctx: (string * bool) list) (id: string) (r: lexpr) : lexpr =
    match List.find_opt (fun s -> fst s = id) ctx  with
    | Some (_, true) -> LSet (LId id, r)
    | Some (_, false) -> raise @@ Failure "Can't assign value to const"
    | None -> let global = (LDeref (LId "$global")) in
        LSet (LId "$global", LUpdateField (global, LString id, r))

and 

desugar_assignment_left (ctx: (string * bool) list) (l: lexpr): lexpr =
    match l with 
    | LGetField (LDeref obj, field) -> LGetField (LDeref obj, field)
    | LId id -> (
        match List.find_opt (fun s -> fst s = id) ctx  with
        | Some (_, true) -> LDeref (LId id)
        | Some (_, false) -> raise @@ Failure "Can't assign value to const"
        | None -> let global = (LDeref (LId "$global")) in
            LGetField (global, LString id)
    )
    | _ -> raise @@ Failure ("Unsupported lvalue assignment")


and

desugar_assignment_right (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.Assignment.t): lexpr =
    match e with {operator = operator; left = left; right = right} ->
    let l = desugar_assignment_left ctx (desugar_pattern ctx left) in
    let r = desugar_expr ctx right in
    match operator with
    | None -> r
    | Some (PlusAssign) -> LApp ((LId "+"), [l; r])
    | Some (MinusAssign) -> LApp ((LId "-"), [l; r])
    | Some (MultAssign) -> LApp ((LId "*"), [l; r])
    | Some (DivAssign) -> LApp ((LId "/"), [l; r])
    | _ -> raise @@ Failure ("Unsupported type assignment")

and

desugar_assignment (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.Assignment.t): lexpr =
    match e with {left = left; right = right} ->
    let l = desugar_pattern ctx left in
    let r = desugar_assignment_right ctx e in
    match l with 
    | LGetField (LDeref obj, field) -> LSet (obj, LUpdateField (LDeref obj, field, r))
    | LId id -> desugar_assignment_var ctx id r
    | _ -> raise @@ Failure ("Unsupported assignment: " ^ s_expr l)

and

desugar_expression_or_spread (ctx: (string * bool) list) (l: (Loc.t, Loc.t) Flow_ast.Expression.expression_or_spread): lexpr =
    match l with 
    | Expression e -> desugar_expr ctx e
    | _ -> raise @@ Failure "Unsupported spread"

and

desugar_arglist (ctx: (string * bool) list) (l: (Loc.t, Loc.t) Flow_ast.Expression.ArgList.t): lexpr list =
    let l' = snd l in
    match l' with {arguments = arguments} ->
    List.map (desugar_expression_or_spread ctx) arguments

and 

desugar_call (ctx: (string * bool) list) (c: (Loc.t, Loc.t) Flow_ast.Expression.Call.t): lexpr =
    match c with {callee = callee; arguments = arguments} ->
    let func = desugar_expr ctx callee in
    let ag = desugar_arglist ctx arguments in
    match func with 
    | LGetField (LDeref (LId "$global"), LString "print") ->
        LApp (LId "print-string", [LApp (LId "prim->string", ag)])
    | LGetField (LDeref (LId "$global"), LString name) -> LApp (func, (LId "$global") :: ag)
    | _ -> raise @@ Failure "Not a valid callee"

and

desugar_delete (ctx: (string * bool) list) (arg: (Loc.t, Loc.t) Flow_ast.Expression.t): lexpr =
    let e = desugar_expr ctx arg in 
    match e with 
    | LGetField (LDeref v, field) -> LSet (v, LDelete (LDeref v, field))
    | _ -> raise @@ Failure "Wrong argument for delete"

and

desugar_unary (ctx: (string * bool) list) (op: (Loc.t, Loc.t) Flow_ast.Expression.Unary.t): lexpr =
    match op with {operator = operator; argument = argument} ->
    match operator with
    | Delete -> desugar_delete ctx argument
    | _ -> raise @@ Failure "Unsupported unary"

and

desugar_binary (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.Binary.t): lexpr =
    match e with {operator = operator; left = left; right = right} ->
    match operator with
    | Plus -> LApp ((LId "+"), [(desugar_expr ctx left); (desugar_expr ctx right)])
    | Minus -> LApp ((LId "-"), [(desugar_expr ctx left); (desugar_expr ctx right)])
    | Mult -> LApp ((LId "*"), [(desugar_expr ctx left); (desugar_expr ctx right)])
    | Div -> LApp ((LId "/"), [(desugar_expr ctx left); (desugar_expr ctx right)])
    | _ -> raise @@ Failure "Unsupported expression"

and 

desugar_array_element (ctx: (string * bool) list) (idx: int) (e: (Loc.t, Loc.t) Flow_ast.Expression.Array.element): (string * lexpr) =
    match e with 
    | Expression exp -> (string_of_int idx, desugar_expr ctx exp)
    | _ -> raise @@ Failure "Unsupported array element"

and 

desugar_array (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.Array.t): lexpr =
    let rec range i j = if i >= j then [] else i :: (range (i + 1) j) in
    match e with {elements = elements} ->
    LAlloc (LObject (("$class", LString "Array") 
        :: (List.map2 (desugar_array_element ctx) (range 0 (List.length elements)) elements)))

and
desugar_expr (ctx: (string * bool) list) (e: (Loc.t, Loc.t) Flow_ast.Expression.t): lexpr =
    let e' = snd e in 
    match e' with
    | Literal l -> desugar_literal ctx l
    | Object obj -> desugar_object ctx obj
    | Member mem -> desugar_member ctx mem
    | Identifier id -> desugar_identifer ctx id
    | Assignment assign -> desugar_assignment ctx assign
    | Call c -> desugar_call ctx c
    | Unary op -> desugar_unary ctx op
    | Binary op -> desugar_binary ctx op
    | Array arr -> desugar_array ctx arr
    | _ -> raise @@ Failure "Unsupported expression"

and

desugar_declarator_init (ctx: (string * bool) list) (init: (Loc.t, Loc.t) Flow_ast.Expression.t option): lexpr =
    match init with
    | None -> LUndefined
    | Some init -> desugar_expr ctx init

and

desugar_declarator (ctx: (string * bool) list) (decl: (Loc.t, Loc.t) Flow_ast.Statement.VariableDeclaration.Declarator.t): lexpr =
    let decl' = snd decl in
    match decl' with {id = id; init = init} ->
    let id' = snd id in 
    match id' with
    | Identifier {name = name} -> (
        let name' = snd name in
        match List.find_opt (fun s -> fst s = name'.name) ctx  with
        | Some (_, true) -> raise @@ Failure "local variable is not supported" 
        | Some (_, false) -> raise @@ Failure "Not assignable"
        | None -> (* It's global. if it exists, do nothing, else set to undefined. *)
            let e = desugar_declarator_init ctx init in
            LSet (LId "$global", (LUpdateField (LDeref (LId "$global"), LString name'.name, e)))
        )
    | _ -> raise @@ Failure "Only Identifier is supported"

and

desugar_variableDeclaration (ctx: (string * bool) list) (decls: (Loc.t, Loc.t) Flow_ast.Statement.VariableDeclaration.t): lexpr =
    match decls with {declarations = declarations} ->
    List.fold_right (fun l r -> LSeq (l, r)) (List.map (desugar_declarator ctx) declarations) LUndefined

and 

desugar_func_id (ctx: (string * bool) list) (id: (Loc.t, Loc.t) Flow_ast.Identifier.t option): string =
    match id with 
    | Some (_, id') -> (match id' with {name = name} -> name)
    | None -> raise @@ Failure "Function must have id"

and

desugar_func_param (ctx: (string * bool) list) (p: (Loc.t, Loc.t) Flow_ast.Function.Param.t): (string * bool) =
    let p' = snd p in
    match p' with {argument = argument} ->
    match desugar_pattern ctx argument with
    | LId id -> (id, true)
    | _ -> raise @@ Failure "parameter is not a identifier"

and

desugar_func_params (ctx: (string * bool) list) (p: (Loc.t, Loc.t) Flow_ast.Function.Params.t): (string * bool) list =
    let p' = snd p in 
    match p' with {params = params} ->
    List.map (desugar_func_param ctx) params

and

desugar_stmt_block (ctx: (string * bool) list) (block: (Loc.t, Loc.t) Flow_ast.Statement.Block.t): lexpr =
    match block with {body = body} ->
    List.fold_right (fun l r -> LSeq (l, r)) (List.map (desugar_stmt ctx) body) LUndefined

and

desugar_func_body (ctx: (string * bool) list) (body: (Loc.t, Loc.t) Flow_ast.Function.body): lexpr =
    match body with
    | BodyBlock (_, block) -> desugar_stmt_block ctx block
    | _ -> raise @@ Failure "Only BodyBlock is supported"

and

allocate_param (param: (string * bool)): (string * lexpr) =
    (fst param, LAlloc (LId (fst param)))

and

desugar_func (ctx: (string * bool) list) (func: (Loc.t, Loc.t) Flow_ast.Function.t): lexpr =
    match func with {id = id; params = params; body = body} ->
    let id' = desugar_func_id ctx id in
    let params' = desugar_func_params ctx params in
    let alloc = List.map allocate_param params' in
    let body' = desugar_func_body (params' @ ctx) body in
    let lambda = LLambda ("this" :: (List.map fst params'), LLet (alloc, LLabel ("$return", body'))) in
    LSet (LId "$global", (LUpdateField (LDeref (LId "$global"), LString id', lambda)))

and

desugar_return (ctx: (string * bool) list) (ret: (Loc.t, Loc.t) Flow_ast.Statement.Return.t): lexpr =
    match ret with {argument = arguments} ->
    match arguments with
    | Some expr -> LBreak ("$return", (desugar_expr ctx expr))
    | None -> raise @@ Failure "Only return value is supported"

and

(* statement is the top level element in js *)
desugar_stmt (ctx: (string * bool) list) (stmt: (Loc.t, Loc.t) Flow_ast.Statement.t): lexpr =
    let stmt' = snd stmt in
    match stmt' with
    | VariableDeclaration var ->  desugar_variableDeclaration ctx var
    | Expression expr ->  desugar_expr ctx expr.expression
    | FunctionDeclaration func ->  desugar_func ctx func
    | Return ret ->  desugar_return ctx ret
    | _ -> raise @@ Failure "Only VariableDeclaration is supported"

and

desugar ((prog, _): (Loc.t, Loc.t) Flow_ast.Program.t * 'b): lexpr =
    let prog' = snd prog in 
    let stmts = prog'.statements in
    List.fold_right (fun l r -> LSeq (l, r)) (List.map (desugar_stmt []) stmts) LUndefined
;;

let set_env (expr: lexpr) : lexpr =
    LLet ([("$global", LAlloc (LObject []))],
    LLet ([("@Object_prototype", LAlloc(LObject []))], expr
    ))
;;

let desguar_code (code: string) =
    print_string code;
    let ast: lexpr = set_env @@ desugar @@ Parser_flow.program code in
    print_string @@ s_expr ast ^ "\n"
;;


let code = "
    var v = {'name': 'liwei', 'answer': 42}; 
    var c = 5, b = 6;
    print (v['name'])
" in desguar_code code;;



let code = "
    var v = {'name': 'liwei', 'answer': 42}; 
    v['name'] = 5;
    print (v['name']);
    delete v['name'];
    print (v['name']);
" in desguar_code code;


let code = "
    var x = {'a': 'b'};
    x = {'x': 'x'};
    print (x['x']);
" in desguar_code code;;


let code = "
    function proc(x) {
        print ('')
        print ('enter <proc>');
        print ('value of x[name]:');
        print (x['name']);

        delete x['name'];
        print ('after delete x[name]:');
        print (x['name']);

        x = {'answer': 99}

        print ('leave <proc>');
        print ('')
        return x['answer'];
    }

    var v = 5;
    v = {'age': 18, 'name': 'liwei', 'answer': 42}; 

    print ('value of v[name]:');
    print (v['name']);

    var c = proc (v);
    print('return from [proc]:');
    print(c);

    print ('value of v[age]:');
    print (v['age']);
    print ('value of v[name]:');
    print (v['name']);
    print('set v[name] to Cui:');
    v['name'] = 'Cui';
    print (v['name']);
" in desguar_code code;;

let code = "
    function proc(x) {
        x = {'b': 'a'};
        print (x['b']);
    }
    var c = {'a': 'b'};
    proc (c);
    print (c['a']);
" in desguar_code code;;


let code = "
    var k = 72;
    var c = k + 12;
    print (c / 2);
" in desguar_code code;;

let code = "
    var k = {'v': 63, 'x': {'y': 42}};
    k.v += 1;
    k['v'] /= (1 + 1);
    print (k['v']);
    print (k.x.y + 1);
" in desguar_code code;;


let code = "
    var k = [1, 'liwei', 3];
    print (k[1]);
    delete k[1];
    print (k[1]);
    k[1] = 5;
    print (k[1]);
" in desguar_code code;;