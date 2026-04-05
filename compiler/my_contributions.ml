(*In our course, the concrete syntax (the characters) is turned into tokens by the reader, then into s-expressions by the tag-parser*)

let rec macro_expand_qq sexpr = (*We decided to macro expand quote and quasiquote*)
    match sexpr with
    | ScmNil -> ScmPair (ScmSymbol "quote", ScmPair (ScmNil, ScmNil)) (*We represent the end of a list by ScmNil. In Scheme, all lists are nested paits*)
    | ScmVoid | ScmBoolean _ | ScmChar _ | ScmString _ | ScmNumber _-> sexpr
    | ScmSymbol sexpr -> ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol sexpr, ScmNil))
    | ScmPair(ScmSymbol "unquote", ScmPair(sexpr, ScmNil)) -> sexpr
    | ScmPair(ScmSymbol "unquote-splicing", ScmPair(sexpr, ScmNil)) -> sexpr
    | ScmVector(sexpr) ->
      let list_vector = List.fold_right(fun car cdr -> (ScmPair(car, cdr))) sexpr ScmNil in (*We turn out vector to a list for ease of access*)
      ScmPair(ScmSymbol "list->vector", ScmPair(macro_expand_qq list_vector, ScmNil)) (*We then turn it back to vector later*)
    | ScmPair(a, b) ->
      match a with 
      | ScmPair(ScmSymbol "unquote-splicing", ScmPair(sexpr, ScmNil)) ->
        (match b with
          | ScmNil -> sexpr (*Since b is scmNil, we return only the s-expression in a, (append (sexpr , scmNil))->sexpr *)
          | _ -> ScmPair (ScmSymbol "append", ScmPair (sexpr, ScmPair (macro_expand_qq b, ScmNil))))
      | _  -> ScmPair(ScmSymbol "cons", ScmPair(macro_expand_qq a, ScmPair(macro_expand_qq b, ScmNil)))
	  
	  
(*Scheme uses lexical addressing, for each appearance of a variable, we link its value in the lexical environment when it was linked
(Either by let or lambda)*)
let annotate_lexical_address =
    let rec run expr params env =
		match expr with
		(*...*)
		| ScmSeq exprs ->
         ScmSeq'(List.map (function expr-> run expr params env) exprs)
		| ScmOr exprs ->
			ScmOr'((List.map (function expr-> run expr params env) exprs))
		| ScmVarSet(Var v, expr) ->
			ScmVarSet' ((tag_lexical_address_for_var v params env), 
				run expr params env)
        (*tag_lexical_address_for_var is when we set the value of the variable to the expr 
        it's being set with(Reminder: Let is macro expanded as (let ((var1 expr1)....(varn exprn))..))*)
		| ScmLambda (params', Simple, expr) ->
			let env = params :: env in
			ScmLambda'(params' , Simple, run expr params' env) (*This is how we add the parameters to the lexical environment and part of the extension*)
		| ScmLambda (params', Opt opt, expr) ->
			let extend = params' @ [opt] in
			let env = params :: env in
			ScmLambda'(params' , Opt opt, run expr extend env) (*Opt is a list of optional arguments to the lambda*)
		| ScmApplic (proc, args) ->
			 ScmApplic'(run proc params env, (List.map (function arg-> run arg params env) args), Non_Tail_Call)
	in
	fun expr -> run expr [] [];;
	
	
(*We label each application if it's tail call or not, for optimization*)
let annotate_tail_calls = 
    let rec run in_tail = function
		(*...*)
		| ScmLambda' (params, kind, expr) ->
			ScmLambda'(params, kind, run true expr)
		| ScmApplic' (proc, args, app_kind) ->
			match in_tail with
			| true -> ScmApplic'(run false proc, List.map (function arg -> run false arg) args, Tail_Call)
			| _ -> ScmApplic'(run false proc, List.map (function arg -> run false arg) args, Non_Tail_Call)
	and runl in_tail expr = function
      | [] -> [run in_tail expr]
      | expr' :: exprs -> (run false expr) :: (runl in_tail expr' exprs)
    in fun expr' -> run false expr';;
	

let code_gen exprs' =
    let consts = make_constants_table exprs' in
    let free_vars = make_free_vars_table exprs' in
    let rec run params env = function
	(*...*)
  (*Here we differentiate between the tail call and non tail call: In non tail call we open a new frame for the application and in tail call we reuse the last frame, saving space*)
	| ScmApplic' (proc, args, Non_Tail_Call) -> 
        let args_count = List.length args in
        let reversed_args = List.rev args in
        let asm_code = 
          let args_code = 
            String.concat ""
              (List.map
                (fun arg ->
                  let arg_code = run params env arg in
                  arg_code
                  ^ "\tpush rax\n")
                reversed_args) in
          let proc_code = (run params env proc) in
            args_code  
            ^ (Printf.sprintf "\tpush %d\n" args_count)
            ^ proc_code
            ^ (Printf.sprintf "\tassert_closure(rax)\n") (*Verify that rax has type closure*)
            ^ (Printf.sprintf "\tpush SOB_CLOSURE_ENV(rax)\n") (*Push rax → env*)
            ^ (Printf.sprintf "\tcall SOB_CLOSURE_CODE(rax)\n") in (*Calling to execute the code*)
        asm_code
      | ScmApplic' (proc, args, Tail_Call) -> 
        (*It wouls be easier to explain this with names: Imagine we have a function call to f, f calls to g and g points to h, where h is a tail call. 
        We oveeride the frame of g with the frame of h *)
        let args_count = List.length args in
        let reversed_args = List.rev args in
        let asm_code = 
          let args_code = 
            String.concat ""
              (List.map
                (fun arg ->
                  let arg_code = run params env arg in
                  arg_code
                  ^ "\tpush rax\n")
                reversed_args) in
          let proc_code = (run params env proc) in
            args_code  
            ^ (Printf.sprintf "\tpush %d\n" args_count)
            ^ proc_code
            ^ (Printf.sprintf "\tassert_closure(rax)\n")        (*Verify that rax has type closure*)
            ^ (Printf.sprintf "\tpush SOB_CLOSURE_ENV(rax)\n")  (*Push rax → env*)
            ^ (Printf.sprintf "\tmov rdx, COUNT\n")             (*Arg count of the old frame*)
            ^ (Printf.sprintf "\tpush RET_ADDR\n")              (*Old ret addr*)
            ^ (Printf.sprintf "\tmov r15, rbp\n")               (*Save OLD_RBP in a register*)
            ^ (Printf.sprintf "\tmov rbp, OLD_RBP\n")           (*Save OLD_RBP in a register*)
            ^ (Printf.sprintf "\tpush rbp\n")                    (*Save OLD_RBP in a register*)
            ^ (Printf.sprintf "\tmov rcx, %d\n" (args_count + 4))(*Saving the amounts of arguments plus the enviroment, argumeent count, return address and the RBP of the frame*)
            ^ (Printf.sprintf "\tlea rsi, [r15 - 8]\n")
            ^ (Printf.sprintf "\tlea rdi, [r15 + 8 * (rdx + 3)]\n")
            ^ (Printf.sprintf "\tstd\n") (*STD was a flag for a command called movsq when we did the following:we move content from where rdi pointed to the address where rsi pointed to and then we increase/decrease both.
             If std was raised we moved downwards*)
            ^ (Printf.sprintf "\trep movsq\n") (*The command itself, it repeats until rcx had 0 in it*)
            (*We put the parameters and the frame information of h in g*)
            ^ (Printf.sprintf "\tcld\n")(*Another flag for movsq, this time we move upwards*)
            ^ (Printf.sprintf "\tsub rdx, %d\n" args_count)
            ^ (Printf.sprintf "\tlea rsp, [r15 + 8 * (rdx + 1)]\n")
            ^ (Printf.sprintf "\tmov rbp, [rsp - 8]\n") (*The rbp now points to the frame of f*)
            ^ (Printf.sprintf "\tjmp SOB_CLOSURE_CODE(rax)\n")
            in
        asm_code
		(*...*)
		