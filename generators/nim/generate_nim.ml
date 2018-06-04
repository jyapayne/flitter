open Common

open Parse_info
open Ast_cpp

module Flag = Flag_parsing_cpp

let process_either _of_a _of_b =
  function
  | Left left -> "" ^ _of_a left
  | Right right -> "" ^ _of_b right

let is_empty s =
  (s = "")

let gen_indent num_spaces =
  let rec aux acc num_spaces =
    match num_spaces with
    | 0 -> acc
    | rest -> aux (acc^" ") (num_spaces-1)
  in
  aux "" num_spaces

let replace search sub str =
  Str.global_replace (Str.regexp search) sub str

let indent ?(level=2) str =
  let indent_str = gen_indent level
  and search = Str.regexp "\n" in
  indent_str ^ String.trim (Str.global_replace search ("\n" ^ indent_str) str)


let process_option ofa x =
  match x with
  | None -> ""
  | Some stuff -> "" ^ ofa stuff

let process_tuple_option ofa x =
  match x with
  | None -> ("", "")
  | Some stuff -> ofa stuff

let process_list ?(delimiter=", ") _of_a node =
  let map = List.map _of_a node
  in String.concat delimiter map


let rec process_info token =
  process_token token

and process_token tok =
  match tok.token with
  | OriginTok loc -> loc.str
  | FakeTokStr (v1, opt) -> ""
  | Ab -> ""
  | ExpandedTok (tok1, tok2, integer) -> tok1.str

and wrap _of_a (v1, v2) =
  _of_a v1

and wrap2 _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = process_info v2 in
  v1 ^ v2

and process_paren _of_a (paren1, arglist, paren2) =
  let paren1 = process_token paren1
  and arglist = _of_a arglist
  and paren2 = process_token paren2
  in paren1 ^ arglist ^ paren2

and process_brace _of_a (br1, arglist, br2) =
  _of_a arglist

and process_bracket _of_a (br1, arglist, br2) =
  let br1 = process_token br1
  and arglist = _of_a arglist
  and br2 = process_token br2 in
  br1 ^ arglist ^ br2

and process_angle _of_a (ang1, args, ang2) =
  let ang1 = process_token ang1
  and args = _of_a args
  and ang2 = process_token ang2
  in ang1 ^ args ^ ang2

and process_comma_list _of_a node =
  process_list (wrap _of_a) node

and process_comma_list2 _of_a =
  process_list (process_either _of_a process_token)

let rec process_token tok =
  match tok.token with
  | OriginTok loc -> loc.str
  | FakeTokStr (v1, opt) -> ""
  | Ab -> ""
  | ExpandedTok (tok1, tok2, integer) ->
    tok1.str

and process_include_kind = function
  | Local -> ""
  | Standard -> ""
  | Weird -> ""

and process_define_expr expr =
  ""

and process_constant =
  function
  | String (str, is_wchar) -> str
  | MultiString -> ""
  | Char (str, is_wchar) -> str
  | Int str -> str
  | Float (str, ftype) -> str
  | Bool bval -> string_of_bool bval

and process_ident ident =
  match ident with
   | IdIdent (name, tok) ->
     process_token tok
   | IdTemplateId (ident, args) ->
     ""
   | IdDestructor (tok, simple_ident) ->
     let (_, idtok) = simple_ident in
     "destructor" ^ process_token idtok
   | IdOperator (tok, operator) ->
     ""
   | IdConverter (tok, fullType) ->
     ""

and process_argument arg =
  process_either process_expression process_weird_arg arg

and process_weird_arg =
  function
  | ArgType arg_type -> process_fullType arg_type
  | ArgAction arg_action -> process_action_macro arg_action

and process_action_macro =
  function
  | ActMisc act_misc ->
      process_list process_token act_misc

and process_typeC (tc, tok_list) =
  process_typeCbis tc

and process_floatType =
  function
  | CFloat -> "cfloat"
  | CDouble -> "cdouble"
  | CLongDouble -> "clongdouble"

and process_intType =
  function
  | CChar -> "cchar"
  | Si signed -> process_signed signed
  | CBool -> "cbool"
  | WChar_t -> "cwchar_t"

and process_signed (sign, base) =
  let sign = process_sign sign and base = process_base base
  in "c" ^ sign ^ base (* cuchar, cint, cuint, etc. *)

and process_base =
  function
  | CChar2 -> "char"
  | CShort -> "short"
  | CInt -> "int"
  | CLong -> "long"
  | CLongLong -> "longlong"
and process_sign =
  function
  | Signed -> ""
  | UnSigned -> "u"


and process_baseType =
  function
  | Void -> "void"
  | IntType intType -> process_intType intType
  | FloatType floatType -> process_floatType floatType

and process_param_name =
  function
  | None -> ""
  | Some (name, tok) -> process_token tok ^ ": "

and process_parameter {
    p_name = p_name;
    p_type = p_type;
    p_register = p_register;
    p_val = p_val
  } =
  let type_str = process_fullType p_type
  and p_name = process_param_name p_name in
  p_name ^ type_str

and process_functionType {
    ft_ret = ft_ret;
    ft_params = ft_params;
    ft_dots = ft_dots;
    ft_const = ft_const;
    ft_throw = ft_throw
  } =
  let ret_type = process_fullType ft_ret
  and paren_str =
    process_paren (process_comma_list process_parameter) ft_params in
  paren_str ^ ": " ^ ret_type

and process_simple_ident (name, tok) =
  name

and process_e_val (tok, cexpr) =
  let equals = process_token tok (* equals sign *)
  and cexpr = process_constExpression cexpr (* const expr *)
  in " " ^ equals ^ " " ^ cexpr

and process_enum_elem { e_name = e_name; e_val = e_val } =
  let e_name = process_simple_ident e_name
  and e_val = process_option process_e_val e_val in
  e_name ^ e_val

and process_constExpression expr = process_expression expr

and process_template_arguments args =
  process_angle (process_comma_list process_template_argument) args

and process_template_argument arg =
  process_either process_fullType process_expression arg

and process_qualifier =
  function
  | QClassname ((name, info)) ->
    name ^ process_info info
  | QTemplateId ((name, args)) ->
    let ident = process_simple_ident name
    and args = process_template_arguments args in
    ident ^ args

and process_name (v1, v2, v3) =
  let v1 = process_option process_token v1
  and v2 =
    process_list
      (fun (v1, v2) ->
         let v1 = process_qualifier v1
         and v2 = process_token v2 in
         v1 ^ v2)
      v2
  and v3 = process_ident v3
  in v1 ^ v2 ^ v3

and process_either_ft_or_expr ft_or_expr =
  process_either process_fullType process_expression ft_or_expr

and process_structUnion =
  function
  | Struct -> "struct"
  | Union -> "union"
  | Class -> "class"

and process_typeCbis =
  function
  | BaseType btype ->
      process_baseType btype
  | Pointer point ->
      "ptr " ^ process_fullType point
  | Reference ref ->
      "ref " ^ process_fullType ref
  | Array ((arr, typ)) ->
      let arr = process_bracket (process_option process_constExpression) arr
      and typ = process_fullType typ
      in arr ^ typ
  | FunctionType ftype ->
      process_functionType ftype
  | EnumDef ((name, ident, elements)) ->
      let ident = process_option process_simple_ident ident
      and elements =
        process_brace (process_comma_list process_enum_elem) elements
      in ident ^ " = enum\n" ^ elements
  | StructDef sdef ->
      "" (*process_class_definition sdef*)
  | EnumName ((enum, name)) ->
    process_simple_ident name
  | StructUnionName ((stype_tok, name)) ->
      let (stype, _) = stype_tok in
      let stype = process_structUnion stype
      and name = process_simple_ident name in
      stype ^ " " ^ name
  | TypeName ((tname)) ->
      process_name tname
  | TypenameKwd ((tname (* 'typename' *), tdef_name)) ->
      process_name tdef_name
  | TypeOf ((typeof, tdef)) ->
      process_paren process_either_ft_or_expr tdef
  | ParenType paren ->
      process_paren process_fullType paren

and process_info token =
  process_token token

and process_expression (expr, toks) =
  process_exprbis toks expr

and process_fixOp expr =
  function
  | Dec -> "- 1"
  | Inc -> "+ 1"

and process_binaryOp =
  function
  | Arith arith ->
    process_arithOp arith
  | Logical log ->
      process_logicalOp log

and process_arithOp =
  function
  | Plus -> "+"
  | Minus -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "mod"
  | DecLeft -> "shl"
  | DecRight -> "shr"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"

and process_logicalOp =
  function
  | Inf -> "<"
  | Sup -> ">"
  | InfEq -> "<="
  | SupEq -> ">="
  | Eq -> "=="
  | NotEq -> "!="
  | AndLog -> "and"
  | OrLog -> "or"

and process_unaryOp expr =
  let expr = process_expression expr in
  function
  | GetRef -> "addr " ^ expr
  | DeRef -> expr ^ "[]"
  | UnPlus -> "+" ^ expr
  | UnMinus -> "-"  ^ expr
  | Tilde -> "not " ^ expr
  | Not -> "not " ^ expr
  | GetRefLabel -> failwith "Ref labels are not supported in flitter!"

and process_assignOp =
  function
  | SimpleAssign -> "="
  | OpAssign arith ->
      process_arithOp arith

and process_exprbis toks =
  function
  | Id ((name, info)) ->
    let (_, _, ident) = name in
    process_ident ident
  | C const ->
    let res = match toks with
      | [] -> ""
      | main_tok :: _ -> process_token main_tok in
    res
  | Call ((expr, args)) ->
      let name = process_expression expr
      and args = process_paren (process_comma_list process_argument) args in
      name ^ args
  | CondExpr ((binary, first_option, second_option)) ->
    let bin = process_expression binary
      and first_op = process_option process_expression first_option
      and second_op = process_expression second_option in
    "if " ^ bin ^ ": " ^ first_op ^ " else: " ^ second_op
  | Sequence ((v1, v2)) ->
      (*let v1 = process_expression v1
      and v2 = process_expression v2
      in Ocaml.VSum (("Sequence", [ v1; v2 ]))*)
    ""
  | Assignment ((left, op, right)) ->
    let left = process_expression left
      and op = process_assignOp op
      and right = process_expression right in
    left ^ " " ^ op ^ " " ^ right
  | Postfix ((expr, op)) ->
      let expr = process_expression expr in
      let op_expr =
        match op with
        | Dec -> "postDec(" ^ expr ^ ")"
        | Inc -> "postInc(" ^ expr ^ ")" in
      op_expr
  | Infix ((expr, op)) ->
      let expr = process_expression expr in
      let op_expr =
        match op with
        | Dec -> "preDec(" ^ expr ^ ")"
        | Inc -> "preInc(" ^ expr ^ ")" in
      op_expr
  | Unary ((expr, op)) ->
      process_unaryOp expr op
  | Binary ((left, op, right)) ->
    let left = process_expression left
    and op = process_binaryOp op
    and right = process_expression right
    in left ^ " " ^ op ^ " " ^ right
  | ArrayAccess ((expr, bracket_expr)) ->
    let expr = process_expression expr
      and bracket_expr = process_bracket process_expression bracket_expr in
    expr ^ bracket_expr
  | RecordAccess ((expr, field)) ->
    let expr = process_expression expr
      and field = process_name field in
    expr ^ "." ^ field
  | RecordPtAccess ((expr, field)) ->
    let expr = process_expression expr
      and field = process_name field in
    expr ^ "." ^ field
  | RecordStarAccess ((left, right)) ->
    (*
     * Not sure what this is, exactly. The nim code
     * will have it commented out for now.
     *)
    let left = process_expression left
      and right = process_expression right in
    "# " ^ left ^ process_list process_token toks ^ right
  | RecordPtStarAccess ((left, right)) ->
    (*
     * Not sure what this is, exactly. The nim code
     * will have it commented out for now.
     *)
    let left = process_expression left
      and right = process_expression right in
    "# " ^ left ^ process_list process_token toks ^ right
  | SizeOfExpr ((sizeof, expr)) ->
    let expr = process_expression expr in
    "sizeof(" ^ expr ^ ")"
  | SizeOfType ((sizeof, fullType)) ->
    let ty = process_paren process_fullType fullType in
    "sizeof" ^ ty
  | Cast (((left, fullType, right), expr)) ->
    let fullType = process_fullType fullType
      and expr = process_expression expr in
      "cast[" ^ fullType ^ "](" ^ expr ^ ")"
  | StatementExpr v1 ->
      process_paren process_compound v1
  | GccConstructor ((v1, v2)) ->
    (*
     * Not sure what this is, exactly. The nim code
     * will have it commented out for now.
     *)
    let v1 = process_paren process_fullType v1
      and v2 = process_brace (process_comma_list process_initialiser) v2 in
    "# " ^ v1 ^ process_list process_token toks ^ v2
  | This v1 ->
    (*let v1 = process_tok v1 in Ocaml.VSum (("This", [ v1 ]))*)
    ""
  | ConstructedObject ((fullType, params)) ->
    let fullType = process_fullType fullType
      and params = process_paren (process_comma_list process_argument) params in
    fullType ^ params
  | TypeId ((v1, v2)) ->
      (*let v1 = process_tok v1
      and v2 = process_paren process_either_ft_or_expr v2
      in Ocaml.VSum (("TypeId", [ v1; v2 ]))*)
    ""
  | CplusplusCast ((cast_op, fullType, expr)) ->
      (*let v1 = process_wrap2 process_cast_operator v1
      and v2 = process_angle process_fullType v2
      and v3 = process_paren process_expression v3
      in Ocaml.VSum (("CplusplusCast", [ v1; v2; v3 ]))*)
    ""
  | New ((v1, v2, v3, v4, v5)) ->
      (*let v1 = Ocaml.process_option process_tok v1
      and v2 = process_tok v2
      and v3 = Ocaml.process_option (process_paren (process_comma_list process_argument)) v3
      and v4 = process_fullType v4
      and v5 = Ocaml.process_option (process_paren (process_comma_list process_argument)) v5
      in Ocaml.VSum (("New", [ v1; v2; v3; v4; v5 ]))*)
    ""
  | Delete ((v1, v2)) ->
      (*let v1 = Ocaml.process_option process_tok v1
      and v2 = process_expression v2
      in Ocaml.VSum (("Delete", [ v1; v2 ]))*)
    ""
  | DeleteArray ((tok, expr)) ->
      let tok = process_option process_token tok
      and expr = process_expression expr
      in tok ^ expr
  | Throw throw ->
      process_option process_expression throw
  | ParenExpr paren_expr ->
      process_paren process_expression paren_expr
  | ExprTodo -> "TODO"

and process_selection =
  function
  | If ((if_tok, paren_expr, stmt1, else_tok, stmt2)) ->
    let paren_expr = process_paren process_expression paren_expr
      and stmt1 = process_statement stmt1
      and else_tok = process_option process_token else_tok
      and stmt2 = process_statement stmt2 in
    "if" ^ paren_expr ^
      stmt1 ^ "\n" ^
    replace "elseif" "elif" (else_tok ^ stmt2)
  | Switch ((v1, v2, v3)) ->
      let v1 = process_token v1
      and v2 = process_paren process_expression v2
      and v3 = process_statement v3
      in v1 ^ v2 ^ v3
and process_iteration =
  function
  | While ((v1, v2, v3)) ->
      let v1 = process_token v1
      and v2 = process_paren process_expression v2
      and v3 = process_statement v3
      in v1 ^ v2 ^ v3
  | DoWhile ((v1, v2, v3, v4, v5)) ->
      let v1 = process_token v1
      and v2 = process_statement v2
      and v3 = process_token v3
      and v4 = process_paren process_expression v4
      and v5 = process_token v5
      in v1 ^ v2 ^ v3 ^ v4 ^v5
  | For ((v1, v2, v3)) ->
      let v1 = process_token v1
      and v2 =
        process_paren
          (fun (v1, v2, v3) ->
             let v1 = wrap process_exprStatement v1
             and v2 = wrap process_exprStatement v2
             and v3 = wrap process_exprStatement v3
             in v1 ^ v2 ^ v3)
          v2
      and v3 = process_statement v3
      in v1 ^ v2 ^ v3
  | MacroIteration ((v1, v2, v3)) ->
      let v1 = process_simple_ident v1
      and v2 = process_paren (process_comma_list process_argument) v2
      and v3 = process_statement v3
      in v1 ^ v2 ^ v3

and process_jump =
  function
  | Goto goto -> failwith "# XXX goto not supported: " ^ goto
  | Continue -> "continue"
  | Break -> "break"
  | Return -> "return"
  | ReturnExpr ret_expr ->
      "return " ^ process_expression ret_expr
  | GotoComputed goto_comp ->
    failwith "#[ XXX goto not supported: " ^ process_expression goto_comp ^ "]#"

and process_handler (v1, v2, v3) =
  let v1 = process_token v1
  and v2 = process_paren process_exception_declaration v2
  and v3 = process_compound v3
  in v1 ^ v2 ^ v3

and process_exception_declaration =
  function
  | ExnDeclEllipsis exn_ellipsis ->
      process_token exn_ellipsis
  | ExnDecl exn_decl ->
      process_parameter exn_decl

and get_tydef_prefix name storage =
  match storage with
  | NoSto -> ""
  | StoTypedef st_tdef ->
    "type " ^ name ^ " = "
  | Sto (sto, tok) -> ""

and process_onedecl {
    v_namei = v_namei;
    v_type = v_type;
    v_storage = v_storage
  } =
  let name =
    process_tuple_option
      (fun (name, init) ->
         let name = process_name name
         and init = process_option process_init init
         in (name, init))
      v_namei in
  let res = process_onedeclFullType "" name v_storage v_type in
  res

and process_onedeclFullType prefix (name, init) storage (qualifier, (typeCbis, tok_list)) =
  match typeCbis with
  | BaseType btype ->
      process_baseType btype
  | Pointer point ->
      process_onedeclFullType "ptr " (name, init) storage point
  | Reference ref ->
      process_onedeclFullType "ref " (name, init) storage ref
  | Array ((arr, typ)) ->
      let arr = process_bracket (process_option process_constExpression) arr
      and typ = process_fullType typ
      in arr ^ typ
  | FunctionType ftype ->
    let ret = match storage with
    | NoSto -> "proc " ^ name^init ^ process_functionType ftype
    | StoTypedef st_tdef ->
      "type " ^ name^init ^ " = " ^ "proc " ^ process_functionType ftype
    | Sto sto -> "proc " ^ name^init ^ process_functionType ftype in
    ret
  | EnumDef ((_, ident, elements)) ->
      let ident = process_option process_simple_ident ident
      and elements =
        process_brace (process_comma_list process_enum_elem) elements
      in
      let ty_str = "type " ^ ident ^ " = enum " ^ elements
      and let_stmt = "\nvar " ^ name ^ ": " ^ ident ^ " " ^ init in
      if is_empty(name) then ty_str else ty_str ^ let_stmt
  | StructDef sdef ->
      "" (*process_class_definition sdef*)
  | EnumName ((enum, name)) ->
    process_simple_ident name
  | StructUnionName ((stype_tok, name)) ->
      let (stype, _) = stype_tok in
      let stype = process_structUnion stype
      and name = process_simple_ident name in
      stype ^ " " ^ name
  | TypeName ((tname)) ->
      process_name tname
  | TypenameKwd ((tname (* 'typename' *), tdef_name)) ->
      process_name tdef_name
  | TypeOf ((typeof, tdef)) ->
      process_paren process_either_ft_or_expr tdef
  | ParenType (left, type_inf, right) ->
      process_onedeclFullType "" (name, init) storage type_inf

and process_storage st = process_storagebis st
and process_storagebis =
  function
  | NoSto -> ""
  | StoTypedef st_tdef ->
    process_token st_tdef
  | Sto sto -> wrap2 process_storageClass sto

and process_storageClass =
  function
  | Auto -> "auto"
  | Static -> "static"
  | Register -> "register"
  | Extern -> "extern"

and process_init =
  function
  | EqInit ((equals, init)) ->
      let equals = process_token equals
      and init = process_initialiser init
      in equals ^ " " ^ init
  | ObjInit v1 ->
      process_paren (process_comma_list process_argument) v1

and process_block_declaration =
  function
  | DeclList ((decl, semi_col)) ->
      process_comma_list process_onedecl decl
  | MacroDecl ((v1, v2, v3, v4)) ->
      let v1 = process_list process_token v1
      and v2 = process_simple_ident v2
      and v3 = process_paren (process_comma_list process_argument) v3
      and v4 = process_token v4
      in v1 ^ v2 ^ v3 ^ v4
  | UsingDecl v1 ->
      let v1 =
        (match v1 with
         | (v1, v2, v3) ->
             let v1 = process_token v1
             and v2 = process_name v2
             and v3 = process_token v3
             in v1 ^ v2 ^ v3)
      in v1
  | UsingDirective ((v1, v2, v3, v4)) ->
      let v1 = process_token v1
      and v2 = process_token v2
      and v3 = process_name v3
      and v4 = process_token v4
      in v1 ^ v2 ^ v3 ^ v4
  | NameSpaceAlias ((v1, v2, v3, v4, v5)) ->
      let v1 = process_token v1
      and v2 = process_simple_ident v2
      and v3 = process_token v3
      and v4 = process_name v4
      and v5 = process_token v5
      in v1 ^ v2 ^ v3 ^ v4 ^ v5
  | Asm ((v1, v2, v3, v4)) ->
      let v1 = process_token v1
      and v2 = process_option process_token v2
      and v3 = process_paren process_asmbody v3
      and v4 = process_token v4
      in v1 ^ v2 ^ v3 ^ v4

and process_asmbody (v1, v2) =
  let v1 = process_list process_token v1
  and v2 = process_list (wrap process_colon) v2
  in v1 ^ v2
and process_colon =
  function
  | Colon v1 ->
      let v1 = process_comma_list process_colon_option v1
      in v1
and process_colon_option v = wrap process_colon_optionbis v
and process_colon_optionbis =
  function
  | ColonMisc -> "colonmisc"
  | ColonExpr v1 ->
      let v1 = process_paren process_expression v1
      in v1

and process_statement stmt = wrap process_statementbis stmt
and process_statementbis =
  function
  | Compound comp ->
    ":\n" ^ indent (process_compound comp)
  | ExprStatement expr ->
    process_exprStatement expr
  | Labeled labeled ->
    process_labeled labeled
  | Selection selection ->
    process_selection selection
  | Iteration iter ->
    process_iteration iter
  | Jump jump ->
    process_jump jump
  | DeclStmt decl ->
    process_block_declaration decl
  | Try ((tok, comp, handler_list)) ->
    let comp = process_compound comp
    and handler_list = process_list process_handler handler_list
    in "try: " ^ comp ^ handler_list
  | NestedFunc nest_func ->
      process_func_definition nest_func
  | MacroStmt -> ""
  | StmtTodo -> "# TODO"

and process_compound comp =
  process_brace (process_list ~delimiter:"\n" process_statement_sequencable) comp

and process_statement_sequencable =
  function
  | StmtElem stmt ->
      process_statement stmt
  | CppDirectiveStmt direc ->
      process_cpp_directive direc
  | IfdefStmt ifdef ->
      process_ifdef_directive ifdef

and process_ifdef_directive if_def = wrap2 process_ifdefkind if_def
and process_ifdefkind =
  function (* TODO fix this for Nim *)
  | Ifdef -> "ifdef"
  | IfdefElse -> "ifdefelse"
  | IfdefElseif -> "ifdefelseif"
  | IfdefEndif -> "ifdefendif"

and process_exprStatement expr_stmt =
  process_option process_expression expr_stmt

and process_labeled =
  function
  | Label ((name, stmt)) ->
    let stmt = process_statement stmt
    in name ^ " " ^ stmt
  | Case ((expr, stmt)) ->
      let expr = process_expression expr
      and stmt = process_statement stmt
      in expr ^ " " ^stmt
  | CaseRange ((expr1, expr2, stmt)) ->
      let expr1 = process_expression expr1
      and expr2 = process_expression expr2
      and stmt = process_statement stmt
      in expr1 ^ expr2 ^ stmt
  | Default def ->
      process_statement def

and process_initialiser =
  function
  | InitExpr v1 ->
      process_expression v1
  | InitList v1 ->
      process_brace (process_comma_list process_initialiser) v1
  | InitDesignators ((v1, v2, v3)) ->
      let v1 = process_list process_designator v1
      and v2 = process_token v2
      and v3 = process_initialiser v3
      in v1 ^ v2 ^ v3
  | InitFieldOld ((v1, v2, v3)) ->
      let v1 = process_simple_ident v1
      and v2 = process_token v2
      and v3 = process_initialiser v3
      in v1 ^ v2 ^ v3
  | InitIndexOld ((v1, v2)) ->
      let v1 = process_bracket process_expression v1
      and v2 = process_initialiser v2
      in v1 ^ v2
and process_designator =
  function
  | DesignatorField ((v1, v2)) ->
      let v1 = process_token v1
      and v2 = process_simple_ident v2
      in v1 ^ v2
  | DesignatorIndex v1 ->
      process_bracket process_expression v1
  | DesignatorRange v1 ->
      process_bracket
        (fun (v1, v2, v3) ->
           let v1 = process_expression v1
           and v2 = process_token v2
           and v3 = process_expression v3
           in v1 ^ v2 ^ v3)
        v1

and process_define_val =
  function
  | DefinePrintWrapper ((if_tok, expr_paren, name)) ->
    let expr_paren = process_paren process_expression expr_paren
    and name = process_name name in
    expr_paren ^ name
  | DefineExpr expr ->
    process_expression expr
  | DefineStmt stmt ->
    process_statement stmt
  | DefineType dtype ->
    process_fullType dtype
  | DefineDoWhileZero (stmt, tok_list) ->
    process_statement stmt
  | DefineFunction dfunc ->
    process_func_definition dfunc
  | DefineInit init ->
    process_initialiser init
  | DefineText (str, toks) ->
    str
  | DefineEmpty -> ""
  | DefineTodo -> ""

and process_define _tok ident kind value =
  match kind with
  | DefineVar ->
    let (idname, _ ) = ident in
    "const " ^ idname ^ " = " ^ process_define_val value
  | DefineFunc func ->
    ""
  (*let (idname, _) = ident
  in *)

and process_include ((tok, kind, path)) =
  let include_file =
    match kind with
    | Local -> path
    | Standard -> path
    | Weird ->
      let search = Str.regexp "_"
      and lower = String.lowercase_ascii path
      in Str.global_replace search "." lower
  in "#" ^ include_file ^ " " ^ process_token tok

and process_cpp_directive = function
  | Define ((tok, ident, kind, value)) ->
    process_define tok ident kind value
  | Include ((tok, inc_kind, path)) ->
    process_include (tok, inc_kind, path)
  | Undef ((name, tok)) ->
    process_token tok
  | PragmaAndCo tok ->
    process_token tok

and process_func_definition {
    f_name = f_name;
    f_type = f_type;
    f_storage = f_storage;
    f_body = f_body
  } =
  let name = process_name f_name
  and def_str = process_functionType f_type
  and body = process_compound f_body in

  "proc " ^ name ^ def_str ^ " =\n" ^ (indent body)

and process_func_or_else =
  function
  | FunctionOrMethod func_meth ->
      process_func_definition func_meth
  | Constructor ((func)) ->
      process_func_definition func
  | Destructor func ->
      process_func_definition func

and process_declaration =
  function
  | BlockDecl block ->
    process_block_declaration block
  | Func func ->
    process_func_or_else func
  | TemplateDecl (v1, v2, v3) ->
    (*let v1 = process_tok v1
    and v2 = process_template_parameters v2
    and v3 = process_declaration v3
    in Ocaml.VSum (("TemplateDecl", [ v1; v2; v3 ]))*)
    ""
  | TemplateSpecialization ((v1, v2, v3)) ->
      (*let v1 = process_tok v1
      and v2 = process_angle Ocaml.process_unit v2
      and v3 = process_declaration v3
      in Ocaml.VSum (("TemplateSpecialization", [ v1; v2; v3 ]))*)
    ""
  | ExternC ((v1, v2, v3)) ->
      (*let v1 = process_tok v1
      and v2 = process_tok v2
      and v3 = process_declaration v3
      in Ocaml.VSum (("ExternC", [ v1; v2; v3 ]))*)
    ""
  | ExternCList ((v1, v2, v3)) ->
      (*let v1 = process_tok v1
      and v2 = process_tok v2
      and v3 = process_brace (Ocaml.process_list process_declaration_sequencable) v3
      in Ocaml.VSum (("ExternCList", [ v1; v2; v3 ]))*)
    ""
  | NameSpace ((v1, v2, v3)) ->
      (*let v1 = process_tok v1
      and v2 = process_wrap2 Ocaml.process_string v2
      and v3 = process_brace (Ocaml.process_list process_declaration_sequencable) v3
      in Ocaml.VSum (("NameSpace", [ v1; v2; v3 ]))*)
    ""
  | NameSpaceExtend ((v1, v2)) ->
      (*let v1 = Ocaml.process_string v1
      and v2 = Ocaml.process_list process_declaration_sequencable v2
      in Ocaml.VSum (("NameSpaceExtend", [ v1; v2 ]))*)
    ""
  | NameSpaceAnon ((v1, v2)) ->
      (*let v1 = process_tok v1
      and v2 = process_brace (Ocaml.process_list process_declaration_sequencable) v2
      in Ocaml.VSu:m (("NameSpaceAnon", [ v1; v2 ]))*)
    ""
  | EmptyDef def -> ""
  | DeclTodo -> "# TODO"

and process_fullType ((qualifier, typeC)) =
  process_typeC typeC

and process_toplevel = function
  | NotParsedCorrectly node ->  "# Error parsing: " ^ process_list ~delimiter:"" process_token node
  | DeclElem node -> process_declaration node
  | CppDirectiveDecl node -> process_cpp_directive node
  | IfdefDecl node -> ""
  | MacroTop ((v1, v2, v3)) -> "# MacroTop"
  | MacroVarTop ((v1, v2)) -> "# MacroVarTop"

let iter_ast ast =
  List.map process_toplevel ast

let generate_nim cfile ?(macro_files = []) =
  Parse_cpp.init_defs cfile;
  List.iter Parse_cpp.add_defs macro_files;
  let ast = Parse_cpp.parse_program cfile in
  let res = iter_ast ast in
  String.concat "\n" res

let test_gen_nim file =
  let macro_list = [!Flag.macros_h] in
  let nim_str = generate_nim file ~macro_files:macro_list in
  pr nim_str

let actions () = [
    "-generate-nim", "   <file>",
    Common.mk_action_1_arg test_gen_nim;
]
