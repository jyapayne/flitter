open Common

open Parse_info
open Ast_cpp

module Flag = Flag_parsing_cpp

let process_either _of_a _of_b =
  function
  | Left left -> "" ^ _of_a left
  | Right right -> "" ^ _of_b right


let process_option ofa x =
  match x with
  | None -> ""
  | Some stuff -> "" ^ ofa stuff

let process_list _of_a node =
  let map = List.map _of_a node
  in String.concat ", " map


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
  | ExpandedTok (tok1, tok2, integer) -> tok1.str

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
  process_token tok

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
  process_exprbis expr

and process_exprbis =
  function
  | Id ((name, info)) ->
    let (_, _, ident) = name in
    process_ident ident
  | C const -> process_constant const
  | Call ((expr, args)) ->
      let name = process_expression expr
      and args = process_paren (process_comma_list process_argument) args
      in name ^ args
  | CondExpr ((v1, v2, v3)) ->
      (*let v1 = vof_expression v1
      and v2 = Ocaml.vof_option vof_expression v2
      and v3 = vof_expression v3*)
    ""
  | Sequence ((v1, v2)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_expression v2
      in Ocaml.VSum (("Sequence", [ v1; v2 ]))*)
    ""
  | Assignment ((v1, v2, v3)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_assignOp v2
      and v3 = vof_expression v3
      in Ocaml.VSum (("Assignment", [ v1; v2; v3 ]))*)
    ""
  | Postfix ((v1, v2)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_fixOp v2
      in Ocaml.VSum (("Postfix", [ v1; v2 ]))*)
    ""
  | Infix ((v1, v2)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_fixOp v2
      in Ocaml.VSum (("Infix", [ v1; v2 ]))*)
    ""
  | Unary ((v1, v2)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_unaryOp v2
      in Ocaml.VSum (("Unary", [ v1; v2 ]))*)
    ""
  | Binary ((v1, v2, v3)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_binaryOp v2
      and v3 = vof_expression v3
      in Ocaml.VSum (("Binary", [ v1; v2; v3 ]))*)
    ""
  | ArrayAccess ((v1, v2)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_bracket vof_expression v2
      in Ocaml.VSum (("ArrayAccess", [ v1; v2 ]))*)
    ""
  | RecordAccess ((v1, v2)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_name v2
      in Ocaml.VSum (("RecordAccess", [ v1; v2 ]))*)
    ""
  | RecordPtAccess ((v1, v2)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_name v2
      in Ocaml.VSum (("RecordPtAccess", [ v1; v2 ]))*)
    ""
  | RecordStarAccess ((v1, v2)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_expression v2
      in Ocaml.VSum (("RecordStarAccess", [ v1; v2 ]))*)
    ""
  | RecordPtStarAccess ((v1, v2)) ->
      (*let v1 = vof_expression v1
      and v2 = vof_expression v2
      in Ocaml.VSum (("RecordPtStarAccess", [ v1; v2 ]))*)
    ""
  | SizeOfExpr ((v1, v2)) ->
      (*let v1 = vof_tok v1
      and v2 = vof_expression v2
      in Ocaml.VSum (("SizeOfExpr", [ v1; v2 ]))*)
    ""
  | SizeOfType ((v1, v2)) ->
      (*let v1 = vof_tok v1
      and v2 = vof_paren vof_fullType v2
      in Ocaml.VSum (("SizeOfType", [ v1; v2 ]))*)
    ""
  | Cast ((v1, v2)) ->
      (*let v1 = vof_paren vof_fullType v1
      and v2 = vof_expression v2
      in Ocaml.VSum (("Cast", [ v1; v2 ]))*)
    ""
  | StatementExpr v1 ->
      (*let v1 = vof_paren vof_compound v1
      in Ocaml.VSum (("StatementExpr", [ v1 ]))*)
    ""
  | GccConstructor ((v1, v2)) ->
      (*let v1 = vof_paren vof_fullType v1
      and v2 = vof_brace (vof_comma_list vof_initialiser) v2
      in Ocaml.VSum (("GccConstructor", [ v1; v2 ]))*)
    ""
  | This v1 ->
    (*let v1 = vof_tok v1 in Ocaml.VSum (("This", [ v1 ]))*)
    ""
  | ConstructedObject ((v1, v2)) ->
      (*let v1 = vof_fullType v1
      and v2 = vof_paren (vof_comma_list vof_argument) v2
      in Ocaml.VSum (("ConstructedObject", [ v1; v2 ]))*)
    ""
  | TypeId ((v1, v2)) ->
      (*let v1 = vof_tok v1
      and v2 = vof_paren vof_either_ft_or_expr v2
      in Ocaml.VSum (("TypeId", [ v1; v2 ]))*)
    ""
  | CplusplusCast ((v1, v2, v3)) ->
      (*let v1 = vof_wrap2 vof_cast_operator v1
      and v2 = vof_angle vof_fullType v2
      and v3 = vof_paren vof_expression v3
      in Ocaml.VSum (("CplusplusCast", [ v1; v2; v3 ]))*)
    ""
  | New ((v1, v2, v3, v4, v5)) ->
      (*let v1 = Ocaml.vof_option vof_tok v1
      and v2 = vof_tok v2
      and v3 = Ocaml.vof_option (vof_paren (vof_comma_list vof_argument)) v3
      and v4 = vof_fullType v4
      and v5 = Ocaml.vof_option (vof_paren (vof_comma_list vof_argument)) v5
      in Ocaml.VSum (("New", [ v1; v2; v3; v4; v5 ]))*)
    ""
  | Delete ((v1, v2)) ->
      (*let v1 = Ocaml.vof_option vof_tok v1
      and v2 = vof_expression v2
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
  | If ((v1, v2, v3, v4, v5)) ->
      let v1 = process_token v1
      and v2 = process_paren process_expression v2
      and v3 = process_statement v3
      and v4 = process_option process_token v4
      and v5 = process_statement v5
      in v1 ^ v2 ^ v3 ^ v4 ^v5
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
  | Goto goto -> "# XXX goto not supported: " ^ goto
  | Continue -> "continue"
  | Break -> "break"
  | Return -> "return"
  | ReturnExpr ret_expr ->
      process_expression ret_expr
  | GotoComputed goto_comp ->
    "#[ XXX goto not supported: " ^ process_expression goto_comp ^ "]#"

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
    process_option
      (fun (name, init) ->
         let name = process_name name
         and init = process_option process_init init
         in name ^ init)
      v_namei in
  let res = process_onedeclFullType "" name v_storage v_type in
  res

and process_onedeclFullType prefix name storage (qualifier, (typeCbis, tok_list)) =
  match typeCbis with
  | BaseType btype ->
      process_baseType btype
  | Pointer point ->
      process_onedeclFullType "ptr " name storage point
  | Reference ref ->
      process_onedeclFullType "ref " name storage ref
  | Array ((arr, typ)) ->
      let arr = process_bracket (process_option process_constExpression) arr
      and typ = process_fullType typ
      in arr ^ typ
  | FunctionType ftype ->
    let ret = match storage with
    | NoSto -> "proc " ^ name ^ process_functionType ftype
    | StoTypedef st_tdef ->
      "type " ^ name ^ " = " ^ "proc " ^ process_functionType ftype
    | Sto sto -> "proc " ^ name ^ process_functionType ftype in
    ret
  | EnumDef ((name, ident, elements)) ->
      let ident = process_option process_simple_ident ident
      and elements =
        process_brace (process_comma_list process_enum_elem) elements
      in "type " ^ ident ^ " = enum " ^ elements
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
      process_onedeclFullType "" name storage type_inf

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
  | EqInit ((v1, v2)) ->
      let v1 = process_token v1
      and v2 = process_initialiser v2
      in v1 ^ v2
  | ObjInit v1 ->
      process_paren (process_comma_list process_argument) v1

and process_block_declaration =
  function
  | DeclList ((decl, semi_col)) ->
      let v1 = process_comma_list process_onedecl decl
      in "DECLLIST " ^ v1
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
    process_compound comp
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

and process_compound comp = process_brace (process_list process_statement_sequencable) comp

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
    "const " ^ idname ^ " = " ^ process_define_val value ^ "\n"
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
  ""

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
    (*let v1 = vof_func_or_else v1 in Ocaml.VSum (("Func", [ v1 ]))*)
    process_func_or_else func
  | TemplateDecl (v1, v2, v3) ->
    (*let v1 = vof_tok v1
    and v2 = vof_template_parameters v2
    and v3 = vof_declaration v3
    in Ocaml.VSum (("TemplateDecl", [ v1; v2; v3 ]))*)
    ""
  | TemplateSpecialization ((v1, v2, v3)) ->
      (*let v1 = vof_tok v1
      and v2 = vof_angle Ocaml.vof_unit v2
      and v3 = vof_declaration v3
      in Ocaml.VSum (("TemplateSpecialization", [ v1; v2; v3 ]))*)
    ""
  | ExternC ((v1, v2, v3)) ->
      (*let v1 = vof_tok v1
      and v2 = vof_tok v2
      and v3 = vof_declaration v3
      in Ocaml.VSum (("ExternC", [ v1; v2; v3 ]))*)
    ""
  | ExternCList ((v1, v2, v3)) ->
      (*let v1 = vof_tok v1
      and v2 = vof_tok v2
      and v3 = vof_brace (Ocaml.vof_list vof_declaration_sequencable) v3
      in Ocaml.VSum (("ExternCList", [ v1; v2; v3 ]))*)
    ""
  | NameSpace ((v1, v2, v3)) ->
      (*let v1 = vof_tok v1
      and v2 = vof_wrap2 Ocaml.vof_string v2
      and v3 = vof_brace (Ocaml.vof_list vof_declaration_sequencable) v3
      in Ocaml.VSum (("NameSpace", [ v1; v2; v3 ]))*)
    ""
  | NameSpaceExtend ((v1, v2)) ->
      (*let v1 = Ocaml.vof_string v1
      and v2 = Ocaml.vof_list vof_declaration_sequencable v2
      in Ocaml.VSum (("NameSpaceExtend", [ v1; v2 ]))*)
    ""
  | NameSpaceAnon ((v1, v2)) ->
      (*let v1 = vof_tok v1
      and v2 = vof_brace (Ocaml.vof_list vof_declaration_sequencable) v2
      in Ocaml.VSu:m (("NameSpaceAnon", [ v1; v2 ]))*)
    ""
  | EmptyDef def -> process_token def
  | DeclTodo -> "# TODO"

and process_fullType ((qualifier, typeC)) =
  process_typeC typeC

and process_toplevel = function
  | NotParsedCorrectly node ->  ""
  | DeclElem node -> process_declaration node
  | CppDirectiveDecl node -> process_cpp_directive node
  | IfdefDecl node -> ""
  | MacroTop ((v1, v2, v3)) -> ""
  | MacroVarTop ((v1, v2)) -> ""

let iter_ast ast =
  List.map process_toplevel ast

let generate_nim cfile macro_files =
  Parse_cpp.init_defs cfile;
  List.iter Parse_cpp.add_defs macro_files;
  let ast = Parse_cpp.parse_program cfile in
  let res = iter_ast ast in
  String.concat "\n" res

let test_gen_nim file =
  let macro_list = [!Flag.macros_h] in
  let nim_str = generate_nim file macro_list in
  pr nim_str

let actions () = [
    "-generate-nim", "   <file>",
    Common.mk_action_1_arg test_gen_nim;
]
