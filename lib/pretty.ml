open Core
open Ast
open PPrint
open Palette

module Make (R : RENDERER with type document = document) = struct
  type pretty_config_ty =
    { mutable line_length: int
    ; mutable indent: int
    ; mutable colors: bool
    ; mutable show_implicit_casts: bool }

  let pretty_config =
    {line_length= 80; indent= 2; colors= true; show_implicit_casts= false}

  let rec ty_to_doc = function
    | BuiltinType t -> typ t
    | PointerType t -> ty_to_doc t ^^ op "*"
    | RecordType name -> kwd "struct" ^^ space ^^ typ name
    | ConstantArrayType {ty; size} ->
        ty_to_doc ty ^^ op "[" ^^ lit (Int.to_string size) ^^ op "]"

  let surround l m r = PPrint.surround pretty_config.indent 1 l m r

  let ( ^//^ ) l r = PPrint.prefix pretty_config.indent 1 l r

  let bop_to_doc = function
    | Eq -> op "=="
    | Assign -> op "="
    | Add -> op "+"
    | Sub -> op "-"
    | Mul -> op "*"
    | Div -> op "/"
    | Mod -> op "%"

  let rec decl_to_doc = function
    | TypedefDecl {name; ty} ->
        flow (break 1) [kwd "typedef"; ty_to_doc ty] ^//^ id name ^^ semi
    | RecordDecl {name; fields} ->
        surround
          (kwd "struct" ^^ space ^^ typ name ^^ space ^^ lbrace)
          (separate (break 1) (List.map fields ~f:decl_to_doc))
          rbrace
    | FieldDecl {name; ty} -> typ ty ^/^ id name ^^ semi
    | EnumDecl {name; values} ->
        surround
          (kwd "enum" ^^ space ^^ typ name ^^ space ^^ lbrace)
          (separate (comma ^^ break 1) (List.map values ~f:decl_to_doc))
          rbrace
    | EnumConstantDecl {name; value} ->
        sp name
        ^^ optional (fun v -> space ^^ op "=" ^/^ stmt_to_doc v) value
    | FunctionDecl {name; ty; params; body} ->
        flow (break 1)
          [ kwd "function"
          ; id name
          ; separate (comma ^^ break 1) (List.map params ~f:decl_to_doc)
            |> parens
          ; typ ty |> brackets ]
        ^^ space ^^ stmt_to_doc body
    | ParmVarDecl {name; ty} -> typ ty ^/^ typ name
    | VarDecl {name; ty; init} ->
        flow (break 1)
          [ typ ty
          ; id name
            ^^ optional (fun v -> space ^^ op "=" ^/^ stmt_to_doc v) init ]

  and stmt_to_doc = function
    | CompoundStmt stmts ->
        surround lbrace
          (separate (break 1)
             (List.map stmts ~f:(fun s -> stmt_to_doc s ^^ semi)) )
          rbrace
    | IfStmt {cond; then_; else_} ->
        flow (break 1)
          [ kwd "if"
          ; parens (stmt_to_doc cond) ^^ space ^^ stmt_to_doc then_
          ; optional
              (fun s -> space ^^ kwd "else" ^^ space ^^ stmt_to_doc s)
              else_ ]
    | ReturnStmt e ->
        kwd "return" ^^ optional (fun e -> space ^^ stmt_to_doc e) e
    | DeclStmt decl -> decl_to_doc decl
    | IntegerLiteral i -> lit i
    | ConstantExpr e -> stmt_to_doc e
    | DeclRefExpr {name} -> id name
    | BinaryOperator {lhs; rhs; op} ->
        infix 2 1 (bop_to_doc op) (stmt_to_doc lhs) (stmt_to_doc rhs)
        |> parens |> align
    | CallExpr {callee; args} ->
        stmt_to_doc callee
        ^^ ( separate (comma ^^ break 1) (List.map args ~f:stmt_to_doc)
           |> parens |> align )
    | ImplicitCastExpr {expr; kind} ->
        if pretty_config.show_implicit_casts then
          stmt_to_doc expr ^^ comment (Printf.sprintf " /* %s */" kind)
        else stmt_to_doc expr
    | MemberExpr {expr; arrow; field} ->
        stmt_to_doc expr ^^ (if arrow then op "->" else op ".") ^^ id field

  let render decls output =
    if pretty_config.colors then Ansi.enable_ansi := true
    else Ansi.enable_ansi := false ;
    let doc =
      separate (hardline ^^ hardline) (List.map decls ~f:decl_to_doc)
    in
    R.pretty 1. pretty_config.line_length output doc
end
