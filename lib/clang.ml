open Core
open Ast
open Yojson.Basic.Util

let rec parse_ty json =
  let kind = json |> member "kind" |> to_string in
  match kind with
  | "BuiltinType" ->
      let ty = json |> member "type" |> member "qualType" |> to_string in
      BuiltinType ty
  | "PointerType" ->
      let ty =
        json |> member "inner" |> to_list |> List.hd_exn |> parse_ty
      in
      PointerType ty
  | "RecordType" ->
      let name = json |> member "decl" |> member "name" |> to_string in
      RecordType name
  | "ConstantArrayType" ->
      let ty =
        json |> member "inner" |> to_list |> List.hd_exn |> parse_ty
      in
      let size = json |> member "size" |> to_int in
      ConstantArrayType {ty; size}
  | "ElaboratedType" ->
      json |> member "inner" |> to_list |> List.hd_exn |> parse_ty
  | _ -> failwith ("unknown type kind: " ^ kind)

let parse_op json =
  let opcode = json |> to_string in
  match opcode with
  | "==" -> Eq
  | "=" -> Assign
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | "%" -> Mod
  | _ -> failwith ("unknown opcode: " ^ opcode)

let rec parse_decl json =
  let kind = json |> member "kind" |> to_string in
  match kind with
  | "TypedefDecl" ->
      let name = json |> member "name" |> to_string in
      let ty =
        json |> member "inner" |> to_list |> List.hd_exn |> parse_ty
      in
      TypedefDecl {name; ty}
  | "RecordDecl" ->
      let name = json |> member "name" |> to_string in
      let fields =
        json |> member "inner" |> to_list |> List.map ~f:parse_decl
      in
      RecordDecl {name; fields}
  | "FieldDecl" ->
      let name = json |> member "name" |> to_string in
      let ty = json |> member "type" |> member "qualType" |> to_string in
      FieldDecl {name; ty}
  | "EnumDecl" ->
      let name = json |> member "name" |> to_string in
      let values =
        json |> member "inner" |> to_list |> List.map ~f:parse_decl
      in
      EnumDecl {name; values}
  | "EnumConstantDecl" ->
      let name = json |> member "name" |> to_string in
      let value =
        json |> member "inner"
        |> to_option (fun json ->
               json |> to_list |> List.hd_exn |> parse_stmt )
      in
      EnumConstantDecl {name; value}
  | "FunctionDecl" ->
      let name = json |> member "name" |> to_string in
      let ty = json |> member "type" |> member "qualType" |> to_string in
      let params =
        json |> member "inner" |> to_list |> List.drop_last_exn
        |> List.map ~f:parse_decl
      in
      let body =
        json |> member "inner" |> to_list |> List.last_exn |> parse_stmt
      in
      FunctionDecl {name; ty; params; body}
  | "ParmVarDecl" ->
      let name = json |> member "name" |> to_string in
      let ty = json |> member "type" |> member "qualType" |> to_string in
      ParmVarDecl {name; ty}
  | "VarDecl" ->
      let name = json |> member "name" |> to_string in
      let ty = json |> member "type" |> member "qualType" |> to_string in
      let init =
        json |> member "inner"
        |> to_option (fun json ->
               json |> to_list |> List.hd_exn |> parse_stmt )
      in
      VarDecl {name; ty; init}
  | _ -> failwith ("unknown decl kind: " ^ kind)

and parse_stmt json =
  let kind = json |> member "kind" |> to_string in
  match kind with
  | "CompoundStmt" ->
      let stmts =
        json |> member "inner" |> to_list |> List.map ~f:parse_stmt
      in
      CompoundStmt stmts
  | "ReturnStmt" ->
      let expr =
        json |> member "inner"
        |> to_option (fun json ->
               json |> to_list |> List.hd_exn |> parse_stmt )
      in
      ReturnStmt expr
  | "IfStmt" ->
      let cond =
        json |> member "inner" |> to_list |> List.hd_exn |> parse_stmt
      in
      let then_ =
        json |> member "inner" |> to_list |> List.tl_exn |> List.hd_exn
        |> parse_stmt
      in
      let else_ =
        json |> member "inner" |> to_list |> List.tl_exn |> List.tl_exn
        |> List.hd
        |> Option.map ~f:(fun json -> json |> parse_stmt)
      in
      IfStmt {cond; then_; else_}
  | "DeclStmt" ->
      let decl =
        json |> member "inner" |> to_list |> List.hd_exn |> parse_decl
      in
      DeclStmt decl
  | "IntegerLiteral" ->
      let value = json |> member "value" |> to_string in
      IntegerLiteral value
  | "ConstantExpr" ->
      let expr =
        json |> member "inner" |> to_list |> List.hd_exn |> parse_stmt
      in
      ConstantExpr expr
  | "DeclRefExpr" ->
      let name =
        json |> member "referencedDecl" |> member "name" |> to_string
      in
      DeclRefExpr {name}
  | "CallExpr" ->
      let callee =
        json |> member "inner" |> to_list |> List.hd_exn |> parse_stmt
      in
      let args =
        json |> member "inner" |> to_list |> List.tl_exn
        |> List.map ~f:parse_stmt
      in
      CallExpr {callee; args}
  | "BinaryOperator" ->
      let op = json |> member "opcode" |> parse_op in
      let lhs =
        json |> member "inner" |> to_list |> List.hd_exn |> parse_stmt
      in
      let rhs =
        json |> member "inner" |> to_list |> List.last_exn |> parse_stmt
      in
      BinaryOperator {op; lhs; rhs}
  | "ImplicitCastExpr" ->
      let kind = json |> member "castKind" |> to_string in
      let expr =
        json |> member "inner" |> to_list |> List.hd_exn |> parse_stmt
      in
      ImplicitCastExpr {kind; expr}
  | "MemberExpr" ->
      let field = json |> member "name" |> to_string in
      let arrow = json |> member "isArrow" |> to_bool in
      let expr =
        json |> member "inner" |> to_list |> List.hd_exn |> parse_stmt
      in
      MemberExpr {expr; arrow; field}
  | _ -> failwith ("unknown stmt kind: " ^ kind)

let from_json json =
  json |> member "inner" |> to_list |> List.map ~f:parse_decl
