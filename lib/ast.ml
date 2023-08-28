type ty =
  | BuiltinType of string
  | PointerType of ty
  | RecordType of string
  | ConstantArrayType of {ty: ty; size: int}
[@@deriving show]

type bop = Eq | Assign | Add | Sub | Mul | Div | Mod [@@deriving show]

type decl =
  | TypedefDecl of {name: string; ty: ty}
  | RecordDecl of {name: string; fields: decl list}
  | FieldDecl of {name: string; ty: string}
  | EnumDecl of {name: string; values: decl list}
  | EnumConstantDecl of {name: string; value: stmt option}
  | FunctionDecl of {name: string; ty: string; params: decl list; body: stmt}
  | ParmVarDecl of {name: string; ty: string}
  | VarDecl of {name: string; ty: string; init: stmt option}
[@@deriving show]

and stmt =
  | CompoundStmt of stmt list
  | IfStmt of {cond: stmt; then_: stmt; else_: stmt option}
  | ReturnStmt of stmt option
  | DeclStmt of decl
  | IntegerLiteral of string
  | ConstantExpr of stmt
  | DeclRefExpr of {name: string}
  | BinaryOperator of {lhs: stmt; rhs: stmt; op: bop}
  | CallExpr of {callee: stmt; args: stmt list}
  | ImplicitCastExpr of {expr: stmt; kind: string}
  | MemberExpr of {expr: stmt; arrow: bool; field: string}
[@@deriving show]
