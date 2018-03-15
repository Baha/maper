-define(ARG_SEP,    ",").
-define(ARG_START,  "(").
-define(ARG_END,    ")").
-define(LIST_SEP,   ",").
-define(LIST_START, "[").
-define(LIST_END,   "]").
-define(SQUOTE_START, "'").
-define(SQUOTE_END, "'").

-define(INT_ATOM,    "int").
-define(ATOM_ATOM,   "atom").
-define(NIL_ATOM,    "nil").
-define(MODULE_ATOM, "module").
-define(FUN_ATOM,    "fun").
-define(LIT_ATOM,    "lit").
-define(VAR_ATOM,    "var").
-define(CONS_ATOM,   "cons").
-define(TUPLE_ATOM,  "tuple").
-define(VALUES_ATOM, "values").
-define(LET_ATOM,    "let").
-define(CASE_ATOM,   "case").
-define(CLAUSE_ATOM, "clause").
-define(APPLY_ATOM,  "apply").
-define(CALL_ATOM,   "call").
-define(PRIMOP_ATOM, "primop").
-define(FUNDEF_ATOM, "fundef").

-define(INT_PRED(Int),
  ?INT_ATOM ++ ?ARG_SEP ++ Int).

-define(ATOM_PRED(Atom),
  ?ATOM_ATOM ++ ?ARG_SEP ++
  ?SQUOTE_START ++ Atom ++ ?SQUOTE_END).

-define(NIL_PRED,
  ?NIL_ATOM ++ ?ARG_SEP ++
  ?LIST_START ++ ?LIST_END).

-define(MODULE_PRED(Name, Defs),
  ?MODULE_ATOM ++ ?ARG_START ++
  Name ++ ?ARG_SEP ++
  Defs ++ ?ARG_END).

-define(FUN_PRED(Vars, Body),
  ?FUN_ATOM ++ ?ARG_START ++
  Vars ++ ?ARG_SEP ++
  Body ++ ?ARG_END).

-define(LIT_PRED(Lit),
  ?LIT_ATOM ++ ?ARG_START ++
  Lit ++ ?ARG_END).

-define(VAR_PRED(Var),
  ?VAR_ATOM ++ ?ARG_START ++
  ?SQUOTE_START ++ Var ++ ?SQUOTE_END ++ ?ARG_END).

-define(VAR_PRED_PAIR(Atom, Int),
  ?VAR_ATOM ++ ?ARG_START ++
  ?SQUOTE_START ++ Atom ++ ?SQUOTE_END ++ ?ARG_SEP ++
  Int ++ ?ARG_END).

-define(CONS_PRED(Hd, Tl),
  ?CONS_ATOM ++ ?ARG_START ++
  Hd ++ ?ARG_SEP ++
  Tl ++ ?ARG_END).

-define(TUPLE_PRED(Es),
  ?TUPLE_ATOM ++ ?ARG_START ++
  Es ++ ?ARG_END).

-define(VALUES_PRED(Es),
  ?VALUES_ATOM ++ ?ARG_START ++
  Es ++ ?ARG_END).

-define(LET_PRED(Vars, Arg, Body),
  ?LET_ATOM ++ ?ARG_START ++
  Vars ++ ?ARG_SEP ++
  Arg  ++ ?ARG_SEP ++
  Body ++ ?ARG_END).

-define(CASE_PRED(Arg, Clauses),
  ?CASE_ATOM ++ ?ARG_START ++
  Arg     ++ ?ARG_SEP ++
  Clauses ++ ?ARG_END).

-define(CLAUSE_PRED(Pats, Guard, Body),
  ?CLAUSE_ATOM ++ ?ARG_START ++
  Pats  ++ ?ARG_SEP ++
  Guard ++ ?ARG_SEP ++
  Body  ++ ?ARG_END).

-define(APPLY_PRED(Op, Args),
  ?APPLY_ATOM ++ ?ARG_START ++
  Op   ++ ?ARG_SEP ++
  Args ++ ?ARG_END).

-define(CALL_PRED(Module, Name, Args),
  ?CALL_ATOM ++ ?ARG_START ++
  Module ++ ?ARG_SEP ++
  Name   ++ ?ARG_SEP ++
  Args   ++ ?ARG_END).

-define(PRIMOP_PRED(Op, Args),
  ?PRIMOP_ATOM ++ ?ARG_START ++
  Op   ++ ?ARG_SEP ++
  Args ++ ?ARG_END).

-define(FUNDEF_PRED(Module, Name, Body),
  ?FUNDEF_ATOM ++ ?ARG_START ++
  Module   ++ ?ARG_SEP ++
  Name     ++ ?ARG_SEP ++
  Body     ++ ?ARG_END).
