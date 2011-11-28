local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = (int*int)
type t__4__ = (int*int)
type t__5__ = string*(int*int)
type t__6__ = (int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = (int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = (int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = string*(int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = int*(int*int)
type t__24__ = (int*int)
type t__25__ = (int*int)
type t__26__ = (int*int)
type t__27__ = (int*int)
type t__28__ = (int*int)
type t__29__ = (int*int)
type t__30__ = (int*int)
type t__31__ = (int*int)
type t__32__ = string*(int*int)
type t__33__ = (int*int)
type t__34__ = (int*int)
in
datatype token =
    ADD of t__1__
  | AMP of t__2__
  | ASSIGN of t__3__
  | COMMA of t__4__
  | CONST of t__5__
  | CONTAINS of t__6__
  | DOT of t__7__
  | END of t__8__
  | EOF of t__9__
  | EQ of t__10__
  | GEQ of t__11__
  | GT of t__12__
  | INIT of t__13__
  | LBRA of t__14__
  | LCUR of t__15__
  | LEQ of t__16__
  | LOC of t__17__
  | LPAR of t__18__
  | LT of t__19__
  | NAME of t__20__
  | NEQ of t__21__
  | NOT of t__22__
  | NUM of t__23__
  | QUE of t__24__
  | RBRA of t__25__
  | RCUR of t__26__
  | RPAR of t__27__
  | RR of t__28__
  | RULE of t__29__
  | SEMI of t__30__
  | STORY of t__31__
  | STRING of t__32__
  | SUB of t__33__
  | UNDEFINED of t__34__
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

val yytransl = #[
  257 (* ADD *),
  258 (* AMP *),
  259 (* ASSIGN *),
  260 (* COMMA *),
  261 (* CONST *),
  262 (* CONTAINS *),
  263 (* DOT *),
  264 (* END *),
  265 (* EOF *),
  266 (* EQ *),
  267 (* GEQ *),
  268 (* GT *),
  269 (* INIT *),
  270 (* LBRA *),
  271 (* LCUR *),
  272 (* LEQ *),
  273 (* LOC *),
  274 (* LPAR *),
  275 (* LT *),
  276 (* NAME *),
  277 (* NEQ *),
  278 (* NOT *),
  279 (* NUM *),
  280 (* QUE *),
  281 (* RBRA *),
  282 (* RCUR *),
  283 (* RPAR *),
  284 (* RR *),
  285 (* RULE *),
  286 (* SEMI *),
  287 (* STORY *),
  288 (* STRING *),
  289 (* SUB *),
  290 (* UNDEFINED *),
    0];

val yylhs = "\255\255\
\\001\000\001\000\001\000\009\000\010\000\010\000\002\000\003\000\
\\003\000\004\000\004\000\025\000\025\000\016\000\016\000\005\000\
\\005\000\022\000\022\000\022\000\023\000\023\000\021\000\021\000\
\\008\000\008\000\007\000\007\000\006\000\006\000\011\000\011\000\
\\011\000\012\000\012\000\012\000\017\000\019\000\019\000\019\000\
\\018\000\018\000\018\000\018\000\024\000\024\000\024\000\020\000\
\\020\000\020\000\020\000\014\000\014\000\013\000\013\000\013\000\
\\015\000\015\000\015\000\015\000\015\000\015\000\000\000";

val yylen = "\002\000\
\\001\000\005\000\006\000\006\000\001\000\002\000\004\000\001\000\
\\002\000\007\000\003\000\001\000\003\000\001\000\003\000\001\000\
\\002\000\001\000\003\000\001\000\001\000\003\000\001\000\004\000\
\\001\000\002\000\001\000\002\000\001\000\001\000\005\000\005\000\
\\007\000\005\000\009\000\003\000\004\000\001\000\003\000\003\000\
\\003\000\001\000\004\000\002\000\001\000\001\000\001\000\001\000\
\\001\000\003\000\003\000\001\000\003\000\003\000\001\000\004\000\
\\001\000\001\000\001\000\001\000\001\000\001\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\001\000\000\000\000\000\063\000\000\000\000\000\
\\000\000\000\000\006\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\002\000\009\000\000\000\015\000\000\000\000\000\000\000\004\000\
\\023\000\029\000\030\000\000\000\000\000\044\000\000\000\000\000\
\\000\000\000\000\007\000\000\000\000\000\011\000\003\000\000\000\
\\046\000\000\000\045\000\047\000\000\000\000\000\000\000\048\000\
\\000\000\000\000\000\000\017\000\013\000\000\000\039\000\040\000\
\\000\000\049\000\000\000\000\000\018\000\000\000\000\000\020\000\
\\000\000\000\000\000\000\000\000\057\000\062\000\060\000\061\000\
\\059\000\058\000\000\000\000\000\000\000\000\000\036\000\043\000\
\\000\000\000\000\000\000\024\000\000\000\053\000\000\000\000\000\
\\000\000\000\000\037\000\000\000\000\000\019\000\022\000\056\000\
\\000\000\032\000\031\000\000\000\010\000\000\000\000\000\000\000\
\\000\000\028\000\033\000\000\000\000\000\035\000";

val yydgoto = "\002\000\
\\006\000\015\000\021\000\022\000\040\000\041\000\120\000\000\000\
\\007\000\008\000\042\000\043\000\061\000\062\000\092\000\018\000\
\\044\000\029\000\030\000\063\000\045\000\078\000\079\000\064\000\
\\031\000";

val yysindex = "\018\000\
\\060\255\000\000\000\000\023\255\059\255\000\000\038\255\040\255\
\\068\255\080\255\000\000\096\255\094\255\102\255\110\255\080\255\
\\125\255\113\255\015\255\132\255\142\255\110\255\110\255\094\255\
\\009\255\137\255\151\255\148\255\008\255\146\255\170\255\155\255\
\\000\000\000\000\168\255\000\000\159\255\129\255\160\255\000\000\
\\000\000\000\000\000\000\153\255\009\255\000\000\162\255\164\255\
\\015\255\015\255\000\000\134\255\171\255\000\000\000\000\133\255\
\\000\000\161\255\000\000\000\000\183\255\165\255\112\255\000\000\
\\000\000\094\255\084\255\000\000\000\000\166\255\000\000\000\000\
\\007\255\000\000\094\255\015\255\000\000\184\255\167\255\000\000\
\\164\255\129\255\172\255\134\255\000\000\000\000\000\000\000\000\
\\000\000\000\000\134\255\134\255\173\255\015\255\000\000\000\000\
\\169\255\175\255\133\255\000\000\176\255\000\000\101\255\000\000\
\\000\000\007\255\000\000\178\255\009\255\000\000\000\000\000\000\
\\121\255\000\000\000\000\187\255\000\000\044\255\115\255\179\255\
\\177\255\000\000\000\000\158\255\180\255\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\181\255\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\120\255\000\000\000\000\000\000\000\000\189\255\000\000\000\000\
\\000\000\000\000\006\255\000\000\182\255\000\000\026\255\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\077\255\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\185\255\000\000\000\000\000\000\
\\086\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\054\255\000\000\000\000\000\000\000\000\186\255\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\049\255\
\\062\255\017\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\154\255\000\000\099\255\188\255\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\182\000\142\000\000\000\224\255\163\255\080\000\000\000\
\\000\000\195\000\104\000\106\000\000\000\128\000\000\000\238\255\
\\000\000\000\000\211\255\206\255\000\000\000\000\116\000\203\255\
\\219\255";

val YYTABLESIZE = 215;
val yytable = "\054\000\
\\065\000\073\000\080\000\071\000\072\000\036\000\012\000\084\000\
\\012\000\012\000\070\000\049\000\068\000\037\000\074\000\012\000\
\\012\000\012\000\001\000\119\000\054\000\012\000\026\000\038\000\
\\012\000\119\000\012\000\009\000\039\000\042\000\098\000\012\000\
\\012\000\104\000\027\000\012\000\028\000\050\000\012\000\091\000\
\\105\000\106\000\054\000\101\000\065\000\080\000\074\000\093\000\
\\108\000\050\000\047\000\042\000\050\000\074\000\074\000\042\000\
\\097\000\041\000\050\000\050\000\050\000\066\000\051\000\010\000\
\\050\000\051\000\004\000\050\000\003\000\050\000\012\000\051\000\
\\051\000\051\000\050\000\065\000\117\000\051\000\050\000\041\000\
\\051\000\050\000\051\000\041\000\016\000\013\000\049\000\051\000\
\\004\000\055\000\005\000\051\000\014\000\016\000\051\000\049\000\
\\049\000\049\000\094\000\012\000\016\000\049\000\012\000\095\000\
\\049\000\016\000\049\000\016\000\012\000\012\000\012\000\055\000\
\\084\000\017\000\012\000\113\000\019\000\012\000\049\000\012\000\
\\039\000\085\000\086\000\087\000\012\000\057\000\020\000\088\000\
\\024\000\038\000\089\000\012\000\090\000\057\000\039\000\038\000\
\\032\000\057\000\057\000\025\000\118\000\046\000\058\000\059\000\
\\091\000\014\000\014\000\076\000\027\000\033\000\058\000\059\000\
\\077\000\027\000\060\000\059\000\059\000\047\000\034\000\037\000\
\\053\000\034\000\060\000\034\000\035\000\048\000\060\000\060\000\
\\034\000\038\000\034\000\051\000\052\000\034\000\039\000\055\000\
\\056\000\066\000\081\000\034\000\067\000\069\000\034\000\027\000\
\\034\000\075\000\082\000\099\000\121\000\125\000\083\000\124\000\
\\096\000\100\000\109\000\103\000\008\000\023\000\122\000\107\000\
\\110\000\011\000\112\000\116\000\123\000\126\000\114\000\038\000\
\\115\000\102\000\052\000\005\000\021\000\027\000\111\000";

val yycheck = "\032\000\
\\038\000\052\000\056\000\049\000\050\000\024\000\001\001\001\001\
\\003\001\004\001\048\000\004\001\045\000\005\001\052\000\010\001\
\\011\001\012\001\001\000\113\000\004\001\016\001\008\001\015\001\
\\019\001\119\000\021\001\005\001\020\001\004\001\076\000\026\001\
\\027\001\084\000\020\001\030\001\022\001\030\001\033\001\033\001\
\\091\000\092\000\026\001\081\000\082\000\099\000\084\000\066\000\
\\094\000\001\001\007\001\026\001\004\001\091\000\092\000\030\001\
\\075\000\004\001\010\001\011\001\012\001\018\001\001\001\005\001\
\\016\001\004\001\029\001\019\001\009\001\021\001\031\001\010\001\
\\011\001\012\001\026\001\113\000\109\000\016\001\030\001\026\001\
\\019\001\033\001\021\001\030\001\008\001\018\001\001\001\026\001\
\\029\001\004\001\031\001\030\001\013\001\017\001\033\001\010\001\
\\011\001\012\001\015\001\001\001\005\001\016\001\004\001\020\001\
\\019\001\029\001\021\001\031\001\010\001\011\001\012\001\026\001\
\\001\001\020\001\016\001\015\001\015\001\019\001\033\001\021\001\
\\020\001\010\001\011\001\012\001\026\001\005\001\017\001\016\001\
\\004\001\015\001\019\001\033\001\021\001\005\001\020\001\015\001\
\\005\001\005\001\005\001\027\001\020\001\005\001\022\001\023\001\
\\033\001\026\001\027\001\015\001\020\001\008\001\022\001\023\001\
\\020\001\020\001\034\001\023\001\023\001\007\001\005\001\005\001\
\\006\001\008\001\034\001\022\000\023\000\018\001\034\001\034\001\
\\015\001\015\001\017\001\026\001\003\001\020\001\020\001\008\001\
\\018\001\018\001\018\001\026\001\028\001\020\001\029\001\020\001\
\\031\001\015\001\004\001\004\001\002\001\032\001\026\001\015\001\
\\027\001\027\001\026\001\024\001\008\001\016\000\119\000\027\001\
\\026\001\007\000\027\001\026\001\026\001\026\001\103\000\026\001\
\\103\000\082\000\026\001\031\001\027\001\026\001\099\000";

val yyact = vector_ 64 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file EParser.grm, line 39 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( (NONE,"NAS", [], []) ) end : ESyntax.GScript))
;
(* Rule 2, file EParser.grm, line 40 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : string*(int*int)
val d__3__ = peekVal 2 : ESyntax.Initial
val d__4__ = peekVal 1 : ESyntax.LocDef list
val d__5__ = peekVal 0 : (int*int)
in
( (NONE,#1 (d__2__), (d__3__), (d__4__)) ) end : ESyntax.GScript))
;
(* Rule 3, file EParser.grm, line 41 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 5 : ESyntax.metarule list
val d__2__ = peekVal 4 : (int*int)
val d__3__ = peekVal 3 : string*(int*int)
val d__4__ = peekVal 2 : ESyntax.Initial
val d__5__ = peekVal 1 : ESyntax.LocDef list
val d__6__ = peekVal 0 : (int*int)
in
( (SOME (d__1__),#1 (d__3__), (d__4__), (d__5__)) ) end : ESyntax.GScript))
;
(* Rule 4, file EParser.grm, line 45 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 5 : (int*int)
val d__2__ = peekVal 4 : string*(int*int)
val d__3__ = peekVal 3 : (int*int)
val d__4__ = peekVal 2 : ESyntax.name list
val d__5__ = peekVal 1 : (int*int)
val d__6__ = peekVal 0 : ESyntax.mrule list
in
(ESyntax.MetaRule(#1 (d__2__),(d__4__),(d__6__))) end : ESyntax.metarule))
;
(* Rule 5, file EParser.grm, line 49 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.metarule
in
([(d__1__)]) end : ESyntax.metarule list))
;
(* Rule 6, file EParser.grm, line 50 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 1 : ESyntax.metarule
val d__2__ = peekVal 0 : ESyntax.metarule list
in
((d__1__) :: (d__2__)) end : ESyntax.metarule list))
;
(* Rule 7, file EParser.grm, line 54 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : ESyntax.update list
val d__4__ = peekVal 0 : (int*int)
in
( (d__3__) ) end : ESyntax.Initial))
;
(* Rule 8, file EParser.grm, line 58 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.LocDef
in
( [(d__1__)] ) end : ESyntax.LocDef list))
;
(* Rule 9, file EParser.grm, line 59 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 1 : ESyntax.LocDef
val d__2__ = peekVal 0 : ESyntax.LocDef list
in
( (d__1__) :: (d__2__) ) end : ESyntax.LocDef list))
;
(* Rule 10, file EParser.grm, line 63 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 6 : (int*int)
val d__2__ = peekVal 5 : string*(int*int)
val d__3__ = peekVal 4 : (int*int)
val d__4__ = peekVal 3 : (int*int)
val d__5__ = peekVal 2 : ESyntax.name list
val d__6__ = peekVal 1 : (int*int)
val d__7__ = peekVal 0 : ESyntax.mrule list
in
( (#1 (d__2__), (d__5__), (d__7__)) ) end : ESyntax.LocDef))
;
(* Rule 11, file EParser.grm, line 64 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : string*(int*int)
val d__3__ = peekVal 0 : ESyntax.mrule list
in
( (#1 (d__2__), [], (d__3__)) ) end : ESyntax.LocDef))
;
(* Rule 12, file EParser.grm, line 68 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( (#1 (d__1__), NONE) ) end : string*string option))
;
(* Rule 13, file EParser.grm, line 69 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 2 : string*(int*int)
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : string*(int*int)
in
( (#1 (d__1__), SOME (#1 (d__3__))) ) end : string*string option))
;
(* Rule 14, file EParser.grm, line 73 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( [#1 (d__1__)] ) end : ESyntax.name list))
;
(* Rule 15, file EParser.grm, line 74 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 2 : string*(int*int)
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : ESyntax.name list
in
((#1 (d__1__)) :: (d__3__) ) end : ESyntax.name list))
;
(* Rule 16, file EParser.grm, line 78 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.mrule
in
( [(d__1__)] ) end : ESyntax.mrule list))
;
(* Rule 17, file EParser.grm, line 79 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 1 : ESyntax.mrule
val d__2__ = peekVal 0 : ESyntax.mrule list
in
( (d__1__) :: (d__2__) ) end : ESyntax.mrule list))
;
(* Rule 18, file EParser.grm, line 83 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
(ESyntax.AID (#1 (d__1__))) end : ESyntax.arg))
;
(* Rule 19, file EParser.grm, line 84 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : ESyntax.update list
val d__3__ = peekVal 0 : (int*int)
in
(ESyntax.AUpdates (d__2__)) end : ESyntax.arg))
;
(* Rule 20, file EParser.grm, line 85 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.value
in
(ESyntax.AValue (d__1__)) end : ESyntax.arg))
;
(* Rule 21, file EParser.grm, line 89 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.arg
in
([(d__1__)]) end : ESyntax.arg list))
;
(* Rule 22, file EParser.grm, line 90 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 2 : ESyntax.arg
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : ESyntax.arg list
in
((d__1__)::(d__3__)) end : ESyntax.arg list))
;
(* Rule 23, file EParser.grm, line 94 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.erule
in
( ESyntax.ERule (d__1__)) end : ESyntax.mrule))
;
(* Rule 24, file EParser.grm, line 95 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : ESyntax.arg list
val d__4__ = peekVal 0 : (int*int)
in
( ESyntax.MetaRuleInst(#1 (d__1__),(d__3__))) end : ESyntax.mrule))
;
(* Rule 25, file EParser.grm, line 99 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.mrule
in
( [ (d__1__) ] ) end : ESyntax.mrule list))
;
(* Rule 26, file EParser.grm, line 100 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 1 : ESyntax.mrule
val d__2__ = peekVal 0 : ESyntax.mrule list
in
( (d__1__) :: (d__2__) ) end : ESyntax.mrule list))
;
(* Rule 27, file EParser.grm, line 105 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.erule
in
( [ (d__1__) ] ) end : ESyntax.erule list))
;
(* Rule 28, file EParser.grm, line 106 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 1 : ESyntax.erule
val d__2__ = peekVal 0 : ESyntax.erule list
in
( (d__1__) :: (d__2__) ) end : ESyntax.erule list))
;
(* Rule 29, file EParser.grm, line 111 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.erule
in
( (d__1__) ) end : ESyntax.erule))
;
(* Rule 30, file EParser.grm, line 112 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.erule
in
( (d__1__) ) end : ESyntax.erule))
;
(* Rule 31, file EParser.grm, line 116 *)
val _ = update_ yyact 31
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : ESyntax.precondition list
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : (int*int)
val d__5__ = peekVal 0 : ESyntax.erule
in
( ESyntax.PRule ((d__2__), [(d__5__)]) ) end : ESyntax.erule))
;
(* Rule 32, file EParser.grm, line 117 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 4 : (int*int)
val d__2__ = peekVal 3 : ESyntax.precondition list
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : (int*int)
val d__5__ = peekVal 0 : ESyntax.erule
in
( ESyntax.PRule ((d__2__), [(d__5__)]) ) end : ESyntax.erule))
;
(* Rule 33, file EParser.grm, line 118 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 6 : (int*int)
val d__2__ = peekVal 5 : ESyntax.precondition list
val d__3__ = peekVal 4 : (int*int)
val d__4__ = peekVal 3 : (int*int)
val d__5__ = peekVal 2 : (int*int)
val d__6__ = peekVal 1 : ESyntax.erule list
val d__7__ = peekVal 0 : (int*int)
in
( ESyntax.PRule ((d__2__), (d__6__)) ) end : ESyntax.erule))
;
(* Rule 34, file EParser.grm, line 122 *)
val _ = update_ yyact 34
(fn () => repr(let
val d__1__ = peekVal 4 : ESyntax.action
val d__2__ = peekVal 3 : (int*int)
val d__3__ = peekVal 2 : (int*int)
val d__4__ = peekVal 1 : ESyntax.update list
val d__5__ = peekVal 0 : (int*int)
in
( ESyntax.ARule ((d__1__), (d__4__), ESyntax.NOP)) end : ESyntax.erule))
;
(* Rule 35, file EParser.grm, line 124 *)
val _ = update_ yyact 35
(fn () => repr(let
val d__1__ = peekVal 8 : ESyntax.action
val d__2__ = peekVal 7 : (int*int)
val d__3__ = peekVal 6 : (int*int)
val d__4__ = peekVal 5 : ESyntax.update list
val d__5__ = peekVal 4 : (int*int)
val d__6__ = peekVal 3 : (int*int)
val d__7__ = peekVal 2 : (int*int)
val d__8__ = peekVal 1 : string*(int*int)
val d__9__ = peekVal 0 : (int*int)
in
( ESyntax.ARule ((d__1__), (d__4__), ESyntax.PRINT (#1 (d__8__)))) end : ESyntax.erule))
;
(* Rule 36, file EParser.grm, line 125 *)
val _ = update_ yyact 36
(fn () => repr(let
val d__1__ = peekVal 2 : ESyntax.action
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : string*(int*int)
in
( ESyntax.SRule ((d__1__), #1 (d__3__))) end : ESyntax.erule))
;
(* Rule 37, file EParser.grm, line 129 *)
val _ = update_ yyact 37
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : ESyntax.name list
val d__4__ = peekVal 0 : (int*int)
in
( ESyntax.PA (#1 (d__1__), (d__3__)) ) end : ESyntax.action))
;
(* Rule 38, file EParser.grm, line 133 *)
val _ = update_ yyact 38
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.update
in
( [ (d__1__) ] ) end : ESyntax.update list))
;
(* Rule 39, file EParser.grm, line 134 *)
val _ = update_ yyact 39
(fn () => repr(let
val d__1__ = peekVal 2 : ESyntax.update
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : ESyntax.update list
in
( (d__1__) :: (d__3__) ) end : ESyntax.update list))
;
(* Rule 40, file EParser.grm, line 135 *)
val _ = update_ yyact 40
(fn () => repr(let
val d__1__ = peekVal 2 : ESyntax.update
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : ESyntax.update list
in
( (d__1__) :: (d__3__) ) end : ESyntax.update list))
;
(* Rule 41, file EParser.grm, line 139 *)
val _ = update_ yyact 41
(fn () => repr(let
val d__1__ = peekVal 2 : string*string option
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : ESyntax.exp
in
( ((d__1__), (d__3__)) ) end : ESyntax.update))
;
(* Rule 42, file EParser.grm, line 140 *)
val _ = update_ yyact 42
(fn () => repr(let
val d__1__ = peekVal 0 : string*string option
in
( ((d__1__), ESyntax.EV (ESyntax.C "True")) ) end : ESyntax.update))
;
(* Rule 43, file EParser.grm, line 141 *)
val _ = update_ yyact 43
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : string*string option
val d__4__ = peekVal 0 : (int*int)
in
( ((d__3__), ESyntax.EV (ESyntax.C "False")) ) end : ESyntax.update))
;
(* Rule 44, file EParser.grm, line 142 *)
val _ = update_ yyact 44
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : string*(int*int)
in
( ((#1 (d__2__),NONE), ESyntax.EV (ESyntax.C "True")) ) end : ESyntax.update))
;
(* Rule 45, file EParser.grm, line 145 *)
val _ = update_ yyact 45
(fn () => repr(let
val d__1__ = peekVal 0 : int*(int*int)
in
( ESyntax.I (#1 (d__1__))) end : ESyntax.value))
;
(* Rule 46, file EParser.grm, line 146 *)
val _ = update_ yyact 46
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( ESyntax.C (#1 (d__1__))) end : ESyntax.value))
;
(* Rule 47, file EParser.grm, line 147 *)
val _ = update_ yyact 47
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( ESyntax.U ) end : ESyntax.value))
;
(* Rule 48, file EParser.grm, line 151 *)
val _ = update_ yyact 48
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.value
in
( ESyntax.EV  (d__1__)) end : ESyntax.exp))
;
(* Rule 49, file EParser.grm, line 152 *)
val _ = update_ yyact 49
(fn () => repr(let
val d__1__ = peekVal 0 : string*string option
in
( ESyntax.EID ((d__1__))) end : ESyntax.exp))
;
(* Rule 50, file EParser.grm, line 153 *)
val _ = update_ yyact 50
(fn () => repr(let
val d__1__ = peekVal 2 : ESyntax.exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : ESyntax.exp
in
( ESyntax.EOP (ESyntax.EADD, [(d__1__), (d__3__)])) end : ESyntax.exp))
;
(* Rule 51, file EParser.grm, line 154 *)
val _ = update_ yyact 51
(fn () => repr(let
val d__1__ = peekVal 2 : ESyntax.exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : ESyntax.exp
in
( ESyntax.EOP (ESyntax.ESUB, [(d__1__), (d__3__)])) end : ESyntax.exp))
;
(* Rule 52, file EParser.grm, line 158 *)
val _ = update_ yyact 52
(fn () => repr(let
val d__1__ = peekVal 0 : ESyntax.precondition
in
( [(d__1__)] ) end : ESyntax.precondition list))
;
(* Rule 53, file EParser.grm, line 159 *)
val _ = update_ yyact 53
(fn () => repr(let
val d__1__ = peekVal 2 : ESyntax.precondition
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : ESyntax.precondition list
in
( (d__1__) :: (d__3__) ) end : ESyntax.precondition list))
;
(* Rule 54, file EParser.grm, line 163 *)
val _ = update_ yyact 54
(fn () => repr(let
val d__1__ = peekVal 2 : ESyntax.exp
val d__2__ = peekVal 1 : ESyntax.vop
val d__3__ = peekVal 0 : ESyntax.exp
in
( ESyntax.Pre ((d__1__), (d__2__), (d__3__)) ) end : ESyntax.precondition))
;
(* Rule 55, file EParser.grm, line 164 *)
val _ = update_ yyact 55
(fn () => repr(let
val d__1__ = peekVal 0 : string*string option
in
( ESyntax.Pre (ESyntax.EID((d__1__)), ESyntax.EQ, ESyntax.EV(ESyntax.C "True")) ) end : ESyntax.precondition))
;
(* Rule 56, file EParser.grm, line 165 *)
val _ = update_ yyact 56
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : string*string option
val d__4__ = peekVal 0 : (int*int)
in
( ESyntax.Pre (ESyntax.EID((d__3__)), ESyntax.NEQ, ESyntax.EV(ESyntax.C "True")) ) end : ESyntax.precondition))
;
(* Rule 57, file EParser.grm, line 169 *)
val _ = update_ yyact 57
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( ESyntax.EQ  ) end : ESyntax.vop))
;
(* Rule 58, file EParser.grm, line 170 *)
val _ = update_ yyact 58
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( ESyntax.NEQ ) end : ESyntax.vop))
;
(* Rule 59, file EParser.grm, line 171 *)
val _ = update_ yyact 59
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( ESyntax.LT  ) end : ESyntax.vop))
;
(* Rule 60, file EParser.grm, line 172 *)
val _ = update_ yyact 60
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( ESyntax.GT  ) end : ESyntax.vop))
;
(* Rule 61, file EParser.grm, line 173 *)
val _ = update_ yyact 61
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( ESyntax.LEQ ) end : ESyntax.vop))
;
(* Rule 62, file EParser.grm, line 174 *)
val _ = update_ yyact 62
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( ESyntax.GEQ ) end : ESyntax.vop))
;
(* Entry GScript *)
val _ = update_ yyact 63 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun GScript lexer lexbuf = yyparse yytables 1 lexer lexbuf;
