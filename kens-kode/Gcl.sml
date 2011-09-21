
(* Abstract syntax *)
type var = string

datatype boolop = AND | OR | IMP | BIIMP 

datatype bexp = BVar of var
              | BBin of bexp * boolop * bexp
              | NOT  of bexp
              | TRUE | FALSE

datatype command = CMD of bexp * (var * bexp) list
datatype program = PRG of (var * bexp) list * command list


(* Semantics *)

(* Mapping of GCL variables to BDD variables.  Each GCL variable maps
   is mapped to a primed an un-primed variable, primed variables are used
   for the after-state of assignments.
*)
type bdd_vars = { var: int, primed: int}
type var_map = (string, bdd_vars) Polyhash.hash_table 

                               
fun program (vars : var_map) (PRG(init, commands)) =
    let fun var v = bdd.ithvar(#var(Polyhash.find vars v))
        fun primed v = bdd.ithvar(#primed(Polyhash.find vars v))
        fun bexp be =
            case be of
                BVar v => var v
              | BBin(lhs, opr, rhs) =>
                let val trans = case opr of
                                    AND => bdd.AND
                                  | OR  => bdd.OR
                                  | IMP => bdd.IMP
                                  | BIIMP => bdd.BIIMP
                in trans(bexp lhs, bexp rhs) end
              | NOT be => bdd.NOT(bexp be)
              | TRUE => bdd.TRUE | FALSE => bdd.FALSE    
                                            
        fun command (CMD(guard, assignments)) =
            let val changed = Polyhash.mkPolyTable (length assignments, Fail"not found")
                val _ = List.app (fn (v,_)=> Polyhash.insert changed (v,())) assignments
                val unchanged = 
                    List.foldl (fn ((v, {var, primed}), res) =>
                                   case Polyhash.peek changed v of
                                       SOME _ => res
                                     | _ => bdd.AND(bdd.BIIMP(bdd.ithvar primed, 
                                                              bdd.ithvar var),
                                                    res)) 
                               bdd.TRUE (Polyhash.listItems vars)
                val assigns = 
                    List.foldl (fn ((v,be), res) => 
                                   bdd.AND(bdd.BIIMP(primed v, bexp be),
                                           res)) 
                               bdd.TRUE assignments
            in bdd.IMP(bexp guard, assigns) end
        val init = 
            List.foldl (fn ((v,be), res) => 
                           bdd.AND(bdd.BIIMP(var v, bexp be),
                                   res)) 
                       bdd.TRUE init
    in (init, List.map command commands) end
    
    
    
(* Embedded lang *)
fun mkBBin opr (x, y) = BBin(x, opr, y)
infix /\ \/ ==> <==>
val (op /\, op \/, op ==>, op <==>) = (mkBBin AND, mkBBin OR, 
                                       mkBBin IMP, mkBBin BIIMP)
(*fun mkRel opr (x, y) = Rel(x, opr, y)
infix == !=
val (op ==, op !=, op <, op <=, op >, op >= ) =
    (mkRel EQ, mkRel NEQ, mkRel LT, mkRel LTEQ, mkRel GR, mkRel GREQ)


fun mkBin opr (x, y) = Bin(x, opr, y)
val (op +, op -, op *, op div, op mod) =
    (mkBin PLUS, mkBin MINUS, mkBin MULT, mkBin DIV, mkBin MOD)
*)
infix ::=
val op ::= = ListPair.zip 

infix ? 
fun g ? ass = [CMD(g, ass)]

infix ||
val op|| = op@
val $ = BVar


(* Milner's scheduler, just 4 cyclers *)
val (c0, t0, h0, c1, t1, h1, c2, t2, h2, c3, t3, h3) =
    ("c0", "t0", "h0", "c1", "t1", "h1", "c2", "t2", "h2", "c3", "t3", "h3")

fun cycler c t h c' = 
      ($c /\ NOT($t)) ? ([t, c, h]  ::= [TRUE, NOT($c), TRUE])
   || ($h ? ([c', h] ::= [TRUE, FALSE])) 

fun task t = $t ? ([t] ::= [FALSE])

val initial = 
    [(c0, TRUE), (t0, FALSE), (h0, FALSE), 
     (c1, FALSE), (t1, FALSE), (h1, FALSE), 
     (c2, FALSE), (t2, FALSE), (h2, FALSE), 
     (c3, FALSE), (t3, FALSE), (h3, FALSE)]

val milner4 = 
    PRG( initial,
         cycler c0 t0 h0 c1 
      || cycler c1 t1 h1 c2 
      || cycler c2 t2 h2 c3 
      || cycler c3 t3 h3 c0
      || task t0  ||  task t1  
      || task t2  ||  task t3  
       )







