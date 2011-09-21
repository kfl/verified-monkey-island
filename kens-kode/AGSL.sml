

datatype condition = 
         HasValue of string * string
       | Not of condition
       | And of condition * condition
       | True | False

datatype update =
         SetToValue of string * string
       | EndGame

type effect = string

type action = string * string list

datatype rule =
         PRule  of condition * rule
       | ARule  of action * update list * effect
       | Choice of rule list

datatype location =
         Location of { name    : string
                     , locals  : string list
                     , rule    : rule
                     }


(* Syntactic sugar *)
infix ::=
val op ::= = SetToValue 

infix ==
val op == = HasValue

infix && 
val op && = And

infix ?
fun c ? rule = PRule(c, rule)

infix ||
val op|| = op@

infix >>
fun a >> (upds, eff) = ARule(a, Choice upds, eff)

infix &
fun x & y = (x,y)


(* actions *)
fun talkto x = ("talkto", [x])
fun walkto x = ("walkto", [x])
fun pickUp x = ("pickUp", [x])
fun close x = ("close",[x])
fun give(x,y) = ("give", [x,y])

fun useon(x,y) = ("use", [x,y])
fun oopen x = ("open",[x])

