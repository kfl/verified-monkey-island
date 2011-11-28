structure ESyntax  =
struct
(* (name of location , items initially in location, location rules) *)
		 
type name = string
type const = string
datatype value = I of int | C of const | U

  type id = string * string option
  type num = int
  type actionname = string
  datatype vop   = EQ | NEQ | LT | GT | LEQ | GEQ
  datatype action = PA of actionname * name list | GA
  datatype eop = EADD | ESUB
  datatype exp = EV of value | EID of id | EOP of eop * (exp list)
  datatype precondition = Pre of exp * vop * exp
  type update = id * exp
  datatype sideeffect = NOP | PRINT of string

  datatype arg =
	   AID of name |
	   AValue of value |
	   AUpdates of update list
		       
  datatype erule = 
           PRule of precondition list * erule list |
	   ARule of action * update list * sideeffect |
	   SRule of action * name

  datatype mrule = 
	   ERule of erule | 
	   MetaRuleInst of name * arg list

  type Initial = update list

		 
  datatype metarule = MetaRule of name * name list * mrule list
  type LocDef = name * name list * mrule list
  type GScript = metarule list option * name * Initial * LocDef list

end
