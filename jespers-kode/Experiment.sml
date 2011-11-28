fun lookup [] x = []
  | lookup ((y, v) :: st) x = if x = y then v else lookup st x

fun insert value list = 
    if List.exists (fn x=>value=x) list 
    then list
    else value::list

fun mapsto [] (x, v) = [(x,[v])]
  | mapsto ((y, l) :: state) (x, v) = 
    if x = y then
	(y,insert v l)::state
    else
	(y, l) :: mapsto state (x,v)

local
fun findAssignmentsUpdate ((name,exp),valuelist) = 
    mapsto valuelist (name,Core.evalexp [] exp)
fun findAssignmentsUpdateList (ul,valuelist)  = 
    foldr findAssignmentsUpdate valuelist ul
fun findAssignmentsRule (Core.Rule(_,_,ul,_),valuelist) = 
    findAssignmentsUpdateList (ul,valuelist)
fun findAssignmentsRuleList (rulelist,valuelist) = 
    foldr findAssignmentsRule valuelist rulelist 
in
fun findAssignmentsStory (initialstate,rulelist) = 
    findAssignmentsRuleList 
	(rulelist, 
	 findAssignmentsUpdateList 
	     (map (fn(name,value)=> (name,Core.EV value)) initialstate
	      ,[])) 
end


val script_name = "MI2part1.fsm"
fun load_script fname = 
    let 
	val (ms, sname, init, locs) = Extended.parse_file fname
    in
	(sname, E2C.transE2C (ms, sname, init, locs))
    end
val (name, s) = load_script script_name;

val initial_state = #1 s
val rules = #2 s
