structure E2C =
struct

(* structure CompactCore = Core.fCore (StateIntInt) *)
(* structure SplayCore = Core.fCore(StateStringStringSplay) *)
val constConst = State.constConst



fun genLocName items loc name = if List.exists (fn x => x = name) items
                                then loc ^ "/" ^ name
                                else name
val ## = genLocName

fun str2name str = Core.constName str

fun transName items loc (name,NONE) = str2name(## items loc name)
  | transName items loc (name,SOME field) = str2name(## items loc name^"."^field)

fun idAction items loc (ESyntax.PA(name,args)) = Core.PA(name, map (str2name o  (## items loc)) args)
  | idAction _ _ _ = raise Fail "idAction: should not happen"

fun idV (ESyntax.I n) = State.constInt n
  | idV (ESyntax.C n) = constConst n
  | idV (ESyntax.U) = State.undefined

fun idEOP (ESyntax.EADD) = Core.EADD
  | idEOP (ESyntax.ESUB) = Core.ESUB

fun idExp items loc (ESyntax.EV v) = Core.EV(idV v)
  | idExp items loc (ESyntax.EID i) = Core.EID(transName items loc i )
  (*| idExp items loc (ESyntax.EOP(e,elist)) = Core.EOP(idEOP e,map (idExp items loc)elist)*)

fun idUpdate items loc (eid,exp) = (transName items loc eid, idExp items loc exp)

fun idVOP (ESyntax.EQ) = Core.EQ
  | idVOP (ESyntax.NEQ) = Core.NEQ
  (*| idVOP (ESyntax.LT) = Core.LT*)
  (*| idVOP (ESyntax.GT) = Core.GT*)
  (*| idVOP (ESyntax.LEQ) = Core.LEQ*)
  (*| idVOP (ESyntax.GEQ) = Core.GEQ*)

fun idPrecond items loc (ESyntax.Pre(e1,vop,e2)) = 
  Core.Pre(idExp items loc e1, idVOP vop, idExp items loc e2)

fun idSideEffect (ESyntax.NOP) = Core.NOP
  | idSideEffect (ESyntax.PRINT s) = Core.PRINT s

local
  open ESyntax
in
fun transERule items loc (ARule(action, updates, se)) = 
    [Core.Rule(
      [],
      idAction items loc action,
      map (idUpdate items loc) updates, 
      idSideEffect se)]
  | transERule items loc (PRule(preconds, [])) = raise Fail "Parse error no action"
  | transERule items loc (PRule(preconds, [SRule(action, name)])) = raise Fail "SRule, should not happen?"
  | transERule items loc (PRule(preconds, [ARule(action, updates, se)])) =
    [Core.Rule (
      map (idPrecond items loc) preconds,
      idAction items loc action,
      map (idUpdate items loc) updates, 
      idSideEffect se)]
  | transERule items loc (PRule(p1, [PRule(p2,l)])) = 
     transERule items loc (PRule(p1 @ p2,l)) 
  | transERule items loc (PRule(preconds, erules)) =
    List.concat(map 
      (transERule items loc) 
      (map (fn erule =>(PRule(preconds,[erule]))) erules))
  | transERule _ _ _ = raise Fail "transERule match"

fun transMRule items loc (ERule erule) = transERule items loc erule
  | transMRule _ _ _ = raise Fail "transMRule match"

fun zip ([], []) = []
  | zip (x :: xs, y :: ys) = 
  (x,y) :: zip (xs, ys)
  | zip _ = raise Fail "zip error"

(* look up name in env and return NONE when not found; *)
  fun lookup [] name = NONE
    | lookup ((name', subname) :: env) name = 
        if name'=name then SOME subname else lookup env name

   fun lookupException [] name = raise Fail ("lookupException: Key not found: ")
     | lookupException ((name', subname) :: env) name = 
     if name'=name then subname else lookupException env name

   fun revlookupException [] name = raise Fail ("revlookupException: Key not found: ")
     | revlookupException ((subname, name') :: env) name = 
     if name'=name then subname else lookupException env name


fun subName env name = 
  case lookup env name of 
                            SOME (AID n) => n 
                          | _ => name

fun subNameUpd env name = case lookup env name of 
                               SOME (AUpdates uList) => uList 
                             | _ => raise Fail "Updatename not bound to a value!"
fun subNameList env names = map (subName env) names
fun suba env (PA (aname, nList)) = PA (subName env aname, subNameList env nList)
  | suba _ _  = raise Fail "suba: should not happen"
fun subid env (name, NONE) = (subName env name, NONE)
  | subid env (name, SOME name') = (subName env name, SOME (subName env name'))
fun subidForVal env (name, NONE) = 
    (case lookup env name of 
         SOME (AID n) => EID (n, NONE)
       | SOME (AValue v) => EV v
       | _ => raise Fail "Unexpected binding")
  | subidForVal env (name, SOME name') = EID (subName env name, SOME (subName env name'))
fun subexp env (EV v) = EV v
  | subexp env (EID id) = subidForVal env id
  | subexp env (EOP (eop, expList)) = EOP (eop, subExpList env expList)
and subExpList env elist = map (subexp env) elist
fun subup env (id, exp) = (subid env id, subexp env exp)
fun subups env upList = map (subup env) upList
fun subPre env (Pre (e1, vop, e2)) = Pre (subexp env e1, vop, subexp env e2)
fun subpres env preList = map (subPre env) preList

fun subERule env x =
         case x of
             (ARule (a, ups, se)) => (ARule (suba env a, subups env ups, se))
           | (PRule(pcs, [])) => raise Fail "Parse errer no action"
           | (PRule(pcs, [SRule(a, n)])) => PRule(subpres env pcs, [ARule (suba
           env a, subNameUpd env n, ESyntax.NOP)])
           | (PRule(pcs, [ARule(a, us,se)])) => PRule(subpres env pcs, [ARule (suba env a, subups env us, se)])
           | (PRule(p1, [PRule (p2,l)])) => subERule env (PRule(p1@p2, l))
           | (PRule(pcs, erules)) => PRule (subpres env pcs, map (subERule env) erules)
           | _ => raise Fail "subERule match"

fun substitute env [] = []
  | substitute env ((MetaRuleInst (n, _)) :: _) = 
      raise Fail (
        "Metarules should still not instantiate other metarules.\n" ^
        "Metarule attempted instantiated: " ^ n)
  | substitute env ((ERule x) :: mrules) = (ERule (subERule env x)) :: substitute env mrules

fun transMetaDef (metas : name -> arg list -> mrule list) (MetaRule (mname, paramNames, mrules)) = 
  fn x => fn argList => if not(x = mname) 
                        then metas x argList
                        else substitute (zip (paramNames, argList)) mrules

fun transLocDef metas (locationName, items, mrules) =
    let
      (* instantiate metarules in locations *)
      val instarules = List.foldr (fn 
        (*(insRs, (MetaRuleInst (mn, ars))) => (metas mn ars) @ insRs |*)
        (MetaRuleInst (mn, ars), insRs) => (metas mn ars) @ insRs |
        (x, insRs) => x :: insRs) [] mrules
      (* prefix all rules with player.loc = locationName *)
      val mrules' = map (fn (ERule erule) => 
      ESyntax.ERule(
        PRule(
         [ESyntax.Pre(
            EID("player", SOME "loc"),
            ESyntax.EQ,ESyntax.EV(ESyntax.C locationName))]
	      ,
        [erule]
       )
       )
      | _ => raise Fail "transLocDef: this shouldn't happen")
      instarules
    in
      (* translate the expanded and slightly transformed mrules' *)
      List.concat (map (transMRule items locationName) mrules')
    end
end

  fun transE2C (metarules,storyName, initialBindings, locdefs) = 
  let
    open Core
    val metarules = case metarules of NONE => [] | SOME mrs => mrs
							      
    fun genItemLocs loc items = 
        map (fn item => (str2name(loc^"/"^ item^".loc"), EV(State.constConst loc))) items
    
    fun getInitialItems [] = []
      | getInitialItems ((locationName,items,_)::l) =
        (genItemLocs locationName items) @ (getInitialItems l)
    
    val itemLocations = getInitialItems locdefs

    val globalInitialBindings = 
      (*(str2name storyName, *)
      (*Core.EV (State.constConst  "False")) ::*)
      (itemLocations @ 
        (map (fn ((x,y),z) => idUpdate [] "Global" ((x,y),z)) initialBindings))
    
    fun evalloc (EV v) = v
      | evalloc _ = raise Fail "Not implemented : evalloc"
    
    fun ev (id,e) = (id, evalloc e)
    
    val state = State.makeState (map ev globalInitialBindings)
    val initialMetaFun = (fn x => fn a => raise Fail ("Reference of undefined metarule :"^x))
    val metas = foldr (fn (mdef, acc) => transMetaDef acc mdef) initialMetaFun metarules

  in
    (state,List.concat(map (transLocDef metas) locdefs))
  end

  fun genMap (state,core_rules) =
      let
	  open Core
	  fun collectExp (exp,acc as(constants,vars)) =
        (case exp of (EV c) =>
        if State.isConst c
        then ((State.valToConst c)::constants,vars)
        else
			     acc
		       | (EID v) =>  (constants,v::vars)
					 (*| (EOP (eop,e::elist)) => collectExp (EOP (eop,elist),collectExp (e,acc))*)
					 (*| (EOP (_,[])) => acc*)
           )

      fun collectPre (Pre(e,_,e'),acc) = collectExp (e',collectExp(e,acc))

      fun collectPreList (prelist,acc) = foldr collectPre acc prelist

      fun collectAction (PA(_,names),(cs,vs)) = (cs,names@vs)

      fun collectUpdate ((varname,e),acc) =
	    let val (cs,vs) = collectExp (e,acc) in (cs,varname::vs) end

      fun collectUpdateList (ul,acc) = foldr collectUpdate acc ul

      fun collectRule (Rule(pre,a,ul,_),acc) =
        collectPreList(pre,collectAction(a,collectUpdateList(ul,acc)))

      fun collectRuleList rulelist = foldr collectRule ([],[]) rulelist
      fun removeDups l =
	  let
              val sorted = Listsort.sort (String.compare) l
              fun rem (x::y::l) =
		  if x = y
		  then rem (y::l)
		  else x::(rem (y::l))
		| rem x = x
	  in
              rem sorted
	  end

      val (constants,variables) = collectRuleList core_rules

      val (constants,variables) =
        (removeDups ( (map (fn(x,v) => if State.isConst v
                                       then State.valToConst v
                                       else raise Fail "No no!"
        ) (List.filter (fn (x,v) => State.isConst v) state)) @ constants),
         removeDups ( (map (fn(x,_) => x ) state) @ variables))

      fun numberList n [] = (n, [])
        | numberList n (s::ss) =
        let
          val (n', li) = numberList (n + 1) ss
        in
          (n', (s,n) :: li)
        end
    in
      let
        val (c, vlist) = numberList 0 variables
        val (_, clist) = numberList 0 constants
      in
        (clist, vlist)
      end
    end

(* structure FromCore = Core  *)
(* structure ToCore = SplayCore *)
(* local *)
(*     fun transEOP (FromCore.EADD) = ToCore.EADD *)
(*       | transEOP (FromCore.ESUB) = ToCore.ESUB *)
			       
(*     fun idUpdate (eid,exp) = (idEId eid,idExp exp) *)
			     
(*     val transVOP =  *)
(* 	(fn (FromCore.EQ) => ToCore.EQ *)
(* 	  | (FromCore.NEQ) => ToCore.NEQ *)
(* 	  | (FromCore.LT) => ToCore.LT *)
(* 	  | (FromCore.GT) => ToCore.GT *)
(* 	  | (FromCore.LEQ) => ToCore.LEQ *)
(* 	  | (FromCore.GEQ) => ToCore.GEQ) *)

(*     fun transValue (constMap,_) (value:FromCore.value) =  *)
(* 	if FromCore.state.isConst value  *)
(* 	then ToCore.state.constConst(constMap (FromCore.state.valToConst value)) *)
(* 	else ToCore.state.constInt(FromCore.state.valToInt value) *)

(*     fun transExp (cvmap as(_,varMap)) exp = *)
(* 	(case exp of (FromCore.EV value) => ToCore.EV(transValue cvmap value) *)
(* 		   | (FromCore.EID v) => (ToCore.EID(varMap v)) *)
(* 		   | (FromCore.EOP (eop,elist)) => ToCore.EOP(transEOP eop,map (transExp cvmap) elist)) *)
(*     fun transPre cvmap (FromCore.Pre(e,vop,e')) = ToCore.Pre(transExp cvmap e,transVOP vop,transExp cvmap e') *)
(*     fun transPreList cvmap prelist = map (transPre cvmap) prelist *)
(*     fun transAction (_,varMap) (FromCore.PA(action,names)) = ToCore.PA(action, map varMap names) *)
(*     fun transUpdate (cvmap as (_,varMap)) (varname,e) = *)
(* 	(varMap varname,transExp cvmap e) *)
(*     fun transUpdateList cvmap ul = map (transUpdate cvmap) ul *)
(*     fun transSideEffect (FromCore.NOP) = ToCore.NOP *)
(*       | transSideEffect (FromCore.PRINT s) = ToCore.PRINT s *)
(*     fun transRule cvmap (FromCore.Rule(pre,a,ul,se)) = *)
(* 	ToCore.Rule(transPreList cvmap pre,transAction cvmap a,transUpdateList cvmap  ul,transSideEffect se) *)
(*     fun transRuleList cvmap rulelist = map (transRule cvmap) rulelist *)

(*     fun transState (cvmap as(_,varMap)) state =  *)
(* 	map  *)
(*             (fn(name,value)=>( *)
(* 			      (varMap name,transValue cvmap value))) state *)

(*     fun rtransValue (constMap,_) (value:ToCore.value) =  *)
(* 	if ToCore.state.isConst value  *)
(* 	then FromCore.state.constConst(constMap (ToCore.state.valToConst value)) *)
(* 	else FromCore.state.constInt(ToCore.state.valToInt value) *)

(*     fun rtransState (cvmap as(_,varMap)) state =  *)
(* 	map  *)
(*             (fn(name,value)=>( *)
(* 			      (varMap name,rtransValue cvmap value))) state *)
	    
(* in  *)
(* val rtransState = rtransState *)
(* val transState = transState *)
(* val transRuleList = transRuleList *)
(* val transPre = transPre *)
(* end *)


(*   fun transE2CCompactRules transE2CArgs = *)
(*   let *)

(* 	  val (state, core_rules) = transE2C transE2CArgs *)
(* 	  val (constMapList,varMapList) = genMap (state, core_rules) *)
(* 	  val cvmap = (lookupException constMapList,lookupException varMapList) *)

(*   in *)
(*       ([](\* transState cvmap state *\), *)
(*        [](\* transRuleList cvmap core_rules *\), *)
(*        constMapList, *)
(*        varMapList) *)
(*   end *)

end
