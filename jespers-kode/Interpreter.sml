structure Interpreter =
struct
  open TextIO

  val prompt = ">>> "

  val welcometext = 
    "Welcome to the GASDOSPELA CORE+ interpreter!\n"

  val choseOneText = "Pick one by number:\n" ^ prompt
  val retrytext = "I was not able to parse you input. Please try again: \n" ^ prompt

  val output = fn x => output (stdOut, x)

  fun getUserNumber maxnum = 
    (
      flushOut stdOut;
      let val SOME numtext = inputLine stdIn
      in
        case (Int.fromString numtext) of
             NONE => (output retrytext; getUserNumber maxnum)
           | SOME(n) => if n > maxnum orelse n < 1
                       then (output "Wrong number\n"; getUserNumber maxnum)
                       else n
      end
    )

  fun presentChoices choices = 
  let
    fun loop n [] =
        "---"
      | loop n (x :: xs) =
        (Int.toString n) ^ " - " ^ x ^ "\n" ^
        loop (n + 1) xs
  in
      output (loop 1 choices)
  end


  fun processInput choices =
    (
      presentChoices (map (fn (a, _) => Core.ppact a) choices);
      output choseOneText;
      let val num = getUserNumber (length choices)
      in
        List.nth(choices, num - 1)
      end
    )

  val quitAction = Core.PA ("Quit", [])
  val invActiion = Core.PA ("Inv?", [])
  val stateAction = Core.PA ("State?", [])
  
  fun print_inv state = 
    output ("\n\nInventory {" ^ 
        List.foldl (fn ((x,v), acc) => 
        let val ss = String.substring (x, size x - 4, 4) handle Subscript => ""
        in
        if ss = ".loc" andalso v = State.constConst "Inv"
        then String.substring (x, 0, size x - 4) ^ ", " ^ acc
        else acc
        end) "}\n\n\n" state
        )

  fun makeCompletedPre storyname = 
      Core.Pre (Core.EID storyname, Core.EQ, Core.EV (State.constConst "True"))

  fun storyCompleted pre state = Core.sat state pre

  fun performEffect state Core.NOP = state
    | performEffect state (Core.PRINT s) = (print (s ^ "\n");state)

  fun run (compl_pred, storyname) state rules = 
  if storyCompleted compl_pred state
  then
    output ("["^ storyname ^ "] completed\n")
  else
  let 
    val choices =  Core.findEnabledActionsEffects (state, rules)
  in
    case processInput 
	     ((quitAction, Core.NOP) ::
	      (stateAction, Core.NOP) ::
	      (invActiion, Core.NOP) ::choices) 
     of
        (Core.PA ("Quit", []), _) => raise Fail "DONE"
      | (Core.PA ("Inv?", []), _) => (print_inv state; (* should use ppstate *) run (compl_pred, storyname) state rules)
      | (Core.PA ("State?", []), _) => 
        (
        output (State.ppstate state);
         (*output "Not implemented\n";*)
         run (compl_pred, storyname) state rules)
      | (act , se) => run 
			  (compl_pred, storyname) 
			  (Core.step compl_pred (performEffect state se, rules) act) 
			  rules
  end

  val script_name = List.nth(Mosml.argv (), 1)

  val ext_story  = Extended.parse_file script_name
  val (_, story_name, _, _) = ext_story
  val (initial_state, core_rules) = (* (Core.s0, [Core.rtest1, Core.rtest2]) *)
      E2C.transE2C ext_story
  fun go () = ((
    output welcometext;
    output ("Script ["^ script_name ^"/"^ story_name ^"]\n");
    run (makeCompletedPre(story_name), story_name) initial_state core_rules) 
    handle Fail "DONE" => output "Bye...\n")
  val _ = go()
end
