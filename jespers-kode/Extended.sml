structure Extended =
struct
  fun createLexerStream ( is : BasicIO.instream ) =
      Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

  fun parse_file filename =  
      let val lexbuf = createLexerStream (BasicIO.open_in filename)
          val defs =  (EParser.GScript ELexer.Token lexbuf)
      in
        defs
      end


  fun errorMess s = (TextIO.output (TextIO.stdErr,s ^ "\n") ; (NONE,"NAS", [], []))

  val defs = fn _ => parse_file (List.nth(Mosml.argv (),1))
          handle Parsing.yyexit ob => errorMess "Parser-exit\n"
               | Parsing.ParseError ob =>
                   let val Location.Loc (p1,p2)
                             = Location.getCurrentLocation ()
                       val (lin,col)
			     = ELexer.getLineCol p2
						(!ELexer.currentLine)
						(!ELexer.lineStartPos)
                   in
                     errorMess ("Parse-error at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
                   end
               | ELexer.LexicalError (mess,(lin,col)) =>
                     errorMess ("Lexical error: " ^mess^ " at line "
                      ^ makestring lin ^ ", column " ^ makestring col)
               | SysErr (s,_) => errorMess ("Exception: " ^ s)
  val _ = defs ()
end
