local open Obj Lexing in


 open Lexing;

 val currentLine = ref 1
 val lineStartPos = ref [0]

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
				(!currentLine)
				(!lineStartPos)

 and getLineCol pos line (p1::ps) =
       if pos>=p1 then (line, pos-p1)
       else getLineCol pos (line-1) ps

 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 fun lexerError lexbuf s = 
     raise LexicalError (s, getPos lexbuf)

 fun keyword (s, pos) =
     case s of
	      "Location"   => EParser.LOC pos
	      |"Initial"   => EParser.INIT pos
	      |"Locals"  => EParser.CONTAINS pos
	      |"End"       => EParser.END pos
	      |"Story"     => EParser.STORY pos
	      |"Rule"      => EParser.RULE pos
	      |"Undefined" => EParser.UNDEFINED pos
	      | _          => EParser.CONST (s,pos);

 
fun action_30 lexbuf = (
 lexerError lexbuf "Illegal symbol in input" )
and action_29 lexbuf = (
 EParser.EOF (getPos lexbuf) )
and action_28 lexbuf = (
 case Int.fromString (getLexeme lexbuf) of
														NONE   => lexerError lexbuf "Bad integer"
														| SOME i => EParser.NUM (i, getPos lexbuf) )
and action_27 lexbuf = (
 EParser.STRING (getLexeme lexbuf, getPos lexbuf) )
and action_26 lexbuf = (
 keyword (getLexeme lexbuf, getPos lexbuf) )
and action_25 lexbuf = (
 EParser.NAME (getLexeme lexbuf, getPos lexbuf) )
and action_24 lexbuf = (
 EParser.DOT (getPos lexbuf) )
and action_23 lexbuf = (
 EParser.NOT (getPos lexbuf) )
and action_22 lexbuf = (
 EParser.SEMI (getPos lexbuf) )
and action_21 lexbuf = (
 EParser.COMMA (getPos lexbuf) )
and action_20 lexbuf = (
 EParser.ASSIGN (getPos lexbuf) )
and action_19 lexbuf = (
 EParser.GEQ (getPos lexbuf) )
and action_18 lexbuf = (
 EParser.LEQ (getPos lexbuf) )
and action_17 lexbuf = (
 EParser.NEQ (getPos lexbuf) )
and action_16 lexbuf = (
 EParser.EQ (getPos lexbuf) )
and action_15 lexbuf = (
 EParser.LT (getPos lexbuf) )
and action_14 lexbuf = (
 EParser.GT (getPos lexbuf) )
and action_13 lexbuf = (
 EParser.SUB (getPos lexbuf) )
and action_12 lexbuf = (
 EParser.ADD (getPos lexbuf) )
and action_11 lexbuf = (
 EParser.RBRA (getPos lexbuf) )
and action_10 lexbuf = (
 EParser.LBRA (getPos lexbuf) )
and action_9 lexbuf = (
 EParser.RCUR (getPos lexbuf) )
and action_8 lexbuf = (
 EParser.LCUR (getPos lexbuf) )
and action_7 lexbuf = (
 EParser.RPAR (getPos lexbuf) )
and action_6 lexbuf = (
 EParser.LPAR (getPos lexbuf) )
and action_5 lexbuf = (
 EParser.AMP (getPos lexbuf) )
and action_4 lexbuf = (
 EParser.QUE (getPos lexbuf) )
and action_3 lexbuf = (
 EParser.RR (getPos lexbuf) )
and action_2 lexbuf = (
 Token lexbuf )
and action_1 lexbuf = (
 currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          Token lexbuf )
and action_0 lexbuf = (
 Token lexbuf )
and state_0 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"A" andalso currChar <= #"Z" then  state_21 lexbuf
 else if currChar >= #"a" andalso currChar <= #"m" then  state_24 lexbuf
 else if currChar >= #"o" andalso currChar <= #"z" then  state_24 lexbuf
 else if currChar >= #"0" andalso currChar <= #"9" then  state_14 lexbuf
 else case currChar of
    #"\t" => action_0 lexbuf
 |  #"\r" => action_0 lexbuf
 |  #" " => action_0 lexbuf
 |  #"\n" => action_1 lexbuf
 |  #"\f" => action_1 lexbuf
 |  #"}" => action_9 lexbuf
 |  #"{" => action_8 lexbuf
 |  #"n" => state_25 lexbuf
 |  #"]" => action_11 lexbuf
 |  #"[" => action_10 lexbuf
 |  #"?" => action_4 lexbuf
 |  #">" => state_19 lexbuf
 |  #"=" => action_16 lexbuf
 |  #"<" => state_17 lexbuf
 |  #";" => action_22 lexbuf
 |  #":" => state_15 lexbuf
 |  #"." => action_24 lexbuf
 |  #"-" => state_12 lexbuf
 |  #"," => action_21 lexbuf
 |  #"+" => action_12 lexbuf
 |  #")" => action_7 lexbuf
 |  #"(" => action_6 lexbuf
 |  #"&" => action_5 lexbuf
 |  #"\"" => state_6 lexbuf
 |  #"!" => state_5 lexbuf
 |  #"\^@" => action_29 lexbuf
 |  _ => action_30 lexbuf
 end)
and state_5 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_30);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_17 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_6 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_30);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_38 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_38 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_38 lexbuf
 else case currChar of
    #"!" => state_38 lexbuf
 |  #" " => state_38 lexbuf
 |  #"'" => state_38 lexbuf
 |  #"." => state_38 lexbuf
 |  #"-" => state_38 lexbuf
 |  #"," => state_38 lexbuf
 |  #"?" => state_38 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_12 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_13);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"-" => state_37 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_14 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_28);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_36 lexbuf
 else backtrack lexbuf
 end)
and state_15 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_30);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_20 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_17 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_15);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"=" => action_18 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_19 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_14);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #">" => action_3 lexbuf
 |  #"=" => action_19 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_21 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_26);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_31 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_31 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_31 lexbuf
 else backtrack lexbuf
 end)
and state_24 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_28 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_28 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_28 lexbuf
 else backtrack lexbuf
 end)
and state_25 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_28 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_28 lexbuf
 else if currChar >= #"a" andalso currChar <= #"n" then  state_28 lexbuf
 else if currChar >= #"p" andalso currChar <= #"z" then  state_28 lexbuf
 else case currChar of
    #"o" => state_29 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_28 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_28 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_28 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_28 lexbuf
 else backtrack lexbuf
 end)
and state_29 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_25);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_28 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_28 lexbuf
 else if currChar >= #"a" andalso currChar <= #"s" then  state_28 lexbuf
 else if currChar >= #"u" andalso currChar <= #"z" then  state_28 lexbuf
 else case currChar of
    #"t" => state_30 lexbuf
 |  _ => backtrack lexbuf
 end)
and state_30 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_23);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_28 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_28 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_28 lexbuf
 else backtrack lexbuf
 end)
and state_31 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_26);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_31 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_31 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_31 lexbuf
 else backtrack lexbuf
 end)
and state_36 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_28);
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_36 lexbuf
 else backtrack lexbuf
 end)
and state_37 lexbuf = (
 setLexLastPos lexbuf (getLexCurrPos lexbuf);
 setLexLastAction lexbuf (magic action_2);
 let val currChar = getNextChar lexbuf in
 case currChar of
    #"\^@" => backtrack lexbuf
 |  #"\n" => backtrack lexbuf
 |  _ => state_37 lexbuf
 end)
and state_38 lexbuf = (
 let val currChar = getNextChar lexbuf in
 if currChar >= #"0" andalso currChar <= #"9" then  state_38 lexbuf
 else if currChar >= #"A" andalso currChar <= #"Z" then  state_38 lexbuf
 else if currChar >= #"a" andalso currChar <= #"z" then  state_38 lexbuf
 else case currChar of
    #"!" => state_38 lexbuf
 |  #" " => state_38 lexbuf
 |  #"'" => state_38 lexbuf
 |  #"." => state_38 lexbuf
 |  #"-" => state_38 lexbuf
 |  #"," => state_38 lexbuf
 |  #"?" => state_38 lexbuf
 |  #"\"" => action_27 lexbuf
 |  _ => backtrack lexbuf
 end)
and Token lexbuf =
  (setLexLastAction lexbuf (magic dummyAction);
   setLexStartPos lexbuf (getLexCurrPos lexbuf);
   state_0 lexbuf)

(* The following checks type consistency of actions *)
val _ = fn _ => [action_30, action_29, action_28, action_27, action_26, action_25, action_24, action_23, action_22, action_21, action_20, action_19, action_18, action_17, action_16, action_15, action_14, action_13, action_12, action_11, action_10, action_9, action_8, action_7, action_6, action_5, action_4, action_3, action_2, action_1, action_0];

end
