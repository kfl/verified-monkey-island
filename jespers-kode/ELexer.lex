{
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

 }

rule Token = parse
    [` ` `\t` `\r`]     { Token lexbuf } (* whitespace *)
  | [`\n` `\012`]       { currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          Token lexbuf } (* newlines *)
  | "--" [^ `\n`]*      { Token lexbuf } (* comment *)
	| ">>"                { EParser.RR (getPos lexbuf) }
	| `?`                 { EParser.QUE (getPos lexbuf) }
	| `&`                 { EParser.AMP (getPos lexbuf) }
	| `(`                 { EParser.LPAR (getPos lexbuf) }
	| `)`                 { EParser.RPAR (getPos lexbuf) }
	| `{`                 { EParser.LCUR (getPos lexbuf) }
	| `}`                 { EParser.RCUR (getPos lexbuf) }
	| `[`                 { EParser.LBRA (getPos lexbuf) }
	| `]`                 { EParser.RBRA (getPos lexbuf) }
	| `+`                 { EParser.ADD (getPos lexbuf) }
	| `-`                 { EParser.SUB (getPos lexbuf) }
	| `>`                 { EParser.GT (getPos lexbuf) }
	| `<`                 { EParser.LT (getPos lexbuf) }
	| `=`                 { EParser.EQ (getPos lexbuf) }
	| "!="                { EParser.NEQ (getPos lexbuf) }
	| "<="                { EParser.LEQ (getPos lexbuf) }
	| ">="                { EParser.GEQ (getPos lexbuf) }
	| ":="                { EParser.ASSIGN (getPos lexbuf) }
	| `,`                 { EParser.COMMA (getPos lexbuf) }
	| `;`                 { EParser.SEMI (getPos lexbuf) }
	| "not"               { EParser.NOT (getPos lexbuf) }
	| `.`                 { EParser.DOT (getPos lexbuf) }
  | [`a`-`z`]([`a`-`z`] | [`A`-`Z`] | [`0`-`9`])*
	                      { EParser.NAME (getLexeme lexbuf, getPos lexbuf) }
  | [`A`-`Z`]([`a`-`z`] | [`A`-`Z`] | [`0`-`9`])*
                        { keyword (getLexeme lexbuf, getPos lexbuf) }
	| [`"`]([`a`-`z`] | [`A`-`Z`] | [`0`-`9`] | [` `] | [`.`] | [`,`] | [`!`] | [`'`] | [`?`] | [`-`])+[`"`] 
	                      { EParser.STRING (getLexeme lexbuf, getPos lexbuf) }
	| [`0`-`9`]+          { case Int.fromString (getLexeme lexbuf) of
														NONE   => lexerError lexbuf "Bad integer"
														| SOME i => EParser.NUM (i, getPos lexbuf) }
  | eof                 { EParser.EOF (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

;
