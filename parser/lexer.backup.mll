{
(*module E = ERRORMSG*)
open Token
open Stack
open Lexing
}

(*let lineNum = E.lineNum
let linePos = E.linePos
let comment_pos = Stack.create() *)

let letter = ['A'-'Z' 'a'-'z']
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let newline = ('\n' | "\r\n")
let whitespace = [' ' '\t']

let digit = ['0'-'9']


rule token = parse
  | newline {
    (*incr lineNum;
    linePos := *)
    token lexbuf
  }
  | identifier as id
      {
	match id with
	  | "type" -> Token.TYPE(Lexing.lexeme_start_p lexbuf)
	  | "var" -> Token.VAR(Lexing.lexeme_start_p lexbuf)
	  | "function" -> Token.FUNCTION(Lexing.lexeme_start_p lexbuf)
	  | "break" -> Token.BREAK(Lexing.lexeme_start_p lexbuf)
	  | "of" -> Token.OF(Lexing.lexeme_start_p lexbuf)
	  | "end" -> Token.END(Lexing.lexeme_start_p lexbuf)
	  | "in" -> Token.IN(Lexing.lexeme_start_p lexbuf)
	  | "nil" -> Token.NIL(Lexing.lexeme_start_p lexbuf)
	  | "let" -> Token.LET(Lexing.lexeme_start_p lexbuf)
	  | "do" -> Token.DO(Lexing.lexeme_start_p lexbuf)
	  | "to" -> Token.TO(Lexing.lexeme_start_p lexbuf)
	  | "for" -> Token.FOR(Lexing.lexeme_start_p lexbuf)
	  | "while" -> Token.WHILE(Lexing.lexeme_start_p lexbuf)
	  | "else" -> Token.ELSE(Lexing.lexeme_start_p lexbuf)
	  | "then" -> Token.THEN(Lexing.lexeme_start_p lexbuf)
	  | "if" -> Token.IF(Lexing.lexeme_start_p lexbuf)
	  | "array" -> Token.ARRAY(Lexing.lexeme_start_p lexbuf)
	  | _ -> let s = Lexing.lexeme lexbuf in
		 ID(s,  Lexing.lexeme_start_p lexbuf)
      }
  | ":=" { Token.ASSIGN(Lexing.lexeme_start_p lexbuf) }
  | "|" { Token.OR(Lexing.lexeme_start_p lexbuf) }
  | "&" { Token.AND(Lexing.lexeme_start_p lexbuf) }
  | ">=" { Token.GE(Lexing.lexeme_start_p lexbuf) }
  | ">" { Token.GT(Lexing.lexeme_start_p lexbuf) }
  | "<=" { Token.LE(Lexing.lexeme_start_p lexbuf) }
  | "<" { Token.LT(Lexing.lexeme_start_p lexbuf) }
  | "<>" { Token.NEQ(Lexing.lexeme_start_p lexbuf) }
  | "=" { Token.EQ(Lexing.lexeme_start_p lexbuf) }
  | "/" { Token.DIVIDE(Lexing.lexeme_start_p lexbuf) }
  | "*" { Token.TIMES(Lexing.lexeme_start_p lexbuf) }
  | "-" { Token.MINUS(Lexing.lexeme_start_p lexbuf) }
  | "+" { Token.PLUS(Lexing.lexeme_start_p lexbuf) }
  | "." { Token.DOT(Lexing.lexeme_start_p lexbuf) }
  | "}" { Token.RBRACE(Lexing.lexeme_start_p lexbuf) }
  | "{" { Token.LBRACE(Lexing.lexeme_start_p lexbuf) }
  | ")" { Token.RBRACK(Lexing.lexeme_start_p lexbuf) }
  | "(" { Token.LBRACK(Lexing.lexeme_start_p lexbuf) }
  | "[" { Token.RPAREN(Lexing.lexeme_start_p lexbuf) }
  | "]" { Token.LPAREN(Lexing.lexeme_start_p lexbuf) }
  | ";" { Token.SEMICOLON(Lexing.lexeme_start_p lexbuf) }
  | ":" { Token.COLON(Lexing.lexeme_start_p lexbuf) }
  | "," { Token.COMMA(Lexing.lexeme_start_p lexbuf) }
  | "hoo" { Token.STRING(Lexing.lexeme lexbuf, Lexing.lexeme_start_p lexbuf) }
  | digit+
      {
	INT(int_of_string(Lexing.lexeme lexbuf), Lexing.lexeme_start_p lexbuf)
      }
  | eof { Token.EOF(Lexing.lexeme_start_p lexbuf) }
