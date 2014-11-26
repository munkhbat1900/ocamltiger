{
module E = ERRORMSG
open Tokens
}

let lineNum = E.lineNum
let linePos = E.linePos
let comment_pos = Stack.create()

let letter = ['A'-'Z' 'a'-'z']
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let newline = ('\n' | "\r\n")
let whitespace = [' ' '\t']

let digit = ['0'-'9']


rule token = parse
  | newline {
    incr lineNum;
    linePos := 
    token lexbuf
  }
  | identifier as id
      {
	match id with
	  | "type" -> TYPE(Lexing.position)
	  | "var" -> VAR(Lexing.position)
	  | "function" -> FUNCTION(Lexing.position)
	  | "break" -> BREAK(Lexing.position)
	  | "of" -> OF(Lexing.position)
	  | "end" -> END(Lexing.position)
	  | "in" -> IN(Lexing.position)
	  | "nil" -> NIL(Lexing.position)
	  | "let" -> LET(Lexing.position)
	  | "do" -> DO(Lexing.position)
	  | "to" -> TO(Lexing.position)
	  | "for" -> FOR(Lexing.position)
	  | "while" -> WHILE(Lexing.position)
	  | "else" -> ELSE(Lexing.position)
	  | "then" -> THEN(Lexing.position)
	  | "if" -> IF(Lexing.position)
	  | "array" -> ARRAY(Lexing.position)
      }
  | ":=" -> ASSIGN(Lexing.position)
  | "|" -> OR(Lexing.position)
  | "&" -> AND(Lexing.position)
  | ">=" -> GE(Lexing.position)
  | ">" -> GT(Lexing.position)
  | "<=" -> LE(Lexing.position)
  | "<" -> LT(Lexing.position)
  | "<>" -> NEQ(Lexing.position)
  | "=" -> EQ(Lexing.position)
  | "/" -> DIVIDE(Lexing.position)
  | "*" -> TIMES(Lexing.position)
  | "-" -> MINUS(Lexing.position)
  | "+" -> PLUS(Lexing.position)
  | "." -> DOT(Lexing.position)
  | "}" -> RBRACE(Lexing.position)
  | "{" -> LBRACE(Lexing.position)
  | ")" -> RBRACK(Lexing.position)
  | "(" -> LBRACK(Lexing.position)
  | "[" -> RPAREN(Lexing.position)
  | "]" -> LPAREN(Lexing.position)
  | ";" -> SEMICOLON(Lexing.position)
  | ":" -> COLON(Lexing.position)
  | "," -> COMMA(Lexing.position)
  | "" -> STRING( (string) *  Lexing.position)
  | letter identifier*
      {
	let s = Lexing.lexeme lexbuf in
	ID(s,  Lexing.position)
      }
  | digit+
      {
	INT(int_of_string(Lexing.lexeme lexbuf, Lexing.position))
      }
  | eof -> EOF(Lexing.position)
