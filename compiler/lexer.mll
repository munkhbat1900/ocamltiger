{
(*module E = ERRORMSG*)
open Lexing
open Parser1

let string_buffer = Buffer.create 10

let escape c = 
  match c with
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 'b' -> '\b'
  | 't' -> '\t'
  | _ -> c
}

(*let lineNum = E.lineNum
let linePos = E.linePos *)

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
  | whitespace {token lexbuf}
  | identifier as id
      {
	match id with
	  | "type" -> TYPE
	  | "var" -> VAR
	  | "function" -> FUNCTION
	  | "break" -> BREAK
	  | "of" -> OF
	  | "end" -> END
	  | "in" -> IN
	  | "nil" -> NIL
	  | "let" -> LET
	  | "do" -> DO
	  | "to" -> TO
	  | "for" -> FOR
	  | "while" -> WHILE
	  | "else" -> ELSE
	  | "then" -> THEN
	  | "if" ->  IF
	  | "array" -> ARRAY
	  | _ -> ID(Lexing.lexeme lexbuf)
      }
  | "/*" { comment 0 lexbuf; token lexbuf }
  | ":=" { ASSIGN }
  | "|" { OR }
  | "&" { AND }
  | ">=" { GE }
  | ">" { GT}
  | "<=" { LE }
  | "<" { LT }
  | "<>" { NEQ }
  | "=" { EQ }
  | "/" { DIVIDE }
  | "*" { TIMES }
  | "-" { MINUS }
  | "+" { PLUS }
  | "." { DOT }
  | "}" { RBRACE }
  | "{" { LBRACE }
  | ")" { RPAREN }
  | "(" { LPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | ";" { SEMICOLON }
  | ":" { COLON }
  | "," { COMMA }
  | "\"" { let s = string lexbuf in STRING(s) }
  | digit+
      {
	INT(int_of_string (Lexing.lexeme lexbuf))
      }
  | eof { Printf.printf "EOF\n"; EOF }

and string = parse
  | "\"" 
      {
	let s = Buffer.contents string_buffer in
	Buffer.clear string_buffer;
	s
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r']
      { 
	Buffer.add_char string_buffer (escape (Lexing.lexeme_char lexbuf 1));
        string lexbuf 
      }
  | [^ '"' '\\'] +
      { 
	Buffer.add_string string_buffer (Lexing.lexeme lexbuf);
        string lexbuf 
      }
  | eof {
    failwith
        (Printf.sprintf
           "unterminated string near characters %d-%d"
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf))
  }

and comment level = parse
  | "/*"
      {
	comment (level + 1) lexbuf
      }
  | "*/" 
      {
	if level = 0 then () else comment (level - 1) lexbuf
      }
  | newline { comment level lexbuf }
  | eof { failwith
        (Printf.sprintf
           "unterminated comment near characters %d-%d"
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
  | _ { comment level lexbuf }

