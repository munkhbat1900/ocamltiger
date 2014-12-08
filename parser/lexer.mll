{
(*module E = ERRORMSG*)
open Lexing
open Parser

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
	  | "type" -> Printf.printf "TYPE\n"; TYPE
	  | "var" -> Printf.printf "VAR\n"; VAR
	  | "function" -> Printf.printf "FUNCTION\n"; FUNCTION
	  | "break" -> Printf.printf "BREAK\n"; BREAK
	  | "of" -> Printf.printf "OF\n"; OF
	  | "end" -> Printf.printf "END\n"; END
	  | "in" -> Printf.printf "IN\n"; IN
	  | "nil" -> Printf.printf "NIL\n"; NIL
	  | "let" -> Printf.printf "LET\n"; LET
	  | "do" -> Printf.printf "DO\n"; DO
	  | "to" -> Printf.printf "TO\n"; TO
	  | "for" -> Printf.printf "FOR\n"; FOR
	  | "while" -> Printf.printf "WHILE\n"; WHILE
	  | "else" -> Printf.printf "ELSE\n"; ELSE
	  | "then" -> Printf.printf "THEN\n"; THEN
	  | "if" -> Printf.printf "IF\n"; IF
	  | "array" -> Printf.printf "ARRAY\n"; ARRAY
	  | _ -> Printf.printf "IDENTIFIER = %s\n" (Lexing.lexeme lexbuf); ID(Lexing.lexeme lexbuf)
      }
  | "/*" { comment 0 lexbuf; token lexbuf }
  | ":=" { Printf.printf "ASSIGN\n"; ASSIGN }
  | "|" { Printf.printf "OR\n"; OR }
  | "&" { Printf.printf "AND\n"; AND }
  | ">=" { Printf.printf "GE\n"; GE }
  | ">" { Printf.printf "GT\n"; GT}
  | "<=" { Printf.printf "LE\n"; LE }
  | "<" { Printf.printf "LT\n"; LT }
  | "<>" { Printf.printf "NEQ\n"; NEQ }
  | "=" { Printf.printf "EQ\n"; EQ }
  | "/" { Printf.printf "DIVIDE\n"; DIVIDE }
  | "*" { Printf.printf "TIMES\n"; TIMES }
  | "-" { Printf.printf "MINUS\n"; MINUS }
  | "+" { Printf.printf "PLUS\n"; PLUS }
  | "." { Printf.printf "DOT\n"; DOT }
  | "}" { Printf.printf "RBRACE\n"; RBRACE }
  | "{" { Printf.printf "LBRACE\n"; LBRACE }
  | ")" { Printf.printf "RPAREN\n"; RPAREN }
  | "(" { Printf.printf "LPAREN\n"; LPAREN }
  | "[" { Printf.printf "LBRACK\n"; LBRACK }
  | "]" { Printf.printf "RBRACK\n"; RBRACK }
  | ";" { Printf.printf "SEMICOLON\n"; SEMICOLON }
  | ":" { Printf.printf "COLON\n"; COLON }
  | "," { Printf.printf "COMMA\n"; COMMA }
  | "\"" { let s = string lexbuf in Printf.printf "STRING = %s\n" s; STRING(Lexing.lexeme lexbuf) }
  | digit+
      {
	Printf.printf "integer literal = %s\n" (Lexing.lexeme lexbuf); INT(int_of_string (Lexing.lexeme lexbuf))
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

