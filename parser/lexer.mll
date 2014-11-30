{
(*module E = ERRORMSG*)
open Token
open Stack
open Lexing

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
  | whitespace {token lexbuf}
  | identifier as id
      {
	match id with
	  | "type" -> Printf.printf "TYPE\n"; token lexbuf
	  | "var" -> Printf.printf "VAR\n"; token lexbuf
	  | "function" -> Printf.printf "FUNCTION\n"; token lexbuf
	  | "break" -> Printf.printf "BREAK\n"; token lexbuf
	  | "of" -> Printf.printf "OF\n"; token lexbuf
	  | "end" -> Printf.printf "END\n"; token lexbuf
	  | "in" -> Printf.printf "IN\n"; token lexbuf
	  | "nil" -> Printf.printf "NIL\n"; token lexbuf
	  | "let" -> Printf.printf "LET\n"; token lexbuf
	  | "do" -> Printf.printf "DO\n"; token lexbuf
	  | "to" -> Printf.printf "TO\n"; token lexbuf
	  | "for" -> Printf.printf "FOR\n"; token lexbuf
	  | "while" -> Printf.printf "WHILE\n"; token lexbuf
	  | "else" -> Printf.printf "ELSE\n"; token lexbuf
	  | "then" -> Printf.printf "THEN\n"; token lexbuf
	  | "if" -> Printf.printf "IF\n"; token lexbuf
	  | "array" -> Printf.printf "ARRAY\n"; token lexbuf
	  | _ -> Printf.printf "IDENTIFIER = %s\n" (Lexing.lexeme lexbuf); token lexbuf 
      }
  | "/*" { comment 0 lexbuf; token lexbuf }
  | ":=" { Printf.printf "ASSIGN\n"; token lexbuf }
  | "|" { Printf.printf "OR\n"; token lexbuf }
  | "&" { Printf.printf "AND\n"; token lexbuf }
  | ">=" { Printf.printf "GE\n"; token lexbuf }
  | ">" { Printf.printf "GT\n"; token lexbuf }
  | "<=" { Printf.printf "LE\n"; token lexbuf }
  | "<" { Printf.printf "LT\n"; token lexbuf }
  | "<>" { Printf.printf "NEQ\n"; token lexbuf }
  | "=" { Printf.printf "EQ\n"; token lexbuf }
  | "/" { Printf.printf "DIVIDE\n"; token lexbuf }
  | "*" { Printf.printf "TIMES\n"; token lexbuf }
  | "-" { Printf.printf "MINUS\n"; token lexbuf }
  | "+" { Printf.printf "PLUS\n"; token lexbuf }
  | "." { Printf.printf "DOT\n"; token lexbuf }
  | "}" { Printf.printf "RBRACE\n"; token lexbuf }
  | "{" { Printf.printf "LBRACE\n"; token lexbuf }
  | ")" { Printf.printf "RBRACK\n"; token lexbuf }
  | "(" { Printf.printf "LBRACK\n"; token lexbuf }
  | "[" { Printf.printf "RPAREN\n"; token lexbuf }
  | "]" { Printf.printf "LPAREN\n"; token lexbuf }
  | ";" { Printf.printf "SEMICOLON\n"; token lexbuf }
  | ":" { Printf.printf "COLON\n"; token lexbuf }
  | "," { Printf.printf "COMMA\n"; token lexbuf }
  | "\"" { let s = string lexbuf in Printf.printf "STRING = %s\n" s; token lexbuf }
  | digit+
      {
	Printf.printf "integer literal = %s\n" (Lexing.lexeme lexbuf); token lexbuf
      }
  | eof { Printf.printf "EOF\n"; }

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

