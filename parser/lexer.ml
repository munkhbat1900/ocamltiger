# 1 "lexer.mll"
 
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

# 18 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\226\255\078\000\228\255\229\255\231\255\232\255\233\255\
    \234\255\235\255\236\255\237\255\238\255\239\255\240\255\241\255\
    \243\255\002\000\031\000\249\255\250\255\033\000\054\000\088\000\
    \254\255\001\000\255\255\252\255\251\255\248\255\244\255\246\255\
    \145\000\252\255\146\000\177\000\255\255\254\255\171\000\251\255\
    \252\255\002\000\253\255\099\000\105\000\255\255\254\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\028\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\010\000\008\000\255\255\255\255\025\000\013\000\002\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\002\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\004\000\255\255\004\000\004\000\255\255\255\255";
  Lexing.lex_default = 
   "\255\255\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\000\000\000\000\255\255\255\255\255\255\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \034\000\000\000\034\000\255\255\000\000\000\000\039\000\000\000\
    \000\000\255\255\000\000\255\255\255\255\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\024\000\026\000\026\000\042\000\025\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \024\000\000\000\003\000\000\000\000\000\000\000\019\000\000\000\
    \008\000\009\000\015\000\013\000\004\000\014\000\012\000\022\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\021\000\005\000\017\000\016\000\018\000\031\000\
    \030\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\007\000\029\000\006\000\028\000\023\000\
    \027\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\010\000\020\000\011\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\046\000\045\000\000\000\000\000\000\000\000\000\
    \000\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\036\000\255\255\042\000\000\000\023\000\
    \041\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\037\000\000\000\043\000\000\000\000\000\
    \037\000\000\000\044\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\035\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\037\000\000\000\000\000\
    \000\000\000\000\000\000\037\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\037\000\
    \000\000\000\000\000\000\037\000\000\000\037\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\033\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\040\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\025\000\041\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\255\255\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
    \017\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\018\000\000\000\021\000\000\000\
    \022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\043\000\044\000\255\255\255\255\255\255\255\255\
    \255\255\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\032\000\034\000\038\000\255\255\023\000\
    \038\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\035\000\255\255\038\000\255\255\255\255\
    \035\000\255\255\038\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\032\000\034\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\035\000\255\255\255\255\
    \255\255\255\255\255\255\035\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\035\000\
    \255\255\255\255\255\255\035\000\255\255\035\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\032\000\034\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\038\000\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 29 "lexer.mll"
            (
    (*incr lineNum;
    linePos := *)
    token lexbuf
  )
# 178 "lexer.ml"

  | 1 ->
# 34 "lexer.mll"
               (token lexbuf)
# 183 "lexer.ml"

  | 2 ->
let
# 35 "lexer.mll"
                  id
# 189 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 36 "lexer.mll"
      (
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
      )
# 213 "lexer.ml"

  | 3 ->
# 57 "lexer.mll"
         ( comment 0 lexbuf; token lexbuf )
# 218 "lexer.ml"

  | 4 ->
# 58 "lexer.mll"
         ( ASSIGN )
# 223 "lexer.ml"

  | 5 ->
# 59 "lexer.mll"
        ( OR )
# 228 "lexer.ml"

  | 6 ->
# 60 "lexer.mll"
        ( AND )
# 233 "lexer.ml"

  | 7 ->
# 61 "lexer.mll"
         ( GE )
# 238 "lexer.ml"

  | 8 ->
# 62 "lexer.mll"
        ( GT)
# 243 "lexer.ml"

  | 9 ->
# 63 "lexer.mll"
         ( LE )
# 248 "lexer.ml"

  | 10 ->
# 64 "lexer.mll"
        ( LT )
# 253 "lexer.ml"

  | 11 ->
# 65 "lexer.mll"
         ( NEQ )
# 258 "lexer.ml"

  | 12 ->
# 66 "lexer.mll"
        ( EQ )
# 263 "lexer.ml"

  | 13 ->
# 67 "lexer.mll"
        ( DIVIDE )
# 268 "lexer.ml"

  | 14 ->
# 68 "lexer.mll"
        ( TIMES )
# 273 "lexer.ml"

  | 15 ->
# 69 "lexer.mll"
        ( MINUS )
# 278 "lexer.ml"

  | 16 ->
# 70 "lexer.mll"
        ( PLUS )
# 283 "lexer.ml"

  | 17 ->
# 71 "lexer.mll"
        ( DOT )
# 288 "lexer.ml"

  | 18 ->
# 72 "lexer.mll"
        ( RBRACE )
# 293 "lexer.ml"

  | 19 ->
# 73 "lexer.mll"
        ( LBRACE )
# 298 "lexer.ml"

  | 20 ->
# 74 "lexer.mll"
        ( RPAREN )
# 303 "lexer.ml"

  | 21 ->
# 75 "lexer.mll"
        ( LPAREN )
# 308 "lexer.ml"

  | 22 ->
# 76 "lexer.mll"
        ( LBRACK )
# 313 "lexer.ml"

  | 23 ->
# 77 "lexer.mll"
        ( RBRACK )
# 318 "lexer.ml"

  | 24 ->
# 78 "lexer.mll"
        ( SEMICOLON )
# 323 "lexer.ml"

  | 25 ->
# 79 "lexer.mll"
        ( COLON )
# 328 "lexer.ml"

  | 26 ->
# 80 "lexer.mll"
        ( COMMA )
# 333 "lexer.ml"

  | 27 ->
# 81 "lexer.mll"
         ( let s = string lexbuf in STRING(s) )
# 338 "lexer.ml"

  | 28 ->
# 83 "lexer.mll"
      (
	INT(int_of_string (Lexing.lexeme lexbuf))
      )
# 345 "lexer.ml"

  | 29 ->
# 86 "lexer.mll"
        ( Printf.printf "EOF\n"; EOF )
# 350 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and string lexbuf =
    __ocaml_lex_string_rec lexbuf 32
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 90 "lexer.mll"
      (
	let s = Buffer.contents string_buffer in
	Buffer.clear string_buffer;
	s
      )
# 365 "lexer.ml"

  | 1 ->
# 96 "lexer.mll"
      ( 
	Buffer.add_char string_buffer (escape (Lexing.lexeme_char lexbuf 1));
        string lexbuf 
      )
# 373 "lexer.ml"

  | 2 ->
# 101 "lexer.mll"
      ( 
	Buffer.add_string string_buffer (Lexing.lexeme lexbuf);
        string lexbuf 
      )
# 381 "lexer.ml"

  | 3 ->
# 105 "lexer.mll"
        (
    failwith
        (Printf.sprintf
           "unterminated string near characters %d-%d"
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf))
  )
# 392 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and comment level lexbuf =
    __ocaml_lex_comment_rec level lexbuf 38
and __ocaml_lex_comment_rec level lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 115 "lexer.mll"
      (
	comment (level + 1) lexbuf
      )
# 405 "lexer.ml"

  | 1 ->
# 119 "lexer.mll"
      (
	if level = 0 then () else comment (level - 1) lexbuf
      )
# 412 "lexer.ml"

  | 2 ->
# 122 "lexer.mll"
            ( comment level lexbuf )
# 417 "lexer.ml"

  | 3 ->
# 123 "lexer.mll"
        ( failwith
        (Printf.sprintf
           "unterminated comment near characters %d-%d"
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) )
# 426 "lexer.ml"

  | 4 ->
# 128 "lexer.mll"
      ( comment level lexbuf )
# 431 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec level lexbuf __ocaml_lex_state

;;

