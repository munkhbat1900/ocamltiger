open Print
open Format
open Syntax
open Symbol

let pp_oper fmtr = pp_variant(function
  | PlusOp -> "PlusOp", [];
  | MinusOp -> "MinusOp", [];
  | TimesOp -> "TimesOp", []; 
  | DivideOp -> "DivideOp", []; 
  | EqOp -> "EqOp", []; 
  | NeqOp -> "NeqOp", []; 
  | LtOp -> "LtOp", []; 
  | LeOp -> "LeOp", []; 
  | GtOp -> "GtOp", []; 
  | GeOp -> "GeOp", [];
) fmtr

let pp_recTy fmtr = pp_variant (function
  | (sym1, boolRef, sym2, position) ->
    "", [pp_poly pp_string (Symbol.name sym1);
	 pp_poly pp_string (Symbol.name sym2);
	];
) fmtr

let rec pp_var fmtr = pp_variant( function
  | SimpleVar(sym, position) ->
    "SimpleVar", [pp_poly pp_string (Symbol.name sym)];
  | FieldVar(var, sym, position) ->
    "FieldVar", [pp_poly pp_var var;
		 pp_poly pp_string (Symbol.name sym);
		];
  | SubscriptVar(var, exp, pos) ->
    "SubscriptVar", [pp_poly pp_var var;
		     pp_poly pp_exp exp;
		    ];
) fmtr

and pp_exp fmtr = pp_variant(
  function
    | VarExp(var) ->
      "VarExp", [pp_poly pp_var var];
    | NilExp ->
      "NilExp", [];
    | IntExp(intExp) ->
      "IntExp", [pp_poly pp_int intExp];
    | StringExp(stringExp, position) ->
      "StringExp", [pp_poly pp_string stringExp];
    | CallExp(sym, expList, position) ->
      "CallExp", [pp_poly pp_string (Symbol.name sym);
		  pp_poly (pp_list pp_exp) expList;
		 ];
    | OpExp(exp1, oper, exp2, position) ->
      "OpExp", [pp_poly pp_exp exp1;
		pp_poly pp_oper oper;
		pp_poly pp_exp exp2;
	       ];
    | RecordExp(recFieldList, sym, position) ->
      "RecordExp", [pp_poly (pp_list pp_recField) recFieldList;
		    pp_poly pp_string (Symbol.name sym);
		   ];
    | SeqExp(seqExp) -> 
      "SeqExp", [pp_poly (pp_list pp_seq) seqExp];
    | AssignExp(var, exp, position) ->
      "AssignExp", [pp_poly pp_var var;
		    pp_poly pp_exp exp;
		   ];
    | IfExp(expTest, expThen, expElse, position) ->
      "IfExp", [pp_poly pp_exp expTest;
		pp_poly pp_exp expThen;
		pp_poly (pp_option pp_exp) expElse;
	       ];
    | WhileExp(expTest, expBody, position) -> 
      "WhileExp", [pp_poly pp_exp expTest;
		   pp_poly pp_exp expBody;
		  ];
    | ForExp(sym, boolRef, exp1, exp2, exp3, position) ->
      "ForExp", [pp_poly pp_string (Symbol.name sym);
		 pp_poly pp_exp exp1;
		 pp_poly pp_exp exp2;
		 pp_poly pp_exp exp3;
		];
    | BreakExp(position) ->
      "BreakExp", [];
    | LetExp(decList, exp, position) ->
      "LetExp", [pp_poly (pp_list pp_dec) decList;
		 pp_poly pp_exp exp;
		];
    | ArrayExp(sym, exp1, exp2, position) ->
      "ArrayExp", [pp_poly pp_string (Symbol.name sym);
		   pp_poly pp_exp exp1;
		   pp_poly pp_exp exp2;
		  ];
) fmtr

and pp_recField fmtr = pp_variant( function
  | (sym, exp, pos) ->
    "", [pp_poly pp_string (Symbol.name sym);
	 pp_poly pp_exp exp;
	];
) fmtr

and pp_seq fmtr = pp_variant( function
  | (exp, position) ->
    "", [pp_poly pp_exp exp];
) fmtr

and pp_field fmtr = pp_variant (function 
  | (sym1, boolRef, sym2, position) ->
    "", [pp_poly pp_string (Symbol.name sym1);
	 pp_poly pp_string (Symbol.name sym2);
	];
) fmtr

and pp_typOption fmtr = pp_variant (function
  | Some (sym, position) -> "", [pp_poly pp_string (Symbol.name sym)];
  | None -> "None", [];
) fmtr

and pp_funDec fmtr = pp_variant (function 
  | (sym, fieldList, typOption, expBody, position) ->
    "", [pp_poly pp_string (Symbol.name sym);
	 pp_poly (pp_list pp_field) fieldList;
	 pp_poly pp_typOption typOption;
	 pp_poly pp_exp expBody;
	];
) fmtr

and pp_ty fmtr = pp_variant (function
  | NameTy(sym, position) ->
    "NameTy", [pp_poly pp_string (Symbol.name sym)];
  | RecordTy(recTyList) ->
    "RecordTy", [pp_poly (pp_list pp_recTy) recTyList];
  | ArrayTy(sym, pos) ->
    "ArrayTy", [pp_poly pp_string (Symbol.name sym)];
)fmtr

and pp_typeDec fmtr = pp_variant (function 
  | (sym, ty1, position) ->
    "", [pp_poly pp_string (Symbol.name sym);
	 pp_poly pp_ty ty1;
	];
) fmtr

and pp_dec fmtr = pp_variant ( function
  | FunctionDec(fundecList) ->
    "FunctionDec", [pp_poly (pp_list pp_funDec) fundecList];
  | VarDec(sym, boolRef, typOption, exp, position) ->
    "VarDec", [pp_poly pp_string (Symbol.name sym);
	       pp_poly pp_typOption typOption;
	       pp_poly pp_exp exp;
	      ];
  | TypeDec(typList) ->
    "TypeDec", [pp_poly (pp_list pp_typeDec) typList];
) fmtr



