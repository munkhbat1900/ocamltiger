(*#load"all.cma"*)
(*open ParsingUtil*)
open Lexing
open JsAST
open Print
open Format

let pp_jsbinop fmtr = pp_variant(function
  | JSMul ->
      "JSMul", [];
  | JSDiv ->
      "JSDiv", [];
  | JSMod ->
      "JSMod", [];
  | JSPlus ->
      "JSPlus", [];
  | JSMinus ->
      "JSMinus", [];
  | JSLsh ->
      "JSLsh", [];
  | JSRsh ->
      "JSRsh", [];
  | JSUrsh ->
      "JSUrsh", [];
  | JSLt ->
      "JSLt", [];
  | JSGt ->
      "JSGt", [];
  | JSLe ->
      "JSLe", [];
  | JSGe ->
      "JSGe", [];
  | JSInstanceof ->
      "JSInstanceof", [];
  | JSIn ->
      "JSIn", [];
  | JSEq ->
      "JSEq", [];
  | JSNe ->
      "JSNe", [];
  | JSStrictEq ->
      "JSStrictEq", [];
  | JSStrictNe ->
      "JSStrictNe", [];
  | JSBitwiseAnd ->
      "JSBitwiseAnd", [];
  | JSBitwiseXor ->
      "JSBitwiseXor", [];
  | JSBitwiseOr ->(** Bitwise expression: [&], [^], [|] *)
      "JSBitwiseOr", [];
  | JSAnd ->
      "JSAnd", [];
  | JSOr ->
      "JSOr", [];
				    ) fmtr

let rec pp_jsunop fmtr = pp_variant(function
  | JSDelete ->
      "JSDelete", []
  | JSVoid ->
      "JSVoid", []
  | JSTypeof ->
      "JSTypeof", []
  | JSIncrement -> 
      "JSIncrement", []
  | JSDecrement ->
      "JSDecrement", []
  | JSUPlus ->
      "JSUMinus", []
  | JSUMinus ->
      "JSUminus", []
  | JSBitwiseNot ->
      "JSBitwiseNot", []
  | JSNot ->
      "JSNot", []
				   ) fmtr
let rec pp_jspost fmtr = pp_variant(function
  | JSPostIncrement -> 
      "JSPostIncrement", []
  | JSPostDecrement ->
      "JSPostDecrement", []
				   ) fmtr

let rec pp_jsassign fmtr = pp_variant(function
  | JSSimpleAssign ->
      "JSSimpleAssign", []
  | JSMulAssign ->
      "JSMulAssign", []
  | JSDivAssign ->
      "JSDivAssign", []
  | JSModAssign ->
      "JSModAssign", []
  | JSPlusAssign ->
      "JSPlusAssign", []
  | JSMinusAssign ->
      "JSMinusAssign", []
  | JSLshAssign ->
      "JSLshAssign", []
  | JSRshAssign ->
      "JSRshAssign", []
  | JSUrshAssign->
      "JSUrshAssign", []
  | JSBitwiseAndAssign-> 
      "JSBitwiseAndAssign", []
  | JSBitwiseXorAssign ->
      "JSBitwiseXorAssign", []
  | JSBitwiseOrAssign->
      "JSBitwiseOrAssign", []
				     ) fmtr

let rec pp_jsaccess fmtr = pp_variant(function
  | JSGet->
      "JSGet", []
  | JSSet->
      "JSSet", []
				     ) fmtr

let rec pp_jskind fmtr = pp_variant(function
  | JSVar->
      "JSVar", []
  | JSConst->
      "JSConst", []
				   ) fmtr

let rec pp_ast fmtr = pp_variant (function
  | JSTrue->
      "JSTrue", []
  | JSFalse->
      "JSFalse", []
  | JSNull ->
      "JSNull", []
  | JSNew -> 
      "JSNew", []
  | JSSemiColon ->
      "JSSemiColon", []
  | JSArguments(nodelist_list, annot) ->
      "JSArguments", [pp_poly (pp_list (pp_list pp_ast)) nodelist_list];
  | JSArrayLiteral(nodelist, annot) ->
      "JSArrayLiteral", [pp_poly (pp_list pp_ast) nodelist];
  | JSBlock(node, annot) ->
      "JSBlock", [pp_poly pp_ast node];
  | JSBreak(node_option, annot) ->
      "JSBreak complete this sentense", []
  | JSCallExpression(str, nodelist, annot) ->
      "JSCallExpression", [pp_poly pp_string str;
			   pp_poly (pp_list pp_ast) nodelist
			 ];
  | JSCase(node1, node2, annot) ->
      "JSCase", [pp_poly pp_ast node1;
		 pp_poly pp_ast node2
	       ];
  | JSCatch(node1, nodelist, node2, annot) ->
      "JSCatch", [pp_poly pp_ast node1;
		  pp_poly (pp_list pp_ast) nodelist;
		  pp_poly pp_ast node2;
		];

  | JSContinue(node_option, annot) ->
      "JSContinue complete this",[];

  | JSDefault(node, annot) ->
      "JSDefault", [pp_poly pp_ast node];
  | JSDoWhile(node1, node2, annot) ->
      "JSDoWhile",[pp_poly pp_ast node1;
		   pp_poly pp_ast node2;
		 ];
  | JSElision(nodelist, annot) ->
      "JSElision", [pp_poly (pp_list pp_ast) nodelist];
  | JSExpressionParen(node, annot) ->
      "JSExpressionParen", [pp_poly pp_ast node];
  | JSExpressionTernary(nodelist1, nodelist2, nodelist3, annot) ->
      "JSExpressionTernary", [pp_poly (pp_list pp_ast) nodelist1;
			      pp_poly (pp_list pp_ast) nodelist2;
			      pp_poly (pp_list pp_ast) nodelist3;
			    ];
  | JSFinally(node1, annot) ->
      "JSFinally", [pp_poly pp_ast node1];
  | JSForIn(nodelist, node1, node2, annot) ->
      "JSForIn",[pp_poly (pp_list pp_ast) nodelist;
		 pp_poly pp_ast node1;
		 pp_poly pp_ast node2];
  | JSForVar(nodelist1, nodelist2, nodelist3,node, annot) ->
      "JSForVar", [pp_poly (pp_list pp_ast) nodelist1;
		     pp_poly (pp_list pp_ast) nodelist2;
		     pp_poly (pp_list pp_ast) nodelist3;
		     pp_poly pp_ast node
		   ];
  |JSForVarIn(node1, node2, node3, annot) ->
      "JSForVarIn", [ pp_poly pp_ast node1;
		      pp_poly pp_ast node2;
		      pp_poly pp_ast node3;
		     ];
  | JSFunctionExpression(nodelist1, nodelist2, node, annot) ->
      "JSFunctionExpression", [pp_poly (pp_list pp_ast) nodelist1;
			       pp_poly (pp_list pp_ast) nodelist2;
			       pp_poly pp_ast node
			     ];
  | JSHexInteger(str, annot) ->
      "JSHexInteger", [pp_poly pp_string str];
  | JSIfElse(node1, node2, node3, annot) ->
      "JSIfElse", [pp_poly pp_ast node1;
		   pp_poly pp_ast node2;
		   pp_poly pp_ast node3;
		 ];
  | JSLabelled(node1, node2, annot) ->
      "JSLabelled", [pp_poly pp_ast node1;
		     pp_poly pp_ast node2;
		   ];
  | JSMemberSquare(nodelist, node, annot) ->
      "JSMemberSquare", [pp_poly (pp_list pp_ast) nodelist;
			 pp_poly pp_ast node
		       ];
  | JSObjectLiteral(nodelist, annot) ->
      "JSObjectLiteral", [pp_poly (pp_list pp_ast) nodelist];
  | JSPropertyNameandValue(node, nodelist, annot) ->
      "JSPropertyNameandValue", [pp_poly pp_ast node;
				 pp_poly (pp_list pp_ast) nodelist
			       ];
  | JSPropertyAccessor(jsaccess, node1, nodelist, node2, annot) ->
      "JSPropertyAccessor", [pp_poly pp_jsaccess jsaccess;
			     pp_poly pp_ast node1;
			     pp_poly (pp_list pp_ast) nodelist;
			     pp_poly pp_ast node2
			   ];
  | JSRegEx(jsregex, annot) ->
      "JSRegEx complete this", [];
  | JSReturn(node_option, annot) ->
      "JSReturn complete this", []
  | JSStringLiteral(str, annot) ->
      "JSStringLiteral", [pp_poly pp_string str];
  | JSSwitch(node, nodelist, annot) ->
      "JSSwitch", [pp_poly pp_ast node;
		   pp_poly (pp_list pp_ast) nodelist
		 ];
  | JSThrow(node, annot) ->
      "JSThrow", [pp_poly pp_ast node];
  | JSTry(node, nodelist, annot) ->
      "JSTry", [pp_poly pp_ast node;
		pp_poly (pp_list pp_ast) nodelist
	      ];
  | JSUnary(unop, annot) -> 
      "JSUnary", [pp_poly pp_jsunop unop];
  | JSVarDecl(node, nodelist, annot) ->
      "JSVarDecl", [pp_poly pp_ast node;
		    pp_poly (pp_list pp_ast) nodelist
		  ];
  | JSVariables(jskind, nodelist, annot) ->
      "JSVariables", [pp_poly pp_jskind jskind;
		      pp_poly (pp_list pp_ast) nodelist
		    ]
  | JSWhile(node1, node2, annot) ->
      "JSWhile", [pp_poly pp_ast node1;
		  pp_poly pp_ast node2;
		];
  | JSWith(node1, node2, annot) ->
      "JSWith", [pp_poly pp_ast node1;
		 pp_poly pp_ast node2;
	       ];
    
    
(*ここから下は実際にテストプログラムでみてみた。*)
  | JSSourceElementsTop(nodelist, annot) ->
      "JSSourceElementsTop", [pp_poly (pp_list pp_ast) nodelist];
  | JSFunction(jsnode, nodelist, jsfunc_body, annot) ->
     "JSFunction", [pp_poly pp_ast jsnode;
                    pp_poly (pp_list pp_ast) nodelist;
                    pp_poly pp_ast jsfunc_body];
    
  | JSIdentifier(id, annot) ->
      "JSIdentifier", [pp_poly pp_string id];
   
  | JSFunctionBody(nodelist, annot) ->
      "JSFunctionBody", [pp_poly (pp_list pp_ast) nodelist];
  | JSSourceElements(nodelist, annot) ->
      "JSSourceElements", [pp_poly (pp_list pp_ast) nodelist];
  | JSExpression(nodelist, annot) ->
      "JSExpression", [pp_poly (pp_list pp_ast) nodelist];
  | JSMemberDot(nodelist, node, annot) ->
      "JSMemberDot", [pp_poly (pp_list pp_ast) nodelist;
		      pp_poly pp_ast node
		    ];
  | JSOperator(jsassign, annot) ->
      "JSOperator", [pp_poly pp_jsassign jsassign];
  
  
  | JSLiteral(str, annot) ->
      "JSLiteral", [pp_poly pp_string str];
  
  
  | JSDecimal(str, annot) ->
      "JSDecimal", [pp_poly pp_string str]
  | JSFor(nodelist1, nodelist2, nodelist3, node, annot) ->
      "JSFor", [pp_poly (pp_list pp_ast) nodelist1;
		pp_poly (pp_list pp_ast) nodelist2;
		pp_poly (pp_list pp_ast) nodelist3;
		pp_poly pp_ast node
	      ]
  | JSStatementBlock(node, annot) ->
      "JSStatementBlock", [pp_poly pp_ast node]
  | JSStatementList(list, annot) ->
      "JSStatementList", [pp_poly (pp_list pp_ast) list]
  | JSExpressionBinary(binop, nodelist1, nodelist2, annot) ->
      "JSExpressionBinary", [pp_poly pp_jsbinop binop;
			     pp_poly (pp_list pp_ast) nodelist1;
			     pp_poly (pp_list pp_ast) nodelist2
			   ]
  | JSIf(node1, node2, annot) ->
      "JSIf", [pp_poly pp_ast node1;
	       pp_poly pp_ast node2
	     ]
  | JSExpressionPostfix(jspost, nodelist, annot) ->
      "JSExpressionPostfix", [pp_poly pp_jspost jspost;
			      pp_poly (pp_list pp_ast) nodelist
			    ];
) fmtr

