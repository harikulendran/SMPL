makeinterpreter: Grammar.y Tokens.x Evaluator.hs
	alex Tokens.x
	happy Grammar.y
	ghci Evaluator.hs
