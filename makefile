makeinterpreter: Grammar.y Tokens.x
	alex Tokens.x
	happy Grammar.y
	ghci Grammar.hs
