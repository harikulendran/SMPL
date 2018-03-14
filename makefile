makeinterpreter: Evaluator

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

Evaluator : Tokens.hs Grammar.hs Helpers.hs Evaluator.hs
	ghc --make -o myinterpeter Evaluator

clean:
	rm -f *.o *.hi
