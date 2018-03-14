makeinterpreter: Evaluator clean

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

Evaluator : Tokens.hs Grammar.hs Helpers.hs Evaluator.hs
	ghc --make -o myinterpreter Evaluator

clean:
	rm -f Grammar.hs Tokens.hs *.o *.hi
