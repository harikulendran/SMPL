makeinterpreter: Evaluator clean

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

Evaluator : Tokens.hs Grammar.hs Helpers.hs Evaluator.hs
	ghc --make -o myinterpreter Evaluator

clean:
	rm -f  *.o *.hi

# Make file structure from example at: https://github.com/ghulette/happy-example/blob/master/Makefile
