makeinterpreter: Evaluator clean

Tokens.hs : Tokens.x
	@printf "\nPlease ensure Alex is installed\n"
	alex Tokens.x

Grammar.hs : Grammar.y
	@printf "\nPlease ensure Happy is installed\n"
	happy Grammar.y

Evaluator : Tokens.hs Grammar.hs Helpers.hs Evaluator.hs
	@printf "\nPlease ensure ghc, and the List.Split and Text packages are installed\n"
	ghc --make -o smpl Evaluator

clean:
	@printf "\nPerfoming cleanup:\n"
	rm -f  *.o *.hi

# Make file structure from example at: https://github.com/ghulette/happy-example/blob/master/Makefile
