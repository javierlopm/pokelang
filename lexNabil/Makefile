all:	Lanscii

Lanscii: Lanscii.hs Lexer.x Parser.y AST.hs ST_functions.hs
	alex Lexer.x
	happy Parser.y
	ghc --make Lanscii
	
clean:
	rm -rf Lex*.hs *.o *.hi Par*.hs Lanscii doc

doc:	Lanscii.hs Parser.hs Lexer.x
	test -d doc || mkdir doc
	rm -rf doc/*
	ghc -cpp -E -optP-P -D__HADDOCK__ Lexer.hs -o Lex.hs
	haddock --html --odir=doc Lanscii.hs Lex.hs
	rm -f Lex.hs
