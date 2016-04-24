target: Pokelang


Tokens.o: FORCE
	ghc tokens/Tokens.hs
FORCE:

# Lexer
Lexer.hs:
	alex lexer/Lexer.x -o lexer/Lexer.hs

Lexer.o: Lexer.hs
	ghc lexer/Lexer.hs -itokens

Pokelang: Lexer.o
	ghc pokelang.hs -ilexer -itokens

	
clean:
	rm pokelang
	cd tokens ; rm -f *.o *.hi
	cd lexer  ; rm -f *.o *.hi Lexer.hs