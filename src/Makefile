target: Pokelang


Tokens.o: 
	ghc  tokens/Tokens.hs

# Lexer
Lexer.hs: Tokens.o
	alex lexer/Lexer.x -o lexer/Lexer.hs

Lexer.o: Lexer.hs Tokens.o
	ghc lexer/Lexer.hs -itokens

Pokelang: Lexer.o Tokens.o
	ghc pokelang.hs -ilexer -itokens

	
clean:
	rm pokelang
	cd tokens ; rm -f *.o *.hi
	cd lexer  ; rm -f *.o *.hi Lexer.hs