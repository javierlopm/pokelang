target: Pokelang


Tokens.o: 
	ghc  tokens/Tokens.hs

# Lexer
Lexer.hs: Tokens.o
	alex --latin1 lexer/Lexer.x -o lexer/Lexer.hs

Lexer.o: Lexer.hs Tokens.o
	ghc lexer/Lexer.hs -itokens

# Parser

Grammar.hs: Tokens.o
	happy parser/Grammar.y -o parser/Grammar.hs --info=grammarinfo.md

Grammar.o: Grammar.hs
	ghc parser/Grammar.hs -itokens

Pokelang: Lexer.o Tokens.o Grammar.o
	ghc pokelang.hs -ilexer -itokens -iparser


clean:
	rm pokelang ; rm *.o *.hi
	cd tokens   ; rm -f *.o *.hi
	cd lexer    ; rm -f *.o *.hi Lexer.hs
	cd parser   ; rm -f *.o *.hi Grammar.hs
