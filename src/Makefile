target: Pokelang


Tokens.o: 
	ghc  tokens/Tokens.hs

# Lexer
#Lexer.hs: Tokens.o
#	alex --latin1 lexer/Lexer.x -o lexer/Lexer.hs

# Lexer.o: Tokens.o  Lexer.hs
#	ghc lexer/Lexer.hs -itokens

# Parser

Grammar.hs: Tokens.o
	happy parser/Grammar.y -o parser/Grammar.hs --info=grammarinfo.md

Grammar.o: Grammar.hs
	ghc parser/Grammar.hs -itokens -isymtable

# Symbol Tables
SymTable.o: 
	ghc symtable/TableTree.hs 

Pokelang:  Tokens.o Grammar.o SymTable.o # Lexer.o
	ghc pokelang.hs -ilexer -itokens -iparser -isymtable


clean:
	rm pokelang ; rm *.o *.hi
	cd tokens   ; rm -f *.o *.hi
	cd lexer    ; rm -f *.o *.hi Lexer.hs
	cd parser   ; rm -f *.o *.hi Grammar.hs
