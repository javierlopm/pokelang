target: Pokelang
nolex: PokelangNoLex # Without lex recompilation

alldirs = -ilexer -itokens -iparser -isymtable -itypes

Tokens.o: 
	ghc  tokens/Tokens.hs

# Lexer
Lexer.hs: Tokens.o
	alex --latin1 lexer/Lexer.x -o lexer/Lexer.hs

# Parser
Grammar.hs: Tokens.o 
	happy parser/Grammar.y -o parser/Grammar.hs # --info=grammarinfo.md


# Symbol Tables
SymTable.o: 
	ghc symtable/TableTree.hs

# Types definition
Types.o: SymTable.o Tokens.o
	ghc types/Types.hs -isymtable -itokens

# Main
Pokelang: Grammar.hs SymTable.o Types.o Lexer.hs
	ghc pokelang.hs $(alldirs)

PokelangNoLex: Grammar.hs SymTable.o Types.o
	ghc pokelang.hs $(alldirs)

clean:
	rm pokelang ; rm *.o *.hi
	cd tokens   ; rm -f *.o *.hi
	cd lexer    ; rm -f *.o *.hi Lexer.hs
	cd parser   ; rm -f *.o *.hi Grammar.hs
	cd symtable ; rm -f *.o *.hi 
