target:  Pokelang
nolex:   PokelangNoLex # Without lex recompilation
noparse: PokelangNoParser
grammar: Grammar.hs

alldirs = -ilexer -itokens -iparser -isymtable -itypes -imonads -ierrors

Tokens.o: 
	ghc  tokens/Tokens.hs


# Lexer
Lexer.hs: Tokens.o
	alex --latin1 lexer/Lexer.x -o lexer/Lexer.hs

# Parser
Rwsgrammar.o: Tokens.o Types.o SymTable.o
	ghc monads/GrammarMonad.hs -itokens -isymtable -itypes

Grammar.hs: Tokens.o 
	happy parser/Grammar.y -o parser/Grammar.hs  --info=grammarinfo.md


# Symbol Tables
SymTable.o: 
	ghc symtable/TableTree.hs

# Types definition
Types.o: SymTable.o Tokens.o
	ghc types/Types.hs -isymtable -itokens

# Errors
ErrorHandle.o: Tokens.o
	ghc errors/ErrorHandle.hs -itokens


# Main
Pokelang: Grammar.hs SymTable.o Types.o Lexer.hs Rwsgrammar.o ErrorHandle.o
	ghc pokelang.hs $(alldirs)

PokelangNoLex: Grammar.hs SymTable.o Types.o Rwsgrammar.o ErrorHandle.o
	ghc pokelang.hs $(alldirs)

PokelangNoParser: SymTable.o Types.o Rwsgrammar.o ErrorHandle.o
	ghc pokelang.hs $(alldirs)


clean:
	rm pokelang ; rm *.o *.hi
	cd tokens   ; rm -f *.o *.hi
	cd lexer    ; rm -f *.o *.hi Lexer.hs
	cd parser   ; rm -f *.o *.hi Grammar.hs
	cd symtable ; rm -f *.o *.hi
	cd monads   ; rm -f *.o *.hi
