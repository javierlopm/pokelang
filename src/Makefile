target:  Pokelang
nolex:   PokelangNoLex 
noparse: PokelangNoParser
grammar: Grammar.hs

alldirs = -ilexer -itokens -iparser -isymtable -itypes -imonads -ierrors -iinstructions

opts    = -O3 

Tokens.o: 
	ghc  tokens/Tokens.hs


# Lexer
Lexer.hs: Tokens.o
	alex --latin1 lexer/Lexer.x -o lexer/Lexer.hs

# Parser
Rwsgrammar.o: Tokens.o Types.o SymTable.o ErrorHandle.o
	ghc monads/GrammarMonad.hs   -itokens -isymtable -itypes -ierrors -iinstructions

Grammar.hs: Tokens.o 
	happy parser/Grammar.y -o parser/Grammar.hs  --info=grammarinfo.md


# Symbol Tables
SymTable.o: 
	ghc symtable/TableTree.hs -iinstructions

# Types definition
Types.o: SymTable.o Tokens.o
	ghc types/Types.hs -isymtable -itokens -iinstructions

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
	-rm -f pokelang  
	-rm -f *.o *.hi
	-rm -f tokens/*.o tokens/*.hi
	-rm -f lexer/*.o lexer/*.hi lexer/Lexer.hs
	-rm -f parser/*.o parser/*.hi parser/Grammar.hs
	-rm -f symtable/*.o symtable/*.hi
	-rm -f monads/*.o monads/*.hi
	-rm -f errors/*.o errors/*.hi
	-rm -f types/*.o types/*.hi
	-rm -f instructions/*.o instructions/*.hi
