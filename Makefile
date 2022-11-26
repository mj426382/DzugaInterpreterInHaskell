GHC        = ghc

.PHONY : all clean distclean

all : compiler

# compiler.hs AbsGrammar.hs LexGrammar.x ParGrammar.y PrintGrammar.hs TestGrammar.hs Types.hs StaticTypeChecker.hs: grammar.cf
# 	bnfc --haskell grammar.cf

compiler : Compiler.hs AbsGrammar.hs LexGrammar.hs ParGrammar.hs PrintGrammar.hs TestGrammar.hs FrontendChecker.hs
	${GHC} Compiler.hs

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsGrammar.hs AbsGrammar.hs.bak ComposOp.hs ComposOp.hs.bak DocGrammar.txt DocGrammar.txt.bak ErrM.hs ErrM.hs.bak LayoutGrammar.hs LayoutGrammar.hs.bak LexGrammar.x LexGrammar.x.bak ParGrammar.y ParGrammar.y.bak PrintGrammar.hs PrintGrammar.hs.bak SkelGrammar.hs SkelGrammar.hs.bak TestGrammar.hs TestGrammar.hs.bak XMLGrammar.hs XMLGrammar.hs.bak ASTGrammar.agda ASTGrammar.agda.bak ParserGrammar.agda ParserGrammar.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak grammar.dtd grammar.dtd.bak TestGrammar LexGrammar.hs ParGrammar.hs ParGrammar.info ParDataGrammar.hs Makefile
