echo "building Parser"
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/AST.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/ParserError.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Parser.hs