
echo "building Parser"
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Direction.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/ParserError.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Pattern.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Sets.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Tape.hs
./src/Parser/Rule/_build.sh
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/AST.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Parser.hs