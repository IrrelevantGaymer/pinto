echo "building parser"
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/parser/ast.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/parser/parser_error.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/parser/parser.hs