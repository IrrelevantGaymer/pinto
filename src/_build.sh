echo "building src"
./src/lexer/_build.sh
./src/parser/_build.sh

ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/main.hs