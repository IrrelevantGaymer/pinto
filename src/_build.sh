echo "building src"

ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/FullResult.hs

./src/Lexer/_build.sh
./src/Parser/_build.sh
./src/Interpreter/_build.sh

ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Main.hs