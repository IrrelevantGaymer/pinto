set -e
./src/_build.sh
ghc-9.10.2 -fforce-recomp -o pinto build/Lexer/*.o build/Parser/Rule/*.o build/Parser/*.o build/Interpreter/*.o build/*.o