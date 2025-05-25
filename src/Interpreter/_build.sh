echo "building Interpreter"
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Interpreter/Interpreter.hs