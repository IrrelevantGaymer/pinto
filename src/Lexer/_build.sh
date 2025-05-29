echo "building Lexer"
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Lexer/Tokens.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Lexer/Lexer.hs