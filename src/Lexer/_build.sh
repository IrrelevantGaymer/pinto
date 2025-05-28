echo "building Lexer"
# For some weird reason, when combining these commands using *.hs, we get a compiler error
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Lexer/Tokens.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Lexer/Lexer.hs