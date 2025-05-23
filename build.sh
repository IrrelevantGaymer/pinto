set -e
./src/_build.sh
ghc-9.10.2 -fforce-recomp -o pinto build/Main.o \
    build/Lexer.o build/Tokens.o `# Lexer` \
    build/AST.o build/ParserError.o build/Parser.o `# Parser`