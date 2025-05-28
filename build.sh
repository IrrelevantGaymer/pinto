set -e
./src/_build.sh
ghc-9.10.2 -fforce-recomp -o pinto build/*.o