echo "building Parser/Rule"
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Rule/Rule.hs-boot
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Rule/BasicRule.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Rule/UQRule.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Rule/BuiltInRule.hs
ghc-9.10.2 -Wall -fforce-recomp -c -O -odir ./build -hidir ./build src/Parser/Rule/Rule.hs