stack build
cp ./.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/DarkstarAH/DarkstarAH ./dist/bin/DarkstarAH
tar czfv DarkstarAH.keter dist/bin/DarkstarAH config/keter.yml
mv DarkstarAH.keter /opt/keter/incoming/DarkstarAH.keter
