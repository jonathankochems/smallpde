all: main fdm.png fdm_hs.png

main: include/*hpp src/*cpp
	#g++ -Wall -fopenmp -march=native -std=c++11 -O3 -Iinclude -g -fno-omit-frame-pointer src/*cpp -ffast-math -o main
	g++ -Wall -march=native -std=c++11 -O3 -Iinclude -g -fno-omit-frame-pointer src/*cpp -ffast-math -o main

DATA:=$(shell mktemp)
fdm.png: main
	$(shell ./main > $(DATA))
	$(shell python -c 'import numpy as np; from matplotlib import pyplot; aa = np.loadtxt("$(DATA)"); pyplot.imsave("fdm.png", aa)')

fdm_hs.png: haskell/test.exe
	$(shell haskell/test.exe > $(DATA))
	$(shell python -c 'import numpy as np; from matplotlib import pyplot; aa = np.loadtxt("$(DATA)"); pyplot.imsave("fdm_hs.png", aa)')

haskell/test.exe:
	cd haskell && ghc --make -isrc -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.3-packages.conf.d src/Main.hs -Odph -rtsopts -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -optc-march=native -optc-ffast-math -optc-g -fforce-recomp -o test.exe
	cd haskell && ghc --make -isrc -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.3-packages.conf.d src/Main.hs -Odph -rtsopts -threaded -eventlog -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -optc-march=native -optc-ffast-math -fforce-recomp -o test.event.exe
	cd haskell && ghc --make -isrc -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.3-packages.conf.d src/Main.hs -Odph -rtsopts -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -optc-march=native -optc-ffast-math -fforce-recomp -prof -osuf prof -o test_prof
	#-threaded -eventlog 


clean:
	rm main fdm.png
