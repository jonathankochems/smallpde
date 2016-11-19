all: main fdm.png fdm_hs.png

main: include/*hpp src/*cpp
	#g++ -Wall -fopenmp -march=native -std=c++11 -O3 -Iinclude -g -fno-omit-frame-pointer src/*cpp -ffast-math -o main
	#g++ -Wall -march=native -std=c++11 -O3 -Iinclude -g -fno-omit-frame-pointer src/*cpp -ffast-math -o main
	~/.llvm/bin/clang++ -Wall -march=native -std=c++11 -O3 -Iinclude -I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/usr/include -g -fno-omit-frame-pointer src/*cpp -ffast-math -o main

DATA:=$(shell mktemp)
fdm.png: main
	$(shell ./main > $(DATA))
	$(shell python -c 'import numpy as np; from matplotlib import pyplot; aa = np.loadtxt("$(DATA)"); pyplot.imsave("fdm.png", aa)')

fdm_hs.png: haskell/test.exe
	$(shell haskell/test.exe > $(DATA))
	$(shell python -c 'import numpy as np; from matplotlib import pyplot; aa = np.loadtxt("$(DATA)"); pyplot.imsave("fdm_hs.png", aa)')

fdm_vector.png: haskell/test-vector-prof.exe
	$(shell haskell/test-vector-prof.exe > $(DATA))
	$(shell python -c 'import numpy as np; from matplotlib import pyplot; aa = np.loadtxt("$(DATA)"); pyplot.imsave("fdm_vector.png", aa)')

fdm_vector1.png: 
	$(shell dist/build/fdm-haskell/fdm-haskell > $(DATA))
	$(shell python -c 'import numpy as np; from matplotlib import pyplot; aa = np.loadtxt("$(DATA)"); pyplot.imsave("fdm_vector1.png", aa)')
# syntax-hack:'


haskell/test.exe:
	cd haskell && ghc --make -isrc -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.3-packages.conf.d src/Main.hs -Odph -rtsopts -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -optc-march=native -optc-ffast-math -optc-g -fforce-recomp -o test.exe
	cd haskell && ghc --make -isrc -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.3-packages.conf.d src/Main.hs -Odph -rtsopts -threaded -eventlog -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -optc-march=native -optc-ffast-math -fforce-recomp -o test.event.exe
	cd haskell && ghc --make -isrc -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.3-packages.conf.d src/Main.hs -Odph -rtsopts -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -optc-march=native -optc-ffast-math -fforce-recomp -prof -osuf prof -o test_prof
	#-threaded -eventlog 

$(warning PERF_PROFILING: $(PERF_PROFILING) $(origin,PERF_PROFILING))
ifeq (YES,$(PERF_PROFILING))
SIMD_OPTS=
ifeq (YES,$(SIMD_PROFILING))
SIMD_OPTS=-fllvm
endif
PROFILING_OPTS= -g -debug -rtsopts -optc-fno-omit-frame-pointer
GHC_OPT_OPTS= -optc-march=native -Odph -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -optlo-O3 -optc-ffast-math -optc-g $(SIMD_OPTS)
else
ifeq (YES,$(GHC_PROFILING))
PROFILING_OPTS= -rtsopts -prof -osuf prof 
GHC_OPT_OPTS= -optc-march=native -Odph -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -optc-ffast-math
else
ifeq (YES,$(DUMP_ASSEMBLY))
PROFILING_OPTS= -rtsopts -optc-S -keep-tmp-files -tmpdir/tmp/ghc
GHC_OPT_OPTS= -optc-march=native -Odph -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -optc-ffast-math 
$(shell rm -rf /tmp/ghc/*)
else
PROFILING_OPTS= -rtsopts 
GHC_OPT_OPTS= -optc-march=native -Odph -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -optc-ffast-math
endif
endif
endif

FORCE_OPTS=
ifeq (YES,$(FORCE_COMPILE))
FORCE_OPTS= -fforce-recomp
endif

TH_OPTS=
ifeq (YES,$(DUMP_TH))
TH_OPTS= -ddump-splices
endif


GHC_OPTS= $(GHC_OPT_OPTS) $(PROFILING_OPTS) $(FORCE_OPTS) $(TH_OPTS) -fsimpl-tick-factor=400
#-fsimpl-tick-factor=200

haskell/test-vector-prof.exe:
	cd haskell && ghc --make -isrc -package-db=.cabal-sandbox/x86_64-linux-ghc-7.10.3-packages.conf.d bench/Main.hs $(GHC_OPTS)  -o test-vector-prof.exe

haskell/tests:
	cd haskell && PATH=.cabal-sandbox/bin:$$PATH ghc --make -isrc -itest -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.3-packages.conf.d test/Spec.hs $(GHC_OPTS) && test/Spec



.PHONY: haskell/test-vector-prof.assembly var tests
var:
	@echo $($(v))

haskell/test-vector-prof.assembly:
	cd haskell && ghc-core -- --make -isrc -package-db=.cabal-sandbox/x86_64-linux-ghc-7.10.3-packages.conf.d $(GHC_OPTS) src/Main.hs  


clean:
	rm main fdm.png
