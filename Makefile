all: main fdm.png

main: include/*hpp src/*cpp
	g++ -Wall -fopenmp -march=native -O3 -Iinclude -g -fno-omit-frame-pointer src/*cpp -ffast-math -o main

DATA:=$(shell mktemp)
fdm.png: main
	$(shell ./main > $(DATA))
	$(shell python -c 'import numpy as np; from matplotlib import pyplot; aa = np.loadtxt("$(DATA)"); pyplot.imsave("fdm.png", aa)')

clean:
	rm main fdm.png
