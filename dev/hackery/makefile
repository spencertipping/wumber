all: avx avx.S

avx: avx.cc
	g++ -march=native -O3 -o avx avx.cc

avx.S: avx.cc
	g++ -march=native -O3 -S -o avx.S avx.cc
