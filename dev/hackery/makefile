all: avx avx.S normal normal.S

avx: avx.cc
	g++ -march=native -O3 -o avx avx.cc

avx.S: avx.cc
	g++ -march=native -O3 -S -o avx.S avx.cc

normal: normal.cc
	g++ -march=native -O3 -o normal normal.cc

normal.S: normal.cc
	g++ -march=native -O3 -S -o normal.S normal.cc
