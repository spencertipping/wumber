all: avx avx.S normal normal.S

avx: avx.cc makefile
	g++ -march=native -mtune=native -O3 -o avx avx.cc

avx.S: avx.cc makefile
	g++ -march=native -mtune=native -O3 -S -o avx.S avx.cc

normal: normal.cc makefile
	g++ -march=native -mtune=native -O3 -o normal normal.cc

normal.S: normal.cc makefile
	g++ -march=native -mtune=native -O3 -S -o normal.S normal.cc
