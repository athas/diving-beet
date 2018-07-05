FUTHARK_SRC=alchemy.fut game.fut step.fut world.fut

all: game.py

run: game.py
	python diving-beet.py

game.py: $(FUTHARK_SRC)
	futhark-pyopencl --library game.fut

game.c: game.fut
	futhark-opencl --library game.fut

_game.so: game.c
	build_futhark_ffi game

clean:
	rm -f *.pyc game.py
