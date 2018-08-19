FUTHARK_SRC=alchemy.fut game.fut step.fut world.fut

all: game.py

run: game.py
	python diving-beet.py

game.py: $(FUTHARK_SRC) lib
	futhark-pyopencl --library game.fut

game.c: game.fut lib
	futhark-opencl --library game.fut

_game.so: game.c
	build_futhark_ffi game

lib: futhark.pkg
	futhark-pkg sync

clean:
	rm -rf *.pyc game.py lib diving-beet *.c

diving-beet: game.c diving-beet.go game.go
	go build
