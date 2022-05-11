FUTHARK_SRC=alchemy.fut game.fut step.fut world.fut
FUTHARK_BACKEND?=multicore

all: diving-beet

game.py: $(FUTHARK_SRC) lib
	futhark pyopencl --library game.fut

game.c: *.fut lib
	futhark $(FUTHARK_BACKEND) --library game.fut

lib: futhark.pkg
	futhark pkg sync

clean:
	rm -rf *.pyc game.py lib diving-beet *.c

diving-beet: game.c diving-beet.go game.go
	go build -tags $(FUTHARK_BACKEND)
