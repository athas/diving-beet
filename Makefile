FUTHARK_SRC=alchemy.fut game.fut step.fut world.fut

all: game.py

run: game.py
	./diving-beet.py

game.py: $(FUTHARK_SRC)
	futhark-pyopencl --library game.fut

clean:
	rm -f *.pyc game.py
