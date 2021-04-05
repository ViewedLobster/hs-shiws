HSCSRC=EPoll.hsc
SRC=$(HSCSRC:.hsc=.hs)

.PHONY: clean

.SUFFIXES: .hsc .hs

.hsc.hs:
	hsc2hs -o $@ $< 

all: $(SRC)
	ghc Main.hs

clean:
	rm -rf *.o *.hi Main

