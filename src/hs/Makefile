HSCSRC=		EPoll.hsc Socket.hsc
FROMHSC=	$(HSCSRC:.hsc=.hs)
SRC=		$(FROMHSC)
OBJS=		$(SRC:.hs=.o)

BIN=		Main

.PHONY: clean objs

.SUFFIXES: .hsc .hs .o

all: $(SRC)
	ghc -threaded $(SRC) Main

.hsc.hs:
	hsc2hs -o $@ $< 

.hs.o:
	ghc $<

objs: $(OBJS)
	@echo "built objects"

clean:
	rm -rf *.o *.hi Main $(FROMHSC)

