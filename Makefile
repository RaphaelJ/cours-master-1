EXEC = resolution
FLAGS  = -Wall -O2
PARSEC_FLAGS = -ilibs/ -XExistentialQuantification -XPolymorphicComponents -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XDeriveDataTypeable -XCPP

SRC=$(wildcard *.c)
OBJ=$(SRC:.c=.o)

all: $(EXEC)	

$(EXEC): Resolution.hs
	ghc $(FLAGS) $(PARSEC_FLAGS) Resolution.hs -o $(EXEC)

clean:
	rm -fv *.o $(EXEC)
