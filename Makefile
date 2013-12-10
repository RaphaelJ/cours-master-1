EXEC = resolution
FLAGS  = -Wall -O2
LIBS_FLAGS = -ilibs/ -Ilibs/include -cpp -XExistentialQuantification -XPolymorphicComponents -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XDeriveDataTypeable -XFunctionalDependencies -XCPP libs/cbits/cbits.c

SRC=$(wildcard *.c)
OBJ=$(SRC:.c=.o)

all: $(EXEC)	

$(EXEC): Resolution.hs
	ghc $(FLAGS) $(LIBS_FLAGS) Resolution.hs -o $(EXEC)

clean:
	rm -fv *.o $(EXEC)
