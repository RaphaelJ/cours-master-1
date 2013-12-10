EXEC = resolution
FLAGS  = -Wall -O2
LIBS_FLAGS = -ilibs/ -cpp -XExistentialQuantification -XPolymorphicComponents -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts -XDeriveDataTypeable -XFunctionalDependencies -XCPP -fno-warn-unused-imports -fno-warn-unused-do-bind

SRC=$(wildcard *.c)
OBJ=$(SRC:.c=.o)

all: $(EXEC)	

$(EXEC): Resolution.hs
	ghc $(FLAGS) $(LIBS_FLAGS) Resolution.hs -o $(EXEC)

clean:
	rm -fv *.o $(EXEC)
