CC = g++
RANLIB = ranlib

LIBSRC = Monitor.cpp
LIBOBJ=$(LIBSRC:.cpp=.o)

CFLAGS = -Wall -g -O0 -std=c++0x -D__x86__ --verbose
LOADLIBS = -L./

RTEMLLIB = librteml.a
TARGETS = $(RTEMLLIB)

all: $(TARGETS)

rteml.o: RTEML_monitor.cpp
	$(CC) $(CFLAGS) $(LOADLIBS) -c RTEML_monitor.cpp -o rteml.o

$(TARGETS): rteml.o
	ar rcs $(RTEMLLIB) rteml.o
	ranlib $(RTEMLLIB)

clean:
	rm rteml.o $(TARGETS) $(RTEMLLIB) $(LIBOBJ)