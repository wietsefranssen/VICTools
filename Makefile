#Makefile
############################## Change Log ##################################
#
############################################################################

# Include definitions (if this does not work, try using gnu make or copy the
# contents of the file include.mk here).

# root directory.
ROOT=.

# Source directories.
MODEL=$(ROOT)/src

# Define make (gnu make works best).
MAKE=/usr/bin/make
CMACH=PC_LINUX1
F_COMP=gfortran
LOADER=gfortran
ARCHIVE=ar rs
F_DEBUGFLAGS=-g 
F_OPTFLAGS=-O3 -DPC_LINUX1

LIBS     := -L$(shell echo $(NETCDF_LIB)) -lnetcdff
INCLUDES := -I$(shell echo $(NETCDF_INCL))

include ./rules.mk

# Define archive and executable names.
BASEPATH=./bin
BASE=$(BASEPATH)/VICCONVERT
EXE=$(BASE).exe
ARC=$(BASE).a

F_COMMAND = $(F_COMP) -c $(F_OPTFLAGS) $(INCLUDES)

include ./objects.mk
  

# Define targets.

all: mkdirs $(EXE)

$(EXE): $(ARC) $(MAIN_SRC) FORCE

	@echo ""
	$(LOADER) -o $(EXE) VICCONVERT.o $(LOADER_OPTS) $(ARC) \
	$(LIBS)
	rm -f $(MAIN_SRC:.f90=.f) *.o
	@echo ""
	@echo Finished building === $(EXE)
	@echo ""

$(MAIN_SRC): FORCE
	@echo ""
	$(F_COMMAND) $@

$(ARC): $(OBJ)

debug: F_COMMAND = $(F_COMP) -c $(F_DEBUGFLAGS) $(INCLUDES)
debug: all

FORCE:

mkdirs:
	mkdir -p $(BASEPATH)

check: FORCE
	@echo ""
	check

install:
	@echo ""
	ln -fs `pwd`/$(EXE) ../run/$(BASE)
	ln -fs `pwd`/$(EXE) ../test/$(BASE)
	@echo ""

clean:
	@echo ""
	rm -f $(ARC) $(EXE) *.o *.mod *.f *.stb
	@echo ""

	
