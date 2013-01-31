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
F_COMP=gfortran44
#F_COMP=ifort
LOADER=gfortran44
#LOADER=ifort -shared-intel
ARCHIVE=ar rs
F_DEBUGFLAGS=-g -DPC_LINUX1
F_OPTFLAGS=-O3 -DPC_LINUX1

#LIBS=-L/share/apps/netcdf/netcdf-intel-4.1.3/lib -lnetcdff
LIBS=-L/share/apps/netcdf/netcdf-gnu-4.1.3/lib -lnetcdff
#INCLUDES  = -I/share/apps/netcdf/netcdf-intel-4.1.3/include
INCLUDES  = -I/share/apps/netcdf/netcdf-gnu-4.1.3/include

include ./rules.mk

# Define archive and executable names.

BASE=./bin/VICCONVERT
EXE=$(BASE)
ARC=$(BASE).a


include ./objects.mk
      
# Define targets.

all: $(EXE)

$(EXE):$(ARC) $(MAIN_SRC) FORCE
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


FORCE:

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

F_COMMAND = $(F_COMP) -c $(F_OPTFLAGS) $(INCLUDES)
VICConvert = /home/frans004/VICCONVERT
