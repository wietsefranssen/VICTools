#Makefile objects.mk

# Define main source.

MAIN_SRC = $(MODEL)/VICCONVERT.f90


# Define objects.

OBJ = $(ARC)($(MODEL)/parse_line.o) \
      $(ARC)($(MODEL)/definitionsAndTypes.o) \
      $(ARC)($(MODEL)/VIC2netcdf.o) \
      $(ARC)($(MODEL)/VIC2bignetcdf.o) \
      $(ARC)($(MODEL)/netcdf2VIC.o)
