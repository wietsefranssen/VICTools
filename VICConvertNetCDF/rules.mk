#Makefile rules include 
############################## Change Log ##################################
# 2.0.0
#
############################################################################


.SUFFIXES:
.SUFFIXES: .F95 .f95 .F90 .f90 .f .c .o .a

# f95 rule.

.f95.a:
	@echo ""
	$(F_COMMAND) $<
	$(ARCHIVE) $@ $(<F:.f95=.o)
	rm -f $(<F:.f95=.o)
	
# F95 rule.

.F95.a:
	@echo ""
	$(F_COMMAND) $<
	$(ARCHIVE) $@ $(<F:.F95=.o)
	rm -f $(<F:.F95=.o)
	
# f90 rule.

.f90.a:
	@echo ""
	$(F_COMMAND) $<
	$(ARCHIVE) $@ $(<F:.f90=.o)
	rm -f $(<F:.f90=.o)

# F90 rule.

.F90.a:
	@echo ""
	$(F_COMMAND) -WF,-D$(CMACH) $<
	$(ARCHIVE) $@ $(<F:.F90=.o)
	rm -f $(<F:.F90=.o)

# c rule.

.c.a:
	@echo ""
	$(C_COMMAND) $<
	$(ARCHIVE) $@ $(<F:.c=.o)
	rm -f $(<F:.c=.o)
