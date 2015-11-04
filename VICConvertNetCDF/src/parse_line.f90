subroutine parse_line (str,nch,tokens,ntok)

!############################# Change Log ####################################
! 6.0.0.1																    ##
!																		    ##
! 150507 (WF) Wietse Franssen											    ##
!       The subroutine rv_parse_soil cuts al the different values      ##
!       of an line of the soil library file and put the values in an        ##
!       array called 'tokens'.                                              ##
!                                                                           ##
!       This subroutine is used by the rv_make_VIC_soilfile subroutine ##
!                                                                           ##
!       The subroutine is derived from an original RAMS subroutine with the ##
!       Following comment:                                                  ## 
!       'This routine "parses" character string STR into different pieces   ##
!        or tokens by looking for one of four possible token separators     ##
!        (TOKS) STR contains NCH characters.                                ##
!        The number of tokens identified is NTO                             ##     
!        the character string tokens are stored in TOKENS.'                 ##
!                                                                           ##
!#############################################################################

implicit none

integer :: nch,ntok
character(len=*) :: str, tokens(*)

integer, parameter :: nsep=5
character(len=1) :: toksep(nsep)
!data toksep/' ','9'Z,'!','=',' '/
!!data toksep/';','9'Z,' ','!','='/ !WF
integer :: nc,ns,npt

toksep(1)= ' '
toksep(2)= CHAR(9)  ! CHAR(9) means the TAB key
toksep(3)= '!'
toksep(4)= ';'
toksep(5)= '='

ntok=0
npt=1
do 10 nc=1,nch
  do 5 ns=1,nsep
    if(str(nc:nc).eq.toksep(ns))then
      if(nc-npt.ge.1)then
        ntok=ntok+1
        tokens(ntok)=str(npt:nc-1)
      endif
!     ntok=ntok+1  !
!      tokens(ntok)=str(nc:nc)  !
      npt=nc+1
      goto 10
    endif
  5 continue
10 continue

return
end
