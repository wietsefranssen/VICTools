#include <libgen.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
/* My own functions */
#include "functions.h"

void 
printInfo (char *programName) {
  printf ("******************************************* INFO *******************************************\n");
  printf ("Program: %s    version: 1.0 (W. Franssen 2012)\n\n",programName);
  printf ("Usage: %s [INFILE]...[OPTIONS]\n\n",programName);
  printf ("Write an ASCII representation, of binary files \n");
  printf ("of INFILE to standard output.\n\n");
  printf ("Options:\n");
  printf ("     -h         for this information\n");
  printf ("     -i         show extra information about the data\n");
  printf ("     -m [LINES] maximum number of lines to display\n");
  printf ("     -t [TYPES] after -t give a list of types:\n");
  printf ("             i 4-byte integer\n");
  printf ("             f 4-byte float\n");
  printf ("             u 2-byte unsigned integer\n");
  printf ("             s 2-byte signed integer\n");
  printf ("             1 skip 1-byte per line output\n");
  printf ("             2 skip 2-byte per line output\n");
  printf ("             4 skip 4-byte per line output\n");
  printf ("             8 skip 8-byte per line output\n");
  printf ("     -d [YEAR_MONTH_METHOD] convert daily to monthly.\n");
  printf ("                 give per output type the method:\n");
  printf ("             s sum of all days per month\n");
  printf ("             a average of the month\n");
  printf ("             eg: -d 1961_01_ddmmm\n\n");
  printf ("eg: %s file.bin -t iiffu -m 2\n",programName);
  printf ("     this means: Read the file 'file.bin' and plot the output\n ");
  printf ("                 to the standard output\n");
  printf ("     as follows: every row has two integers, then 2 floats and then 1 unsigned integer\n\n");
  printf ("example output could be:\n");
  printf ("                324513     27    0.05600    1.00000   117\n");
  printf ("                343312     27    0.00600    0.02230   118\n");
  printf ("\n");
}

int 
main (int argc, char *argv[])
{
  // file
  long int              fileSize;
  struct stat           file_status;
  char                  fileName[512];
  int                   lineSize;
  int                   nLine, iLine;
  // nr of variables
  short unsigned int    nVar, iVar;
  // list of types
  unsigned char        *aType;
  // data types
  short unsigned int  **data_suint;
  short signed int    **data_ssint;
  int                 **data_int;
  float               **data_float;
  short unsigned int    n_suint, n_ssint, n_int, n_float;
  short unsigned int    n_skip1, n_skip2, n_skip4, n_skip8;
  short unsigned int    i_suint, i_ssint, i_int, i_float;
  short unsigned int    i_skip1, i_skip2, i_skip4, i_skip8;
  char                  dummy1;
  short int             dummy2;
  int                   dummy4;
  double                dummy8;
  int                   l, ch, i, m; /* just counters */  
  short int             extraInfo = 0;
  int                   maxLines = 0;
  /* day 2 month */
  short int             day2month = 0;
  short int             startYear;
  short int             startMonth;
  char                 *token;
  unsigned char        *aD2MMethod;
  short unsigned int  **dataM_suint;
  short signed int    **dataM_ssint;
  int                 **dataM_int;
  float               **dataM_float;
  int                   iMonth, nMonth, iYear;
  short int             iMonthOfYear;
  int                  *aStartDayPerMonth;

  
  /* Define program name */
  char *programName = (char *)basename(argv[0]);

  for( i = 1; i < argc; i++ )
    {           /* Scan through args. */
      switch( (int)argv[i][0] )
	{        /* Check for option character. */
	case '-':
	case '/': ;                  /* Bail out if 1. */
	  l = strlen( argv[i] );
	  for( m = 1; m < l; ++m )
	    { /* Scan through options. */
	      ch = (int)argv[i][m];
	      switch( ch ) 
		{
		case 'h': 
		  printInfo(programName);
		  exit(EXIT_SUCCESS);
		case 'i':
		  //printf("Extra info (-i)\n");
		  extraInfo = 1;
		  break;
		case 't':
		  i++;
		  nVar = strlen( argv[i] );
		  //printf("nVars: %d\n",nVar);
		  if (nVar < 1) { printf("there is nothing after -t...."); exit(EXIT_SUCCESS); }
		  aType=malloc(nVar*sizeof(char));
		  i_suint = 0; i_ssint = 0; i_int = 0; i_float = 0;
		  for( iVar = 0; iVar < nVar; ++iVar )
		    { /* Scan through options. */
		      ch = (int)argv[i][iVar];
		      switch( ch ) {
		      case '1': aType[iVar] = '1'; break;
		      case '2': aType[iVar] = '2'; break;
		      case '4': aType[iVar] = '4'; break;
		      case '8': aType[iVar] = '8'; break;
		      case 'i': aType[iVar] = 'i'; break;
		      case 'f': aType[iVar] = 'f'; break;
		      case 's': aType[iVar] = 's'; break;
		      case 'u': aType[iVar] = 'u'; break;
		      default: printf("Not the right arguments given after -t...\n\n"); printInfo(programName); exit(EXIT_SUCCESS);
		      }
		    }
		  break;
		case 'd':
		  i++;
		  day2month = 1;
		  nVar = strlen( argv[i] ) - 8;
		  if (nVar < 1) { printf("there is nothing after -d...."); exit(EXIT_SUCCESS); }
		  aD2MMethod=malloc(nVar*sizeof(char));
		  token = strtok(argv[i],"_"); token = strtok(NULL, "_"); startMonth=atoi(token);// printf("%d\n",startMonth); 
		  token = strtok(argv[i],"_"); startYear=atoi(token); //printf("%d\n",startYear);
		  for( iVar = 0; iVar < nVar; ++iVar ) { /* Scan through options. */
		    ch = (int)argv[i][iVar+8];
		    switch( ch ) {
		    case 's': aD2MMethod[iVar] = 's'; break;
		    case 'a': aD2MMethod[iVar] = 'a'; break;
		    default: printf("Not the right arguments given after -d...\n\n"); printInfo(programName); exit(EXIT_SUCCESS);
		    }
		  }
		  break;
		case 'm':
		  i++;
		  maxLines = atoi(argv[i]);
		  break;
		}
	    }
	}
    }
  if (argc < 3)
    {
      printf ("Not enough arguments given...\n\nfor help type \"%s -h\"\n\n",programName);
      exit (EXIT_SUCCESS);
    }

  // Define filename
  strcpy (fileName,argv[1]);

  // Count occurences of the types in de commandline
  i_skip1 = 0; i_skip2 = 0; i_skip4 = 0; i_skip8 = 0; 
  i_suint = 0; i_ssint = 0; i_int = 0; i_float = 0;
  for (iVar = 0; iVar < nVar; iVar++)
    {
      if (aType[iVar] == '1') { i_skip1++; }
      if (aType[iVar] == '2') { i_skip2++; }
      if (aType[iVar] == '4') { i_skip4++; }
      if (aType[iVar] == '8') { i_skip8++; }
      if (aType[iVar] == 'u') { i_suint++; }
      if (aType[iVar] == 's') { i_ssint++; }
      if (aType[iVar] == 'i') { i_int++; }
      if (aType[iVar] == 'f') { i_float++; }
    }
  n_skip1 = i_skip1;
  n_skip2 = i_skip2;
  n_skip4 = i_skip4;
  n_skip8 = i_skip8;
  n_int   = i_int;
  n_float = i_float;
  n_suint = i_suint;
  n_ssint = i_ssint;

  /* FileSize */
  int file_exist = stat(fileName, &file_status);
  if (file_exist == -1) { printf("Cannot find the file '%s'...\n",fileName);    exit(EXIT_FAILURE); }
  fileSize = file_status.st_size; 
  lineSize = n_skip1*sizeof(char) + n_skip2*sizeof(short int) + n_skip4*sizeof(int) + n_skip8*sizeof(double); 
  lineSize = lineSize + n_suint*sizeof(short unsigned int) + n_ssint*sizeof(short signed int) + n_int*sizeof(int) + n_float*sizeof(float);
  if (maxLines == 0) { nLine = fileSize / lineSize; }
  else { nLine = maxLines; }  
  // nLine=5;
  
  //print info:
  if (extraInfo == 1)
    {
      printf ("**************************************************\n");
      printf ("inputfile:            %s\n", fileName);
      printf ("linesize:             %d \n", lineSize);
      printf ("Number of bytes:      %ld \n", fileSize);
      printf ("Number of variables:  %d \n", nVar);
      printf ("Number of lines:      %d\n",nLine);
      printf ("Max lines to display: %d\n\n",maxLines);
      printf ("n_skip1: %d\n",n_skip1);
      printf ("n_skip2: %d\n",n_skip2);
      printf ("n_skip4: %d\n",n_skip4);
      printf ("n_skip8: %d\n",n_skip8);
      printf ("n_int:   %d\n",n_int);
      printf ("n_float: %d\n",n_float);
      printf ("n_suint: %d\n",n_suint);
      printf ("n_ssint: %d\n",n_ssint);
      if (day2month == 1) 
	{
	  printf ("\nDay to month option given (-d):\n");
	  printf ("start year:  %d\n", startYear);
	  printf ("start month: %d\n", startMonth);
	}
      printf ("*************************************************\n");
    }
  // Array allocation
  data_suint = malloc((n_suint+1) * sizeof(short unsigned int *));                              // if(data_suint == NULL) { fprintf(stderr, "out of memory\n");}
  for(i = 0; i < n_suint+1; i++) { data_suint[i] = malloc((nLine+1) * sizeof(short unsigned int)); }//if(data_suint[i] == NULL) { fprintf(stderr, "%d  out of memory\n",i);} }
  data_ssint = malloc((n_ssint+1) * sizeof(short signed int *));                                 //if(data_ssint == NULL) { fprintf(stderr, "out of memory\n");}
  for(i = 0; i < n_ssint+1; i++) { data_ssint[i] = malloc((nLine+1) * sizeof(short signed int)); } // if(data_ssint[i] == NULL) { fprintf(stderr, "out of memory\n");}  }
  data_int   = malloc((n_int+1)   * sizeof(int *));                                              //if(data_int == NULL) { fprintf(stderr, "out of memory\n");}
  for(i = 0; i < n_int+1;   i++) { data_int[i]   = malloc((nLine+1) * sizeof(int)); }               //if(data_int[i] == NULL) { fprintf(stderr, "out of memory\n");}  }
  data_float = malloc((n_float+1) * sizeof(float *));                                            //if(data_float == NULL) { fprintf(stderr, "out of memory\n");}
  for(i = 0; i < n_float+1; i++) { data_float[i] = malloc((nLine+1) * sizeof(float)); }  //       if(data_float[i] == NULL) { fprintf(stderr, "out of memory\n");} }
  
  FILE *f=fopen(fileName, "r");
  for (iLine = 0; iLine < nLine;iLine++)
    {
      i_skip1 = 0; i_skip2 = 0; i_skip4 = 0; i_skip8 = 0; 
      i_suint = 0; i_ssint = 0; i_int = 0; i_float = 0;
      for (iVar = 0; iVar < nVar;iVar++)
	{
	  if (aType[iVar] == '1') { if(fread(&dummy1, sizeof(char), 1, f) != 1) { fileReadError(f); } }
	  if (aType[iVar] == '2') { if(fread(&dummy2, sizeof(short int), 1, f) != 1) { fileReadError(f); } }
	  if (aType[iVar] == '4') { if(fread(&dummy4, sizeof(int), 1, f) != 1) { fileReadError(f); } }
	  if (aType[iVar] == '8') { if(fread(&dummy8, sizeof(double), 1, f) != 1) { fileReadError(f); } }
	  if (aType[iVar] == 'u') {
	    if(fread(&data_suint[i_suint][iLine], sizeof(short unsigned int), 1, f) != 1) { fileReadError(f); }
	    i_suint++;
	  }
	  if (aType[iVar] == 's') {
	    if(fread(&data_ssint[i_ssint][iLine], sizeof(short signed int), 1, f) != 1) { fileReadError(f); }
	    i_ssint++;
	  }
	  if (aType[iVar] == 'i') {
	    if(fread(&data_int[i_int][iLine], sizeof(int), 1, f) != 1) { fileReadError(f); }
	    i_int++;
	  }
	  if (aType[iVar] == 'f') {
	    if(fread(&data_float[i_float][iLine], sizeof(float), 1, f) != 1) { fileReadError(f); }
	    i_float++;
	  }
	}
    }
  fclose(f);
  
  /* DAILY */
  if (day2month != 1) 
    {
      for (iLine = 0; iLine < nLine;iLine++)
	{
	  i_suint = 0; i_ssint = 0; i_int = 0; i_float = 0;
	  //    printf("line: %6d, ", iLine+1);
	  for (iVar = 0; iVar < nVar;iVar++)
	    {
	      if (aType[iVar] == 'u') 
		{
		  printf("%5d ", data_suint[i_suint][iLine]);
		  i_suint++;
		}
	      if (aType[iVar] == 's') 
		{
		  printf("%5d ", data_ssint[i_ssint][iLine]);
		  i_ssint++;
		}
	      if (aType[iVar] == 'i') 
		{
		  printf("%6d ", data_int[i_int][iLine]);
		  i_int++;
		}
	      if (aType[iVar] == 'f') 
		{
		  printf("%e ",data_float[i_float][iLine]);
		  i_float++;
		}
	    }
	  printf("\n");
	}
    }

  /* MONTHLY */
  if (day2month == 1) {  
    nMonth = nCalcNrMonths(nLine, startYear, startMonth);
    aStartDayPerMonth = malloc((nMonth+1) * sizeof(int *)); // this is more than needed but prevents an extra loop to invest the amount of months.
    iYear=startYear; iMonthOfYear=1; iMonth=startMonth-1;
    aStartDayPerMonth[0] = 1;
    for (iMonth = 0; iMonth < nMonth;iMonth++)
      {
	aStartDayPerMonth[iMonth+1] = aStartDayPerMonth[iMonth] + daysInMonth(iYear, iMonthOfYear);
	iMonthOfYear = iMonthOfYear + 1; if (iMonthOfYear>12) { iMonthOfYear = 1; iYear++;};
	//  printf("startday: %d    endday: %d     total: %d\n", aStartDayPerMonth[iMonth],aStartDayPerMonth[iMonth+1],aStartDayPerMonth[iMonth+1]-aStartDayPerMonth[iMonth]);
      }  
    
    // Array allocation
    dataM_suint = malloc((n_suint+1) * sizeof(short unsigned int *));                              // if(data_suint == NULL) { fprintf(stderr, "out of memory\n");}
    for(i = 0; i < n_suint+1; i++) { dataM_suint[i] = malloc((nMonth+1) * sizeof(short unsigned int)); }//if(data_suint[i] == NULL) { fprintf(stderr, "%d  out of memory\n",i);} }
    dataM_ssint = malloc((n_ssint+1) * sizeof(short signed int *));                                 //if(data_ssint == NULL) { fprintf(stderr, "out of memory\n");}
    for(i = 0; i < n_ssint+1; i++) { dataM_ssint[i] = malloc((nMonth+1) * sizeof(short signed int)); } // if(data_ssint[i] == NULL) { fprintf(stderr, "out of memory\n");}  }
    dataM_int   = malloc((n_int+1)   * sizeof(int *));                                              //if(data_int == NULL) { fprintf(stderr, "out of memory\n");}
    for(i = 0; i < n_int+1;   i++) { dataM_int[i]   = malloc((nMonth+1) * sizeof(int)); }               //if(data_int[i] == NULL) { fprintf(stderr, "out of memory\n");}  }
    dataM_float = malloc((n_float+1) * sizeof(float *));                                            //if(data_float == NULL) { fprintf(stderr, "out of memory\n");}
    for(i = 0; i < n_float+1; i++) { dataM_float[i] = malloc((nMonth+1) * sizeof(float)); }  //       if(data_float[i] == NULL) { fprintf(stderr, "out of memory\n");} }

    i_suint = 0; i_ssint = 0; i_int = 0; i_float = 0;
    for (iVar = 0; iVar < nVar;iVar++)
      {
	if (aType[iVar] == 'i') 
	  {
	    for (iMonth = 0; iMonth < nMonth;iMonth++) 
	      {
		dataM_int[i_int][iMonth] = 0;
		if (aD2MMethod[iVar] == 'a') { dataM_int[i_int][iMonth] = aveIntArray(data_int[i_int], aStartDayPerMonth[iMonth]-1, aStartDayPerMonth[iMonth+1]-1); }
		if (aD2MMethod[iVar] == 's') { dataM_int[i_int][iMonth] = sumIntArray(data_int[i_int], aStartDayPerMonth[iMonth]-1, aStartDayPerMonth[iMonth+1]-1); }
	      }
	    i_int++;
	  }
	if (aType[iVar] == 'u') 
	  {
	    for (iMonth = 0; iMonth < nMonth;iMonth++) 
	      {
		dataM_suint[i_suint][iMonth] = 0;
		if (aD2MMethod[iVar] == 'a') { dataM_suint[i_suint][iMonth] = aveSUIntArray(data_suint[i_suint], aStartDayPerMonth[iMonth]-1, aStartDayPerMonth[iMonth+1]-1); }
		if (aD2MMethod[iVar] == 's') { dataM_suint[i_suint][iMonth] = sumSUIntArray(data_suint[i_suint], aStartDayPerMonth[iMonth]-1, aStartDayPerMonth[iMonth+1]-1); }
	      }
	    i_suint++;
	  }
	if (aType[iVar] == 's') 
	  {
	    for (iMonth = 0; iMonth < nMonth;iMonth++) 
	      {
		dataM_ssint[i_ssint][iMonth] = 0;
		if (aD2MMethod[iVar] == 'a') { dataM_ssint[i_ssint][iMonth] = aveSSIntArray(data_ssint[i_ssint], aStartDayPerMonth[iMonth]-1, aStartDayPerMonth[iMonth+1]-1); }
		if (aD2MMethod[iVar] == 's') { dataM_ssint[i_ssint][iMonth] = sumSSIntArray(data_ssint[i_ssint], aStartDayPerMonth[iMonth]-1, aStartDayPerMonth[iMonth+1]-1); }
	      }
	    i_ssint++;
	  }
	if (aType[iVar] == 'f') 
	  {
	    for (iMonth = 0; iMonth < nMonth;iMonth++) 
	      {
		dataM_float[i_float][iMonth] = 0;
		if (aD2MMethod[iVar] == 'a') { dataM_float[i_float][iMonth] = aveFloatArray(data_float[i_float], aStartDayPerMonth[iMonth]-1, aStartDayPerMonth[iMonth+1]-1); }
		if (aD2MMethod[iVar] == 's') { dataM_float[i_float][iMonth] = sumFloatArray(data_float[i_float], aStartDayPerMonth[iMonth]-1, aStartDayPerMonth[iMonth+1]-1); }
	      }
	    i_float++;
	  }
      }
 
    //printf("nMonth: %d\n", nMonth);
    for (iMonth = 0; iMonth < nMonth;iMonth++)
      {
	i_suint = 0; i_ssint = 0; i_int = 0; i_float = 0;
	for (iVar = 0; iVar < nVar;iVar++)
	  {
	    if (aType[iVar] == 'u') 
	      {
		printf("%5d ", dataM_suint[i_suint][iMonth]);
		i_suint++;
	      }
	    if (aType[iVar] == 's') 
	      {
		printf("%5d ", dataM_ssint[i_ssint][iMonth]);
		i_ssint++;
	      }
	    if (aType[iVar] == 'i') 
	      {
		printf("%6d ", dataM_int[i_int][iMonth]);
		i_int++;
	      }
	    if (aType[iVar] == 'f') 
	      {
		printf("%e ", dataM_float[i_float][iMonth]);
		i_float++;
	      }
	  }
	printf("\n");
      }  
  }
  
  /* FREE */  
  if (day2month == 1) 
    {
      free(aStartDayPerMonth);
      for(i = 0; i < n_suint+1; i++) { free(dataM_suint[i]); }
      free(dataM_suint);
      for(i = 0; i < n_ssint+1; i++) { free(dataM_ssint[i]); }
      free(dataM_ssint);
      for(i = 0; i < n_int+1; i++) { free(dataM_int[i]); }
      free(dataM_int);
      for(i = 0; i < n_float+1; i++) { free(dataM_float[i]); }
      free(dataM_float);
    }
  free(aType);
  for(i = 0; i < n_suint+1; i++) { free(data_suint[i]); }
  free(data_suint);
  for(i = 0; i < n_ssint+1; i++) { free(data_ssint[i]); }
  free(data_ssint);
  for(i = 0; i < n_int+1; i++) { free(data_int[i]); }
  free(data_int);
  for(i = 0; i < n_float+1; i++) { free(data_float[i]); }
  free(data_float);

  return 0;
}
