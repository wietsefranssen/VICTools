/* functions */
void 
fileReadError (FILE *fp) 
{
  if (feof (fp))
  {
    printf ("Premature end of file.\n");
  }else {
    printf ("File read error.\n");
  }
  
  fclose (fp);
  exit (1);
}

int 
aveIntArray (int * array, int first, int last) 
{
  /*  
   * 
   * 
   */
  int i = 0;
  long int result = 0;
  if ( (last - first) < 1) { printf ("error in function 'aveIntArray' last value (%d) is lower that first value (%d)...\n", last,first); exit(EXIT_FAILURE); }
  for (i = first; i < last; i++) { result += array[i]; }
  result /= (last - first);
  return result;
}

short unsigned int 
aveSUIntArray (short unsigned int * array, int first, int last) 
{
  /*  
   * 
   * 
   */
  int i = 0;
  int result = 0;
  if ( (last - first) < 1) { printf ("error in function 'aveIntArray' last value (%d) is lower that first value (%d)...\n", last,first); exit(EXIT_FAILURE); }
  for (i = first; i < last; i++) { result += array[i]; }
  result /= (last - first);
  return result;
}

short signed int 
aveSSIntArray (short signed int * array, int first, int last) 
{
  /*  
   * 
   * 
   */
  int i = 0;
  int result = 0;
  if ( (last - first) < 1) { printf ("error in function 'aveIntArray' last value (%d) is lower that first value (%d)...\n", last,first); exit(EXIT_FAILURE); }
  for (i = first; i < last; i++) { result += array[i]; }
  result /= (last - first);
  return result;
}

float 
aveFloatArray (float * array, int first, int last)  
{
  /*  
   * 
   * 
   */
  int i = 0;
  float result = 0;
  if ( (last - first) < 1) { printf("error in function 'aveIntArray' last value (%d) is lower that first value (%d)...\n", last,first); exit(EXIT_FAILURE); }
  for (i = first; i < last; i++) { result += array[i]; }
  result /= (last - first);
  return result;
}

int 
sumIntArray (int * array, int first, int last) 
{
  /*  
   * 
   * 
   */
  int i;
  long int result = 0;
  if ((last - first) < 1) { printf("error in function 'aveIntArray' last value (%d) is lower that first value (%d)...\n", last,first); exit(EXIT_FAILURE); }
  for (i = first; i < last; i++) { result += array[i]; }
  return result;
}

short unsigned int 
sumSUIntArray (short unsigned int * array, int first, int last) 
{
  /*  
   * 
   * 
   */
  int i;
  long int result = 0;
  if ((last - first) < 1) { printf("error in function 'aveIntArray' last value (%d) is lower that first value (%d)...\n", last,first); exit(EXIT_FAILURE); }
  for (i = first; i < last; i++) { result += array[i]; }
  return result;
}

short signed int 
sumSSIntArray (short signed int * array, int first, int last) 
{
  /*  
   * 
   * 
   */
  int i;
  long int result = 0;
  if ((last - first) < 1) { printf("error in function 'aveIntArray' last value (%d) is lower that first value (%d)...\n", last,first); exit(EXIT_FAILURE); }
  for (i = first; i < last; i++) { result += array[i]; }
  return result;
}

float 
sumFloatArray (float * array, int first, int last) 
{
  /*  
   * 
   * 
   */
  int i;
  long int result = 0;
  if ( (last - first) < 1) { printf("error in function 'aveIntArray' last value (%d) is lower that first value (%d)...\n", last,first); exit(EXIT_FAILURE); }
  for (i = first; i < last; i++) { result += array[i]; }
  return result;
}

int 
daysInMonth (int year, int month) 
{
  /* example: daysInMonth(1964, 2));
   * gives: 29 (days) 
   */
  int nDays;
  int nDaysMonths[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
  month = month-1;
  nDays = nDaysMonths[month];
  if (month == 1) {
    if (year%400 ==0 || (year%100 != 0 && year%4 == 0 )) 
      { 
	nDays = nDaysMonths[month]+1;
      }
  }
  return nDays;
}

/* calculate nr of months based on:
 * -nr of days
 * -start year
 * -start month 
 * rounded down 
 * 
 * Uses the function: daysInMonth
 * example1: nCalcNrMonths(365, 1961, 1));
 * result: 12 (months)
 * example2: nCalcNrMonths(365, 1964, 1));
 * result: 11 (months)
 */
int 
nCalcNrMonths (int nDay, int startyear, int startmonth) 
{
  int iDay;
  int iYear, nMonth, iMonth;

  nDay++;
  iMonth = startmonth;
  iYear = startyear;
  iDay = 0;
  nMonth = -1;
  while (iDay < nDay) 
    {
      iDay = iDay + daysInMonth(iYear, iMonth);
      iMonth++;
      nMonth++; if (iMonth>12) { iMonth = 1; iYear++; };
    }
  return nMonth;
} 

