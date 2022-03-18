#include <ctype.h>
#include <string.h>
#include "ut_trim.h"

void ut_trim(char * str) {
   char * start = str;
   char * end = start + strlen(str);

   while (--end >= start) {   /* trim right */
      if (!isspace(*end))
         break;
   }
   *(++end) = '\0';

   while (isspace(*start))    /* trim left */
      start++;

   if (start != str)          /* there is a string */
      memmove(str, start, end - start + 1);
}
