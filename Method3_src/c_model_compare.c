/** ----------------------------------------------
  * c_model_compare.c
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Feb. 1, 2022
  * Last date of modification: Feb 18, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: Compare every state from two bmi model objects.
  * 		 return 1 on the first non-matching state. Return 0 when
  * 		 all states match.
  */
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>   // to get filesize
#include <msgpack.h>
#include <msgpack/fbuffer.h>

#include "serializer.h"
#include "c_serializer.h"
#include "iso_c_bmif_2_0.h"
#include "ut_trim.h"

int c_model_compare(void* box_handle1, void* box_handle2 )
{
    char* temp = (char*)NULL;
    char* temp2 = (char*)NULL;
    int* inttemp = (int*)NULL;
    int* inttemp2 = (int*)NULL;
    int8_t* int1temp = (int8_t*)NULL;
    int8_t* int1temp2 = (int8_t*)NULL;
    short* shorttemp = (short*)NULL;
    short* shorttemp2 = (short*)NULL;
    long* longtemp = (long*)NULL;
    long* longtemp2 = (long*)NULL;
    float* floattemp = (float*)NULL;
    float* floattemp2 = (float*)NULL;
    double* doubletemp = (double*)NULL;
    double* doubletemp2 = (double*)NULL;
    bool* booltemp = (bool*)NULL;
    bool* booltemp2 = (bool*)NULL;

    int status, var_count, var_length, var_length2;
  
    char name[2048];
    char type[2048];
    char ctype[2048];
    char role[2048]="all";

    char** names = NULL;
    char** cnames = NULL;

    status = get_var_count(box_handle1, role, &var_count);
//    printf("In c_model_compare: role=%s, count=%d\n", role, var_count);

    names = malloc( sizeof(char*));
    cnames = malloc( var_count * sizeof(char*));
    /*Now the get_var_names copies the names to the given array of strings
     * The following is not needed.
     */
//    cnames[0] = malloc( 2048 * var_count * sizeof(char) );
    for ( int i = 0; i < var_count; ++i )
    {
       cnames[i] = malloc( 2048  * sizeof(char) );
    }

    status = get_var_names(box_handle1, role, cnames);
//    strcpy(cnames[0], names[0]);
    //compare each model state
    for ( int i = 0; i < var_count; ++i )
    {
    /*Now the get_var_names copies the names to the given array of strings
     * The following is not needed.
     */
      //such that we can access the variable name in C fashion, i.e., 
      //cnames[i].
//      cnames[0][2048 *(i+1) -1 ] = '\0';
//      cnames[i]  = &cnames[0][i * 2048];
//      ut_trim( cnames[i] );
//      printf( "names[ %d ] = %s \n", i, cnames[i] );

      status = get_var_length(box_handle1, cnames[i], &var_length);
      status = get_var_length(box_handle1, cnames[i], &var_length2);

      if ( var_length != var_length2 )
      {
          printf( "variable: %s length is not equal! ", cnames[i] );
          printf( "  model 1 length = %d, model 2 length = %d \n", var_length,
	                       var_length2 );
          return(1);
      }

      status = get_var_type(box_handle1, cnames[i], type);
      if ( strcmp(type, "integer4" ) == 0 )
      {
          inttemp  = (int*)malloc( var_length * sizeof(int) );
          inttemp2  = (int*)malloc( var_length * sizeof(int) );
	  status = get_value_int(box_handle1, cnames[i], (int*)inttemp );
	  status = get_value_int(box_handle2, cnames[i], (int*)inttemp2 );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( inttemp[ j ] != inttemp2[ j ] )
	      {
                  printf( "variable: %s[%d] is not equal! ", cnames[i], j );
                  printf( "  model 1 = %d, model 2 = %d \n", inttemp[j],
				  inttemp2[j] );
	          free(inttemp);
	          free(inttemp2);
                  return(1);
	      }
	  }
	  free(inttemp);
	  free(inttemp2);
      }
      else if ( strcmp(type, "integer1" ) == 0 )
      {
          int1temp  = (int8_t*)malloc( var_length * sizeof(int8_t) );
          int1temp2  = (int8_t*)malloc( var_length * sizeof(int8_t) );
	  status = get_value_int1(box_handle1, cnames[i], int1temp );
	  status = get_value_int1(box_handle2, cnames[i], int1temp2 );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( int1temp[ j ] != int1temp2[ j ] )
	      {
                  printf( "variable: %s[%d] is not equal! ", cnames[i], j );
                  printf( "  model 1 = %d, model 2 = %d \n", int1temp[j],
				  int1temp2[j] );
	          free(int1temp);
	          free(int1temp2);
                  return(1);
	      }
	  }
	  free(int1temp);
	  free(int1temp2);
      }
      else if ( strcmp(type, "integer2" ) == 0 )
      {
          shorttemp  = (short*)malloc( var_length * sizeof(short) );
          shorttemp2  = (short*)malloc( var_length * sizeof(short) );
	  status = get_value_int2(box_handle1, cnames[i], shorttemp );
	  status = get_value_int2(box_handle2, cnames[i], shorttemp2 );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( shorttemp[ j ] != shorttemp2[ j ] )
	      {
                  printf( "variable: %s[%d] is not equal! ", cnames[i], j );
                  printf( "  model 1 = %d, model 2 = %d \n", shorttemp[j],
				  shorttemp2[j] );
	          free(shorttemp);
	          free(shorttemp2);
                  return(1);
	      }
	  }
	  free(shorttemp);
	  free(shorttemp2);
      }
      else if ( strcmp(type, "integer8" ) == 0 )
      {
          longtemp  = (long*)malloc( var_length * sizeof(long) );
          longtemp2  = (long*)malloc( var_length * sizeof(long) );
	  status = get_value_int8(box_handle1, cnames[i], longtemp );
	  status = get_value_int8(box_handle2, cnames[i], longtemp2 );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( longtemp[ j ] != longtemp2[ j ] )
	      {
                  printf( "variable: %s[%d] is not equal! ", cnames[i], j );
                  printf( "  model 1 = %d, model 2 = %d \n", longtemp[j],
				  longtemp2[j] );
	          free(longtemp);
	          free(longtemp2);
                  return(1);
	      }
	  }
	  free(longtemp);
	  free(longtemp2);
      }
      else if ( strcmp(type, "real4" ) == 0 )
      {
          floattemp  = (float*)malloc( var_length * sizeof(float) );
          floattemp2  = (float*)malloc( var_length * sizeof(float) );
	  status = get_value_float(box_handle1, cnames[i], floattemp );
	  status = get_value_float(box_handle2, cnames[i], floattemp2 );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( floattemp[ j ] != floattemp2[ j ] )
	      {
                  printf( "variable: %s[%d] is not equal! ", cnames[i], j );
                  printf( "  model 1 = %f, model 2 = %f \n", floattemp[j],
				  floattemp2[j] );
	          free(floattemp);
	          free(floattemp2);
                  return(1);
	      }
	  }
	  free(floattemp);
	  free(floattemp2);
      }
      else if ( strcmp(type, "real8" ) == 0 )
      {
          doubletemp  = (double*)malloc( var_length * sizeof(double) );
          doubletemp2  = (double*)malloc( var_length * sizeof(double) );
	  status = get_value_double(box_handle1, cnames[i], doubletemp );
	  status = get_value_double(box_handle2, cnames[i], doubletemp2 );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( doubletemp[ j ] != doubletemp2[ j ] )
	      {
                  printf( "variable: %s[%d] is not equal! ", cnames[i], j );
                  printf( "  model 1 = %f, model 2 = %f \n", doubletemp[j],
				  doubletemp2[j] );
	          free(doubletemp);
	          free(doubletemp2);
                  return(1);
	      }
	  }
	  free(doubletemp);
	  free(doubletemp2);
      }
      else if ( strcmp(type, "logical" ) == 0 )
      {
          booltemp  = (bool*)malloc( var_length * sizeof(bool) );
          booltemp2  = (bool*)malloc( var_length * sizeof(bool) );
	  status = get_value_logical(box_handle1, cnames[i], booltemp );
	  status = get_value_logical(box_handle2, cnames[i], booltemp2 );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( booltemp[ j ] != booltemp2[ j ] )
	      {
                  printf( "variable: %s[%d] is not equal! ", cnames[i], j );
                  printf( "  model 1 = %d, model 2 = %d \n", booltemp[j],
				  booltemp2[j] );
	          free(booltemp);
	          free(booltemp2);
                  return(1);
	      }
	  }
	  free(booltemp);
	  free(booltemp2);
      }
      else if ( strcmp(type, "character" ) == 0 )
      {
          temp  = (char*)malloc( var_length * sizeof(char) );
          temp2  = (char*)malloc( var_length * sizeof(char) );
	  status = get_value_string(box_handle1, cnames[i], temp );
	  status = get_value_string(box_handle2, cnames[i], temp2 );
          if ( strncmp( temp, temp2, var_length ) != 0 )
	  {
                  printf( "variable: %s is not equal! ", cnames[i] );
                  printf( "  model 1 = %s, model 2 = %s \n", temp, temp2 );
	          free(temp);
	          free(temp2);
                  return(1);
	  }
	  free(temp);
	  free(temp2);
      }
      else
      {
            printf("  WARNING: Unknown type = %s\n", type );
	    return(1);
      }
    }


    for ( int i = 0; i < var_count; ++i )
    {
       free(cnames[i]);
    }
    //free(cnames[0]);
    free(cnames);
    free(names);

    return(0);
}
