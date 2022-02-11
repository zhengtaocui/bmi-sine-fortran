/** ----------------------------------------------
  * c_serializer.c
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Feb. 1, 2022
  * Last date of modification: Feb 10, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: For a given Fortran BMI model object, 
  *              serialize the model states to a binary file using
  *              the exporsed Fortran BMI procedures by ISO C Binding. 
  *              It requries the msgpack-c library for serialization.
  * 		 
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

int c_serialize_states(void** box_handle, const char* ser_file )
{
    /* declare the pointer variabls for various data types 
     * supported.
     */
    char* temp = (char*)NULL;
    int* inttemp = (int*)NULL;
    short* shorttemp = (short*)NULL;
    long* longtemp = (long*)NULL;
    float* floattemp = (float*)NULL;
    double* doubletemp = (double*)NULL;
    double** doubledoubletemp = (double**)NULL;
    bool* booltemp = (bool*)NULL;

    int status, var_count, var_length, var_grid;
  
    char name[2048];
    char type[2048];
    char ctype[2048];
    char role[2048]="all";

    char** names = NULL;
    char** cnames = NULL;

    FILE *fp = fopen(ser_file, "w+");
    msgpack_packer pk;
    msgpack_packer_init(&pk, fp, msgpack_fbuffer_write);

    /* get the component name
     */
    status = get_component_name(box_handle, name);
//    printf("In c_serialize_states: name=%s\n", name);

    /*
     * get the total number of variables 
     */
    status = get_var_count(box_handle, role, &var_count);
//    printf("In c_serialize_states: role=%s, count=%d\n", role, var_count);

    /*
     * allocate memory to held variable names
     */
    names = malloc( sizeof(char*));
    cnames = malloc( var_count * sizeof(char*));
    cnames[0] = malloc( 2048 * var_count * sizeof(char) );

    /*
     * get variable names in an array
     */
    status = get_var_names(box_handle, role, names);
    /*
     * names points to a memory allocated by the Fortran model,
     * copy it to cnames to manuplate
     */
    strcpy(cnames[0], names[0]);
    /*
     * loop throught all variables
     */
    for ( int i = 0; i < var_count; ++i )
    {
      /*
       * set the null char to terminate the string
       */
      cnames[0][2048 *(i+1) -1 ] = '\0';
      /*
       * each name has a length of 2048 chars
       */
      cnames[i]  = &cnames[0][i * 2048];
      /*
       * remove white spaces
       */
      ut_trim( cnames[i] );
//      printf( "names[ %d ] = %s \n", i, cnames[i] );

      /*
       * get variable length
       */
      status = get_var_length(box_handle, cnames[i], &var_length);
//      printf( "names[ %d ] = %s, length = %d \n", i, cnames[i], var_length );
      /*
       * get data type for a given variable 
       */
      status = get_var_type(box_handle, cnames[i], type);
//      printf( "names[ %d ] = %s, type = %s \n", i, cnames[i], type );
      if ( strcmp(type, "integer4" ) == 0 )
      {
	  /*
	   * pre-allocate space becasue we will use get_value_* functions
	   * the values will be copied into this space.
	   */
          inttemp  = (int*)malloc( var_length * sizeof(int) );
	  status = get_value_int(box_handle, cnames[i], (int*)inttemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
//              printf("       %s[%d] = %d \n", cnames[i], j, inttemp[j] );
              msgpack_pack_int(&pk, inttemp[j]);
	  }
	  free(inttemp);
      }
      else if ( strcmp(type, "integer1" ) == 0 )
      {
	  /*
	   * pre-allocate space becasue we will use get_value_* functions
	   * the values will be copied into this space.
	   */
          temp  = (char*)malloc( var_length * sizeof(char) );
	  status = get_value_int1(box_handle, cnames[i], (char*)temp );
	  for ( int j = 0; j < var_length; ++j )
	  {
//              printf("       %s[%d] = %d \n", cnames[i], j, temp[j] );
              msgpack_pack_char(&pk, temp[j]);
	  }
	  free(temp);
      }
      else if ( strcmp(type, "integer2" ) == 0 )
      {
	  /*
	   * pre-allocate space becasue we will use get_value_* functions
	   * the values will be copied into this space.
	   */
          shorttemp  = (short*)malloc( var_length * sizeof(short) );
	  status = get_value_int2(box_handle, cnames[i], shorttemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
//              printf("       %s[%d] = %d \n", cnames[i], j, shorttemp[j] );
              msgpack_pack_short(&pk, shorttemp[j]);
	  }
	  free(shorttemp);
      }
      else if ( strcmp(type, "integer8" ) == 0 )
      {
	  /*
	   * pre-allocate space becasue we will use get_value_* functions
	   * the values will be copied into this space.
	   */
          longtemp  = (long*)malloc( var_length * sizeof(long) );
	  status = get_value_int8(box_handle, cnames[i], longtemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
//              printf("       %s[%d] = %d \n", cnames[i], j, longtemp[j] );
              msgpack_pack_long(&pk, longtemp[j]);
	  }
	  free(longtemp);
      }
      else if ( strcmp(type, "real4" ) == 0 )
      {
	  /*
	   * pre-allocate space becasue we will use get_value_* functions
	   * the values will be copied into this space.
	   */
          floattemp  = (float*)malloc( var_length * sizeof(float) );
	  status = get_value_float(box_handle, cnames[i], floattemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
//              printf("       %s[%d] = %f \n", cnames[i], j, floattemp[j] );
              msgpack_pack_float(&pk, floattemp[j] );
	  }
	  free(floattemp);
      }
      else if ( strcmp(type, "real8" ) == 0 )
      {
         // doubletemp  = (double*)malloc( var_length * sizeof(double) );
	 //
	 /*
	  * Here we use the get_value_ptr_* function instead of the 
	  * get_value_* function. Because Fortran argument is pass-by-reference,
	  * we need a double** type here.
	  */
          doubledoubletemp  = (double**)malloc( sizeof(double*) );
	  
	  status = get_var_grid(box_handle, cnames[i], &var_grid );
	  if ( var_grid == 0 )
	  {
	    status = get_value_ptr_double_scalar(box_handle, cnames[i], doubledoubletemp );
	  }
	  else if ( var_grid == 1 )
	  {
	    status = get_value_ptr_double_1darray(box_handle, cnames[i], doubledoubletemp );
	  }
	  else if (var_grid == 2 )
	  {
	    status = get_value_ptr_double_2darray(box_handle, cnames[i], doubledoubletemp );
	  }
	  else
	  {
              printf("      c_serialize_states unknown grid: %d\n", var_grid );
	      exit(1);
	  }
//	  status = get_value_ptr_double(box_handle, cnames[i], doubledoubletemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              printf("       %s[%d] = %f \n", cnames[i], j, doubledoubletemp[0][j] );
              msgpack_pack_double(&pk, doubledoubletemp[0][j] );
	  }
	  //
	  //prevent memory leak
	  //This isn't ideal, here the variable name should not be known.
	  //There are other better ways to do it.
	  //Just demonstrate that we can use get_value_ptr function.
	  if ( strcmp(cnames[i], "double2d") == 0 ||
               strcmp(cnames[i], "sinevalue_tmp" ) == 0 ) 
	  {
   	       free(doubledoubletemp[0]);
	  }
	  free(doubledoubletemp);
      }
      else if ( strcmp(type, "logical" ) == 0 )
      {
	 /*
	  * Here we use the get_value_ptr_* function instead of the 
	  * get_value_* function. Because Fortran argument is pass-by-reference,
	  * we need a double** type here.
	  */
          booltemp  = (bool*)malloc( var_length * sizeof(bool) );
	  status = get_value_logical(box_handle, cnames[i], (bool*)booltemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
//              printf("       %s[%d] = %d \n", cnames[i], j, booltemp[j] );
	      if ( booltemp[j] )
	      {
                msgpack_pack_true(&pk );
	      }
	      else
	      {
                msgpack_pack_false(&pk );
	      }

	  }
	  free(booltemp);
      }
      else if ( strcmp(type, "character" ) == 0 )
      {
	 /*
	  * Here we use the get_value_ptr_* function instead of the 
	  * get_value_* function. Because Fortran argument is pass-by-reference,
	  * we need a double** type here.
	  *
	  * Doesn't work for string arrays, only use string, ie. char arrays
	  * here
	  */
          temp  = (char*)malloc( var_length * sizeof(char) );
	  status = get_value_string(box_handle, cnames[i], temp );
//          printf("       %s = %s \n", cnames[i], temp );
          msgpack_pack_str(&pk, var_length - 1 );
          msgpack_pack_str_body(&pk, temp, var_length - 1 );
	  free(temp);
      }
      else
      {
            printf("  WARNING: Unknown type = %s\n", type );
            msgpack_pack_nil(&pk);  // Need something; will this work?
      }
    }


    /*
     * clean allocated spaces
     */
    free(cnames[0]);
    free(cnames);
    free(names);
    fclose(fp);

    return(0);
}
