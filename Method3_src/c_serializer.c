/** ----------------------------------------------
  * c_serializer.c
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Feb. 1, 2022
  * Last date of modification: Feb 18, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: For a given Fortran BMI model object, 
  *              serialize the model states to a binary file using
  *              the exporsed Fortran BMI procedures by ISO C Binding. 
  *              It requries the msgpack-c library for serialization.
  * 		 
  * 		 Added code to use get_value_ptr_* functions for float
  * 		 and double data type variables.
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

/*
 * This function does the actual work of serialize the model to a file
 * using the msgpack-c library. 
 */
int c_serialize_states(void* box_handle, const char* ser_file )
{
    /* declare the pointer variabls for various data types 
     * supported.
     */
    char* temp = (char*)NULL;
    char** charchartemp = (char**)NULL;
    int* inttemp = (int*)NULL;
    int8_t* int1temp = (int8_t*)NULL;
    int** intinttemp = (int**)NULL;
    short* shorttemp = (short*)NULL;
    long* longtemp = (long*)NULL;
    float* floattemp = (float*)NULL;
    float** floatfloattemp = (float**)NULL;
    double* doubletemp = (double*)NULL;
    double** doubledoubletemp = (double**)NULL;
    bool* booltemp = (bool*)NULL;
    bool** boolbooltemp = (bool**)NULL;

    int status, var_count, var_length, var_grid;
  
    char name[2048];
    char type[2048];
    char ctype[2048];
    char role[2048]="all";

    char** names = NULL;
    char** cnames = NULL;

    int verbose = 1;

    FILE *fp = fopen(ser_file, "w+");
    msgpack_packer pk;
    msgpack_packer_init(&pk, fp, msgpack_fbuffer_write);

    /* get the component name
     */
    status = get_component_name(box_handle, name);
    if ( verbose )
    {
      printf("In c_serialize_states: name=%s\n", name);
    }

    /*
     * get the total number of variables 
     */
    status = get_var_count(box_handle, role, &var_count);
    if ( verbose )
    {
      printf("In c_serialize_states: role=%s, count=%d\n", role, var_count);
    }

    /*
     * allocate memory to held variable names
     */
    names = malloc( sizeof(char*));
    cnames = malloc( var_count * sizeof(char*));
    /*Now the get_var_names copies the names to the given array of strings
     * The following is not needed.
     */
    //cnames[0] = malloc( 2048 * var_count * sizeof(char) );
    for ( int i = 0; i < var_count; ++i )
    {
       cnames[i] = malloc( 2048  * sizeof(char) );
    }

    /*
     * get variable names in an array
     */
    status = get_var_names(box_handle, role, cnames);

    /*Now the get_var_names copies the names to the given array of strings
     * The following is not needed.
     */
    /*
     * names points to a memory allocated by the Fortran model,
     * copy it to cnames to manuplate
     */
//    strcpy(cnames[0], names[0]);
    /*
     * loop throught all variables
     */
    for ( int i = 0; i < var_count; ++i )
    {
    /*Now the get_var_names copies the names to the given array of strings
     * The following is not needed.
     */
      /*
       * set the null char to terminate the string
       */
 //     cnames[0][2048 *(i+1) -1 ] = '\0';
      /*
       * each name has a length of 2048 chars
       */
//      cnames[i]  = &cnames[0][i * 2048];
      /*
       * remove white spaces
       */
//      ut_trim( cnames[i] );
//      printf( "names[ %d ] = %s \n", i, cnames[i] );

      /*
       * get variable length
       */
      status = get_var_length(box_handle, cnames[i], &var_length);
      if ( verbose )
      {
        printf( "names[ %d ] = %s, length = %d \n", i, cnames[i], var_length );
      }
      /*
       * get data type for a given variable 
       */
      status = get_var_type(box_handle, cnames[i], type);
      if ( verbose )
      {
        printf( "names[ %d ] = %s, type = %s \n", i, cnames[i], type );
      }
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
              if ( verbose )
              {
                 printf("       %s[%d] = %d \n", cnames[i], j, inttemp[j] );
              }
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
          int1temp  = (int8_t*)malloc( var_length * sizeof(int8_t) );
	  status = get_value_int1(box_handle, cnames[i], (int8_t*)int1temp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( verbose )
              {
                 printf("       %s[%d] = %d \n", cnames[i], j, int1temp[j] );
              }
              msgpack_pack_char(&pk, int1temp[j]);
	  }
	  free(int1temp);
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
              if ( verbose )
              {
                 printf("       %s[%d] = %d \n", cnames[i], j, shorttemp[j] );
              }
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
              if ( verbose )
              {
                 printf("       %s[%d] = %d \n", cnames[i], j, longtemp[j] );
              }
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
	  
	  /*
	   * Now we use the get_value_ptr_* function instead of the get_value_*
	   * function. Allocate a pointer of pointer to hold the 
	   * return pointer from Fortran. The allocation of the pointer
	   * is not needed because we use the get_value_ptr function.
	   */
          //floatfloattemp  = (float**)malloc( sizeof(float*) );
	  
	  status = get_value_float(box_handle, cnames[i], floattemp );
	  //
	  //call the get_value_ptr function
	  //status = get_value_ptr_float(box_handle, cnames[i], floatfloattemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( verbose )
              {
                printf("       %s[%d] = %f \n", cnames[i], j, floattemp[j] );
            //  printf("       %s[%d] = %f \n", cnames[i], j, floatfloattemp[0][j] );
            //  msgpack_pack_float(&pk, floatfloattemp[0][j] );
              }
              msgpack_pack_float(&pk, floattemp[j] );
	  }
	  free(floattemp);
	  //free(floatfloattemp);
      }
      else if ( strcmp(type, "real8" ) == 0 )
      {
          doubletemp  = (double*)malloc( var_length * sizeof(double) );
	  status = get_value_double(box_handle, cnames[i], doubletemp );
	 //
	 /*
	  * Here we use the get_value_ptr_* function instead of the 
	  * get_value_* function. Because Fortran argument is pass-by-reference,
	  * we need a double** type here.
	  */
         // doubledoubletemp  = (double**)malloc( sizeof(double*) );
	 // status = get_value_ptr_double(box_handle, cnames[i], doubledoubletemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( verbose )
              {
                printf("       %s[%d] = %f \n", cnames[i], j, doubletemp[j] );
              }
	      //write to the file 
          //    msgpack_pack_double(&pk, doubledoubletemp[0][j] );
              msgpack_pack_double(&pk, doubletemp[j] );
	  }
	  //
	  //free(doubledoubletemp);
	  free(doubletemp);
      }
      else if ( strcmp(type, "logical" ) == 0 )
      {
	 /*
	  * pre-allocate space becasue we will use get_value_* functions
	  * the values will be copied into this space.
	  */
          booltemp  = (bool*)malloc( var_length * sizeof(bool) );
	  status = get_value_logical(box_handle, cnames[i], (bool*)booltemp );
	  /*
	   * Now use the get_value_ptr_* version
	   */
          //intinttemp  = (int**)malloc( sizeof(int*) );
	  /*
	   * the Fortran logical use 4 bytes
	   */
	 // status = get_value_ptr_logical(box_handle, cnames[i], intinttemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              if ( verbose )
              {
                printf("       %s[%d] = %d \n", cnames[i], j, booltemp[j] );
              }
	  //    if ( intinttemp[0][j] )
	      if ( booltemp[j] )
	      {
                msgpack_pack_true(&pk );
	      }
	      else
	      {
                msgpack_pack_false(&pk );
	      }

	  }
	  //free(intinttemp);
	  free(booltemp);
      }
      else if ( strcmp(type, "character" ) == 0 )
      {
	 /*
	  * Doesn't work for string arrays, only use string, ie. char arrays
	  * here
	  */
          temp  = (char*)malloc( var_length * sizeof(char) );
          status = get_value_string(box_handle, cnames[i], temp );
          if ( verbose )
          {
             printf("       %s = %s \n", cnames[i], temp );
          }
         /*
	  * Now we try the get_value_ptr_* version
	  */ 
      //    charchartemp  = (char**)malloc( sizeof(char*) );
      //  status = get_value_ptr_string(box_handle, cnames[i], charchartemp );
      //    ut_trim( charchartemp[0] );
      //    printf("       %s = %s \n", cnames[i], charchartemp[0] );

          //call the msgpack-c functions.
          msgpack_pack_str(&pk, var_length - 1 );
          //msgpack_pack_str_body(&pk, charchartemp[0], var_length - 1 );
          msgpack_pack_str_body(&pk, temp, var_length - 1 );

	  //free(charchartemp);
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
    for ( int i = 0; i < var_count; ++i )
    {
       free( cnames[i] );
    }
    //free(cnames[0]);
    free(cnames);
    free(names);
    fclose(fp);

    return(0);
}
