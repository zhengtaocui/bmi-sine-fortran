/** ----------------------------------------------
  * c_serialize.c
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Jan. 5, 2022
  * Last date of modification: Feb 18, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: Performs the actual model serialization using the
  *              msgpack-c library. This need to be called by the Fortran
  *              serialize function in state_serializer.f90. 
  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>     // for strchr()
#include <sys/stat.h>   // to get filesize
#include <msgpack.h>
#include <msgpack/fbuffer.h>
#include "ut_trim.h"

/*
 * The fortran passes all variables and meta data of the variables.
 * names: variables in an character array, input
 * cl : the maximum length of each varaiable name, input
 * types: datatype of each variable in an array, input
 * typelength: the length of each datatype name, input
 * namecount: total number of variables
 * var_size: array size of each variable, each variable is passed as a 
 *           1d array. input
 * cptr2: the pointer array in which each element is a pointer to one
 *        model state values in a 1d array, input
 * ser_file: the disk filename used to store serialized values. 
 *
 */
int c_serialize( char** names, int* cl, char** types, int* typelength, 
		int* namecount, int* var_size, char** cptr2, char* ser_file)
{
    char** cnames = (char**)NULL;
    char** ctypes = (char**)NULL;
    int end = *cl;

    //trim the ser_file 
    ut_trim(ser_file);
    //printf( "inside c_serialize: ser_file = %s \n", ser_file );

    FILE *fp = fopen(ser_file, "w+");
    msgpack_packer pk;
    msgpack_packer_init(&pk, fp, msgpack_fbuffer_write);

    cnames = malloc( *namecount * sizeof(char*));
    cnames[ 0 ] = malloc( *namecount * (*cl) * sizeof(char) );

    ctypes = malloc( *namecount * sizeof(char*));
    ctypes[ 0 ] = malloc( *namecount * (*typelength) * sizeof(char) );

    strcpy( cnames[0], names[0] );
    strcpy( ctypes[0], types[0] );
    for( int i = 0; i < *namecount; ++i)
    {
      //to access the variable names in C string array fashion
      end = (i + 1) * (*cl);
      strcpy( cnames[0], names[0] ); 
      cnames[0][ end - 1] = '\0'; 
      cnames[i] = &cnames[0][ i * *cl];
      ut_trim( cnames[i] );
//      printf( "inside c_serialize: end = %d \n", end );
//      printf( "inside c_serialize: %s \n", cnames[i] );

      //to access the variable type names in C string array fashion
      end = (i + 1) * (*typelength);
      strcpy( ctypes[0], types[0] ); 
      ctypes[0][ end - 1] = '\0'; 
      ctypes[i] = &ctypes[0][ i * *typelength];
      ut_trim( ctypes[i] );
//      printf( "inside c_serialize: type end = %d \n", end );
//      printf( "inside c_serialize type =  %s \n", ctypes[i] );
      //printf( "inside c_serialize %f \n", *(float*)(float**)cptr2[i]);
//      printf( "inside c_serialize: var_size = %d \n", var_size[i]);
      if (strcmp(ctypes[i], "real4") == 0 )
      {
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             //cptr2 is passed a (char**) type,
	     //we need cast to to be float**, the access the values
	     //as a 2d array, i is the index of the ith variable name, 
	     //j is the index of the jth variable value
             msgpack_pack_float(&pk, ((float**)cptr2)[i][j] );
//             printf( "inside c_serialize: %s float(1) = %f \n", cnames[i],
//	           *(((float**)cptr2)[i] + j ) );
	  }
      }
      else if ( strcmp(ctypes[i], "real8") ==  0 )
      {
          for ( int j = 0; j < var_size[i]; ++j )
	  {
              //pointer calculation is to cumbersome, see the next statement
	      //for the simplified version
//             printf( "inside c_serialize: %s double = %f \n", cnames[i],
//	           *(double*)(double**)(cptr2[i] + j * sizeof(double)) );
//
             //cptr2 is passed a (char**) type,
	     //we need cast to to be double**, the access the values
	     //as a 2d array, i is the index of the ith variable name, 
	     //j is the index of the jth variable value
             msgpack_pack_double(&pk, ((double**)cptr2)[i][j] );
	  }
      }
      else if ( strcmp(ctypes[i], "integer4") ==  0 )
      {
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             //cptr2 is passed a (char**) type,
	     //we need cast to to be int**, the access the values
	     //as a 2d array, i is the index of the ith variable name, 
	     //j is the index of the jth variable value
             msgpack_pack_int(&pk, ((int**)cptr2)[i][j] );
//             printf( "inside c_serialize: %s int = %d \n", cnames[i],
//	           *(int*)(int**)(cptr2[i] + j * sizeof(float)) );
	  }
      }
      else if ( strcmp(ctypes[i], "integer1") ==  0 )
      {
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             msgpack_pack_signed_char(&pk, ((int8_t**)cptr2)[i][j] );
//             printf( "inside c_serialize: %s int1 = %d \n", cnames[i],
//	           *(int8_t*)(int8_t**)(cptr2[i] + j * sizeof(int8_t)) );
	  }
      }
      else if ( strcmp(ctypes[i], "integer2") ==  0 )
      {
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             msgpack_pack_short(&pk, ((short**)cptr2)[i][j] );
//             printf( "inside c_serialize: %s int2 = %d \n", cnames[i],
//	           *(short*)(short**)(cptr2[i] + j * sizeof(short)) );
	  }
      }
      else if ( strcmp(ctypes[i], "integer8") ==  0 )
      {
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             msgpack_pack_long(&pk, ((long**)cptr2)[i][j] );
//             printf( "inside c_serialize: %s int8 = %d \n", cnames[i],
//	           *(long*)(long**)(cptr2[i] + j * sizeof(long)) );
	  }
      }
      else if ( strcmp(ctypes[i], "character") ==  0 )
      {
          cptr2[i][ var_size[i] - 1] = '\0';
//          printf( "inside c_serialize: %s string = %s \n", cnames[i],
//	           (char*)(cptr2[i]) );
          msgpack_pack_str(&pk, var_size[i] - 1 );
	  //remove trailing null char
          msgpack_pack_str_body(&pk, cptr2[i], var_size[i] - 1 );
      }
      else if ( strcmp(ctypes[i], "logical") ==  0 )
      {
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             //for gcc and ifort, the default size for logical is 4 bytes.
	     if ( ((bool**)cptr2)[i][j] )
             {
                msgpack_pack_true(&pk);
	     }
	     else
	     {
                msgpack_pack_false(&pk);
	     }
//             printf( "inside c_serialize: %s logical = %d \n", cnames[i],
//	            ((bool**)cptr2)[i][j] );
	  }
      }
      else
      {
            printf("  WARNING: Unknown type = %s\n", types[i] );
            msgpack_pack_nil(&pk);  // Need something; will this work?
      }

    }
    //cleaning up
    free( cnames[0] );
    free( cnames );
    free( ctypes[0] );
    free( ctypes );
    fclose(fp);
/*
    printf( "inside c_serialize 1: %f \n", cptr[0] );
    printf( "inside c_serialize 1: %f \n", cptr[1] );
    printf( "inside c_serialize 1: %f \n", cptr[2] );
    printf( "inside c_serialize 2: %c \n", *cptr1 );
    printf( "inside c_serialize 2: %d \n", *(int*)cptr1 );
    printf( "inside c_serialize 3: %f \n", *(float*)(fcptr2[0]) );
    printf( "inside c_serialize 3: %f \n", fcptr2[0][1]);
    printf( "inside c_serialize 3: %f \n", fcptr2[0][2]);
    printf( "inside c_serialize 4: %f \n", *(float*)(float**)cptr2[0]);
    printf( "inside c_serialize 4: %f \n", *(float*)(float**)(cptr2[0] + sizeof(float)));
    printf( "inside c_serialize 4: %f \n", *(float*)(float**)(cptr2[0] + 2* sizeof(float)));
    printf( "inside c_serialize 4: %f \n", *(double*)(double**)cptr2[1]);
    printf( "inside c_serialize 4: %f \n", *(double*)(double**)(cptr2[1] + sizeof(double)));
    */
/*
    for( int i = 0; i < *namecount; ++i)
    {
       //printf( "inside c_serialize 1: %f \n", *((float*)((float**)cptr[0] + i*sizeof(float*))) );
       printf( "inside c_serialize 1: %f \n", cptr[i] );
    }
*/
    return( 0 );
}
