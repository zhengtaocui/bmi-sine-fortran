/** ----------------------------------------------
  * c_deserialize.c
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Jan. 5, 2022
  * Last date of modification: Feb 18, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: Performs the actual deserialization using the
  *              msgpack-c library. This need to be called by the Fortran
  *              deserialize function in state_serializer.f90. 
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
 *        model state values in a 1d array, which will be populated
 *        by the function call. The space should be allocated by the
 *        Fortran deserialize function. output
 * ser_file: the disk filename used to store serialized values. input
 *
 */
int c_deserialize( char** names, int* cl, char** types, int* typelength, 
		int* namecount, int* var_size, char** cptr2, char* ser_file)
{
    char** cnames = (char**)NULL;
    char** ctypes = (char**)NULL;
    int end = *cl;
    unsigned long int buffer_size, unpacked_buffer_size;

    ut_trim(ser_file);
    //printf( "inside c_deserialize: ser_file = %s \n", ser_file );

    //-------------------------------------
    // Get the file size; set buffer_size
    //-------------------------------------
    struct stat st;
    stat( ser_file, &st );
    buffer_size = st.st_size;
    unpacked_buffer_size = 2 * buffer_size;

    char inbuffer[buffer_size];
    char unpacked_buffer[unpacked_buffer_size]; 

    FILE *fp = fopen(ser_file, "rb");
    int i = 0;
    size_t off = 0;
    size_t len = 0;

    msgpack_unpacked unpacked;
    msgpack_unpack_return ret;
    msgpack_unpacked_init(&unpacked);

    len = fread(inbuffer, sizeof(char), buffer_size, fp); 

    cnames = malloc( *namecount * sizeof(char*));
    cnames[ 0 ] = malloc( *namecount * (*cl) * sizeof(char) );

    ctypes = malloc( *namecount * sizeof(char*));
    ctypes[ 0 ] = malloc( *namecount * (*typelength) * sizeof(char) );

    strcpy( cnames[0], names[0] );
    strcpy( ctypes[0], types[0] );
    for( int i = 0; i < *namecount; ++i)
    {
//      printf( "inside c_deserialize: i = %d \n", i );
      //to access the variable names in C string array fashion
      end = (i + 1) * (*cl);
      strcpy( cnames[0], names[0] ); 
//      printf( "inside c_deserialize: end = %d \n", end );
      cnames[0][ end - 1] = '\0'; 
//      printf( "inside c_deserialize: done strcpy 1 \n");
      cnames[i] = &cnames[0][ i * *cl];
//      printf( "inside c_deserialize: call ut_trim \n");
      ut_trim( cnames[i] );
//      printf( "inside c_deserialize: %s \n", cnames[i] );
      //to access the variable type names in C string array fashion
      end = (i + 1) * (*typelength);
      strcpy( ctypes[0], types[0] ); 
      ctypes[0][ end - 1] = '\0'; 
      ctypes[i] = &ctypes[0][ i * *typelength];
      ut_trim( ctypes[i] );
//      printf( "inside c_deserialize: type end = %d \n", end );
//      printf( "inside c_deserialize type =  %s \n", ctypes[i] );
//      printf( "inside c_deserialize var_size =  %d \n", var_size[i] );
      if (strcmp(ctypes[i], "real4") == 0 )
      {
//          printf( "inside c_deserialize deserialize float, %s  \n", cnames[i] );
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             //deserialize from the file
             ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
             //cast `cptr2` to float** and populate the values
	     ((float**)cptr2)[i][j] = (float)(unpacked.data.via.f64);
	     //
	     //cumbersome pointer calculation here, see above for the 
	     //simplified version.
//             printf( "inside c_deserialize %d, %f \n", j, 
//			     //*(float*)(float**)(cptr2[i]+ j*sizeof(float)));
//			     ((float**)cptr2)[i][j]);
	  }
      }
      else if ( strcmp(ctypes[i], "real8") ==  0 )
      {
//          printf( "inside c_deserialize deserialize double, %s \n", cnames[i] );
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
             //cast `cptr2` to double** and populate the values
	     ((double**)cptr2)[i][j] = (double)(unpacked.data.via.f64);
//             printf( "inside c_deserialize %f \n", 
//			  *(double*)(double**)(cptr2[i]+ j * sizeof(double)));
	  }
      }
      else if ( strcmp(ctypes[i], "integer4") ==  0 )
      {
//          printf( "inside c_deserialize deserialize int, %s  \n", cnames[i] );
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
             //cast `cptr2` to int** and populate the values
	     ((int**)cptr2)[i][j] = (int)(unpacked.data.via.i64);
//             printf( "inside c_deserialize %d \n", 
//		      *(int*)(int**)(cptr2[i] + j * sizeof(int)));
	  }
      }
      else if ( strcmp(ctypes[i], "integer1") ==  0 )
      {
//          printf( "inside c_deserialize deserialize int, %s  \n", cnames[i] );
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
             //cast `cptr2` to signed char** and populate the values
	     ((int8_t**)cptr2)[i][j] = 
		                          (int8_t)(unpacked.data.via.i64);
//             printf( "inside c_deserialize %d \n", 
//		   *(signed char*)(signed char**)(cptr2[i] + j * sizeof(int)));
	  }
      }
      else if ( strcmp(ctypes[i], "integer2") ==  0 )
      {
//          printf( "inside c_deserialize deserialize int, %s  \n", cnames[i] );
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
             //cast `cptr2` to short** and populate the values
	     ((short**)cptr2)[i][j] = (short)(unpacked.data.via.i64);
//             printf( "inside c_deserialize %d \n", 
//		      *(short*)(short**)(cptr2[i] + j * sizeof(shsort)));
	  }
      }
      else if ( strcmp(ctypes[i], "integer8") ==  0 )
      {
//          printf( "inside c_deserialize deserialize int, %s  \n", cnames[i] );
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
             //cast `cptr2` to long** and populate the values
	     ((long**)cptr2)[i][j] = (long)(unpacked.data.via.i64);
//             printf( "inside c_deserialize %d \n", 
//		      *(long*)(long**)(cptr2[i] + j * sizeof(long)));
	  }
      }
      else if ( strcmp(ctypes[i], "character") ==  0 || 
		                     strcmp(ctypes[i], "string") ==  0 )
      {
          ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
          //copy the string to cptr2[i]
          strncpy(cptr2[i], (char*)unpacked.data.via.array.ptr, var_size[i] );
	  cptr2[i][ var_size[i] - 1] = ' ';
//          printf( "inside c_deserialize: %s string = %s \n", cnames[i],
//	           (char*)(cptr2[i]) );
      }
      else if ( strcmp(ctypes[i], "logical") ==  0 )
      {
//          printf( "inside c_deserialize deserialize int, %s  \n", cnames[i] );
          for ( int j = 0; j < var_size[i]; ++j )
	  {
             ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
//	     *(int*)(int**)(cptr2[i] + j * sizeof(int)) = 
             //cast `cptr2` to bool** and populate the values
	     ((bool**)cptr2)[i][j] = (bool)(unpacked.data.via.i64);
//             printf( "inside c_deserialize %d \n", ((bool**)cptr2)[i][j]);
	  }
      }
    }

    //cleaning up
    free( cnames[0] );
    free( cnames );
    free( ctypes[0] );
    free( ctypes );
    fclose(fp);
    return( 0 );
}
