#include <stdio.h>
#include <stdlib.h>
#include <string.h>     // for strchr()
#include <sys/stat.h>   // to get filesize
#include <msgpack.h>
#include <msgpack/fbuffer.h>
#include "ut_trim.h"

int c_deserialize( char** names, int* cl, char** types, int* typelength, 
		int* namecount, char** cptr2, char* ser_file)
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
      end = (i + 1) * (*cl);
      strcpy( cnames[0], names[0] ); 
      cnames[0][ end - 1] = '\0'; 
      cnames[i] = &cnames[0][ i * *cl];
      ut_trim( cnames[i] );
      //printf( "inside c_deserialize: end = %d \n", end );
      //printf( "inside c_deserialize: %s \n", cnames[i] );
      end = (i + 1) * (*typelength);
      strcpy( ctypes[0], types[0] ); 
      ctypes[0][ end - 1] = '\0'; 
      ctypes[i] = &ctypes[0][ i * *typelength];
      ut_trim( ctypes[i] );
      //printf( "inside c_deserialize: type end = %d \n", end );
      //printf( "inside c_deserialize type =  %s \n", ctypes[i] );
      if (strcmp(ctypes[i], "real") == 0 )
      {
          for ( int j = 0; j < 1; ++j )
	  {
             ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	     *(float*)(((float**)cptr2)[i] + j * sizeof(float)) =
		                               (float)(unpacked.data.via.f64);
             //printf( "inside c_deserialize %f \n", *(float*)(((float**)cptr2)[i]+ j*sizeof(float)));
	  }
      }
      else if ( strcmp(ctypes[i], "int") ==  0 )
      {
          for ( int j = 0; j < 1; ++j )
	  {
             ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	     *(int*)(((int**)cptr2)[i] + j * sizeof(int)) = 
		                                (int)(unpacked.data.via.i64);
             //printf( "inside c_deserialize %f \n", *(int*)(((int**)cptr2)[i] + j * sizeof(int)));
	  }
      }
      else if ( strcmp(ctypes[i], "double") ==  0 )
      {
          for ( int j = 0; j < 1; ++j )
	  {
             ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	     *(double*)(((double**)cptr2)[i] + j * sizeof(double)) = 
		                                (double)(unpacked.data.via.f64);
             //printf( "inside c_deserialize %f \n", *(double*)(((double**)cptr2)[i]+ j * sizeof(double)));
	  }
      }
    }

    free( cnames[0] );
    free( cnames );
    free( ctypes[0] );
    free( ctypes );
    fclose(fp);
    return( 0 );
}
