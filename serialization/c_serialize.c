#include <stdio.h>
#include <stdlib.h>
#include <string.h>     // for strchr()
#include <sys/stat.h>   // to get filesize
#include <msgpack.h>
#include <msgpack/fbuffer.h>
#include "ut_trim.h"

int c_serialize( char** names, int* cl, char** types, int* typelength, 
		int* namecount, char** cptr2, char* ser_file)
{
    char** cnames = (char**)NULL;
    char** ctypes = (char**)NULL;
    int end = *cl;

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
      end = (i + 1) * (*cl);
      strcpy( cnames[0], names[0] ); 
      cnames[0][ end - 1] = '\0'; 
      cnames[i] = &cnames[0][ i * *cl];
      ut_trim( cnames[i] );
      //printf( "inside c_serialize: end = %d \n", end );
      //printf( "inside c_serialize: %s \n", cnames[i] );
      end = (i + 1) * (*typelength);
      strcpy( ctypes[0], types[0] ); 
      ctypes[0][ end - 1] = '\0'; 
      ctypes[i] = &ctypes[0][ i * *typelength];
      ut_trim( ctypes[i] );
      //printf( "inside c_serialize: type end = %d \n", end );
      //printf( "inside c_serialize type =  %s \n", ctypes[i] );
      //printf( "inside c_serialize %f \n", *(float*)(float**)cptr2[i]);
      if (strcmp(ctypes[i], "real") == 0 )
      {
          for ( int j = 0; j < 1; ++j )
	  {
             msgpack_pack_float(&pk, *(float*)(((float**)cptr2)[i]));
	  }
      }
      else if ( strcmp(ctypes[i], "int") ==  0 )
      {
          for ( int j = 0; j < 1; ++j )
	  {
             msgpack_pack_int(&pk, *(int*)(((int**)cptr2)[i]));
	  }
      }
      else if ( strcmp(ctypes[i], "double") ==  0 )
      {
          for ( int j = 0; j < 1; ++j )
	  {
             msgpack_pack_double(&pk, *(double*)(((double**)cptr2)[i]));
	  }
      }
      else
      {
            printf("  WARNING: Unknown type = %s\n", types[i] );
            msgpack_pack_nil(&pk);  // Need something; will this work?
      }

    }
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
