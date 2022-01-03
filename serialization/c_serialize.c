#include <stdio.h>
#include <msgpack.h>
#include "ut_trim.h"

int c_serialize( char** names, int* cl, int* namecount, float* cptr, char* cptr1,
		 char** cptr2)
{
    char** cnames = (char**)NULL;
    int end = *cl;
    float** fcptr2 = (float**)cptr2;
    cnames = malloc( *namecount * sizeof(char*));
    cnames[ 0 ] = malloc( *namecount * *cl * sizeof(char) );

    //strcpy( cnames[0], names[0] );
    strcpy( cnames[0], cptr2[2] );
    for( int i = 0; i < *namecount; ++i)
    {
      end = (i + 1) * (*cl);
      //strcpy( cnames[0], names[0] ); 
      strcpy( cnames[0], cptr2[2] ); 
      cnames[0][ end - 1] = '\0'; 
      cnames[i] = &cnames[0][ i * *cl];
      ut_trim( cnames[i] );
      printf( "inside c_serialize: end = %d \n", end );
      printf( "inside c_serialize: %s \n", cnames[i] );
      if ( 0 == strcmp( "Radian", cnames[i] ) )
      {
           printf( "cnames equal %s\n", cnames[i] );
      }
      else
      {
           printf( "cnames not equal %s\n", cnames[i] );
      }

      //names[0][ end - 1] = ' '; 
    }
    free( cnames[0] );
    free( cnames );

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
/*
    for( int i = 0; i < *namecount; ++i)
    {
       //printf( "inside c_serialize 1: %f \n", *((float*)((float**)cptr[0] + i*sizeof(float*))) );
       printf( "inside c_serialize 1: %f \n", cptr[i] );
    }
*/
    return( 0 );
}
