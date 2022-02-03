#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>   // to get filesize
#include <msgpack.h>
#include <msgpack/fbuffer.h>

#include "iso_c_bmif_2_0.h"
#include "serializer.h"
#include "c_deserializer.h"
#include "ut_trim.h"

int c_deserialize_states(void** box_handle, const char* ser_file )
{
    char* temp = (char*)NULL;
    int* inttemp = (int*)NULL;
    short* shorttemp = (short*)NULL;
    long* longtemp = (long*)NULL;
    float* floattemp = (float*)NULL;
    double* doubletemp = (double*)NULL;
    bool* booltemp = (bool*)NULL;

    int status, var_count, var_length;
    unsigned long int buffer_size, unpacked_buffer_size;
  
    char name[2048];
    char type[2048];
    char ctype[2048];
    char role[2048]="all";

    char** names = NULL;
    char** cnames = NULL;

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


    status = get_var_count(box_handle, role, &var_count);
//    printf("In c_deserialize_states: role=%s, count=%d\n", role, var_count);

    names = malloc( sizeof(char*));
    cnames = malloc( var_count * sizeof(char*));
    cnames[0] = malloc( 2048 * var_count * sizeof(char) );

    status = get_var_names(box_handle, role, names);
    strcpy(cnames[0], names[0]);
    for ( int i = 0; i < var_count; ++i )
    {
      cnames[0][2048 *(i+1) -1 ] = '\0';
      cnames[i]  = &cnames[0][i * 2048];
      ut_trim( cnames[i] );
//      printf( "names[ %d ] = %s \n", i, cnames[i] );

      status = get_var_length(box_handle, cnames[i], &var_length);
//      printf( "names[ %d ] = %s, length = %d \n", i, cnames[i], var_length );
      status = get_var_type(box_handle, cnames[i], type);
//      printf( "names[ %d ] = %s, type = %s \n", i, cnames[i], type );
      if ( strcmp(type, "integer4" ) == 0 )
      {
          inttemp  = (int*)malloc( var_length * sizeof(int) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	      inttemp[j] = (int)(unpacked.data.via.i64); 
//              printf("       %s[%d] = %d \n", cnames[i], j, inttemp[j] );
	  }
	  set_value_int(box_handle, cnames[i], inttemp);
	  free(inttemp);
      }
      else if ( strcmp(type, "integer1" ) == 0 )
      {
          temp  = (char*)malloc( var_length * sizeof(char) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	      temp[j] = (signed char)(unpacked.data.via.i64);
//              printf("       %s[%d] = %d \n", cnames[i], j, temp[j] );
	  }
	  set_value_int1(box_handle, cnames[i], temp);
	  free(temp);
      }
      else if ( strcmp(type, "integer2" ) == 0 )
      {
          shorttemp  = (short*)malloc( var_length * sizeof(short) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	      shorttemp[j] = (short)(unpacked.data.via.i64);
//              printf("       %s[%d] = %d \n", cnames[i], j, shorttemp[j] );
	  }
	  set_value_int2(box_handle, cnames[i], shorttemp);
	  free(shorttemp);
      }
      else if ( strcmp(type, "integer8" ) == 0 )
      {
          longtemp  = (long*)malloc( var_length * sizeof(long) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	      longtemp[j] = (long)(unpacked.data.via.i64);
//              printf("       %s[%d] = %d \n", cnames[i], j, longtemp[j] );
	  }
	  set_value_int8(box_handle, cnames[i], longtemp);
	  free(longtemp);
      }
      else if ( strcmp(type, "real4" ) == 0 )
      {
          floattemp  = (float*)malloc( var_length * sizeof(float) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
              floattemp[j] = (float)(unpacked.data.via.f64);
//              printf("       %s[%d] = %f \n", cnames[i], j, floattemp[j] );
	  }
	  set_value_float(box_handle, cnames[i], floattemp);
	  free(floattemp);
      }
      else if ( strcmp(type, "real8" ) == 0 )
      {
          doubletemp  = (double*)malloc( var_length * sizeof(double) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
              doubletemp[j] = (double)(unpacked.data.via.f64);
//              printf("       %s[%d] = %f \n", cnames[i], j, doubletemp[j] );
	  }
	  set_value_double(box_handle, cnames[i], doubletemp);
	  free(doubletemp);
      }
      else if ( strcmp(type, "logical" ) == 0 )
      {
          booltemp  = (bool*)malloc( var_length * sizeof(bool) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	      booltemp[j] = (bool)(unpacked.data.via.i64); 
//              printf("       %s[%d] = %d \n", cnames[i], j, booltemp[j] );
	  }
	  set_value_logical(box_handle, cnames[i], booltemp);
	  free(booltemp);
      }
      else if ( strcmp(type, "character" ) == 0 )
      {
          temp  = (char*)malloc( var_length * sizeof(char) );
          ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
          strcpy(temp, (char*)unpacked.data.via.array.ptr );
	  temp[ var_length - 1] = '\0';
//          printf("       %s = %s \n", cnames[i], temp );
	  set_value_string(box_handle, cnames[i], temp);
	  free(temp);
      }
      else
      {
            printf("  WARNING: Unknown type = %s\n", type );
      }
    }


    free(cnames[0]);
    free(cnames);
    free(names);
    fclose(fp);

    return(0);
}