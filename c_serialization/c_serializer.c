#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>   // to get filesize
#include <msgpack.h>
#include <msgpack/fbuffer.h>

#include "serializer.h"
#include "c_serializer.h"
#include "ut_trim.h"

int c_serialize_states(void** box_handle, const char* ser_file )
{
    char* temp = (char*)NULL;
    int* inttemp = (int*)NULL;
    short* shorttemp = (short*)NULL;
    long* longtemp = (long*)NULL;
    float* floattemp = (float*)NULL;
    double* doubletemp = (double*)NULL;

    int status, var_count, var_length;
  
    char name[2048];
    char type[2048];
    char ctype[2048];
    char role[2048]="all";

    char** names = NULL;
    char** cnames = NULL;

    FILE *fp = fopen(ser_file, "w+");
    msgpack_packer pk;
    msgpack_packer_init(&pk, fp, msgpack_fbuffer_write);

    status = get_component_name(box_handle, name);
    printf("In c_serialize_states: name=%s\n", name);
    status = get_var_count(box_handle, role, &var_count);
    printf("In c_serialize_states: role=%s, count=%d\n", role, var_count);

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
      printf( "names[ %d ] = %s \n", i, cnames[i] );

      status = get_var_length(box_handle, cnames[i], &var_length);
      printf( "names[ %d ] = %s, length = %d \n", i, cnames[i], var_length );
      status = get_var_type(box_handle, cnames[i], type);
      printf( "names[ %d ] = %s, type = %s \n", i, cnames[i], type );
      if ( strcmp(type, "integer4" ) == 0 )
      {
          inttemp  = (int*)malloc( var_length * sizeof(int) );
	  status = get_value_int(box_handle, cnames[i], (int*)inttemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              printf("       %s[%d] = %d \n", cnames[i], j, inttemp[j] );
              msgpack_pack_int(&pk, inttemp[j]);
	  }
	  free(inttemp);
      }
      else if ( strcmp(type, "integer1" ) == 0 )
      {
          temp  = (char*)malloc( var_length * sizeof(char) );
	  status = get_value_int1(box_handle, cnames[i], (char*)temp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              printf("       %s[%d] = %d \n", cnames[i], j, temp[j] );
              msgpack_pack_int(&pk, temp[j]);
	  }
	  free(temp);
      }
      else if ( strcmp(type, "integer2" ) == 0 )
      {
          shorttemp  = (short*)malloc( var_length * sizeof(short) );
	  status = get_value_int2(box_handle, cnames[i], shorttemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              printf("       %s[%d] = %d \n", cnames[i], j, shorttemp[j] );
              msgpack_pack_short(&pk, shorttemp[j]);
	  }
	  free(shorttemp);
      }
      else if ( strcmp(type, "integer8" ) == 0 )
      {
          longtemp  = (long*)malloc( var_length * sizeof(long) );
	  status = get_value_int8(box_handle, cnames[i], longtemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              printf("       %s[%d] = %d \n", cnames[i], j, longtemp[j] );
              msgpack_pack_long(&pk, longtemp[j]);
	  }
	  free(longtemp);
      }
      else if ( strcmp(type, "real4" ) == 0 )
      {
          floattemp  = (float*)malloc( var_length * sizeof(float) );
	  status = get_value_float(box_handle, cnames[i], floattemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              printf("       %s[%d] = %f \n", cnames[i], j, floattemp[j] );
              msgpack_pack_float(&pk, floattemp[j] );
	  }
	  free(floattemp);
      }
      else if ( strcmp(type, "real8" ) == 0 )
      {
          doubletemp  = (double*)malloc( var_length * sizeof(double) );
	  status = get_value_double(box_handle, cnames[i], doubletemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              printf("       %s[%d] = %f \n", cnames[i], j, doubletemp[j] );
              msgpack_pack_float(&pk, doubletemp[j] );
	  }
	  free(doubletemp);
      }
      else if ( strcmp(type, "logical" ) == 0 )
      {
          inttemp  = (int*)malloc( var_length * sizeof(int) );
	  status = get_value_logical(box_handle, cnames[i], (int*)inttemp );
	  for ( int j = 0; j < var_length; ++j )
	  {
              printf("       %s[%d] = %d \n", cnames[i], j, inttemp[j] );
              msgpack_pack_int(&pk, inttemp[j] );
	  }
	  free(inttemp);
      }
      else if ( strcmp(type, "character" ) == 0 )
      {
          temp  = (char*)malloc( var_length * sizeof(char) );
	  status = get_value_string(box_handle, cnames[i], temp );
          printf("       %s = %s \n", cnames[i], temp );
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


    free(cnames[0]);
    free(cnames);
    free(names);
    fclose(fp);

    return(0);
}
