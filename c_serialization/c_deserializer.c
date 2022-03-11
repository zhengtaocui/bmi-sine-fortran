/** ----------------------------------------------
  * c_deserializer.c
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Feb. 1, 2022
  * Last date of modification: Feb 18, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: Deserialize a given model serialization file and populate
  *              the model using states read from the serialization file
  *              by the exporsed Fortran BMI procedures by ISO C Binding. 
  *              It requries the msgpack-c library for deserialization.
  * 		 
  */
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

/*
 * read (deserialize) model states from a given model serailze file and
 * populate the mdoel object with the states. 
 */
int c_deserialize_states(void* box_handle, const char* ser_file )
{
    /*
     * temporary memory spaces
     */
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

    char** names = NULL; //variable names from Fortran
    char** cnames = NULL;//copy of variables from Fortran

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

    //get the total variable count
    status = get_var_count(box_handle, role, &var_count);
//    printf("In c_deserialize_states: role=%s, count=%d\n", role, var_count);

    names = malloc( sizeof(char*));
    cnames = malloc( var_count * sizeof(char*));
    /*Now the get_var_names copies the names to the given array of strings
     * The following is not needed.
     */
    //2048 is the BMI_MAX_VARIABLE_NAME  defined in bmi.f90
 //   cnames[0] = malloc( 2048 * var_count * sizeof(char) );
    for ( int i = 0; i < var_count; ++i )
    {
       cnames[i] = malloc( 2048  * sizeof(char) );
    }

    status = get_var_names(box_handle, role, cnames);
    //copy variable names to a temporary memory location
//    strcpy(cnames[0], names[0]);
    for ( int i = 0; i < var_count; ++i )
    {
    /*Now the get_var_names copies the names to the given array of strings
     * The following is not needed.
     */
      
      //for accessing the variable names in a C array fashion
//      cnames[0][2048 *(i+1) -1 ] = '\0';
//      cnames[i]  = &cnames[0][i * 2048];
//      ut_trim( cnames[i] );
//      printf( "names[ %d ] = %s \n", i, cnames[i] );

      //get the variable length
      status = get_var_length(box_handle, cnames[i], &var_length);
//      printf( "names[ %d ] = %s, length = %d \n", i, cnames[i], var_length );
      //get the variable data type
      status = get_var_type(box_handle, cnames[i], type);
//      printf( "names[ %d ] = %s, type = %s \n", i, cnames[i], type );
      if ( strcmp(type, "integer4" ) == 0 )
      {
          //store values in a 1D array
          inttemp  = (int*)malloc( var_length * sizeof(int) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              //deserialize the array element from the file
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	      inttemp[j] = (int)(unpacked.data.via.i64); 
//              printf("       %s[%d] = %d \n", cnames[i], j, inttemp[j] );
	  }
	  //populate the restored states to the model object 
	  set_value_int(box_handle, cnames[i], inttemp);
	  //cleaning up
	  free(inttemp);
      }
      else if ( strcmp(type, "integer1" ) == 0 )
      {
          //store values in a 1D array
          temp  = (char*)malloc( var_length * sizeof(char) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              //deserialize the array element from the file
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	      temp[j] = (signed char)(unpacked.data.via.i64);
//              printf("       %s[%d] = %d \n", cnames[i], j, temp[j] );
	  }
	  //populate the restored states to the model object 
	  set_value_int1(box_handle, cnames[i], temp);
	  //cleaning up
	  free(temp);
      }
      else if ( strcmp(type, "integer2" ) == 0 )
      {
          //store values in a 1D array
          shorttemp  = (short*)malloc( var_length * sizeof(short) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              //deserialize the array element from the file
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	      shorttemp[j] = (short)(unpacked.data.via.i64);
//              printf("       %s[%d] = %d \n", cnames[i], j, shorttemp[j] );
	  }
	  //populate the restored states to the model object 
	  set_value_int2(box_handle, cnames[i], shorttemp);
	  //cleaning up
	  free(shorttemp);
      }
      else if ( strcmp(type, "integer8" ) == 0 )
      {
          //store values in a 1D array
          longtemp  = (long*)malloc( var_length * sizeof(long) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              //deserialize the array element from the file
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	      longtemp[j] = (long)(unpacked.data.via.i64);
//              printf("       %s[%d] = %d \n", cnames[i], j, longtemp[j] );
	  }
	  //populate the restored states to the model object 
	  set_value_int8(box_handle, cnames[i], longtemp);
	  //cleaning up
	  free(longtemp);
      }
      else if ( strcmp(type, "real4" ) == 0 )
      {
          //store values in a 1D array
          floattemp  = (float*)malloc( var_length * sizeof(float) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              //deserialize the array element from the file
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
              floattemp[j] = (float)(unpacked.data.via.f64);
//              printf("       %s[%d] = %f \n", cnames[i], j, floattemp[j] );
	  }
	  //populate the restored states to the model object 
	  set_value_float(box_handle, cnames[i], floattemp);
	  //cleaning up
	  free(floattemp);
      }
      else if ( strcmp(type, "real8" ) == 0 )
      {
          //store values in a 1D array
          doubletemp  = (double*)malloc( var_length * sizeof(double) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              //deserialize the array element from the file
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
              doubletemp[j] = (double)(unpacked.data.via.f64);
//              printf("       %s[%d] = %f \n", cnames[i], j, doubletemp[j] );
	  }
	  //populate the restored states to the model object 
	  set_value_double(box_handle, cnames[i], doubletemp);
	  //cleaning up
	  free(doubletemp);
      }
      else if ( strcmp(type, "logical" ) == 0 )
      {
          //store values in a 1D array
          booltemp  = (bool*)malloc( var_length * sizeof(bool) );
	  for ( int j = 0; j < var_length; ++j )
	  {
              //deserialize the array element from the file
              ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
	      booltemp[j] = (bool)(unpacked.data.via.i64); 
//              printf("       %s[%d] = %d \n", cnames[i], j, booltemp[j] );
	  }
	  //populate the restored states to the model object 
	  set_value_logical(box_handle, cnames[i], booltemp);
	  //cleaning up
	  free(booltemp);
      }
      else if ( strcmp(type, "character" ) == 0 )
      {
          //store values in a 1D array
          temp  = (char*)malloc( var_length * sizeof(char) );
          //deserialize the string from the file
          ret = msgpack_unpack_next(&unpacked, inbuffer, len, &off);
          strcpy(temp, (char*)unpacked.data.via.array.ptr );
	  //set the null character to mark the end of the C string
	  temp[ var_length - 1] = '\0';
          printf("       %s = %s \n", cnames[i], temp );
          //restore the value to the model
	  set_value_string(box_handle, cnames[i], temp);
	  //cleaning up
	  free(temp);
      }
      else
      {
            printf("  WARNING: Unknown type = %s\n", type );
      }
    }

    //cleaning up
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
