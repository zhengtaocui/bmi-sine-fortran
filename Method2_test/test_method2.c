/** ----------------------------------------------
  * test_serialize_c.c
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Feb. 2, 2022
  * Last date of modification: Feb 18, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: test program for the the Fortran version of the 
  *             serialization/deserialization code in ../serialization,
  *             using C code.
  */
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

#include "iso_c_bmif_2_0.h"
#include "serializer.h"

int BMI_SUCCESS = 0;
void check_status(int* status, char* name){
    printf("%s: ", name);
    if(*status == BMI_SUCCESS){
        printf("SUCCESS\n");
    }
    else{
        printf("FAILURE\n");
        exit(*status);
    }
}

int main(int argc, char** argv)
{
    /*
     * handles for the first and second model
     */
    void* box_handle = NULL;
    void* box_handle2 = NULL;

    /*
     * handles for the Fortran serializer
     */
    void* serializer_handle = NULL;
    void* serializer_adapter_handle = NULL;

    char config_file[2048] = "sample.cfg";
    char ser_file[2048] = "serialize.out";

    int status = -1;

    double start_time, current_time, end_time;
    double pause_time = 50;

    char name[2048];

    /*
     *  create a serializer object
     */
    status = get_serializer_handle(&serializer_handle);
    check_status(&status, "serializer handle");

    status = get_serializer_box(&serializer_adapter_handle, &serializer_handle);
    check_status(&status, "create adapter");

    /*
     * The first model
     */
    status = register_bmi(&box_handle);
    check_status(&status, "model1 create_box");

    printf( "config file: %s\n", config_file);

    /*
     * initialize the first model
     */
    status = initialize(box_handle, config_file);
    check_status(&status, "model1 initialize");

    /*
     * The second model
     */
    status = register_bmi(&box_handle2);
    check_status(&status, "model2 create_box");

    /*
     * initialize the second model
     */
    status = initialize(box_handle2, config_file);
    check_status(&status, "model2 initialize");

    /*
     * component name of model 1
     */
    status = get_component_name(box_handle, name);
    check_status(&status, "get_component_name");
    printf( "component name: %s\n", name);

    /*
     * get start time
     */
    status = get_start_time(box_handle, &start_time);
    check_status(&status, "get_start_time");
    printf( "start time: %f\n", start_time);

    /*
     * get end time
     */
    status = get_end_time(box_handle, &end_time);
    check_status(&status, "get_end_time");
    printf( "end time: %f\n", end_time);

    /*
     *  Run model 1 to the middel and pause
     */
    current_time = start_time;
    while( current_time <= pause_time )
    {
      status = update(box_handle);
      check_status(&status, "update");
      status = get_current_time(box_handle, &current_time);
      check_status(&status, "get_current_time");
      printf( "current time: %f\n", current_time);
    }

    printf( "Before serializing, comparing two models ...\n" );

    status = compare( serializer_adapter_handle, box_handle, box_handle2 );
    if ( status != BMI_SUCCESS )
    {
       printf( "Comparing failed! Model1 is not equal to Model2!\n" );
    }
    else
    {
       printf( "Comparing succeed! Model1 is equal to Model2!\n" );
    }
    printf( "Done comparing before serializing.\n" );

    printf( "Now serialize the first model ... \n" );
    status = serialize( serializer_adapter_handle, box_handle, ser_file );
    check_status(&status, "serialize");

    printf( "Now deserialize the first model to the second model... \n" );
    status = deserialize( serializer_adapter_handle, box_handle2, ser_file );
    check_status(&status, "deserialize");

    printf( "After deserializing, comparing two models ...\n" );
    status = compare( serializer_adapter_handle, box_handle, box_handle2 );
    if ( status != BMI_SUCCESS )
    {
       printf( "Comparing failed! Model1 is not equal to Model2!\n" );
    }
    else
    {
       printf( "Comparing succeed! Model1 is equal to Model2!\n" );
    }
    printf( "Done comparing after deserializing.\n" );

    printf( "Now run both model to the end.\n" );

    /*
     * Run model 1 to end
     */
    while( current_time <= end_time )
    {
      status = update(box_handle);
      check_status(&status, "update");
      status = get_current_time(box_handle, &current_time);
      check_status(&status, "get_current_time");
      printf( "current time: %f\n", current_time);
    }

    /*
     * Run model 2 to end
     */
    status = get_current_time(box_handle2, &current_time);
    check_status(&status, "get_current_time model 2");
    while( current_time <= end_time )
    {
      status = update(box_handle2);
      check_status(&status, "update model 2");
      status = get_current_time(box_handle2, &current_time);
      check_status(&status, "get_current_time");
      printf( "current time: %f\n", current_time);
    }

    printf( "After running to end, comparing two models ...\n" );
    /*
     * This is the compare_states function in state_serializer.f90. 
     * see iso_c_serializer.f90
     * Because we use the ISO C binding function here, a separate
     * compare function in C is not needed.
     */
    status = compare( serializer_adapter_handle, box_handle, box_handle2 );
    if ( status != BMI_SUCCESS )
    {
       printf( "Comparing failed! Model1 is not equal to Model2!\n" );
    }
    else
    {
       printf( "Comparing succeed! Model1 is equal to Model2!\n" );
    }
    printf( "Done comparing after both running to end.\n" );

    //
    //cleaning up
    status = delete_serializer_handle(&serializer_handle);
    check_status(&status, "delete serializer handle");

    status = delete_serializer_box(&serializer_adapter_handle);
    check_status(&status, "delete adapter");

//    status = finalize(&box_handle);
//    check_status(&status, "finalize");

    status = unregister_bmi(&box_handle);
    check_status(&status, "destroy_box model 1 box");

    status = unregister_bmi(&box_handle2);
    check_status(&status, "delete_box model 2 box");

    return(0);
}
