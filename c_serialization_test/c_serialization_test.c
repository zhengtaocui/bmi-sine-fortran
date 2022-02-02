#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

#include "iso_c_bmif_2_0.h"
#include "serializer.h"
#include "c_serializer.h"
#include "c_deserializer.h"

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
     * handles for the first model
     */
    void* bmi_handle = NULL;
    void* bmi_handle2 = NULL;

    /*
     * handles for the first model
     */
    void* box_handle = NULL;
    void* box_handle2 = NULL;

    char config_file[2048] = "sample.cfg";
    char ser_file[2048] = "serialize.out";

    int status = -1;

    double start_time, current_time, end_time;
    double pause_time = 50;

    char name[2048];

    /*
     * The first model
     */
    status = bmi_factory(&bmi_handle);
    check_status(&status, "model1 factory");

    status = c_create_box(&box_handle, &bmi_handle);
    check_status(&status, "model1 create_box");

    printf( "config file: %s\n", config_file);

    /*
     * initialize the first model
     */
    status = initialize(&box_handle, config_file);
    check_status(&status, "model1 initialize");

    /*
     * The second model
     */
    status = bmi_factory(&bmi_handle2);
    check_status(&status, "model2 factory");

    status = c_create_box(&box_handle2, &bmi_handle2);
    check_status(&status, "model2 create_box");

    /*
     * initialize the second model
     */
    status = initialize(&box_handle2, config_file);
    check_status(&status, "model2 initialize");

    /*
     * component name of model 1
     */
    status = get_component_name(&box_handle, name);
    check_status(&status, "get_component_name");
    printf( "component name: %s\n", name);

    /*
     * get start time
     */
    status = get_start_time(&box_handle, &start_time);
    check_status(&status, "get_start_time");
    printf( "start time: %f\n", start_time);

    /*
     * get end time
     */
    status = get_end_time(&box_handle, &end_time);
    check_status(&status, "get_end_time");
    printf( "end time: %f\n", end_time);

    /*
     *  Run model 1 to the middel and pause
     */
    current_time = start_time;
    while( current_time <= pause_time )
    {
      status = update(&box_handle);
      check_status(&status, "update");
      status = get_current_time(&box_handle, &current_time);
      check_status(&status, "get_current_time");
      printf( "current time: %f\n", current_time);
    }

    printf( "Now serialize the first model ... \n" );
    status = c_serialize_states(&box_handle, ser_file );
    check_status(&status, "serialize");

    status = c_deserialize_states(&box_handle2, ser_file );
    check_status(&status, "deserialize");

    /*
     * Run model 1 to end
     */
    while( current_time <= end_time )
    {
      status = update(&box_handle);
      check_status(&status, "update");
      status = get_current_time(&box_handle, &current_time);
      check_status(&status, "get_current_time");
      printf( "current time: %f\n", current_time);
    }

    /*
     * Run model 2 to end
     */
    status = get_current_time(&box_handle2, &current_time);
    check_status(&status, "get_current_time model 2");
    while( current_time <= end_time )
    {
      status = update(&box_handle2);
      check_status(&status, "update model 2");
      status = get_current_time(&box_handle2, &current_time);
      check_status(&status, "get_current_time");
      printf( "current time: %f\n", current_time);
    }

//    status = finalize(&box_handle);
//    check_status(&status, "finalize");

    status = bmi_destroy(&bmi_handle);
    check_status(&status, "destroy model 1");

    status = c_delete_box(&box_handle);
    check_status(&status, "delete_box model 1 box");

    status = bmi_destroy(&bmi_handle2);
    check_status(&status, "destroy model 2");

    status = c_delete_box(&box_handle2);
    check_status(&status, "delete_box model 1 box");


    return(0);
}
