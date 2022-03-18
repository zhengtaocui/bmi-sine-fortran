/** ----------------------------------------------
  * test_fortran_model.c
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Mar 7, 2022
  * Last date of modification: Mar 7, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: test a Fortran BMI model using the the CFE C version of the 
  *             serialization/deserialization code in 
  *             https://github.com/NOAA-OWP/cfe.git
  */
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

#include "iso_c_bmif_2_0.h"
#include "bmi_fortran.h"
#include "serialize_state.h"

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

int test_fortran_model()
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

    Bmi *model1 = (Bmi *) malloc(sizeof(Bmi));
    Bmi *model2 = (Bmi *) malloc(sizeof(Bmi));

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
     * Registering Fortran model 1
     */
    register_bmi_fortran( model1, box_handle );

    /*
     * initialize the first model
     */
    status = model1->initialize(model1, config_file);
    check_status(&status, "model2 initialize");

    /*
     * The second model
     */
    status = bmi_factory(&bmi_handle2);
    check_status(&status, "model2 factory");

    status = c_create_box(&box_handle2, &bmi_handle2);
    check_status(&status, "model2 create_box");

    /*
     * Registering Fortran model 2
     */
    register_bmi_fortran( model2, box_handle2 );
    /*
     * initialize the second model
     */
    status = model1->initialize(model2, config_file);
    check_status(&status, "model2 initialize");

    /*
     * component name of model 1
     */
    status = model1->get_component_name(model1, name);
    check_status(&status, "get_component_name");
    printf( "component name: %s\n", name);

    /*
     * get start time
     */
    status = model1->get_start_time(model1, &start_time);
    check_status(&status, "get_start_time");
    printf( "start time: %f\n", start_time);

    /*
     * get end time
     */
    status = model1->get_end_time(model1, &end_time);
    check_status(&status, "get_end_time");
    printf( "end time: %f\n", end_time);

    /*
     *  Run model 1 to the middel and pause
     */
    current_time = start_time;
    while( current_time <= pause_time )
    {
      status = model1->update(model1);
      check_status(&status, "update");
      status = model1->get_current_time(model1, &current_time);
      check_status(&status, "get_current_time");
      printf( "current time: %f\n", current_time);
    }

    printf( "Before serializing, comparing two models ...\n" );
    status = compare_states(model1, model2);
    //check_status(&status, "comparing beofre serialization");
    if ( status != BMI_SUCCESS )
    {
       printf( "Comparing failed! Model1 is not equal to Model2!\n" );
    }
    else
    {
       printf( "Comparing succeeded! Model1 is equal to Model2!\n" );
    }
    printf( "Done comparing before serializing.\n" );

    printf( "Now serialize the first model ... \n" );

    status = serialize( model1, ser_file );
    check_status(&status, "serialize model1");

    printf( "Now deserialize the first model to the second model ... \n" );
    status = deserialize_to_state(ser_file, model2, 1 );
    check_status(&status, "deserialize");

    printf( "After deserializing, comparing two models ...\n" );
    status = compare_states(model1, model2);
    if ( status != BMI_SUCCESS )
    {
       printf( "Comparing failed! Model1 is not equal to Model2!\n" );
    }
    else
    {
       printf( "Comparing succeeded! Model1 is equal to Model2!\n" );
    }
    printf( "Done comparing after deserializing.\n" );

    /*
     * Run model 1 to the end
     */
    while( current_time <= end_time )
    {
      status = model1->update(model1);
      check_status(&status, "update");
      status = model1->get_current_time(model1, &current_time);
      check_status(&status, "get_current_time");
      printf( "current time: %f\n", current_time);
    }

    /*
     * Run model 2 to the end
     */
    status = model2->get_current_time(model2, &current_time);
    check_status(&status, "get_current_time model 2");
    while( current_time <= end_time )
    {
      status = model2->update(model2);
      check_status(&status, "update model 2");
      status = model2->get_current_time(model2, &current_time);
      check_status(&status, "get_current_time");
      printf( "current time: %f\n", current_time);
    }

    /*
     * compare the two models again
     */
    printf( "After running to end, comparing two models ...\n" );
    status = compare_states(model1, model2);
    if ( status != BMI_SUCCESS )
    {
       printf( "Comparing failed! Model1 is not equal to Model2!\n" );
    }
    else
    {
       printf( "Comparing succeeded! Model1 is equal to Model2!\n" );
    }
    printf( "Done comparing after both running to end.\n" );
    /*
     * cleaning up
     */
    status = bmi_destroy(&bmi_handle);
    check_status(&status, "destroy model 1");

    status = c_delete_box(&box_handle);
    check_status(&status, "delete_box model 1 box");

    status = bmi_destroy(&bmi_handle2);
    check_status(&status, "destroy model 2");

    status = c_delete_box(&box_handle2);
    check_status(&status, "delete_box model 1 box");

    free(model1);
    free(model2);
    return(status);
}
