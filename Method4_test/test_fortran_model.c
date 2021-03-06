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
#include <time.h>

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

    clock_t t0 = clock();
    clock_t t1;
    /*
     * The first model
     */
    status = register_bmi(&box_handle);
    check_status(&status, "get model1 box handle");

    printf( "config file: %s\n", config_file);

    /*
     * Registering Fortran model 1
     */
    create_bmi_fortran_model_handle( model1, box_handle );

    /*
     * initialize the first model
     */
    status = model1->initialize(model1, config_file);
    check_status(&status, "model2 initialize");

    /*
     * The second model
     */
    status = register_bmi(&box_handle2);
    check_status(&status, "model2 create_box");

    /*
     * Registering Fortran model 2
     */
    create_bmi_fortran_model_handle( model2, box_handle2 );
    /*
     * initialize the second model
     */
    status = model1->initialize(model2, config_file);
    check_status(&status, "model2 initialize");

    t1 = clock();
    printf( "Model initialization time = %f seconds\n",
		     ( (double)( t1 - t0 ) ) / CLOCKS_PER_SEC );
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
    status = deserialize_to_state(ser_file, model2, 0 );
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
    status = unregister_bmi(&box_handle);
    check_status(&status, "delete_box model 1 box");

    status = unregister_bmi(&box_handle2);
    check_status(&status, "delete_box model 2 box");

    free(model1);
    free(model2);

    printf( "Model run time = %f seconds\n",
		     ( (double)( clock() - t1 ) ) / CLOCKS_PER_SEC );
    printf( "Total program run time = %f seconds\n",
		     ( (double)( clock() - t0 ) ) / CLOCKS_PER_SEC );
    return(status);
}
