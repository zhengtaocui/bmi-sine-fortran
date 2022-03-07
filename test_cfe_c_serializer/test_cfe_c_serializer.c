/** ----------------------------------------------
  * test_cfe_c_serializer.c
  * ----------------------------------------------
  * auther: Zhengtao Cui
  * created on Feb. 24, 2022
  * Last date of modification: Feb 24, 2022
  * Reference: https://github.com/NOAA-OWP/cfe.git
  *            test_serialize/serialize_state.c
  *
  * Description: test program for the the CFE C version of the 
  *             serialization/deserialization code in 
  *             https://github.com/NOAA-OWP/cfe.git
  */
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

#include "iso_c_bmif_2_0.h"
#include "bmi_fortran.h"
#include "serialize_state.h"
#include "test_fortran_model.h"

int main(int argc, char** argv)
{
   int status;

   status = test_fortran_model();

   printf("Done testing the Fortran BMI model!\n");

}
