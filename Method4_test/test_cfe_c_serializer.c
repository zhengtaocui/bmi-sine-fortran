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
  * Last modified on March 9, 2022. Added test code for C CFE model
  */
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

#include "iso_c_bmif_2_0.h"
#include "bmi_fortran.h"
#include "serialize_state.h"
#include "test_fortran_model.h"
#include "test_c_cfe_model.h"

int main(int argc, char** argv)
{
   int status;

   status = test_fortran_model();

   printf("Done testing the Fortran BMI model!\n");

   status = test_c_cfe_model();

   printf("Done testing the C BMI model (CFE)!\n");
}
