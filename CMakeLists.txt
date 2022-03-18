# bmi-example-fortran

cmake_minimum_required(VERSION 3.0)

project(bmi-sine-fortran Fortran C)

set(model_name sinef)
set(Method1and2_name Method1and2)
set(Method3_name Method3)
set(bmi_name bmi${model_name})
set(Method4_name Method4)
set(bmi_cfe_name bmi_cfe)

# Determine the Fortran BMI version.
if(DEFINED ENV{BMIF_VERSION})
  set(bmif_version $ENV{BMIF_VERSION})
else()
  set(bmif_version "2.0")
endif()
string(REPLACE "." "_" bmif_module_version ${bmif_version})
message("-- BMIF version - ${bmif_version}")
message("-- BMIF module version - ${bmif_module_version}")

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

# Locate the installed Fortran BMI bindings (bmif library and module file)
# through CMAKE_PREFIX_PATH.
find_library(bmif_lib bmif)
find_path(bmif_inc bmif_${bmif_module_version}.mod)
include_directories(${bmif_inc})
message("-- bmif_lib - ${bmif_lib}")
message("-- bmif_inc - ${bmif_inc}")

find_library(msgpackc_lib msgpackc)
find_path(msgpackc_inc msgpack.h)
include_directories(${msgpackc_inc})
message("-- msgpackc_lib - ${msgpackc_lib}")
message("-- msgpackc_inc - ${msgpackc_inc}")

add_subdirectory(sine)
add_subdirectory(bmi_sine)
add_subdirectory(Method1_and_Method2_src)
add_subdirectory(Method3_src)
add_subdirectory(Method1_test)
add_subdirectory(Method2_test)
add_subdirectory(Method3_test)
add_subdirectory(Method4_src)
add_subdirectory(Method4_test)
add_subdirectory(cfe)
#add_subdirectory(examples)

include(CTest)
