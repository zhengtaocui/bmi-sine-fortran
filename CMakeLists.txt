# bmi-example-fortran

cmake_minimum_required(VERSION 3.0)

project(bmi-sine-fortran Fortran C)

set(model_name sinef)
set(serializer_name serializerf)
set(c_serializer_name serializerc)
set(bmi_name bmi${model_name})
set(cfe_c_serializer cfecserial)
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
add_subdirectory(serialization)
add_subdirectory(c_serialization)
add_subdirectory(serialization_tests)
add_subdirectory(serialization_test_c)
add_subdirectory(c_serialization_test)
add_subdirectory(cfe_c_serializer)
add_subdirectory(test_cfe_c_serializer)
add_subdirectory(cfe)
#add_subdirectory(examples)

include(CTest)
