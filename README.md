# bmi-sine-fortran
A minimal BMI Fortran model using the Sine function

It dependents on the bmi-fortran and msgpack-c library.

Here, we explored 4 approaches to implement a model serialiser for BMI Fortran models. Each approach has its own pros and cons. A testing code is also included to test these approaches using the mimial dummy 
Fortran BMI model. Here are the 4 approaches,
Approach 1: Using Fortran and msgpack-c for serialization/deserialiation of the model states. Testing program is also in Fortran. C language is only used when calling msgpack-c procedures.
Approach 2: Using Fortran and msgpack-c for serialization/deserialization of the model states, but created C wrappers using ISO C binding for those Fortran serialization/deserialization codes, such the they can be used by C/C++ framework code. The testing code is in C language.
Approach 3: Using C language for serialization/deserialization of the Fortran model states. The Fortran BMI ISO C binding codes are needed by the C serializer to run, get and set Fortran model. The testing code is in C.
Approach 4: Using the C serialization/deserialization code created for the CFE model at https://github.com/NOAA-OWP/cfe.git/test_serialize/serialize_state.c to serialize/deserialize the Fortran model states. C wrappers for the BMI ISO C binding codes are used to create the compatiable C BMI struct object for CFE. The testing code is in C. 

Explaination of directories structures:
sine -- The minimal dummy Fortran model with testing data and program
bmi_sine -- The Fortran BMI implementation of of the 'sine' model with testing data and program
serialization -- The Fortran version of serialization/deserialzation code, i.e. Approach 1, and it's C wrapper codes, i.e., Approach 2
serialization_tests -- Testing data and program for Approach 1.
serialization_test_c -- Testing data and program for Approach 2.
c_serialization -- The version that uses C language to serialize/deserialize the Fortran model states, i.e., Approach 3
c_serialization_c -- Testing data and program for Approach 3
cfe_serializer -- The C serialize/deserialize code for CFE, i.e., Approach 4
test_cfe_c_serializer -- Testing data and program for Approach 4.

Prerequisites:
1) msgpack-c
2) bmi-fortran at https://github.com/zhengtaocui/bmi-fortran.git
3) Gnu compilers 
4) CMake

How to build:
mkdir _build
cd _build
cmake .. -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=/path/to/bmi-fortran -DCMAKE_LIBRARY_PATH=/path/to/msgpack-c  -DCMAKE_INCLUDE_PATH=/path/to/msgpack-c/include
make
make install
cd ../
