# bmi-sine-fortran
A minimal BMI Fortran model using the Sine function to explore Fortran BMI serialization approaches.

It dependents on the bmi-fortran and msgpack-c library.

Here, we explored 4 methods to implement a model serializer for BMI Fortran models. Each method will be evaluated based on its own pros and cons. A testing case for each method is also included to test these approaches using a mimial dummy Fortran BMI model. Here is a brief description of these 4 methods,

Method 1: Using codes in Fortran language and msgpack-c library to serialize and deserialize the model states. The application program should also use Fortran. C language is only used when the Fortran serializer/deserializer interacts with msgpack-c APIs.

Method 2: Using codes in Fortran language and msgpack-c library to serialize/deserialize the model states. C wrappers functions using ISO C binding were developed for those Fortran serialization/deserialization codes. Thereforre, the serialization/deserialization codes developed in Fortran can be utilized by C/C++ framework code. A test case developed in C language is incldued to demonstrate how these ISO C Binding APIs can be used in the NextGen framework.

Method 3: Using codes in C language to serialize/deserialize the Fortran model states. ISO C binding codes for Fortran BMI are needed by the C serializer/deserializer to run the Fortran model, get and set model properties. A test case using C language was included. Here the serializer code uses get_value_* functions to retrieve the model states. That is a copy of the state values is returned.

Method 4: This method combine the C serialization/deserialization code developed for the C CFE model at https://github.com/NOAA-OWP/cfe.git/test_serialize/serialize_state.c with the serialization code developed in Method 3. Therefore, both C and Fortran BMI models can use the same serialization/deserialization codes developed in this approach. For this method to work, C wrapper codes for the BMI ISO C binding codes are necessary to create compatiable C BMI struct objects between C and Fortran models. The test case is programed in C language. Here the serializer code uses get_value_ptr_* functions to retrieve the model states. That is the pointers of the state values are returned.

Explaination of directories structures:

sine -- The minimal dummy Fortran model with testing data and program.

bmi_sine -- The Fortran BMI implementation of of the 'sine' model with testing data and program.

examples -- Example configuration files for the Sine model.

cfe -- The CFE model code and its C BMI code. 

Method1_and_Method2_src -- Source codes of the Fortran version of serialization/deserialzation code, i.e. Method 1, and it's C wrapper codes, i.e., Method 2.

Method1_test -- Test data and program for Method 1.

Method2_test -- Test data and program for Method 2.

Method3_src -- The source code that uses C language to serialize/deserialize the Fortran model states, i.e., Method 3.

Method3_test -- Testing data and program for Method 3.

Method4_src -- The combined C serialize/deserialize code from both Method 3 and  the CFE serialization code, i.e., Method 4.

Method4_test -- Test data and program for Method 4.

include -- Include files.

Prerequisites:

1) msgpack-c
3) bmi-fortran at https://github.com/zhengtaocui/bmi-fortran.git
4) Gnu compilers 
5) CMake

How to build:

mkdir _build

cd _build

cmake .. -DCMAKE_BUILD_TYPE=Debug -DCMAKE_INSTALL_PREFIX=/path/to/bmi-fortran -DCMAKE_LIBRARY_PATH=/path/to/msgpack-c  -DCMAKE_INCLUDE_PATH=/path/to/msgpack-c/include

make

make install

cd ../

Note: The CMAKE_INSTALL_PREFIX variable should point to the same install directory of the bmi-fortran package.

How to run tests:

Run Method 1 test:
  cd _build/Method1_test
  ./test_method1 ./sample.cfg

Run Method 2 test:
  cd _build/Method2_test
  ./test_method2 ./sample.cfg

Run Method 3 test:
  cd _build/Method3_test
  ./test_method3 ./sample.cfg

Run Method 4 test:
  cd _build/Method4_test
  ./test_method4
