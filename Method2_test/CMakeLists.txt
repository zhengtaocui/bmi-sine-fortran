include(CTest)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY} ${CMAKE_SOURCE_DIR}/serialization)

function(make_test test_name)
  add_test(NAME ${test_name} COMMAND ${test_name})
  add_executable(${test_name} ${test_name}.c)
#  target_include_directories(${test_name} PRIVATE ${CMAKE_SOURCE_DIR}/serialization)
  target_link_libraries(${test_name} ${bmi_name} ${serializer_name})
endfunction(make_test)

make_test(test_serialize_c)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/sample.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/Method2_test)
