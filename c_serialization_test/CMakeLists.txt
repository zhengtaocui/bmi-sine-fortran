include(CTest)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY} ${CMAKE_SOURCE_DIR}/serialization ${CMAKE_SOURCE_DIR}/c_serialization)

function(make_test test_name)
  add_test(NAME ${test_name} COMMAND ${test_name})
  add_executable(${test_name} ${test_name}.c)
  target_link_libraries(${test_name} ${bmi_name} ${serializer_name} ${c_serializer_name})
endfunction(make_test)

make_test(c_serialization_test)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/sample.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/c_serialization_test)
