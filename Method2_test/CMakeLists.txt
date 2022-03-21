include(CTest)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY} ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/Method1_and_Method2_src)

function(make_test test_name)
  add_test(NAME ${test_name} COMMAND ${test_name})
  add_executable(${test_name} ${test_name}.c)
  target_link_libraries(${test_name} ${bmi_name} ${Method1and2_name})
endfunction(make_test)

make_test(test_method2)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/sample.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/Method2_test)
