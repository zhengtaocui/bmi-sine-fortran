include(CTest)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

function(make_test test_name)
  add_test(NAME ${test_name} COMMAND ${test_name})
  add_executable(${test_name} ${test_name}.f90 fixtures.f90)
  target_link_libraries(${test_name} ${bmi_name} ${Method1and2_name})
endfunction(make_test)

make_test(test_method1)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/sample.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/Method1_test)
