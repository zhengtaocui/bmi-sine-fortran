include(CTest)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY} ${CMAKE_SOURCE_DIR}/serialization ${CMAKE_SOURCE_DIR}/include)

function(make_test test_name)
  add_test(NAME ${test_name} COMMAND ${test_name})
  add_executable(${test_name} ${test_name}.c test_fortran_model.c test_c_cfe_model.c)
  target_link_libraries(${test_name} ${bmi_name} ${model_name} ${cfe_c_serializer} ${bmi_cfe_name})
endfunction(make_test)

make_test(test_cfe_c_serializer)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/cat89_01Dec2015.csv
  DESTINATION ${CMAKE_BINARY_DIR}/test_cfe_c_serializer)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/cat_89_bmi_config_cfe.txt
  DESTINATION ${CMAKE_BINARY_DIR}/test_cfe_c_serializer)
