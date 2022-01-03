# bmi-heat

# Create shared library, except on Windows.
if(WIN32)
  add_library(${serializer_name} serializer.f90 state_serializer.f90, ut_trim.h ut_trim.c c_wrapper_test.c c_serialize.c)
else()
  add_library(${serializer_name} SHARED serializer.f90 state_serializer.f90 ut_trim.h ut_trim.c c_wrapper_test.c c_serialize.c)
endif()
target_link_libraries(${serializer_name} ${bmif_lib} ${msgpackc_lib})

install(
  TARGETS ${serializer_name}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin
)
install(
  FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/serialization.mod
  DESTINATION include
)
install(
  FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/state_serialization.mod
  DESTINATION include
)
