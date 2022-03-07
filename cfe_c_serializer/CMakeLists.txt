# bmi-heat

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY} {CMAKE_SOURCE_DIR}/include)

# Create shared library, except on Windows.
if(WIN32)
	add_library(${cfe_c_serializer} ${CMAKE_SOURCE_DIR}/include/serialize_state.h serialize_state.c)
else()
	add_library(${cfe_c_serializer} SHARED ${CMAKE_SOURCE_DIR}/include/serialize_state.h serialize_state.c)
endif()

target_include_directories(${cfe_c_serializer} PRIVATE ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(${cfe_c_serializer} ${msgpackc_lib})

install(
  TARGETS ${cfe_c_serializer}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin
  PUBLIC_HEADER DESTINATION include
)

install(
  FILES ${CMAKE_SOURCE_DIR}/include/serialize_state.h
  DESTINATION include
)

