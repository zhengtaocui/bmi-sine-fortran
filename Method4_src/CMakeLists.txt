# bmi-heat

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY} {CMAKE_SOURCE_DIR}/include)

# Create shared library, except on Windows.
if(WIN32)
	add_library(${Method4_name} ${CMAKE_SOURCE_DIR}/include/serialize_state.h serialize_state.c)
else()
	add_library(${Method4_name} SHARED ${CMAKE_SOURCE_DIR}/include/serialize_state.h serialize_state.c)
endif()

target_include_directories(${Method4_name} PRIVATE ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(${Method4_name} ${msgpackc_lib})

install(
  TARGETS ${Method4_name}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin
  PUBLIC_HEADER DESTINATION include
)

install(
  FILES ${CMAKE_SOURCE_DIR}/include/serialize_state.h
  DESTINATION include
)

