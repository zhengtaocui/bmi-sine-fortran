# bmi-heat

# Create shared library, except on Windows.
if(WIN32)
  add_library(${c_serializer_name} c_serializer.h c_serializer.c )
else()
  add_library(${c_serializer_name} SHARED c_serializer.h c_serializer.c)
endif()

target_link_libraries(${c_serializer_name} ${serializer_name} ${bmif_lib} ${msgpackc_lib})

install(
  TARGETS ${c_serializer_name}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin
  PUBLIC_HEADER DESTINATION include
)

install(
  FILES ${CMAKE_SOURCE_DIR}/c_serialization/c_serializer.h
  DESTINATION include
)
