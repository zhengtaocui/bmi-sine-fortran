# bmi-heat

# Create shared library, except on Windows.
if(WIN32)
  add_library(${c_serializer_name} c_serializer.h c_serializer.c c_deserializer.h c_deserializer.c c_model_compare.h c_model_compare.c)
else()
  add_library(${c_serializer_name} SHARED c_serializer.h c_serializer.c c_deserializer.h c_deserializer.c c_model_compare.h c_model_compare.c)
endif()

#target_include_directories(${c_serializer_name} PRIVATE ${CMAKE_SOURCE_DIR}/Method1_and_Method2_src)
target_link_libraries(${c_serializer_name} ${serializer_name} ${bmif_lib} ${msgpackc_lib})

install(
  TARGETS ${c_serializer_name}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin
  PUBLIC_HEADER DESTINATION include
)

install(
  FILES ${CMAKE_SOURCE_DIR}/Method3_src/c_serializer.h
  DESTINATION include
)

install(
  FILES ${CMAKE_SOURCE_DIR}/Method3_src/c_deserializer.h
  DESTINATION include
)

install(
  FILES ${CMAKE_SOURCE_DIR}/Method3_src/c_model_compare.h
  DESTINATION include
)
