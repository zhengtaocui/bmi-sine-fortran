# bmi-cfe

include_directories(${CMAKE_SOURCE_DIR}/include ${bmif_inc})
# Create shared library, except on Windows.
if(WIN32)
  add_library(${bmi_cfe_name} bmi_cfe.c cfe.c)
else()
  add_library(${bmi_cfe_name} SHARED bmi_cfe.c cfe.c)
endif()
target_link_libraries(${bmi_cfe_name} ${cfe_name})

add_executable(run_${bmi_cfe_name} main.c)
target_link_libraries(run_${bmi_cfe_name} ${bmi_cfe_name})

install(
  TARGETS run_${bmi_cfe_name}
  RUNTIME DESTINATION bin
)
install(
  TARGETS ${bmi_cfe_name}
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
  RUNTIME DESTINATION bin
)
install(
  FILES ${CMAKE_SOURCE_DIR}/include/cfe.h ${CMAKE_SOURCE_DIR}/include/bmi_cfe.h 
  DESTINATION include
)
