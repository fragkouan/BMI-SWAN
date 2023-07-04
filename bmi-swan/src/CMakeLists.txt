add_library(${bmiswan_lib} SHARED
bmi.f90 bmi.f90 bmi_swan.f90 m_swan.f90)
target_link_libraries(${bmiswan_lib} ${EXTRA_LIBS})

install(
  TARGETS ${bmiswan_lib}
  EXPORT ${bmiswan_lib}-config
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  INCLUDES DESTINATION include
)

install(
  DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
  DESTINATION include
  FILES_MATCHING PATTERN "*.mod"
  PATTERN "CMakeFiles" EXCLUDE)

install(
  EXPORT ${bmiswan_lib}-config
  DESTINATION lib/cmake)
export(
  TARGETS ${bmipswan_lib}
FILE ${bmiswan_lib}-config.cmake)
