project(api_c)

set(SRC
    tquant_api.h
    impl_tquant_api.h
    impl_tquant_api.cpp
    impl_data_api.h
    impl_trade_api.h
)

add_library(tqapi-static STATIC ${SRC})

target_link_libraries(tqapi-static
  myutils
  msgpack
  snappy
  ${COMMON_LIB}
)

if (NOT BUILD_TQS)
  add_library(tqapi SHARED ${SRC})
  
  target_link_libraries(tqapi
    myutils
    msgpack
    snappy
    ${COMMON_LIB}
  )
  
  install(TARGETS tqapi DESTINATION cpp)

  target_compile_definitions(tqapi PRIVATE _TQAPI_DLL=1)

  set(SRC
    tquant_api_test.cpp
  )

 add_executable(tqapi_test ${SRC})
 
 target_link_libraries(tqapi_test tqapi-static)
 
 install(TARGETS tqapi_test DESTINATION bin)
endif()
 
 install(FILES tquant_api.h DESTINATION cpp)
 