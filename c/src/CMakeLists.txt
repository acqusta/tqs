project(tqslib)

set(SRC
  bt/backtest.cpp
  bt/backtest.h
  rt/realtime.cpp
  rt/realtime.h
  bt/sim_context.cpp
  bt/sim_context.h
  bt/sim_data.cpp
  bt/sim_data.h
  bt/sim_trade.cpp
  bt/sim_trade.h
  bt/demo.cpp
  bt/demo_rbreaker.cpp
  stralet.cpp
  stralet.h    
)

include_directories(.)

#add_library(tqslib STATIC ${SRC})
add_executable(tqs_demo ${SRC})

target_link_libraries(tqs_demo tqapi)

install(TARGETS tqs_demo RUNTIME DESTINATION bin)