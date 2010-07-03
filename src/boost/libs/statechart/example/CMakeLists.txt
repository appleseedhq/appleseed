macro(statechart_example TESTNAME)
  set(SOURCES)
  foreach(ARG ${ARGN})
    set(SOURCES ${SOURCES} "${TESTNAME}/${ARG}.cpp")
  endforeach(ARG ${ARGN})

  if (NOT)
    set(SOURCES "${TESTNAME}/${TESTNAME}.cpp")
  endif (NOT)

  boost_test_link("${TESTNAME}Example"
    ${SOURCES}
    COMPILE_FLAGS "-DBOOST_DISABLE_THREADS"
    )
endmacro(statechart_example)

statechart_example(BitMachine)
#statechart_example(Camera
#  : Camera : Camera Configuring Main Shooting)
#statechart_example(Handcrafted : Handcrafted : Handcrafted)
#statechart_example(Keyboard : Keyboard : Keyboard :)
#statechart_example(Performance : Performance : Performance)
#statechart_example(PingPong : PingPong : PingPong)
#statechart_example(StopWatch : StopWatch : StopWatch)
#statechart_example(StopWatch2 : StopWatch : StopWatch2) ;
