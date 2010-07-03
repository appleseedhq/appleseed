boost_additional_test_dependencies(spirit BOOST_DEPENDS test variant function_types intrusive lambda assign iostreams math random dynamic_bitset fusion)
message(STATUS "Boost/libs/spirit/test needs GCC C++ -ftemplate-depth-300 flag")

message(STATUS "Check libs/range/test: need -Wall -Wunused here when under gcc")
set( test_compile_flags "")
IF(CMAKE_COMPILER_IS_GNUCC)
  SET(test_compile_flags "-ftemplate-depth-300")
ENDIF(CMAKE_COMPILER_IS_GNUCC)

boost_test_run(qi_actions qi/actions.cpp COMPILE_FLAGS ${test_compile_flags} BOOST_LIB spirit COMPILE_FLAGS ${test_compile_flags} )
boost_test_run(qi_char qi/char.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_char_class qi/char_class.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_lit qi/lit.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_int qi/int.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_uint qi/uint.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_real qi/real.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_eps qi/eps.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_none qi/none.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_lazy qi/lazy.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_tst qi/tst.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_symbols qi/symbols.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_range_run qi/range_run.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_no_case qi/no_case.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_lexeme qi/lexeme.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_raw qi/raw.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_sequence qi/sequence.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_alternative qi/alternative.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_sequential_or qi/sequential_or.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_permutation qi/permutation.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_difference qi/difference.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_list qi/list.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_optional qi/optional.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_kleene qi/kleene.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_plus qi/plus.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_and_predicate qi/and_predicate.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_not_predicate qi/not_predicate.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_expect qi/expect.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_rule qi/rule.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_grammar qi/grammar.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_functor qi/functor.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_match_manip qi/match_manip.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_binary qi/binary.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_debug qi/debug.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(qi_end qi/end.cpp COMPILE_FLAGS ${test_compile_flags})

boost_test_compile_fail(qi_grammar_fail qi/grammar_fail.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_compile_fail(qi_rule_fail qi/rule_fail.cpp COMPILE_FLAGS ${test_compile_flags})

    # run Karma tests
boost_test_run(karma_actions karma/actions.cpp COMPILE_FLAGS ${test_compile_flags}) #              : : : : karma_actions ]
boost_test_run(karma_alternative karma/alternative.cpp COMPILE_FLAGS ${test_compile_flags}) #              : : : : karma_alternative ]
boost_test_run(karma_binary karma/binary.cpp COMPILE_FLAGS ${test_compile_flags}) #                   : : : : karma_binary ]
boost_test_run(karma_case_handling karma/case_handling.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(karma_center_alignment karma/center_alignment.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(karma_char karma/char.cpp COMPILE_FLAGS ${test_compile_flags}) #                     : : : : karma_char ]
boost_test_run(karma_delimieter karma/delimiter.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(karma_eol karma/eol.cpp COMPILE_FLAGS ${test_compile_flags}) #                      : : : : karma_eol ]
boost_test_run(karma_eps karma/eps.cpp COMPILE_FLAGS ${test_compile_flags}) #                      : : : : karma_eps ]
boost_test_run(karma_format_manip karma/format_manip.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(karma_functor karma/functor.cpp COMPILE_FLAGS ${test_compile_flags}) #                  : : : : karma_functor ]
boost_test_run(karma_grammar karma/grammar.cpp COMPILE_FLAGS ${test_compile_flags}) #                  : : : : karma_grammar ]
boost_test_run(karma_int_numerics karma/int_numerics.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(karma_kleene karma/kleene.cpp COMPILE_FLAGS ${test_compile_flags}) #                   : : : : karma_kleene ]
boost_test_run(karma_lazy karma/lazy.cpp COMPILE_FLAGS ${test_compile_flags}) #                     : : : : karma_lazy ]
boost_test_run(karma_left_alignment karma/left_alignment.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(karma_list karma/list.cpp COMPILE_FLAGS ${test_compile_flags}) #                     : : : : karma_list ]
boost_test_run(karma_lit karma/lit.cpp COMPILE_FLAGS ${test_compile_flags}) #                      : : : : karma_lit ]
boost_test_run(karma_none karma/none.cpp COMPILE_FLAGS ${test_compile_flags}) #                     : : : : karma_none ]
boost_test_run(karma_optional karma/optional.cpp COMPILE_FLAGS ${test_compile_flags}) #                 : : : : karma_optional ]
boost_test_run(karma_pattern karma/pattern.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(karma_real_numerics karma/real_numerics.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(karma_right_alignment karma/right_alignment.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(karma_sequence karma/sequence.cpp COMPILE_FLAGS ${test_compile_flags}) #                 : : : : karma_sequence ]

boost_test_compile_fail(karma_grammar_fail karma/grammar_fail.cpp COMPILE_FLAGS ${test_compile_flags}) #   : : karma_grammar_fail ]
boost_test_compile_fail(karma_rule_fail karma/rule_fail.cpp COMPILE_FLAGS ${test_compile_flags}) #      : : karma_rule_fail ]

    # run support tests
boost_test_run(support_hold_any support/hold_any.cpp COMPILE_FLAGS ${test_compile_flags})
# the multi_pass tests are not completed yet
#    [ run support/multi_pass_compile.cpp COMPILE_FLAGS ${test_compile_flags}      : : : : ]
#    [ run support/multi_pass.cpp COMPILE_FLAGS ${test_compile_flags}              : : : : ]

    # run lexer tests
boost_test_run(lex_lexertl1 lex/lexertl1.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(lex_lexertl2 lex/lexertl2.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(lex_lexertl3 lex/lexertl3.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(lex_lexertl4 lex/lexertl4.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(lex_lexertl5 lex/lexertl5.cpp COMPILE_FLAGS ${test_compile_flags})
boost_test_run(lex_state_switcher_test lex/state_switcher_test.cpp COMPILE_FLAGS ${test_compile_flags})




