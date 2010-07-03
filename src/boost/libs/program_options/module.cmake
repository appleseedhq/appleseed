boost_module(program_options DEPENDS any bind smart_ptr tokenizer)

# bind is needed because of a dependency on boost/mem_fn.hpp
