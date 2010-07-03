//  Copyright (c) 2003 Vladimir Prus
//
//  Distributed under the Boost Software License, Version 1.0. (See
//  accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
//  http://www.boost.org
// 

// This file will declare 'foo' is 'MACROS' is defined.

#ifdef MACROS
void
# ifdef _WIN32
__declspec(dllexport)
# endif 
foo() {}
#endif

# ifdef _WIN32
int __declspec(dllexport) force_implib_creation;
# endif 
