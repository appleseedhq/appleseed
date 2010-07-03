// Copyright David Abrahams 2002.
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#if NOCOMPILE
1 = 1;
#endif 

void lib();

void f()
{
    lib();
}

#ifndef NOLINK
int main()
{
}
#endif

