# Copyright David Abrahams 2004. Distributed under the Boost
# Software License, Version 1.0. (See accompanying
# file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
r"""
>>> from builtin_converters_ext import *

# Synthesize idendity functions in case long long not supported
>>> if not 'rewrap_value_long_long' in dir():
...     def rewrap_value_long_long(x): return long(x)
...     def rewrap_value_unsigned_long_long(x): return long(x)
...     def rewrap_const_reference_long_long(x): return long(x)
...     def rewrap_const_reference_unsigned_long_long(x): return long(x)

>>> try: bool_exists = bool
... except: pass
... else:
...     rewrap_value_bool(True)
...     rewrap_value_bool(False)
True
False

>>> rewrap_value_bool(None)
0
>>> rewrap_value_bool(0)
0
>>> rewrap_value_bool(33)
1
>>> rewrap_value_char('x')
'x'

  Note that there's currently silent truncation of strings passed to
  char arguments.

>>> rewrap_value_char('xy')
'x'
>>> rewrap_value_signed_char(42)
42
>>> rewrap_value_unsigned_char(42)
42
>>> rewrap_value_int(42)
42
>>> rewrap_value_unsigned_int(42)
42
>>> rewrap_value_short(42)
42
>>> rewrap_value_unsigned_short(42)
42
>>> rewrap_value_long(42)
42
>>> rewrap_value_unsigned_long(42)
42

    test unsigned long values which don't fit in a signed long.
    strip any 'L' characters in case the platform has > 32 bit longs
        
>>> hex(rewrap_value_unsigned_long(0x80000001L)).replace('L','')
'0x80000001'

>>> rewrap_value_long_long(42)
42L
>>> rewrap_value_unsigned_long_long(42)
42L

   show that we have range checking. 
 
>>> try: rewrap_value_unsigned_short(-42)
... except OverflowError: pass
... else: print 'expected an OverflowError!'

>>> try: rewrap_value_int(sys.maxint * 2)
... except OverflowError: pass
... else: print 'expected an OverflowError!'


>>> assert abs(rewrap_value_float(4.2) - 4.2) < .000001
>>> rewrap_value_double(4.2) - 4.2
0.0
>>> rewrap_value_long_double(4.2) - 4.2
0.0

>>> assert abs(rewrap_value_complex_float(4+.2j) - (4+.2j)) < .000001
>>> assert abs(rewrap_value_complex_double(4+.2j) - (4+.2j)) < .000001
>>> assert abs(rewrap_value_complex_long_double(4+.2j) - (4+.2j)) < .000001

>>> rewrap_value_cstring('hello, world')
'hello, world'
>>> rewrap_value_string('yo, wassup?')
'yo, wassup?'

>>> try:
...     if unicode: pass
... except:
...     print "u'yo, wassup?'"
... else:
...     eval("rewrap_value_wstring(u'yo, wassup?')")
u'yo, wassup?'
    
   test that overloading on unicode works:

>>> try:
...     if unicode: pass
... except:
...     print "u'yo, wassup?'"
... else:
...     eval("rewrap_value_string(u'yo, wassup?')")
u'yo, wassup?'

   wrap strings with embedded nulls:
   
>>> rewrap_value_string('yo,\0wassup?')
'yo,\x00wassup?'

>>> rewrap_value_handle(1)
1
>>> x = 'hi'
>>> assert rewrap_value_handle(x) is x
>>> assert rewrap_value_object(x) is x

  Note that we can currently get a mutable pointer into an immutable
  Python string:
  
>>> rewrap_value_mutable_cstring('hello, world')
'hello, world'

>>> rewrap_const_reference_bool(None)
0
>>> rewrap_const_reference_bool(0)
0

>>> try: rewrap_const_reference_bool('yes')
... except TypeError: pass
... else: print 'expected a TypeError exception'

>>> rewrap_const_reference_char('x')
'x'

  Note that there's currently silent truncation of strings passed to
  char arguments.

>>> rewrap_const_reference_char('xy')
'x'
>>> rewrap_const_reference_signed_char(42)
42
>>> rewrap_const_reference_unsigned_char(42)
42
>>> rewrap_const_reference_int(42)
42
>>> rewrap_const_reference_unsigned_int(42)
42
>>> rewrap_const_reference_short(42)
42
>>> rewrap_const_reference_unsigned_short(42)
42
>>> rewrap_const_reference_long(42)
42
>>> rewrap_const_reference_unsigned_long(42)
42
>>> rewrap_const_reference_long_long(42)
42L
>>> rewrap_const_reference_unsigned_long_long(42)
42L


>>> assert abs(rewrap_const_reference_float(4.2) - 4.2) < .000001
>>> rewrap_const_reference_double(4.2) - 4.2
0.0
>>> rewrap_const_reference_long_double(4.2) - 4.2
0.0

>>> assert abs(rewrap_const_reference_complex_float(4+.2j) - (4+.2j)) < .000001
>>> assert abs(rewrap_const_reference_complex_double(4+.2j) - (4+.2j)) < .000001
>>> assert abs(rewrap_const_reference_complex_long_double(4+.2j) - (4+.2j)) < .000001

>>> rewrap_const_reference_cstring('hello, world')
'hello, world'
>>> rewrap_const_reference_string('yo, wassup?')
'yo, wassup?'

>>> rewrap_const_reference_handle(1)
1
>>> x = 'hi'
>>> assert rewrap_const_reference_handle(x) is x
>>> assert rewrap_const_reference_object(x) is x
>>> assert rewrap_reference_object(x) is x


Check that None <==> NULL

>>> rewrap_const_reference_cstring(None)

But None cannot be converted to a string object:

>>> try: rewrap_const_reference_string(None)
... except TypeError: pass
... else: print 'expected a TypeError exception'

Now check implicit conversions between floating/integer types

>>> rewrap_const_reference_float(42)
42.0

>>> rewrap_const_reference_float(42L)
42.0

>>> try: rewrap_const_reference_int(42.0)
... except TypeError: pass
... else: print 'expected a TypeError exception'

>>> rewrap_value_float(42)
42.0

>>> try: rewrap_value_int(42.0)
... except TypeError: pass
... else: print 'expected a TypeError exception'

Check that classic classes also work

>>> class FortyTwo:
...     def __int__(self):
...         return 42
...     def __float__(self):
...         return 42.0
...     def __complex__(self):
...         return complex(4+.2j)
...     def __str__(self):
...         return '42'

>>> try: rewrap_const_reference_float(FortyTwo())
... except TypeError: pass
... else: print 'expected a TypeError exception'

>>> try: rewrap_value_int(FortyTwo())
... except TypeError: pass
... else: print 'expected a TypeError exception'

>>> try: rewrap_const_reference_string(FortyTwo())
... except TypeError: pass
... else: print 'expected a TypeError exception'

>>> try: rewrap_value_complex_double(FortyTwo())
... except TypeError: pass
... else: print 'expected a TypeError exception'

# show that arbitrary handle<T> instantiations can be returned
>>> assert get_type(1) is type(1)

>>> assert return_null_handle() is None
"""

def run(args = None):
    import sys
    import doctest
    import builtin_converters_ext
    
    if 'rewrap_value_long_long' in dir(builtin_converters_ext):
        print 'LONG_LONG supported, testing...'
    else:
        print 'LONG_LONG not supported, skipping those tests...'
        
    if args is not None:
        sys.argv = args
    return doctest.testmod(sys.modules.get(__name__))
    
if __name__ == '__main__':
    print "running..."
    import sys
    status = run()[0]
    if (status == 0): print "Done."
    sys.exit(status)
