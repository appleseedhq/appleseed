

.. Copyright Aleksey Gurtovoy, David Abrahams 2007.
.. Distributed under the Boost
.. Software License, Version 1.0. (See accompanying
.. file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
From the implementation standpoint, iterators are almost-opaque types which
guarantee to provide us with the only memeber that we can access directly:
their category. Incrementing, dereferencing and the rest of iterator 
functionality is available to us through the accosiated iterator 
metafunctions.

