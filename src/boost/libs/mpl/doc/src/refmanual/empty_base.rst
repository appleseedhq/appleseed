.. Data Types/Miscellaneous//empty_base |20

.. Copyright Aleksey Gurtovoy, David Abrahams 2007.
.. Distributed under the Boost
.. Software License, Version 1.0. (See accompanying
.. file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

empty_base
==========

Synopsis
--------

.. parsed-literal::
    
    struct empty_base {};


Description
-----------

An empty base class. Inheritance from |empty_base| through the |inherit| 
metafunction is a no-op.


Header
------

.. parsed-literal::
    
    #include <boost/mpl/empty_base.hpp>


See also
--------

|Data Types|, |inherit|, |inherit_linearly|, |void_|
