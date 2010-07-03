.. Data Types/Numeric//long_ |30

.. Copyright Aleksey Gurtovoy, David Abrahams 2007.
.. Distributed under the Boost
.. Software License, Version 1.0. (See accompanying
.. file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

long\_
======

Synopsis
--------

.. parsed-literal::
    
    template<
          long N
        >
    struct long\_
    {
        // |unspecified|
        // ...
    };


Description
-----------

An |Integral Constant| wrapper for ``long``.


Header
------

.. parsed-literal::
    
    #include <boost/mpl/long.hpp>


Model of
--------

|Integral Constant|


Parameters
----------

+---------------+-------------------------------+---------------------------+
| Parameter     | Requirement                   | Description               |
+===============+===============================+===========================+
| ``N``         | An integral constant          | A value to wrap.          | 
+---------------+-------------------------------+---------------------------+

Expression semantics
--------------------

|Semantics disclaimer...| |Integral Constant|.

For arbitrary integral constant ``n``:

+-------------------+-----------------------------------------------------------+
| Expression        | Semantics                                                 |
+===================+===========================================================+
| ``long_<c>``      | An |Integral Constant| ``x`` such that ``x::value == c``  |
|                   | and ``x::value_type`` is identical to ``long``.           |
+-------------------+-----------------------------------------------------------+


Example
-------

.. parsed-literal::

    typedef long_<8> eight;
    
    BOOST_MPL_ASSERT(( is_same< eight::value_type, long > ));
    BOOST_MPL_ASSERT(( is_same< eight::type, eight > ));
    BOOST_MPL_ASSERT(( is_same< next< eight >::type, long_<9> > ));
    BOOST_MPL_ASSERT(( is_same< prior< eight >::type, long_<7> > ));
    BOOST_MPL_ASSERT_RELATION( (eight::value), ==, 8 );
    assert( eight() == 8 );


See also
--------

|Data Types|, |Integral Constant|, |int_|, |size_t|, |integral_c|

