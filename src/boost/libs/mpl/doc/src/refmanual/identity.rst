.. Metafunctions/Miscellaneous//identity |10

.. Copyright Aleksey Gurtovoy, David Abrahams 2007.
.. Distributed under the Boost
.. Software License, Version 1.0. (See accompanying
.. file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

identity
========

Synopsis
--------

.. parsed-literal::
    
    template< 
          typename X
        >
    struct identity
    {
        typedef X type;
    };


Description
-----------

The `identity`__ metafunction. Returns ``X`` unchanged.

__ http://mathworld.wolfram.com/IdentityFunction.html


Header
------

.. parsed-literal::
    
    #include <boost/mpl/identity.hpp>


Model of
--------

|Metafunction|


Parameters
----------

+---------------+-------------------+-----------------------------------+
| Parameter     | Requirement       | Description                       |
+===============+===================+===================================+
| ``X``         | Any type          | An argument to be returned.       |
+---------------+-------------------+-----------------------------------+


Expression semantics
--------------------

For an arbitrary type ``x``:


.. parsed-literal::

    typedef identity<x>::type r;

:Return type:
    A type.

:Semantics:
    Equivalent to
    
    .. parsed-literal::
    
        typedef x r;


:Postcondition:
    ``is_same<r,x>::value == true``.
    
    

Example
-------

.. parsed-literal::
    
    typedef apply< identity<_1>, char >::type t1;
    typedef apply< identity<_2>, char,int >::type t2;
    
    BOOST_MPL_ASSERT(( is_same< t1, char > ));
    BOOST_MPL_ASSERT(( is_same< t2, int > ));


See also
--------

|Metafunctions|, |Placeholders|, |Trivial Metafunctions|, |always|, |apply|
