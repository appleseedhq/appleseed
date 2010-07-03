.. Metafunctions/Composition and Argument Binding//protect |60

.. Copyright Aleksey Gurtovoy, David Abrahams 2007.
.. Distributed under the Boost
.. Software License, Version 1.0. (See accompanying
.. file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

protect
=======

Synopsis
--------

.. parsed-literal::
    
    template< 
          typename F
        >
    struct protect
    {
        // |unspecified|
        // |...|
    };



Description
-----------

``protect`` is an identity wrapper for a |Metafunction Class| that prevents
its argument from being recognized as a |bind expression|.


Header
------

.. parsed-literal::
    
    #include <boost/mpl/protect.hpp>


Parameters
----------

+---------------+---------------------------+---------------------------------------+
| Parameter     | Requirement               | Description                           |
+===============+===========================+=======================================+
| ``F``         | |Metafunction Class|      | A metafunction class to wrap.         |
+---------------+---------------------------+---------------------------------------+


Expression semantics
--------------------

For any |Metafunction Class| ``f``:


.. parsed-literal::

    typedef protect<f> g;

:Return type:
    |Metafunction Class|.

:Semantics:
    If ``f`` is a |bind expression|, equivalent to
    
    .. parsed-literal::

        struct g
        {
            template< 
                  typename U1 = |unspecified|\,\ |...| typename U\ *n* = |unspecified|
                >
            struct apply
                : apply_wrap\ *n*\<f,U1,\ |...|\ U\ *n*\ >
            {
            };
        };
    
    otherwise equivalent to ``typedef f g;``.


Example
-------

.. parsed-literal::
    
    FIXME
    
    struct f
    {
        template< typename T1, typename T2 > struct apply
        {
            // |...|
        };
    };
    
    typedef bind<_1, protect< bind<f,_1,_2> > >
    
    typedef apply_wrap0< f0 >::type r1;
    typedef apply_wrap0< g0 >::type r2;
    typedef apply_wrap2< f2,int,char >::type r3;

    BOOST_MPL_ASSERT(( is_same<r1,char> ));
    BOOST_MPL_ASSERT(( is_same<r2,char> ));
    BOOST_MPL_ASSERT(( is_same<r3,char> ));


See also
--------

|Composition and Argument Binding|, |Invocation|, |bind|, |quote|, |apply_wrap|
