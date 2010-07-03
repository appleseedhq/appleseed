

.. Copyright Aleksey Gurtovoy, David Abrahams 2007.
.. Distributed under the Boost
.. Software License, Version 1.0. (See accompanying
.. file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
The MPL Reference Manual
************************

:Copyright: Copyright ©  Aleksey Gurtovoy and David Abrahams, 2001-2007.

:License:   Distributed under the Boost Software License, Version 1.0. (See 
            accompanying file ``LICENSE_1_0.txt`` or copy at 
            `http://www.boost.org/LICENSE_1_0.txt`__)


__ http://www.boost.org/LICENSE_1_0.txt



.. no .. section-numbering::


.. raw:: latex

   \setcounter{secnumdepth}{2}
   \setcounter{tocdepth}{2}
    
.. contents:: Table of Contents
    :depth: 3

.. |Boost.Bind| replace:: `Boost.Bind`__
__ http://www.boost.org/libs/bind/bind.html

.. |Boost.Lambda| replace:: `Boost.Lambda`__
__ http://www.boost.org/libs/lambda/doc/index.html

.. role:: refentry(literal)

.. |t1| replace:: \ *t*\ :sub:`1`
.. |t2| replace:: \ *t*\ :sub:`2`
.. |tn| replace:: \ *t*\ :sub:`n`
.. workaround weird substitution bug (used to work!):
.. |t1...tn| replace:: \ *t*\ :sub:`1`,\ *t*\ :sub:`2`,... |tn|

.. |p1...pn| replace:: \ *p*\ :sub:`1`,\ *p*\ :sub:`2`,... \ *p*\ :sub:`n`


.. |c1| replace:: \ *c*\ :sub:`1`
.. |c2| replace:: \ *c*\ :sub:`2`
.. |cn| replace:: \ *c*\ :sub:`n`
.. workaround weird substitution bug (works with t's!):
.. |c1...cn| replace:: \ *c*\ :sub:`1`,\ *c*\ :sub:`2`,... \ *c*\ :sub:`n`

.. |x1...xn| replace:: *x*\ :sub:`1`,\ *x*\ :sub:`2`,... \ *x*\ :sub:`n`

.. |...| replace:: *...*
.. |T1...Tn| replace:: ``T1``, ``T2``,... ``Tn``

.. |F1...Fn| replace:: ``F1``, ``F2``,... ``Fn``
.. |f1...fn| replace:: ``f1``, ``f2``,... ``fn``

.. |A1...An| replace:: ``A1``,... ``An``
.. |a1...an| replace:: ``a1``,... ``an``



.. |begin/end<Sequence>| replace:: [``begin<Sequence>::type``, ``end<Sequence>::type``)
.. |begin/end<Seq>| replace:: [``begin<Seq>::type``, ``end<Seq>::type``)
.. |begin/end<Seq1>| replace:: [``begin<Seq1>::type``, ``end<Seq1>::type``)
.. |begin/end<Seq2>| replace:: [``begin<Seq2>::type``, ``end<Seq2>::type``)

.. |begin/end<s>| replace:: [``begin<s>::type``, ``end<s>::type``)
.. |begin/end<v>| replace:: [``begin<v>::type``, ``end<v>::type``)
.. |begin/end<s1>| replace:: [``begin<s1>::type``, ``end<s1>::type``)
.. |begin/end<s2>| replace:: [``begin<s2>::type``, ``end<s2>::type``)

.. |is_same| replace:: is_same
.. |unspecified| replace:: *unspecified*
.. |unspecified-token-seq| replace:: *unspecified token sequence*

.. |idic| replace:: *implementation-defined integral constant*

.. |true if and only if| replace:: A boolean `Integral Constant`_ ``c`` such that
   ``c::value == true`` if and only if 


.. |O(1)| replace:: *O(1)*

.. |_1| replace:: |``_1``|__
.. |_2| replace:: |``_2``|__
.. |_3| replace:: |``_3``|__
.. |_4| replace:: |``_4``|__
.. |_5| replace:: |``_5``|__
.. |``_1``| replace:: :refentry:`_1`
.. |``_2``| replace:: :refentry:`_2`
.. |``_3``| replace:: :refentry:`_3`
.. |``_4``| replace:: :refentry:`_4`
.. |``_5``| replace:: :refentry:`_5`
__ `Placeholders`_
__ `Placeholders`_
__ `Placeholders`_
__ `Placeholders`_
__ `Placeholders`_

.. |placeholder| replace:: `placeholder`__
__ `Placeholders`_

.. |_1,_2,..._n| replace:: |_1|, |_2|, |_3|,\ |...|


.. |--| unicode:: U+02014 .. EM DASH


.. |Note:| replace:: [*Note:*
.. |-- end note| replace:: |--| *end note*\]


.. |Semantics disclaimer...| replace:: The semantics of an expression are defined only 
   where they differ from, or are not defined in


.. |numeric metafunction note| replace:: The requirements listed in this specification 
   are the ones imposed by the default implementation. See |Numeric Metafunction| concept 
   for the details on how to provide an implementation for a user-defined numeric type 
   that does not satisfy the `Integral Constant`_ requirements.
   

.. "[*Note:*" instead of "|Note:|" to workaround another subst. bug


.. |preprocessed headers disclaimer| replace:: [*Note:* Overriding will take effect 
   *only* if the library is configured not to use `preprocessed headers`__. See 
   |+BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS+|__ for more information. |--| *end note*\]

.. |+BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS+| replace:: :refentry:`BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS`
__ `BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS`_
__ `BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS`_


.. |transformation algorithm disclaimer| replace:: 
   [*Note:* This wording applies to a no-inserter version(s) of the algorithm. See the 
   `Expression semantics` subsection for a precise specification of the algorithm's 
   details in all cases |--| *end note*\]

.. |In the following table...| replace:: In the following table and subsequent specifications, 
