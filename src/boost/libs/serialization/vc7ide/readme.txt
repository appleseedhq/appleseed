Notes on vc 7.1 ide project files
=================================

These project files use the directory structure created by the Boost Build system.
This means that BJAM has to have been run at least once for the directories to
be created in the expected places.  Once this has been done, one should be able
to rebuild/rerun any demo or test from within the VC 7 IDE.

There are a large number of project configurations.  There is one for each permitted
boost configuration.  E.G. Debug runtime-dynamic, Debug runtime-static threading-mulit,
etc.  which correspond to boost build configurations.

For tests which are designed to be run with different types of archives, the number
of project configurations is mulitplied by 5 - one for each type archive.  For example, for 
Debug runtime-static there exists Debug runtime-static text_archive, 
Debug runtime-static text_warchive, ... Debug runtime-static xml_warchive.

By selecting the appropriate configuration it should be possible to replicate the 
behavior of the failed test from withing the IDE.  This is helpful in debugging.

After each test is built, it is run and the ouput is shown in the IDE Output window.

Note that the fact that all configurations are not valid for all tests precludes building 
the all the tests from withing the IDE in one batch.

(C) Copyright 2008 Robert Ramey - http://www.rrsd.com . 
Use, modification and distribution is subject to the Boost Software
// License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
http://www.boost.org/LICENSE_1_0.txt)
