/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * $Log$
 * Revision 1.10  2004/09/08 13:57:05  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.9  2003/05/30 13:08:27  gareth
 * move over to macros for std:: and iostream/iostream.h issues.
 *
 * Revision 1.8  2002/02/01 22:46:28  peiyongz
 * sane_include
 *
 * Revision 1.7  2000/03/02 19:55:49  roddey
 * This checkin includes many changes done while waiting for the
 * 1.1.0 code to be finished. I can't list them all here, but a list is
 * available elsewhere.
 *
 * Revision 1.6  2000/02/06 07:48:39  rahulj
 * Year 2K copyright swat.
 *
 * Revision 1.5  2000/01/19 00:59:06  roddey
 * Get rid of dependence on old utils output streams.
 *
 * Revision 1.4  2000/01/12 00:17:48  roddey
 * Removed validator tests temporarily, since they have changed and the tests need
 * to be rewritten. Added new tests for the new URL class.
 *
 * Revision 1.3  1999/12/15 19:59:12  roddey
 * Added new tests and updated tests for new split transcoder architecture.
 *
 * Revision 1.2  1999/12/07 23:11:01  roddey
 * Add in some new tests for transcoders and update the URL tests
 * a bit.
 *
 * Revision 1.1.1.1  1999/11/09 01:02:03  twl
 * Initial checkin
 *
 * Revision 1.2  1999/11/08 20:42:28  rahul
 * Swat for adding in Product name and CVS comment log variable.
 *
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include "CoreTests.hpp"
#include <xercesc/util/XMLASCIITranscoder.hpp>
#include <xercesc/util/Janitor.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/TransService.hpp>



// ---------------------------------------------------------------------------
//  Local test methods
// ---------------------------------------------------------------------------

// The transcoding system was changed, and there is no time at the moment
// to update the tests, so they were temporarily removed.


// ---------------------------------------------------------------------------
//  Test entry point
// ---------------------------------------------------------------------------
bool testTranscoders()
{
    XERCES_STD_QUALIFIER wcout  << L"----------------------------------\n"
                << L"Testing transcoder classes\n"
                << L"----------------------------------" << XERCES_STD_QUALIFIER endl;

    return true;
}
