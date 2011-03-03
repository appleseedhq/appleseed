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
 * $Id: CoreTests_Transcoders.cpp 470088 2006-11-01 20:35:12Z amassari $
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
