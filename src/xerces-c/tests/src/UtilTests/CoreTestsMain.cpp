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
 * $Id: CoreTestsMain.cpp 470088 2006-11-01 20:35:12Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include "CoreTests.hpp"
#include <xercesc/util/PlatformUtils.hpp>



// ---------------------------------------------------------------------------
//  Externs for our testing methods. There is just one per file so a header
//  for each one is a little overkill, so we just do it the old fashioned way.
// ---------------------------------------------------------------------------
extern bool testTranscoders();
extern bool testCountedPointer();
extern bool testBitSet();
extern bool testRefArray();
extern bool testRefHashTable();
extern bool testRefStack();
extern bool testRefVector();
extern bool testString();
extern bool testURL();
extern bool testValueArray();
extern bool testValueStack();
extern bool testValueVector();


int main()
{
    // Do the platform initialization
    try
    {
        XMLPlatformUtils::Initialize();
    }

    catch(const XMLException& toCatch)
    {
        XERCES_STD_QUALIFIER wcout << L"Parser Init Failed!\n   INFO: ("
                   << toCatch.getSrcFile() << L"." << toCatch.getSrcLine()
                   << L") -" << toCatch.getMessage() << XERCES_STD_QUALIFIER endl;
        return 0xFFFF;
    }

    XERCES_STD_QUALIFIER wcout << L"\nXML4C2 Core Utilities Unit Tester\n" << XERCES_STD_QUALIFIER endl;

    // This value will return the number of failed tests
    int retVal = 0;


    try
    {
        // -------------------------------------------------------------------
        // Test the basic transcoding services
        // -------------------------------------------------------------------
        if (!testTranscoders())
        {
            XERCES_STD_QUALIFIER wcout << L"Transcoder tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the String class
        // -------------------------------------------------------------------
        if (!testString())
        {
            XERCES_STD_QUALIFIER wcout << L"String tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the CountedPointerTo template class
        // -------------------------------------------------------------------
        if (!testCountedPointer())
        {
            XERCES_STD_QUALIFIER wcout << L"CountedPointerTo tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the URL class
        // -------------------------------------------------------------------
        if (!testURL())
        {
            XERCES_STD_QUALIFIER wcout << L"URL tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the ValueVectorOf template class
        // -------------------------------------------------------------------
        if (!testValueVector())
        {
            XERCES_STD_QUALIFIER wcout << L"ValueVectorOf tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the ValueArrayOf template class
        // -------------------------------------------------------------------
        if (!testValueArray())
        {
            XERCES_STD_QUALIFIER wcout << L"ValueArrayOf tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the ValueStackOf template class
        // -------------------------------------------------------------------
        if (!testValueStack())
        {
            XERCES_STD_QUALIFIER wcout << L"ValueStackOf tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the RefArrayOf template class
        // -------------------------------------------------------------------
        if (!testRefArray())
        {
            XERCES_STD_QUALIFIER wcout << L"RefArrayOf tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the RefStackOf template class
        // -------------------------------------------------------------------
        if (!testRefStack())
        {
            XERCES_STD_QUALIFIER wcout << L"RefStackOf tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the RefVectorOf template class
        // -------------------------------------------------------------------
        if (!testRefVector())
        {
            XERCES_STD_QUALIFIER wcout << L"RefVectorOf tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the RefHashtableOf template class
        // -------------------------------------------------------------------
        if (!testRefHashTable())
        {
            XERCES_STD_QUALIFIER wcout << L"RefHashTableOf tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;


        // -------------------------------------------------------------------
        // Test the BitSet class
        // -------------------------------------------------------------------
        if (!testBitSet())
        {
            XERCES_STD_QUALIFIER wcout << L"BitSet tests failed" << XERCES_STD_QUALIFIER endl;
            retVal++;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;
    }

    catch(const XMLException& toCatch)
    {
        XERCES_STD_QUALIFIER wcout  << L"Exception During Test!\n   INFO: ("
                    << toCatch.getSrcFile() << L"."
                    << toCatch.getSrcLine() << L") -"
                    << toCatch.getMessage() << XERCES_STD_QUALIFIER endl;
        return 0xFFFF;
    }

    // If we failed any tests, display a message
    XERCES_STD_QUALIFIER wcout << L"--------------------------------\n";
    if (retVal == 0)
        XERCES_STD_QUALIFIER wcout << L"<<PASSED>>: All tests passed\n";
    else
        XERCES_STD_QUALIFIER wcout << L"<<FAILED>>: Some tests failed\n";
    XERCES_STD_QUALIFIER wcout << L"--------------------------------\n" << XERCES_STD_QUALIFIER endl;

    return retVal;
}
