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
 * $Id: CoreTests_CountedPointer.cpp 470088 2006-11-01 20:35:12Z amassari $
 */


// ---------------------------------------------------------------------------
//  XML4C2 Includes
// ---------------------------------------------------------------------------
#include "CoreTests.hpp"
#include <xercesc/util/CountedPointer.hpp>


// ---------------------------------------------------------------------------
//  A local class used for testing
// ---------------------------------------------------------------------------
class TestClass
{
public :
    static unsigned int gCounter;

    TestClass()
    {
        gCounter++;
    }

    ~TestClass()
    {
        gCounter--;
    }

    void addRef()
    {
        refCount++;
    }

    void removeRef()
    {
        refCount--;
        if (refCount == 0)
            delete this;
    }

private :
    unsigned int refCount;
};

unsigned int TestClass::gCounter = 0;


// ---------------------------------------------------------------------------
//  Force a full instantiation to test syntax
// ---------------------------------------------------------------------------
template class CountedPointerTo<TestClass>;


// ---------------------------------------------------------------------------
//  Test entry point
// ---------------------------------------------------------------------------
bool testCountedPointer()
{
    XERCES_STD_QUALIFIER wcout  << L"----------------------------------\n"
                << L"Testing CountedPointerTo class\n"
                << L"----------------------------------" << XERCES_STD_QUALIFIER endl;

    bool retVal = true;

    try
    {
    }

    catch(const XMLException& toCatch)
    {
        XERCES_STD_QUALIFIER wcout << L"  ERROR: Unexpected exception!\n   Msg: "
                   << toCatch.getMessage() << XERCES_STD_QUALIFIER endl;
        return false;
    }
    return retVal;
}
