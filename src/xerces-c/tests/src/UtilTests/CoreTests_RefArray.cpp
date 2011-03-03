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
 * $Id: CoreTests_RefArray.cpp 470088 2006-11-01 20:35:12Z amassari $
 */

// ---------------------------------------------------------------------------
//  XML4C2 includes
// ---------------------------------------------------------------------------
#include "CoreTests.hpp"
#include <xercesc/util/RefArrayOf.hpp>
#include <xercesc/util/ArrayIndexOutOfBoundsException.hpp>



// ---------------------------------------------------------------------------
//  Force a full instantiation of our array and its enumerator, just to
//  insure that all methods get instantiated and compiled.
// ---------------------------------------------------------------------------
template RefArrayOf<int>;
template RefArrayEnumerator<int>;


// ---------------------------------------------------------------------------
//  Local functions
// ---------------------------------------------------------------------------
static bool constructorTests()
{
    // Some values to test with
    double testVals[16];
    unsigned int index;
    for (index = 0; index < 16; index++)
        testVals[index] = index;

    // Do a basic constructor with just the count of elements
    RefArrayOf<double> testArray1(255);

    // Make sure that it has the right initial size
    if (testArray1.length() != 255)
    {
        XERCES_STD_QUALIFIER wcout  << L"    The ctor created wrong length() value"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    // Copy construct another array from it and test the length
    RefArrayOf<double> testArray2(testArray1);

    if (testArray2.length() != 255)
    {
        XERCES_STD_QUALIFIER wcout  << L"    The copy ctor created wrong length() value"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    // Test the equality of the two arrays
    if (testArray1 != testArray2)
    {
        XERCES_STD_QUALIFIER wcout  << L"    The copy ctor created unequal arrays"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    //
    //  Do another one where we provide the initial values.
    //
    double* initValues[16];
    for (index = 0; index < 16; index++)
        initValues[index ] = &testVals[index];
    RefArrayOf<double> testArray3(initValues, 16);

    if (testArray3.length() != 16)
    {
        XERCES_STD_QUALIFIER wcout  << L"    The init values ctor created wrong length() value"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    // Make sure the initial values are correct
    for (index = 0; index < 16; index++)
    {
        if (*testArray3[index] != (double)index)
        {
            XERCES_STD_QUALIFIER wcout  << L"    The init values ctor did not init contents correctly"
                        << XERCES_STD_QUALIFIER endl;
            return false;
        }
    }

    //
    //  Create another array of a different size and assign one of the
    //  existing ones to it and make sure that they are equal.
    //
    RefArrayOf<double> testArray4(15);
    testArray4 = testArray3;

    if (testArray4 != testArray3)
    {
        XERCES_STD_QUALIFIER wcout  << L"    Assignment did not create equal arrays"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    return true;
}


static bool accessTests()
{
    // Some values to test with
    unsigned int testVals[16];
    unsigned int index;
    for (index = 0; index < 16; index++)
        testVals[index] = index;

    RefArrayOf<unsigned int> testArray1(16);

    // Fill in the array
    for (index = 0; index < 16; index++)
        testArray1[index] = &testVals[index];

    // Read them back again
    for (index = 0; index < 16; index++)
    {
        if (testArray1[index] != &testVals[index])
        {
            XERCES_STD_QUALIFIER wcout  << L"    Failed to read back values just set"
                        << XERCES_STD_QUALIFIER endl;
            return false;
        }
    }

    // Make sure we get the expected array index error
    bool caughtIt = false;
    try
    {
        testArray1[16];
    }

    catch(const ArrayIndexOutOfBoundsException&)
    {
        caughtIt = true;
    }

    if (!caughtIt)
    {
        XERCES_STD_QUALIFIER wcout << L"    Failed to catch index error" << XERCES_STD_QUALIFIER endl;
        return false;
    }

    return true;
}


// ---------------------------------------------------------------------------
//  Test entry point
// ---------------------------------------------------------------------------
bool testRefArray()
{
    XERCES_STD_QUALIFIER wcout  << L"----------------------------------\n"
                << L"Testing RefArrayOf template class\n"
                << L"----------------------------------" << XERCES_STD_QUALIFIER endl;

    bool retVal = true;

    try
    {
        // Call other local methods to do specific tests
        XERCES_STD_QUALIFIER wcout << L"Testing RefArrayOf contructors" << XERCES_STD_QUALIFIER endl;
        if (!constructorTests())
        {
            XERCES_STD_QUALIFIER wcout  << L"RefArrayOf constructor tests failed"
                        << XERCES_STD_QUALIFIER endl;
            retVal = false;
        }
         else
        {
            XERCES_STD_QUALIFIER wcout  << L"RefArrayOf constructor tests passed"
                        << XERCES_STD_QUALIFIER endl;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;

        XERCES_STD_QUALIFIER wcout << L"Testing RefArrayOf element access" << XERCES_STD_QUALIFIER endl;
        if (!accessTests())
        {
            XERCES_STD_QUALIFIER wcout  << L"RefArrayOf element access tests failed"
                        << XERCES_STD_QUALIFIER endl;
            retVal = false;
        }
         else
        {
            XERCES_STD_QUALIFIER wcout  << L"RefArrayOf element access tests passed"
                        << XERCES_STD_QUALIFIER endl;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;
    }

    catch(const XMLException& toCatch)
    {
        XERCES_STD_QUALIFIER wcout  << L"  ERROR: Unexpected exception!\n   Msg: "
                    << toCatch.getMessage() << XERCES_STD_QUALIFIER endl;
        return false;
    }
    return retVal;
}
