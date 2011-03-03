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
 * $Id: CoreTests_BitSet.cpp 470088 2006-11-01 20:35:12Z amassari $
 */

// ---------------------------------------------------------------------------
//  XML4C2 Includes
// ---------------------------------------------------------------------------
#include "CoreTests.hpp"
#include <xercesc/util/BitSet.hpp>



// ---------------------------------------------------------------------------
//  Local testing methods
// ---------------------------------------------------------------------------
static bool basicTests()
{
    //
    //  Create a bitset with 32 bits. We just happen to know that this is
    //  the unit of expansion, so it should come back with exactly that
    //  number of bits of size.
    //
    BitSet setTest(32);

    if (setTest.size() != 32)
    {
        XERCES_STD_QUALIFIER wcout  << L"    Ctor did not create set of correct size"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    //
    //  Check the value of all of the bits and make sure that they all come
    //  back zero.
    //
    const unsigned int count = setTest.size();
    unsigned int index;
    for (index = 0; index < count; index++)
    {
        if (setTest.get(index))
        {
            XERCES_STD_QUALIFIER wcout << L"    A bit's initial value was not zero"
                       << XERCES_STD_QUALIFIER endl;
            return false;
        }
    }

    // Make sure that allAreCleared() agrees
    if (!setTest.allAreCleared())
    {
        XERCES_STD_QUALIFIER wcout  << L"    allAreCleared() disagrees with individual bit gets"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    // Set and clear each bit and make sure that they come back right
    for (index = 0; index < count; index++)
    {
        setTest.set(index);
        if (!setTest.get(index))
        {
            XERCES_STD_QUALIFIER wcout << L"    Bit was set but get returned false"
                       << XERCES_STD_QUALIFIER endl;
            return false;
        }

        setTest.clear(index);
        if (setTest.get(index))
        {
            XERCES_STD_QUALIFIER wcout  << L"    Bit was cleared but get returned true"
                        << XERCES_STD_QUALIFIER endl;
            return false;
        }
    }

    // And once more make sure they are all zero
    for (index = 0; index < count; index++)
    {
        if (setTest.get(index))
        {
            XERCES_STD_QUALIFIER wcout << L"    A bit remained set after clearing"
                       << XERCES_STD_QUALIFIER endl;
            return false;
        }
    }

    //
    //  Set some bits, then copy construct another bitset from this one. Then
    //  see if they come out equal.
    //
    setTest.set(1);
    setTest.set(16);
    setTest.set(20);
    setTest.set(24);

    BitSet setTest2(setTest);
    if (!setTest.equals(setTest2))
    {
        XERCES_STD_QUALIFIER wcout  << L"    Copy ctor did not create equal sets"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    // Clear all bits of the copy and make sure they are all cleared
    setTest2.clearAll();
    for (index = 0; index < count; index++)
    {
        if (setTest2.get(index))
        {
            XERCES_STD_QUALIFIER wcout  << L"    clearAll() did not clear all bits"
                        << XERCES_STD_QUALIFIER endl;
            return false;
        }
    }

    // Set a bit beyond the current size
    setTest2.set(32);

    // Make sure it expanded
    if (setTest2.size() != 64)
    {
        XERCES_STD_QUALIFIER wcout  << L"    Set of bit beyond size did not expand"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    // Set all the bits
    for (index = 0; index < count; index++)
        setTest.set(index);

    // Make sure that allAreSet() sees them all set
    if (!setTest.allAreSet())
    {
        XERCES_STD_QUALIFIER wcout  << L"    After setting all bits, allAreSet() returned false"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    return true;
}

static bool bitopsTests()
{
    // Create a bit set to test
    BitSet setTest(48);

    // Set some bits
    setTest.set(1);
    setTest.set(10);
    setTest.set(16);
    setTest.set(21);
    setTest.set(33);
    setTest.set(41);

    // Create another set to do ops on
    BitSet setTest2(48);

    // Or with the new set
    setTest2.orWith(setTest);

    // They should be equal now
    if (!setTest.equals(setTest2))
    {
        XERCES_STD_QUALIFIER wcout  << L"    OR of set with empty set did not create equal sets"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    // Xor with original which should get back an empty set
    setTest2.xorWith(setTest);
    if (!setTest2.allAreCleared())
    {
        XERCES_STD_QUALIFIER wcout  << L"    XOR against original set did not get back original"
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }

    // And them, which should have no effect
    setTest2.andWith(setTest);
    if (!setTest2.allAreCleared())
    {
        XERCES_STD_QUALIFIER wcout << L"    AND against empty set changed bits" << XERCES_STD_QUALIFIER endl;
        return false;
    }

    return true;
}


// ---------------------------------------------------------------------------
//  Test entry point
// ---------------------------------------------------------------------------
bool testBitSet()
{
    XERCES_STD_QUALIFIER wcout  << L"----------------------------------\n"
                << L"Testing BitSet class\n"
                << L"----------------------------------" << XERCES_STD_QUALIFIER endl;

    bool retVal = true;

    try
    {
        XERCES_STD_QUALIFIER wcout << L"Testing basic BitSet methods" << XERCES_STD_QUALIFIER endl;
        if (!basicTests())
        {
            XERCES_STD_QUALIFIER wcout << L"Bitset basic test methods failed" << XERCES_STD_QUALIFIER endl;
            retVal = false;
        }
         else
        {
            XERCES_STD_QUALIFIER wcout << L"Bitset basic tests passed" << XERCES_STD_QUALIFIER endl;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;

        XERCES_STD_QUALIFIER wcout << L"Testing BitSet logical bit ops methods" << XERCES_STD_QUALIFIER endl;
        if (!bitopsTests())
        {
            XERCES_STD_QUALIFIER wcout << L"Bitset logical bit ops failed" << XERCES_STD_QUALIFIER endl;
            retVal = false;
        }
         else
        {
            XERCES_STD_QUALIFIER wcout << L"Bitset logical bit ops passed" << XERCES_STD_QUALIFIER endl;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;
    }

    catch(const XMLException& toCatch)
    {
        XERCES_STD_QUALIFIER wcout << L"  ERROR: Unexpected exception!\n   Msg: "
                << toCatch.getMessage() << XERCES_STD_QUALIFIER endl;
        return false;
    }
    return retVal;
}
