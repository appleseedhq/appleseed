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
 * $Id: BitSet.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/BitSet.hpp>
#include <xercesc/framework/MemoryManager.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local const data
//
//  kBitsPerUnit
//      The number of bits in each of our allocation units, which is a 32
//      bit value in this case.
//
//  kGrowBy
//      The minimum allocation units to grow the buffer by.
// ---------------------------------------------------------------------------
const unsigned int  kBitsPerUnit    = 32;
const unsigned int  kGrowBy         = 1;



// ---------------------------------------------------------------------------
//  BitSet: Constructors and Destructor
// ---------------------------------------------------------------------------
BitSet::BitSet( const unsigned int size
              , MemoryManager* const manager) :

    fMemoryManager(manager)
    , fBits(0)
    , fUnitLen(0)
{
    ensureCapacity(size);
}

BitSet::BitSet(const BitSet& toCopy) :
    XMemory(toCopy)
    , fMemoryManager(toCopy.fMemoryManager)
    , fBits(0)
    , fUnitLen(toCopy.fUnitLen)
{
    fBits = (unsigned long*) fMemoryManager->allocate
    (
        fUnitLen * sizeof(unsigned long)
    ); //new unsigned long[fUnitLen];
    for (unsigned int i = 0; i < fUnitLen; i++)
        fBits[i] = toCopy.fBits[i];
}

BitSet::~BitSet()
{
    fMemoryManager->deallocate(fBits); //delete [] fBits;
}


// ---------------------------------------------------------------------------
//  BitSet: Equality methods
// ---------------------------------------------------------------------------
bool BitSet::equals(const BitSet& other) const
{
    if (this == &other)
        return true;

    if (fUnitLen != other.fUnitLen)
        return false;

    for (unsigned int i = 0; i < fUnitLen; i++)
    {
        if (fBits[i] != other.fBits[i])
            return false;
    }
    return true;
}


// ---------------------------------------------------------------------------
//  BitSet: Getter methods
// ---------------------------------------------------------------------------
bool BitSet::get(const unsigned int index) const
{
    const unsigned int unitOfBit = (index / kBitsPerUnit);
    const unsigned int bitWithinUnit = index % kBitsPerUnit;

    //
    //  If the index is beyond our size, don't actually expand. Just return
    //  false, which is what the state would be if we did expand it.
    //
    bool retVal = false;
    if (unitOfBit <= fUnitLen)
    {
        if (fBits[unitOfBit] & (1 << bitWithinUnit))
            retVal = true;
    }
    return retVal;
}


unsigned int BitSet::size() const
{
    return fUnitLen * kBitsPerUnit;
}



// ---------------------------------------------------------------------------
//  BitSet: Setter methods
// ---------------------------------------------------------------------------
bool BitSet::allAreCleared() const
{
    for (unsigned int index = 0; index < fUnitLen; index++)
    {
        if (fBits[index])
            return false;
    }
    return true;
}

bool BitSet::allAreSet() const
{
    for (unsigned int index = 0; index < fUnitLen; index++)
    {
        if (fBits[index] != 0xFFFFFFFF)
            return false;
    }
    return true;
}

void BitSet::clearAll()
{
    // Just zero out all the units
    for (unsigned int index = 0; index < fUnitLen; index++)
        fBits[index] = 0;
}

void BitSet::clear(const unsigned int index)
{
    ensureCapacity(index+1);

    const int unitOfBit = (index / kBitsPerUnit);
    const int bitWithinUnit = index % kBitsPerUnit;

    fBits[unitOfBit] &= ~(1UL << bitWithinUnit);
}


void BitSet::set(const unsigned int index)
{
    ensureCapacity(index+1);

    const int unitOfBit = (index / kBitsPerUnit);
    const int bitWithinUnit = index % kBitsPerUnit;

    fBits[unitOfBit] |= (1UL << bitWithinUnit);
}



// ---------------------------------------------------------------------------
//  BitSet: Bitwise logical methods
// ---------------------------------------------------------------------------
void BitSet::andWith(const BitSet& other)
{
    if (fUnitLen < other.fUnitLen)
        ensureCapacity(other.fUnitLen * kBitsPerUnit);

    for (unsigned int index = 0; index < other.fUnitLen; index++)
        fBits[index] &= other.fBits[index];
}


void BitSet::orWith(const BitSet& other)
{
    if (fUnitLen < other.fUnitLen)
        ensureCapacity(other.fUnitLen * kBitsPerUnit);

    for (unsigned int index = 0; index < other.fUnitLen; index++)
        fBits[index] |= other.fBits[index];
}


void BitSet::xorWith(const BitSet& other)
{
    if (fUnitLen < other.fUnitLen)
        ensureCapacity(other.fUnitLen * kBitsPerUnit);

    for (unsigned int index = 0; index < other.fUnitLen; index++)
        fBits[index] ^= other.fBits[index];
}



// ---------------------------------------------------------------------------
//  BitSet: Miscellaneous methods
// ---------------------------------------------------------------------------
unsigned int BitSet::hash(const unsigned int hashModulus) const
{
    const unsigned char* pBytes = (const unsigned char*)fBits;
    const int unsigned len = fUnitLen * sizeof(unsigned long);

    unsigned int  hashVal = 0;
    for (unsigned int index = 0; index < len; index++)
    {
        hashVal <<= 1;
        hashVal ^= *pBytes;
    }
    return hashVal % hashModulus;
}



// ---------------------------------------------------------------------------
//  BitSet: Private methods
// ---------------------------------------------------------------------------
void BitSet::ensureCapacity(const unsigned int size)
{
    // Calculate the units required to hold the passed bit count.
    unsigned int unitsNeeded = size / kBitsPerUnit;
    if (size % kBitsPerUnit)
        unitsNeeded++;

    // If its more than we have, then reallocate
    if (unitsNeeded > fUnitLen)
    {
        // Regrow the unit length by at least the expansion unit
        if (unitsNeeded < (fUnitLen + kGrowBy))
            unitsNeeded = fUnitLen + kGrowBy;

        // Allocate the array, copy the old stuff, and zero the new stuff
        unsigned long* newBits = (unsigned long*) fMemoryManager->allocate
        (
            unitsNeeded * sizeof(unsigned long)
        ); //new unsigned long[unitsNeeded];

        unsigned int index;
        for (index = 0; index < fUnitLen; index++)
            newBits[index] = fBits[index];

        for (; index < unitsNeeded; index++)
            newBits[index] = 0;

        fMemoryManager->deallocate(fBits); //delete [] fBits;
        fBits = newBits;
        fUnitLen = unitsNeeded;
    }
}

XERCES_CPP_NAMESPACE_END
