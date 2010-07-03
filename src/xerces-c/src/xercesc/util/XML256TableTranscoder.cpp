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


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/BitOps.hpp>
#include <xercesc/util/TranscodingException.hpp>
#include <xercesc/util/XML256TableTranscoder.hpp>
#include <xercesc/util/XMLString.hpp>
#include <string.h>

XERCES_CPP_NAMESPACE_BEGIN


// ---------------------------------------------------------------------------
//  XML256TableTranscoder: Public Destructor
// ---------------------------------------------------------------------------
XML256TableTranscoder::~XML256TableTranscoder()
{
    // We don't own the tables, we just reference them
}


// ---------------------------------------------------------------------------
//  XML256TableTranscoder: Implementation of the transcoder API
// ---------------------------------------------------------------------------
unsigned int
XML256TableTranscoder::transcodeFrom(const  XMLByte* const       srcData
                                    , const unsigned int         srcCount
                                    ,       XMLCh* const         toFill
                                    , const unsigned int         maxChars
                                    ,       unsigned int&        bytesEaten
                                    ,       unsigned char* const charSizes)
{
    // If debugging, make sure that the block size is legal
    #if defined(XERCES_DEBUG)
    checkBlockSize(maxChars);
    #endif

    //
    //  Calculate the max chars we can do here. Its the lesser of the
    //  max output chars and the number of chars in the source.
    //
    const unsigned int countToDo = srcCount < maxChars ? srcCount : maxChars;

    //
    //  Loop through the count we have to do and map each char via the
    //  lookup table.
    //
    const XMLByte*  srcPtr = srcData;
    const XMLByte*  endPtr = (srcPtr + countToDo);
    XMLCh*          outPtr = toFill;
    while (srcPtr < endPtr)
    {
        const XMLCh uniCh = fFromTable[*srcPtr++];
        if (uniCh != 0xFFFF)
        {
            *outPtr++ = uniCh;
            continue;
        }
    }


    // Set the bytes eaten
    bytesEaten = countToDo;

    // Set the character sizes to the fixed size
    memset(charSizes, 1, countToDo);

    // Return the chars we transcoded
    return countToDo;
}


unsigned int
XML256TableTranscoder::transcodeTo( const   XMLCh* const    srcData
                                    , const unsigned int    srcCount
                                    ,       XMLByte* const  toFill
                                    , const unsigned int    maxBytes
                                    ,       unsigned int&   charsEaten
                                    , const UnRepOpts       options)
{
    // If debugging, make sure that the block size is legal
    #if defined(XERCES_DEBUG)
    checkBlockSize(maxBytes);
    #endif

    //
    //  Calculate the max chars we can do here. Its the lesser of the
    //  max output chars and the number of chars in the source.
    //
    const unsigned int countToDo = srcCount < maxBytes ? srcCount : maxBytes;

    //
    //  Loop through the count we have to do and map each char via the
    //  lookup table.
    //
    const XMLCh*    srcPtr = srcData;
    const XMLCh*    endPtr = (srcPtr + countToDo);
    XMLByte*        outPtr = toFill;
    XMLByte         nextOut;
    while (srcPtr < endPtr)
    {
        //
        //  Get the next src char out to a temp, then do a binary search
        //  of the 'to' table for this entry.
        //
        if ((nextOut = xlatOneTo(*srcPtr)))
        {
            *outPtr++ = nextOut;
            srcPtr++;
            continue;
        }

        //
        //  Its not representable so, according to the options, either
        //  throw or use the replacement.
        //
        if (options == UnRep_Throw)
        {
            XMLCh tmpBuf[17];
            XMLString::binToText((unsigned int)*srcPtr, tmpBuf, 16, 16, getMemoryManager());
            ThrowXMLwithMemMgr2
            (
                TranscodingException
                , XMLExcepts::Trans_Unrepresentable
                , tmpBuf
                , getEncodingName()
                , getMemoryManager()
            );
        }

        // Eat the source char and use the replacement char
        srcPtr++;
        *outPtr++ = 0x3F;
    }

    // Set the chars eaten
    charsEaten = countToDo;

    // Return the bytes we transcoded
    return countToDo;
}


bool XML256TableTranscoder::canTranscodeTo(const unsigned int toCheck) const
{
    return (xlatOneTo(toCheck) != 0);
}


// ---------------------------------------------------------------------------
//  XML256TableTranscoder: Hidden constructor
// ---------------------------------------------------------------------------
XML256TableTranscoder::
XML256TableTranscoder(  const   XMLCh* const                     encodingName
                        , const unsigned int                     blockSize
                        , const XMLCh* const                     fromTable
                        , const XMLTransService::TransRec* const toTable
                        , const unsigned int                     toTableSize
                        , MemoryManager* const                   manager) :

    XMLTranscoder(encodingName, blockSize, manager)
    , fFromTable(fromTable)
    , fToSize(toTableSize)
    , fToTable(toTable)
{
}


// ---------------------------------------------------------------------------
//  XML256TableTranscoder: Private helper methods
// ---------------------------------------------------------------------------
XMLByte XML256TableTranscoder::xlatOneTo(const XMLCh toXlat) const
{
    unsigned int    lowOfs = 0;
    unsigned int    hiOfs = fToSize - 1;
    do
    {
        // Calc the mid point of the low and high offset.
        const unsigned int midOfs = ((hiOfs - lowOfs) / 2) + lowOfs;

        //
        //  If our test char is greater than the mid point char, then
        //  we move up to the upper half. Else we move to the lower
        //  half. If its equal, then its our guy.
        //
        if (toXlat > fToTable[midOfs].intCh)
        {
            lowOfs = midOfs;
        }
         else if (toXlat < fToTable[midOfs].intCh)
        {
            hiOfs = midOfs;
        }
         else
        {
            return fToTable[midOfs].extCh;
        }
    }   while (lowOfs + 1 < hiOfs);

    // Check the high end of the range otherwise the
    // last item in the table may never be found.
        if (toXlat == fToTable[hiOfs].intCh)
        {
            return fToTable[hiOfs].extCh;
        }

    return 0;
}

XERCES_CPP_NAMESPACE_END
