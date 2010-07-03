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
#include <xercesc/util/XMLASCIITranscoder.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/TranscodingException.hpp>
#include <string.h>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  XMLASCIITranscoder: Constructors and Destructor
// ---------------------------------------------------------------------------
XMLASCIITranscoder::XMLASCIITranscoder( const   XMLCh* const    encodingName
                                        , const unsigned int    blockSize
                                        , MemoryManager* const manager) :

    XMLTranscoder(encodingName, blockSize, manager)
{
}


XMLASCIITranscoder::~XMLASCIITranscoder()
{
}


// ---------------------------------------------------------------------------
//  XMLASCIITranscoder: Implementation of the transcoder API
// ---------------------------------------------------------------------------
unsigned int
XMLASCIITranscoder::transcodeFrom(  const   XMLByte* const       srcData
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
    //  max output chars and the source byte count.
    //
    const unsigned int countToDo = srcCount < maxChars ? srcCount : maxChars;

    //
    //  Now loop through that many source chars and just cast each one
    //  over to the XMLCh format. Check each source that its really a
    //  valid ASCI char.
    //
    const XMLByte*  srcPtr = srcData;
    XMLCh*          outPtr = toFill;
    unsigned int    countDone = 0;
    for (; countDone < countToDo; countDone++)
    {
        // Do the optimistic work up front
        if (*srcPtr < 0x80)
        {
            *outPtr++ = XMLCh(*srcPtr++);
            continue;
        }

        //
        //  We got non source encoding char. If we got more than 32 chars,
        //  the just break out. We'll come back here later to hit this again
        //  and give an error much closer to the real source position.
        //
        if (countDone > 32)
            break;

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

    // Set the bytes we ate
    bytesEaten = countDone;

    // Set the char sizes to the fixed size
    memset(charSizes, 1, countDone);

    // Return the chars we transcoded
    return countDone;
}


unsigned int
XMLASCIITranscoder::transcodeTo(const   XMLCh* const    srcData
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
    //  max output chars and the source byte count.
    //
    const unsigned int countToDo = srcCount < maxBytes ? srcCount : maxBytes;

    const XMLCh*    srcPtr = srcData;
    XMLByte*        outPtr = toFill;
    for (unsigned int index = 0; index < countToDo; index++)
    {
        // If its legal, do it and jump back to the top
        if (*srcPtr < 0x80)
        {
            *outPtr++ = XMLByte(*srcPtr++);
            continue;
        }

        //
        //  Its not representable so use a replacement char. According to
        //  the options, either throw or use the replacement.
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

        // Use the replacement char
        *outPtr++ = 0x1A;
        srcPtr++;
    }

    // Set the chars we ate
    charsEaten = countToDo;

    // Return the byte we transcoded
    return countToDo;
}


bool XMLASCIITranscoder::canTranscodeTo(const unsigned int toCheck) const
{
    return (toCheck < 0x80);
}

XERCES_CPP_NAMESPACE_END
