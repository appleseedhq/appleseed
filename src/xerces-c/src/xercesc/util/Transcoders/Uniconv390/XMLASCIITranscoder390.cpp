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
 * $Id: XMLASCIITranscoder390.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/Transcoders/Uniconv390/XMLASCIITranscoder390.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/TranscodingException.hpp>
#include <string.h>

XERCES_CPP_NAMESPACE_BEGIN
extern "OS" void TROTASC(const XMLByte * input,
                         XMLCh * output,
                         unsigned int * count,
                         XMLCh *table,
                         int STOP,
                         int * FLAG
                         );

//Add a long double in front of the table, the compiler will set the 
//table starting address on a double word boundary
struct temp{
   long double pad;
   XMLCh gFromTable[256];
};

static struct temp padding_temp={
 0,                   
    0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007
  , 0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F
  , 0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017
  , 0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F
  , 0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027
  , 0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F
  , 0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037
  , 0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F
  , 0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047
  , 0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F
  , 0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057
  , 0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F
  , 0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067
  , 0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F
  , 0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077
  , 0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
  , 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF
};



// ---------------------------------------------------------------------------
//  XMLASCIITranscoder390: Constructors and Destructor
// ---------------------------------------------------------------------------
XMLASCIITranscoder390::XMLASCIITranscoder390( const   XMLCh* const    encodingName
                                        , const unsigned int    blockSize
                                        , MemoryManager* const  manager) :

    XMLTranscoder(encodingName, blockSize, manager)
{
}


XMLASCIITranscoder390::~XMLASCIITranscoder390()
{
}


// ---------------------------------------------------------------------------
//  XMLASCIITranscoder390: Implementation of the transcoder API
// ---------------------------------------------------------------------------
unsigned int
XMLASCIITranscoder390::transcodeFrom(  const   XMLByte* const       srcData
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
    unsigned int    countDone = countToDo;
    int             flag = 0;

    // if flag is set to 1, an non-ASCII character is encountered
    TROTASC(srcPtr, toFill, &countDone, padding_temp.gFromTable, 0xFFFF, &flag);

    if (flag == 1 && countDone < 32){
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
    }//end if
       

    // Set the bytes we ate
    bytesEaten = countDone;

    // Set the char sizes to the fixed size
    memset(charSizes, 1, countDone);

    // Return the chars we transcoded
    return countDone;
}


unsigned int
XMLASCIITranscoder390::transcodeTo(const   XMLCh* const    srcData
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


bool XMLASCIITranscoder390::canTranscodeTo(const unsigned int toCheck) const
{
    return (toCheck < 0x80);
}

XERCES_CPP_NAMESPACE_END
