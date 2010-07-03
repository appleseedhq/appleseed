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
 * $Id: XML88591Transcoder390.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/TranscodingException.hpp>
#include <xercesc/util/Transcoders/Uniconv390/XML88591Transcoder390.hpp>
#include <xercesc/util/XMLString.hpp>
#include <string.h>


extern "OS" void TROT(const XMLByte * input, XMLCh * output,
                      int length, const XMLCh *table,
                      int STOP);

XERCES_CPP_NAMESPACE_BEGIN
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
  , 0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087
  , 0x0088, 0x0089, 0x008A, 0x008B, 0x008C, 0x008D, 0x008E, 0x008F
  , 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097
  , 0x0098, 0x0099, 0x009A, 0x009B, 0x009C, 0x009D, 0x009E, 0x009F
  , 0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7
  , 0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF
  , 0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7
  , 0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF
  , 0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7
  , 0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF
  , 0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7
  , 0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF
  , 0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7
  , 0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF
  , 0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7
  , 0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00FF
};


// ---------------------------------------------------------------------------
//  XML88591Transcoder390: Constructors and Destructor
// ---------------------------------------------------------------------------
XML88591Transcoder390::XML88591Transcoder390( const   XMLCh* const    encodingName
                                        , const unsigned int    blockSize
                                        , MemoryManager* const  manager) :

    XMLTranscoder(encodingName, blockSize, manager)
{
}


XML88591Transcoder390::~XML88591Transcoder390()
{
}


// ---------------------------------------------------------------------------
//  XML88591Transcoder390: Implementation of the transcoder API
// ---------------------------------------------------------------------------
unsigned int
XML88591Transcoder390::transcodeFrom(  const   XMLByte* const       srcData
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
    //  max output chars and the number of bytes in the source.
    //
    const unsigned int countToDo = srcCount < maxChars ? srcCount : maxChars;

    //
    //  Loop through the bytes to do and convert over each byte. Its just
    //  a cast to the wide char type.
    //
    const XMLByte*  srcPtr = srcData;
    XMLCh*          destPtr = toFill;
    const XMLByte*  srcEnd = srcPtr + countToDo;

    TROT(srcPtr, destPtr, countToDo, padding_temp.gFromTable, 0xFFFF);

    // Set the bytes eaten, and set the char size array to the fixed size
    bytesEaten = countToDo;
    memset(charSizes, 1, countToDo);

    // Return the chars we transcoded
    return countToDo;
}


unsigned int
XML88591Transcoder390::transcodeTo(const   XMLCh* const    srcData
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
    //  max output bytes and the number of chars in the source.
    //
    const unsigned int countToDo = srcCount < maxBytes ? srcCount : maxBytes;

    //
    //  Loop through the bytes to do and convert over each byte. Its just
    //  a downcast of the wide char, checking for unrepresentable chars.
    //
    const XMLCh*    srcPtr  = srcData;
    const XMLCh*    srcEnd  = srcPtr + countToDo;
    XMLByte*        destPtr = toFill;
    while (srcPtr < srcEnd)
    {
        // If its legal, take it and jump back to top
        if (*srcPtr < 256)
        {
            *destPtr++ = XMLByte(*srcPtr++);
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
        *destPtr++ = 0x1A;
        srcPtr++;
    }

    // Set the chars eaten
    charsEaten = countToDo;

    // Return the bytes we transcoded
    return countToDo;
}


bool XML88591Transcoder390::canTranscodeTo(const unsigned int toCheck) const
{
    return (toCheck < 256);
}

XERCES_CPP_NAMESPACE_END
