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
 * $Id: Iconv390TransService.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include "Iconv390TransService.hpp"
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/regx/XMLUniCharacter.hpp>
#include <xercesc/framework/MemoryManager.hpp>

#include <wchar.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef OS390BATCH
#include <unistd.h>
#endif
#include <ctype.h>

XERCES_CPP_NAMESPACE_BEGIN

//
//  Cannot use the OS/390 c/c++ towupper and towlower functions in the
//  Unicode environment. We will use mytowupper and mytowlower here.
//
#undef towupper
#undef towlower
#define towupper mytowupper
#define towlower mytowlower

// ---------------------------------------------------------------------------
//  Local, const data
// ---------------------------------------------------------------------------
static const int gTempBuffArraySize = 1024;
static const XMLCh  gMyServiceId[] =
{
    chLatin_I, chLatin_C, chLatin_o, chLatin_n, chLatin_v, chNull
};
// ---------------------------------------------------------------------------
//  gUnicodeToIBM037XlatTable
//      This is the translation table for Unicode to ibm-037. This table
//      contains 255 entries.
// ---------------------------------------------------------------------------
static const XMLByte gUnicodeToIBM037XlatTable[256] =
{
        0x00, 0x01, 0x02, 0x03, 0x37, 0x2D, 0x2E, 0x2F
    ,   0x16, 0x05, 0x25, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F
    ,   0x10, 0x11, 0x12, 0x13, 0x3C, 0x3D, 0x32, 0x26
    ,   0x18, 0x19, 0x3F, 0x27, 0x1C, 0x1D, 0x1E, 0x1F
    ,   0x40, 0x5A, 0x7F, 0x7B, 0x5B, 0x6C, 0x50, 0x7D
    ,   0x4D, 0x5D, 0x5C, 0x4E, 0x6B, 0x60, 0x4B, 0x61
    ,   0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7
    ,   0xF8, 0xF9, 0x7A, 0x5E, 0x4C, 0x7E, 0x6E, 0x6F
    ,   0x7C, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7
    ,   0xC8, 0xC9, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6
    ,   0xD7, 0xD8, 0xD9, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6
    ,   0xE7, 0xE8, 0xE9, 0xBA, 0xE0, 0xBB, 0xB0, 0x6D
    ,   0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87
    ,   0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96
    ,   0x97, 0x98, 0x99, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6
    ,   0xA7, 0xA8, 0xA9, 0xC0, 0x4F, 0xD0, 0xA1, 0x07
    ,   0x20, 0x21, 0x22, 0x23, 0x24, 0x15, 0x06, 0x17
    ,   0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x09, 0x0A, 0x1B
    ,   0x30, 0x31, 0x1A, 0x33, 0x34, 0x35, 0x36, 0x08
    ,   0x38, 0x39, 0x3A, 0x3B, 0x04, 0x14, 0x3E, 0xFF
    ,   0x41, 0xAA, 0x4A, 0xB1, 0x9F, 0xB2, 0x6A, 0xB5
    ,   0xBD, 0xB4, 0x9A, 0x8A, 0x5F, 0xCA, 0xAF, 0xBC
    ,   0x90, 0x8F, 0xEA, 0xFA, 0xBE, 0xA0, 0xB6, 0xB3
    ,   0x9D, 0xDA, 0x9B, 0x8B, 0xB7, 0xB8, 0xB9, 0xAB
    ,   0x64, 0x65, 0x62, 0x66, 0x63, 0x67, 0x9E, 0x68
    ,   0x74, 0x71, 0x72, 0x73, 0x78, 0x75, 0x76, 0x77
    ,   0xAC, 0x69, 0xED, 0xEE, 0xEB, 0xEF, 0xEC, 0xBF
    ,   0x80, 0xFD, 0xFE, 0xFB, 0xFC, 0xAD, 0xAE, 0x59
    ,   0x44, 0x45, 0x42, 0x46, 0x43, 0x47, 0x9C, 0x48
    ,   0x54, 0x51, 0x52, 0x53, 0x58, 0x55, 0x56, 0x57
    ,   0x8C, 0x49, 0xCD, 0xCE, 0xCB, 0xCF, 0xCC, 0xE1
    ,   0x70, 0xDD, 0xDE, 0xDB, 0xDC, 0x8D, 0x8E, 0xDF
};
iconvconverter * converterList;
XMLMutex  converterListMutex;

iconvconverter * addConverter(const char* const EncodingName
                             ,XMLTransService::Codes& resValue)
{
    XMLMutexLock lockConverterlist(&converterListMutex);
    iconvconverter *tconv=converterList;
    while ( (tconv) &&
            (strcmp(tconv->name,EncodingName)) )
      tconv = tconv->nextconverter;

    if (tconv) {
      tconv->usecount++;
    }
    else {
      tconv = new iconvconverter;
      strcpy(tconv->name,EncodingName);
      tconv->usecount=1;
      tconv->fIconv390Descriptor = iconv_open("UCS-2",EncodingName);
      if (tconv->fIconv390Descriptor == (iconv_t)(-1)) {
         resValue = XMLTransService::UnsupportedEncoding;
         delete tconv;
         return 0;
      }
      tconv->nextconverter = converterList;
      converterList = tconv;
    }
    return tconv;
}

void removeConverter(iconvconverter* const converter)
{
    iconvconverter *pconv,*tconv;
    tconv = 0;
    if (converter) {
      XMLMutexLock lockConverterlist(&converterListMutex);
      if (--converter->usecount==0) {
        tconv = converterList;
        pconv = (iconvconverter*)&converterList;
        while ( (tconv) && (tconv!=converter) ) {
          pconv=tconv;
          tconv=tconv->nextconverter;
        }

        pconv->nextconverter=tconv->nextconverter;
      }
    }

    if (tconv) {
      iconv_close(tconv->fIconv390Descriptor);
      delete tconv;
    }
}

// ---------------------------------------------------------------------------
//  Local methods
// ---------------------------------------------------------------------------
static unsigned int  getWideCharLength(const XMLCh* const src)
{
    if (!src)
        return 0;

    unsigned int len = 0;
    const XMLCh* pTmp = src;
    while (*pTmp++)
        len++;
    return len;
}

// ---------------------------------------------------------------------------
//  Iconv390TransService: Constructors and Destructor
// ---------------------------------------------------------------------------
Iconv390TransService::Iconv390TransService()
{
}

Iconv390TransService::~Iconv390TransService()
{
}


// ---------------------------------------------------------------------------
//  Iconv390TransService: The virtual transcoding service API
// ---------------------------------------------------------------------------
int Iconv390TransService::compareIString(  const   XMLCh* const    comp1
                                           , const XMLCh* const    comp2)
{
    const XMLCh* cptr1 = comp1;
    const XMLCh* cptr2 = comp2;

    while ( (*cptr1 != 0) && (*cptr2 != 0) )
    {
        wint_t wch1 = towupper(*cptr1);
        wint_t wch2 = towupper(*cptr2);
        if (wch1 != wch2)
            break;

        cptr1++;
        cptr2++;
    }
    return (int) ( towupper(*cptr1) - towupper(*cptr2) );
}

int Iconv390TransService::compareNIString( const   XMLCh* const    comp1
                                           , const XMLCh* const    comp2
                                           , const unsigned int    maxChars)
{
    unsigned int  n = 0;
    const XMLCh* cptr1 = comp1;
    const XMLCh* cptr2 = comp2;

    while (true && maxChars)
    {
        wint_t wch1 = towupper(*cptr1);
        wint_t wch2 = towupper(*cptr2);

        if (wch1 != wch2)
            return (int) (wch1 - wch2);

        // If either ended, then both ended, so equal
        if (!*cptr1 || !*cptr2)
            break;

        cptr1++;
        cptr2++;

        //  Bump the count of chars done. If it equals the count then we
        //  are equal for the requested count, so break out and return
        //  equal.
        n++;
        if (n == maxChars)
            break;
    }

    return 0;
}

const XMLCh* Iconv390TransService::getId() const
{
    return gMyServiceId;
}

bool Iconv390TransService::isSpace(const XMLCh toCheck) const
{
   unsigned short chartype = XMLUniCharacter::getType(toCheck);
   if ( (chartype == XMLUniCharacter::SPACE_SEPARATOR) ||
        (chartype == XMLUniCharacter::LINE_SEPARATOR)   ||
        (chartype == XMLUniCharacter::PARAGRAPH_SEPARATOR) )
      return true;
   else
      return false;
}


XMLLCPTranscoder* Iconv390TransService::makeNewLCPTranscoder()
{
    XMLTransService::Codes resValue;
    // native MVS default code page is IBM-037
    iconvconverter *tconv=addConverter("IBM-037",resValue);

    if (tconv == 0) {
        return 0;
    }

    return new Iconv390LCPTranscoder(tconv);
}

bool Iconv390TransService::supportsSrcOfs() const
{
    return true;
}


// ---------------------------------------------------------------------------
//  Iconv390TransService: The protected virtual transcoding service API
// ---------------------------------------------------------------------------
XMLTranscoder*
Iconv390TransService::makeNewXMLTranscoder(const   XMLCh* const            encodingName
                                        ,       XMLTransService::Codes& resValue
                                        , const unsigned int
                                        ,       MemoryManager* const)
{
    //  This is a minimalist transcoding service, that only supports a local
    //  default transcoder. All named encodings return zero as a failure,
    //  which means that only the intrinsic encodings supported by the parser
    //  itself will work for XML data.
    //
    resValue = XMLTransService::UnsupportedEncoding;
    return 0;
}

void Iconv390TransService::upperCase(XMLCh* const toUpperCase) const
{
    XMLCh* outPtr = toUpperCase;
    while (*outPtr != 0) {
	if ((*outPtr >= 0x61) && (*outPtr <= 0x7A))
	    *outPtr = *outPtr - 0x20;
	outPtr++;
    }
}

void Iconv390TransService::lowerCase(XMLCh* const toLowerCase) const
{
    XMLCh* outPtr = toLowerCase;
    while (*outPtr != 0) {
	if ((*outPtr >= 0x41) && (*outPtr <= 0x5A))
	    *outPtr = *outPtr + 0x20;
	outPtr++;
    }
}

// ---------------------------------------------------------------------------
unsigned int Iconv390LCPTranscoder::calcRequiredSize(const char* const srcText
                                                     , MemoryManager* const manager)
{
    if (!srcText)
        return 0;

    unsigned charLen = ::mblen(srcText, MB_CUR_MAX);
    if (charLen == -1)
        return 0;
    else if (charLen != 0)
        charLen = strlen(srcText)/charLen;

    if (charLen == -1)
        return 0;
    return charLen;
}


unsigned int Iconv390LCPTranscoder::calcRequiredSize(const XMLCh* const srcText
                                                     , MemoryManager* const manager)
{
    if (!srcText)
        return 0;

    unsigned int  wLent = getWideCharLength(srcText);
    wchar_t       tmpWideCharArr[gTempBuffArraySize];
    wchar_t*      allocatedArray = 0;
    wchar_t*      wideCharBuf = 0;

    if (wLent >= gTempBuffArraySize)
        wideCharBuf = allocatedArray = (wchar_t*) manager->allocate
        (
            (wLent + 1) * sizeof(wchar_t)
        );//new wchar_t[wLent + 1];
    else
        wideCharBuf = tmpWideCharArr;

    for (unsigned int i = 0; i < wLent; i++)
    {
        wideCharBuf[i] = srcText[i];
    }
    wideCharBuf[wLent] = 0x00;

    const unsigned int retVal = ::wcstombs(NULL, wideCharBuf, 0);
    manager->deallocate(allocatedArray);//delete [] allocatedArray;

    if (retVal == -1)
        return 0;
    return retVal;
}



char* Iconv390LCPTranscoder::transcode(const XMLCh* const toTranscode)
{
    if (!toTranscode)
        return 0;

    char* retVal = 0;
    if (*toTranscode)
    {
        unsigned int  wLent = getWideCharLength(toTranscode);
	//
	//  Translate the input from Unicode XMLCh format into
	//  ibm-037 char format via the lookup table.
	//
        retVal = new char[wLent + 1];
        const XMLCh *srcPtr = toTranscode;
        char *outPtr = retVal;

	while (*srcPtr != 0)
	    *outPtr++ = gUnicodeToIBM037XlatTable[*srcPtr++];
	*outPtr=0;
    }
    else
    {
        retVal = new char[1];
        retVal[0] = 0;
    }
    return retVal;
}

char* Iconv390LCPTranscoder::transcode(const XMLCh* const toTranscode,
                                       MemoryManager* const manager)
{
    if (!toTranscode)
        return 0;

    char* retVal = 0;
    if (*toTranscode)
    {
        unsigned int  wLent = getWideCharLength(toTranscode);
	//
	//  Translate the input from Unicode XMLCh format into
	//  ibm-037 char format via the lookup table.
	//
        retVal = (char*) manager->allocate((wLent + 1) * sizeof(char));//new char[wLent + 1];
        const XMLCh *srcPtr = toTranscode;
        char *outPtr = retVal;

	while (*srcPtr != 0)
	    *outPtr++ = gUnicodeToIBM037XlatTable[*srcPtr++];
	*outPtr=0;
    }
    else
    {
        retVal = (char*) manager->allocate(sizeof(char));//new char[1];
        retVal[0] = 0;
    }
    return retVal;
}


bool Iconv390LCPTranscoder::transcode( const   XMLCh* const    toTranscode
                                    ,       char* const     toFill
                                    , const unsigned int    maxBytes
                                    , MemoryManager* const  manager)
{
    // Watch for a couple of pyscho corner cases
    if (!toTranscode || !maxBytes)
    {
        toFill[0] = 0;
        return true;
    }

    if (!*toTranscode)
    {
        toFill[0] = 0;
        return true;
    }

    const XMLCh *srcPtr = toTranscode;
    char *outPtr = toFill;
    int bytectr = maxBytes;

    while (bytectr-- && *srcPtr)
       *outPtr++ = gUnicodeToIBM037XlatTable[*srcPtr++];
    *outPtr=0;

    return true;
}



XMLCh* Iconv390LCPTranscoder::transcode(const char* const toTranscode)
{
    if (!toTranscode)
        return 0;

    XMLCh* retVal = 0;
    if (*toTranscode)
    {
        const unsigned int len = calcRequiredSize(toTranscode);
        if (len == 0)
        {
            retVal = new XMLCh[1];
            retVal[0] = 0;
            return retVal;
        }


        wchar_t       tmpWideCharArr[gTempBuffArraySize];
        wchar_t*      allocatedArray = 0;
        wchar_t*      wideCharBuf = 0;

        retVal = new XMLCh[len + 1];

        size_t retCode;
        char *tmpInPtr = (char*) toTranscode;
        char *tmpOutPtr = (char*) retVal;
        size_t inByteLeft = len;
        size_t outByteLeft = len*2;
        {
         XMLMutexLock lockConverter(&converter->fMutex);
         retCode = iconv(converter->fIconv390Descriptor, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
        }
        if (retCode == -1) {
            delete [] retVal;
            return 0;
        }
        retVal[len] = 0x00;
        delete [] allocatedArray;
    }
    else
    {
        retVal = new XMLCh[1];
        retVal[0] = 0;
    }
    return retVal;
}

XMLCh* Iconv390LCPTranscoder::transcode(const char* const toTranscode,
                                        MemoryManager* const manager)
{
    if (!toTranscode)
        return 0;

    XMLCh* retVal = 0;
    if (*toTranscode)
    {
        const unsigned int len = calcRequiredSize(toTranscode, manager);
        if (len == 0)
        {
            retVal = (XMLCh*) manager->allocate(sizeof(XMLCh));//new XMLCh[1];
            retVal[0] = 0;
            return retVal;
        }

        wchar_t       tmpWideCharArr[gTempBuffArraySize];
        wchar_t*      allocatedArray = 0;
        wchar_t*      wideCharBuf = 0;

        retVal = (XMLCh*) manager->allocate((len + 1) * sizeof(XMLCh));//new XMLCh[len + 1];

        size_t retCode;
        char *tmpInPtr = (char*) toTranscode;
        char *tmpOutPtr = (char*) retVal;
        size_t inByteLeft = len;
        size_t outByteLeft = len*2;
        {
         XMLMutexLock lockConverter(&converter->fMutex);
         retCode = iconv(converter->fIconv390Descriptor, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
        }
        if (retCode == -1) {
            manager->deallocate(retVal);//delete [] retVal;
            return 0;
        }
        retVal[len] = 0x00;
        manager->deallocate(allocatedArray);//delete [] allocatedArray;
    }
    else
    {
        retVal = (XMLCh*) manager->allocate(sizeof(XMLCh));//new XMLCh[1];
        retVal[0] = 0;
    }
    return retVal;
}


bool Iconv390LCPTranscoder::transcode( const   char* const     toTranscode
                                    ,       XMLCh* const    toFill
                                    , const unsigned int    maxChars
                                    , MemoryManager* const  manager)
{
    // Check for a couple of psycho corner cases
    if (!toTranscode || !maxChars)
    {
        toFill[0] = 0;
        return true;
    }

    if (!*toTranscode)
    {
        toFill[0] = 0;
        return true;
    }

    size_t retCode;
    char *tmpInPtr = (char*) toTranscode;
    char *tmpOutPtr = (char*) toFill;
    size_t inByteLeft = maxChars;
    size_t outByteLeft = maxChars*2;
    {
     XMLMutexLock lockConverter(&converter->fMutex);
     retCode = iconv(converter->fIconv390Descriptor, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
    }
    if ( (retCode == -1) && (outByteLeft!=0) ) {
        return false;
    }
    toFill[maxChars] = 0x00;
    return true;
}



// ---------------------------------------------------------------------------
//  Iconv390LCPTranscoder: Constructors and Destructor
// ---------------------------------------------------------------------------
Iconv390LCPTranscoder::Iconv390LCPTranscoder()
{
}

Iconv390LCPTranscoder::Iconv390LCPTranscoder(iconvconverter_t* const toAdopt) :
        converter (toAdopt)
{
}

Iconv390LCPTranscoder::~Iconv390LCPTranscoder()
{
    removeConverter(converter);
    converter=0;
}

XERCES_CPP_NAMESPACE_END
