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
 * $Id: XMLString.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <errno.h>
#include <assert.h>

#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/ArrayIndexOutOfBoundsException.hpp>
#include <xercesc/util/IllegalArgumentException.hpp>
#include <xercesc/util/NumberFormatException.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/util/TranscodingException.hpp>

#include <xercesc/util/Janitor.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/RefArrayVectorOf.hpp>
#include <xercesc/util/TransService.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/XMLUri.hpp>
#include <xercesc/util/XMLURL.hpp>
#include <xercesc/internal/XMLReader.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local static data
//
//  gConverter
//      This is initialized when the user calls the platform init method,
//      which calls our init method. This is the converter used for default
//      conversion to/from the local code page.
// ---------------------------------------------------------------------------
static XMLLCPTranscoder*    gTranscoder = 0;
static XMLCh                gNullStr[] =
{
    chOpenCurly, chLatin_n, chLatin_u, chLatin_l, chLatin_l, chCloseCurly, chNull
};

MemoryManager* XMLString::fgMemoryManager = 0;


// ---------------------------------------------------------------------------
//  XMLString: Public static methods
// ---------------------------------------------------------------------------
void XMLString::binToText(  const   unsigned long   toFormat
                            ,       char* const     toFill
                            , const unsigned int    maxChars
                            , const unsigned int    radix
                            , MemoryManager* const  manager)
{
    static const char digitList[16] =
    {
          '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
        , 'A', 'B', 'C', 'D', 'E', 'F'
    };

    if (!maxChars)
        ThrowXMLwithMemMgr(IllegalArgumentException, XMLExcepts::Str_ZeroSizedTargetBuf, manager);

    // Handle special case
    if (!toFormat)
    {
        toFill[0] = '0';
        toFill[1] = 0;
        return;
    }

    // This is used to fill the temp buffer
    unsigned int tmpIndex = 0;

    // A copy of the conversion value that we can modify
    unsigned int tmpVal = toFormat;

    //
    //  Convert into a temp buffer that we know is large enough. This avoids
    //  having to check for overflow in the inner loops, and we have to flip
    //  the resulting XMLString anyway.
    //
    char   tmpBuf[128];

    //
    //  For each radix, do the optimal thing. For bin and hex, we can special
    //  case them and do shift and mask oriented stuff. For oct and decimal
    //  there isn't much to do but bull through it with divides.
    //
    if (radix == 2)
    {
        while (tmpVal)
        {
            if (tmpVal & 0x1UL)
                tmpBuf[tmpIndex++] = '1';
            else
                tmpBuf[tmpIndex++] = '0';
            tmpVal >>= 1;
        }
    }
     else if (radix == 16)
    {
        while (tmpVal)
        {
            const unsigned int charInd = (tmpVal & 0xFUL);
            tmpBuf[tmpIndex++] = digitList[charInd];
            tmpVal >>= 4;
        }
    }
     else if ((radix == 8) || (radix == 10))
    {
        while (tmpVal)
        {
            const unsigned int charInd = (tmpVal % radix);
            tmpBuf[tmpIndex++] = digitList[charInd];
            tmpVal /= radix;
        }
    }
    else
    {
        ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Str_UnknownRadix, manager);
    }

    // See if have enough room in the caller's buffer
    if (tmpIndex > maxChars)
    {
        ThrowXMLwithMemMgr(IllegalArgumentException, XMLExcepts::Str_TargetBufTooSmall, manager);
    }

    // Reverse the tmp buffer into the caller's buffer
    unsigned int outIndex = 0;
    for (; tmpIndex > 0; tmpIndex--)
        toFill[outIndex++] = tmpBuf[tmpIndex-1];

    // And cap off the caller's buffer
    toFill[outIndex] = char(0);
}

void XMLString::binToText(  const   unsigned int    toFormat
                            ,       char* const     toFill
                            , const unsigned int    maxChars
                            , const unsigned int    radix
                            , MemoryManager* const  manager)
{
    // Just call the unsigned long version
    binToText((unsigned long)toFormat, toFill, maxChars, radix, manager);
}

void XMLString::binToText(  const   long            toFormat
                            ,       char* const     toFill
                            , const unsigned int    maxChars
                            , const unsigned int    radix
                            , MemoryManager* const  manager)
{
    //
    //  If its negative, then put a negative sign into the output and flip
    //  the sign of the local temp value.
    //
    unsigned int startInd = 0;
    unsigned long actualVal;
    if (toFormat < 0)
    {
        toFill[0] = '-';
        startInd++;
        actualVal = (unsigned long)(toFormat * -1);
    }
     else
    {
        actualVal = (unsigned long)(toFormat);
    }

    // And now call the unsigned long version
    binToText(actualVal, &toFill[startInd], maxChars, radix, manager);
}

void XMLString::binToText(  const   int             toFormat
                            ,       char* const     toFill
                            , const unsigned int    maxChars
                            , const unsigned int    radix
                            , MemoryManager* const  manager)
{
    //
    //  If its negative, then put a negative sign into the output and flip
    //  the sign of the local temp value.
    //
    unsigned int startInd = 0;
    unsigned long actualVal;
    if (toFormat < 0)
    {
        toFill[0] = '-';
        startInd++;
        actualVal = (unsigned long)(toFormat * -1);
    }
     else
    {
        actualVal = (unsigned long)(toFormat);
    }

    // And now call the unsigned long version
    binToText(actualVal, &toFill[startInd], maxChars, radix, manager);
}


void XMLString::catString(char* const target, const char* const src)
{
    strcat(target, src);
}


int XMLString::compareIString(const char* const str1, const char* const str2)
{
    return stricmp(str1, str2);
}


int XMLString::compareNString(  const   char* const     str1
                                , const char* const     str2
                                , const unsigned int    count)
{
    // Watch for pathological secenario
    if (!count)
        return 0;

    return strncmp(str1, str2, count);
}


int XMLString::compareNIString( const   char* const     str1
                                , const char* const     str2
                                , const unsigned int    count)
{
    if (!count)
        return 0;

    return strnicmp(str1, str2, count);
}


int XMLString::compareString(   const   char* const    str1
                                , const char* const    str2)
{
    return strcmp(str1, str2);
}


void XMLString::copyString(         char* const    target
                            , const char* const    src)
{
    strcpy(target, src);
}


void XMLString::cut(        XMLCh* const    toCutFrom
                    , const unsigned int    count)
{
    #if defined(XML_DEBUG)
    if (count > stringLen(toCutFrom))
    {
        // <TBD> This is bad of course
    }
    #endif

    // If count is zero, then nothing to do
    if (!count)
        return;

    XMLCh* targetPtr = toCutFrom;
    XMLCh* srcPtr = toCutFrom + count;
    while (*srcPtr)
        *targetPtr++ = *srcPtr++;

    // Cap it off at the new end
    *targetPtr = 0;
}


unsigned int XMLString::hash(   const   char* const     tohash
                                , const unsigned int    hashModulus
                                , MemoryManager* const)
{    
    assert(hashModulus);

    unsigned int hashVal = 0;
    if (tohash) {
        const char* curCh = tohash;
        while (*curCh)
        {
            unsigned int top = hashVal >> 24;
            hashVal += (hashVal * 37) + top + (unsigned int)(*curCh);
            curCh++;
        }
    }

    // Divide by modulus
    return hashVal % hashModulus;
}


int XMLString::indexOf(const char* const toSearch, const char ch)
{
    const unsigned int len = strlen(toSearch);
    for (unsigned int i = 0; i < len; i++)
    {
        if (toSearch[i] == ch)
            return i;
    }
    return -1;
}


int XMLString::indexOf( const   char* const     toSearch
                        , const char            ch
                        , const unsigned int    fromIndex
                        , MemoryManager* const  manager)
{
    const unsigned int len = strlen(toSearch);

    // Make sure the start index is within the XMLString bounds
	if ((int)fromIndex > ((int)len)-1)
        ThrowXMLwithMemMgr(ArrayIndexOutOfBoundsException, XMLExcepts::Str_StartIndexPastEnd, manager);

    for (unsigned int i = fromIndex; i < len; i++)
    {
        if (toSearch[i] == ch)
            return i;
    }
    return -1;
}

int XMLString::lastIndexOf(const char* const toSearch, const char ch)
{
    const int len = strlen(toSearch);
    for (int i = len-1; i >= 0; i--)
    {
        if (toSearch[i] == ch)
            return i;
    }
    return -1;
}

int XMLString::lastIndexOf( const   char* const     toSearch
                            , const char            ch
                            , const unsigned int    fromIndex
                            , MemoryManager* const  manager)
{
    const int len = strlen(toSearch);

    // Make sure the start index is within the XMLString bounds
	if ((int)fromIndex > len-1)
        ThrowXMLwithMemMgr(ArrayIndexOutOfBoundsException, XMLExcepts::Str_StartIndexPastEnd, manager);

    for (int i = (int)fromIndex; i >= 0; i--)
    {
        if (toSearch[i] == ch)
            return i;
    }
    return -1;
}


unsigned int XMLString::replaceTokens(          XMLCh* const    errText
                                        , const unsigned int    maxChars
                                        , const XMLCh* const    text1
                                        , const XMLCh* const    text2
                                        , const XMLCh* const    text3
                                        , const XMLCh* const    text4
                                        , MemoryManager* const  manager)
{
    //
    //  We have to build the string back into the source string, so allocate
    //  a temp string and copy the orignal text to it. We'll then treat the
    //  incoming buffer as a target buffer. Put a janitor on it to make sure
    //  it gets cleaned up.
    //
    XMLCh* orgText = replicate(errText, manager);
    ArrayJanitor<XMLCh> janText(orgText, manager);

    XMLCh* pszSrc = orgText;
    unsigned int curOutInd = 0;

    while (*pszSrc && (curOutInd < maxChars))
    {
        //
        //  Loop until we see a { character. Until we do, just copy chars
        //  from src to target, being sure not to overrun the output buffer.
        //
        while ((*pszSrc != chOpenCurly) && (curOutInd < maxChars))
        {
            if (!*pszSrc)
                break;
            errText[curOutInd++] = *pszSrc++;
        }

        // If we did not find a curly, then we are done
        if (*pszSrc != chOpenCurly)
            break;

        //
        //  Probe this one to see if it matches our pattern of {x}. If not
        //  then copy over those chars and go back to the first loop.
        //
        if ((*(pszSrc+1) >= chDigit_0)
        &&  (*(pszSrc+1) <= chDigit_3)
        &&  (*(pszSrc+2) == chCloseCurly))
        {
            //
            //  Its one of our guys, so move the source pointer up past the
            //  token we are replacing. First though get out the token number
            //  character.
            //
            XMLCh tokCh = *(pszSrc+1);
            pszSrc += 3;

            // Now copy over the replacement text
            const XMLCh* repText = 0;
            if (tokCh == chDigit_0)
                repText = text1;
            else if (tokCh == chDigit_1)
                repText = text2;
            else if (tokCh == chDigit_2)
                repText = text3;
            else if (tokCh == chDigit_3)
                repText = text4;

            // If this one is null, copy over a null string
            if (!repText)
                repText = gNullStr;

            while (*repText && (curOutInd < maxChars))
                errText[curOutInd++] = *repText++;
        }
         else
        {
            // Escape the curly brace character and continue
            errText[curOutInd++] = *pszSrc++;
        }
    }

    // Copy over a null terminator
    errText[curOutInd] = 0;

    // And return the count of chars we output
    return curOutInd;
}


XMLCh* XMLString::replicate(const XMLCh* const toRep)
{
    // If a null string, return a null string!
    XMLCh* ret = 0;
    if (toRep)
    {
        const unsigned int len = stringLen(toRep);
        ret = new XMLCh[len + 1];
        memcpy(ret, toRep, (len + 1) * sizeof(XMLCh));
    }
    return ret;
}

char* XMLString::replicate(const char* const toRep)
{
    // If a null string, return a null string
    if (!toRep)
        return 0;

    //
    //  Get the len of the source and allocate a new buffer. Make sure to
    //  account for the nul terminator.
    //
    const unsigned int srcLen = strlen(toRep);
    char* ret = new char[srcLen+1];

    // Copy over the text, adjusting for the size of a char
    memcpy(ret, toRep, (srcLen+1) * sizeof(char));
    return ret;
}

char* XMLString::replicate( const char* const    toRep
                          , MemoryManager* const manager)
{
    // If a null string, return a null string
    if (!toRep)
        return 0;

    //
    //  Get the len of the source and allocate a new buffer. Make sure to
    //  account for the nul terminator.
    //
    const unsigned int srcLen = strlen(toRep);
    char* ret = (char*) manager->allocate((srcLen+1) * sizeof(char)); //new char[srcLen+1];

    // Copy over the text, adjusting for the size of a char
    memcpy(ret, toRep, (srcLen+1) * sizeof(char));
    return ret;
}


bool XMLString::startsWith(const char* const toTest, const char* const prefix)
{
    return (strncmp(toTest, prefix, strlen(prefix)) == 0);
}


bool XMLString::startsWithI(const   char* const toTest
                            , const char* const prefix)
{
    return (strnicmp(toTest, prefix, strlen(prefix)) == 0);
}


unsigned int XMLString::stringLen(const char* const src)
{
    return strlen(src);
}


char* XMLString::transcode(const XMLCh* const toTranscode)
{
    return gTranscoder->transcode(toTranscode);
}

char* XMLString::transcode(const XMLCh* const toTranscode,
                           MemoryManager* const manager)
{
    return gTranscoder->transcode(toTranscode, manager);
}

bool XMLString::transcode(  const   XMLCh* const    toTranscode
                            ,       char* const     toFill
                            , const unsigned int    maxChars
                            , MemoryManager* const  manager)
{
    if (!gTranscoder->transcode(toTranscode, toFill, maxChars, manager))
        return false;
    return true;
}

XMLCh* XMLString::transcode(const char* const toTranscode)
{
    return gTranscoder->transcode(toTranscode);
}

XMLCh* XMLString::transcode(const char* const toTranscode,
                            MemoryManager* const manager)
{
    return gTranscoder->transcode(toTranscode, manager);
}

bool XMLString::transcode(  const   char* const     toTranscode
                            ,       XMLCh* const    toFill
                            , const unsigned int    maxChars
                            , MemoryManager* const  manager)
{
    if (!gTranscoder->transcode(toTranscode, toFill, maxChars, manager))
        return false;
    return true;
}


void XMLString::trim(char* const toTrim)
{
    const unsigned int len = strlen(toTrim);

    unsigned int skip, scrape;
    for (skip = 0; skip < len; skip++)
    {
        if (! isspace(toTrim[skip]))
            break;
    }

    for (scrape = len; scrape > skip; scrape--)
    {
        if (! isspace(toTrim[scrape - 1] ))
            break;
    }

    // Cap off at the scrap point
    if (scrape != len)
        toTrim[scrape] = 0;

    if (skip)
    {
        // Copy the chars down
        unsigned int index = 0;
        while (toTrim[skip])
            toTrim[index++] = toTrim[skip++];

        toTrim[index] = 0;
    }
}


void XMLString::subString(char* const targetStr, const char* const srcStr
                          , const int startIndex, const int endIndex
                          , MemoryManager* const manager)
{
    if (targetStr == 0)
        ThrowXMLwithMemMgr(IllegalArgumentException, XMLExcepts::Str_ZeroSizedTargetBuf, manager);

    const int srcLen = strlen(srcStr);
    const int copySize = endIndex - startIndex;

    // Make sure the start index is within the XMLString bounds
    if ( startIndex < 0 || startIndex > endIndex || endIndex > srcLen)
        ThrowXMLwithMemMgr(ArrayIndexOutOfBoundsException, XMLExcepts::Str_StartIndexPastEnd, manager);

    for (int i= startIndex; i < endIndex; i++) {
        targetStr[i-startIndex] = srcStr[i];
    }

    targetStr[copySize] = 0;
}

bool XMLString::isValidNOTATION(const XMLCh*         const name
                              ,       MemoryManager* const manager )
{
    //
    //  NOTATATION: <URI>:<localPart>
    //  where URI is optional
    //        ':' and localPart must be present
    //
    int nameLen = XMLString::stringLen(name);
    int colPos = XMLString::lastIndexOf(name, chColon);

    if ((colPos == -1)         ||      // no ':'
        (colPos == nameLen - 1)  )     // <URI>':'
        return false;


    // Examine localpart
    if (!XMLString::isValidNCName(&name[colPos+1]))
    {
        return false;
    }
    else if (colPos == 0)
    {
        return true;
    }
    else
    {
        // Examine URI
        XMLCh* const temp =
            (XMLCh*) manager->allocate((colPos + 1) * sizeof(XMLCh));
        const ArrayJanitor<XMLCh> jan(temp, manager);

        copyNString(temp, name, colPos);
        temp[colPos] = 0;

        try
        {            
            XMLUri  newURI(temp, manager); // no relative uri support here
        }
        catch (const MalformedURLException&)
        {
            return false;
        }

        return true;
    }
}


/**
  * Deprecated: isValidNCName
  *    Check first char, and then the rest of the name char.
  *    But colon should be excluded
  */
bool XMLString::isValidNCName(const XMLCh* const name) {
    return XMLChar1_0::isValidNCName(name, XMLString::stringLen(name));
}

/**
  * Deprecated: isValidName
  *    Check first char, and then the rest of the name char
  *
  */
bool XMLString::isValidName(const XMLCh* const name) {
    return XMLChar1_0::isValidName(name, XMLString::stringLen(name));
}

/**
  * isValidEncName
  *
  * [80] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
  *
  */
bool XMLString::isValidEncName(const XMLCh* const name)
{

    if ( XMLString::stringLen(name) == 0 )
        return false;

    const XMLCh* tempName = name;
    XMLCh firstChar = *tempName++;

    if (!isAlpha(firstChar))
        return false;

    while(*tempName)
    {
        if (( !isAlpha(*tempName))       &&
            ( !isDigit(*tempName))       &&
            ( *tempName != chPeriod)     &&
            ( *tempName != chUnderscore) &&
            ( *tempName != chDash)        )
            return false;

        tempName++;
    }

    return true;
}

/**
  * Deprecated: isValidQName
  *
  */
bool XMLString::isValidQName(const XMLCh* const name)
{
    return XMLChar1_0::isValidQName(name, XMLString::stringLen(name));
}

bool XMLString::isAlpha(XMLCh const theChar)
{
    if ((( theChar >= chLatin_a ) && ( theChar <= chLatin_z )) ||
        (( theChar >= chLatin_A ) && ( theChar <= chLatin_Z )) )
        return true;

    return false;
}

bool XMLString::isDigit(XMLCh const theChar)
{
    if (( theChar >= chDigit_0 ) && ( theChar <= chDigit_9 ))
        return true;

    return false;
}

bool XMLString::isAlphaNum(XMLCh const theChar)
{
    return (isAlpha(theChar) || isDigit(theChar));
}

bool XMLString::isHex(XMLCh const theChar)
{
	return (isDigit(theChar) ||
			(theChar >= chLatin_a && theChar <= chLatin_f) ||
			(theChar >= chLatin_A && theChar <= chLatin_F));
}

// Deprecated
bool XMLString::isAllWhiteSpace(const XMLCh* const toCheck)
{
    return XMLChar1_0::isAllSpaces(toCheck, XMLString::stringLen(toCheck));
}

// ---------------------------------------------------------------------------
//  Wide char versions of most of the string methods
// ---------------------------------------------------------------------------
void XMLString::binToText(  const   unsigned long   toFormat
                            ,       XMLCh* const    toFill
                            , const unsigned int    maxChars
                            , const unsigned int    radix
                            , MemoryManager* const  manager)
{
    static const XMLCh digitList[16] =
    {
            chDigit_0, chDigit_1, chDigit_2, chDigit_3, chDigit_4, chDigit_5
        ,   chDigit_6, chDigit_7, chDigit_8, chDigit_9, chLatin_A, chLatin_B
        ,   chLatin_C, chLatin_D, chLatin_e, chLatin_F
    };

    if (!maxChars)
        ThrowXMLwithMemMgr(IllegalArgumentException, XMLExcepts::Str_ZeroSizedTargetBuf, manager);

    // Handle special case
    if (!toFormat)
    {
        toFill[0] = chDigit_0;
        toFill[1] = chNull;
        return;
    }

    // This is used to fill the temp buffer
    unsigned int tmpIndex = 0;

    // A copy of the conversion value that we can modify
    unsigned int tmpVal = toFormat;

    //
    //  Convert into a temp buffer that we know is large enough. This avoids
    //  having to check for overflow in the inner loops, and we have to flip
    //  the resulting sring anyway.
    //
    XMLCh   tmpBuf[128];

    //
    //  For each radix, do the optimal thing. For bin and hex, we can special
    //  case them and do shift and mask oriented stuff. For oct and decimal
    //  there isn't much to do but bull through it with divides.
    //
    if (radix == 2)
    {
        while (tmpVal)
        {
            if (tmpVal & 0x1UL)
                tmpBuf[tmpIndex++] = chDigit_1;
            else
                tmpBuf[tmpIndex++] = chDigit_0;
            tmpVal >>= 1;
        }
    }
     else if (radix == 16)
    {
        while (tmpVal)
        {
            const unsigned int charInd = (tmpVal & 0xFUL);
            tmpBuf[tmpIndex++] = digitList[charInd];
            tmpVal >>= 4;
        }
    }
     else if ((radix == 8) || (radix == 10))
    {
        while (tmpVal)
        {
            const unsigned int charInd = (tmpVal % radix);
            tmpBuf[tmpIndex++] = digitList[charInd];
            tmpVal /= radix;
        }
    }
     else
    {
        ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Str_UnknownRadix, manager);
    }

    // See if have enough room in the caller's buffer
    if (tmpIndex > maxChars)
    {
        ThrowXMLwithMemMgr(IllegalArgumentException, XMLExcepts::Str_TargetBufTooSmall, manager);
    }

    // Reverse the tmp buffer into the caller's buffer
    unsigned int outIndex = 0;
    for (; tmpIndex > 0; tmpIndex--)
        toFill[outIndex++] = tmpBuf[tmpIndex-1];

    // And cap off the caller's buffer
    toFill[outIndex] = chNull;
}

void XMLString::binToText(  const   unsigned int    toFormat
                            ,       XMLCh* const    toFill
                            , const unsigned int    maxChars
                            , const unsigned int    radix
                            , MemoryManager* const  manager)
{
    // Just call the unsigned long version
    binToText((unsigned long)toFormat, toFill, maxChars, radix, manager);
}

void XMLString::binToText(  const   long            toFormat
                            ,       XMLCh* const    toFill
                            , const unsigned int    maxChars
                            , const unsigned int    radix
                            , MemoryManager* const  manager)
{
    //
    //  If its negative, then put a negative sign into the output and flip
    //  the sign of the local temp value.
    //
    unsigned int startInd = 0;
    unsigned long actualVal;
    if (toFormat < 0)
    {
        toFill[0] = chDash;
        startInd++;
        actualVal = (unsigned long)(toFormat * -1);
    }
     else
    {
        actualVal = (unsigned long)(toFormat);
    }

    // And now call the unsigned long version
    binToText(actualVal, &toFill[startInd], maxChars, radix, manager);
}

void XMLString::binToText(  const   int             toFormat
                            ,       XMLCh* const    toFill
                            , const unsigned int    maxChars
                            , const unsigned int    radix
                            , MemoryManager* const  manager)
{
    //
    //  If its negative, then put a negative sign into the output and flip
    //  the sign of the local temp value.
    //
    unsigned int startInd = 0;
    unsigned long actualVal;
    if (toFormat < 0)
    {
        toFill[0] = chDash;
        startInd++;
        actualVal = (unsigned long)(toFormat * -1);
    }
     else
    {
        actualVal = (unsigned long)(toFormat);
    }

    // And now call the unsigned long version
    binToText(actualVal, &toFill[startInd], maxChars, radix, manager);
}


void XMLString::catString(XMLCh* const target, const XMLCh* const src)
{
    // Get the starting point for the cat on the target XMLString
    unsigned int index = stringLen(target);

    // While the source is not zero, add them to target and bump
    const XMLCh* pszTmp = src;
    while (*pszTmp)
        target[index++] = *pszTmp++;

    // Cap off the target where we ended
    target[index] = chNull;
}


int XMLString::compareIString(  const   XMLCh* const    str1
                                , const XMLCh* const    str2)
{
    // Refer this one to the transcoding service
    return XMLPlatformUtils::fgTransService->compareIString(str1, str2);
}

int XMLString::compareIStringASCII(  const   XMLCh* const    str1
                                     , const XMLCh* const    str2)
{
    const XMLCh* psz1 = str1;
    const XMLCh* psz2 = str2;

    if (psz1 == 0 || psz2 == 0) {

        if (psz1 == 0) {
            return 0 - XMLString::stringLen(psz2);
        }
		else if (psz2 == 0) {
            return XMLString::stringLen(psz1);
        }
    }

    XMLCh ch1;
    XMLCh ch2;

    while (true) {
        if (*psz1 >= chLatin_A && *psz1 <= chLatin_Z)
            ch1 = *psz1 - chLatin_A + chLatin_a;
        else
            ch1 = *psz1;
        if (*psz2 >= chLatin_A && *psz2 <= chLatin_Z)
            ch2 = *psz2 - chLatin_A + chLatin_a;
        else
            ch2 = *psz2;

        // If an inequality, then return difference
        if (ch1 != ch2)
            return int(ch1) - int(ch2);

        // If either ended, then both ended, so equal
        if (!ch1)
            break;

        // Move upwards to next chars
        psz1++;
        psz2++;
    }
    return 0;
}    

int XMLString::compareNString(  const   XMLCh* const    str1
                                , const XMLCh* const    str2
                                , const unsigned int    maxChars)
{
    const XMLCh* psz1 = str1;
    const XMLCh* psz2 = str2;

    unsigned int curCount = 0;
    while (curCount < maxChars)
    {
        // If an inequality, then return difference
        if (*psz1 != *psz2)
            return int(*psz1) - int(*psz2);

        // If either ended, then both ended, so equal
        if (!*psz1)
            break;

        // Move upwards to next chars
        psz1++;
        psz2++;

        //
        //  Bump the count of chars done. 
        //
        curCount++;
    }
    // If we inspected all the maxChars, then we are equal.
    return 0;
}


int XMLString::compareNIString( const   XMLCh* const    str1
                                , const XMLCh* const    str2
                                , const unsigned int    maxChars)
{
    // Refer this oneto the transcoding service
    return XMLPlatformUtils::fgTransService->compareNIString(str1, str2, maxChars);
}


int XMLString::compareString(   const   XMLCh* const    str1
                                , const XMLCh* const    str2)
{
    const XMLCh* psz1 = str1;
    const XMLCh* psz2 = str2;

    if (psz1 == 0 || psz2 == 0) {

        if (psz1 == 0) {
            return 0 - XMLString::stringLen(psz2);
        }
		else if (psz2 == 0) {
            return XMLString::stringLen(psz1);
        }
    }

    while (true)
    {
        // If an inequality, then return the difference
        if (*psz1 != *psz2)
            return int(*psz1) - int(*psz2);

        // If either has ended, then they both ended, so equal
        if (!*psz1)
            break;

        // Move upwards for the next round
        psz1++;
        psz2++;
    }
    return 0;
}


bool XMLString::regionMatches(const   XMLCh* const	str1
							  , const int			offset1
							  , const XMLCh* const	str2
							  , const int			offset2
							  , const unsigned int	charCount)
{
	if (!validateRegion(str1, offset1,str2, offset2, charCount))
		return false;

	if (compareNString(str1+offset1, str2+offset2, charCount) != 0)
		return false;

	return true;
}

bool XMLString::regionIMatches(const   XMLCh* const	str1
						 	   , const int			offset1
							   , const XMLCh* const	str2
							   , const int			offset2
							   , const unsigned int	charCount)
{
	if (!validateRegion(str1, offset1,str2, offset2, charCount))
		return false;

	if (compareNIString(str1+offset1, str2+offset2, charCount) != 0)
		return false;

	return true;
}

void XMLString::copyString(XMLCh* const target, const XMLCh* const src)
{
    if (!src)
    {
        *target = 0;
        return;
    }

    XMLCh* pszOut = target;
    const XMLCh* pszIn = src;
    while (*pszIn)
        *pszOut++ = *pszIn++;

    // Capp off the target where we ended
    *pszOut = 0;
}


bool XMLString::copyNString(        XMLCh* const    target
                            , const XMLCh* const    src
                            , const unsigned int    maxChars)
{
    XMLCh* outPtr = target;
    const XMLCh* srcPtr = src;
    const XMLCh* endPtr = target + maxChars - 1;

    while (*srcPtr && (outPtr <= endPtr))
        *outPtr++ = *srcPtr++;

    // Cap it off here
    *outPtr = 0;

    // Return whether we copied it all or hit the max
    return (*srcPtr == 0);
}

const XMLCh* XMLString::findAny(const   XMLCh* const    toSearch
                                , const XMLCh* const    searchList)
{
    const XMLCh* srcPtr = toSearch;
    while (*srcPtr)
    {
        const XMLCh* listPtr = searchList;
        const XMLCh  curCh = *srcPtr;

        while (*listPtr)
        {
            if (curCh == *listPtr++)
                return srcPtr;
        }
        srcPtr++;
    }
    return 0;
}

XMLCh* XMLString::findAny(          XMLCh* const    toSearch
                            , const XMLCh* const    searchList)
{
    XMLCh* srcPtr = toSearch;
    while (*srcPtr)
    {
        const XMLCh* listPtr = searchList;
        const XMLCh  curCh = *srcPtr;

        while (*listPtr)
        {
            if (curCh == *listPtr++)
                return srcPtr;
        }
        srcPtr++;
    }
    return 0;
}

int XMLString::patternMatch(  const XMLCh* const    toSearch
                            , const XMLCh* const    pattern)
{
    if (!toSearch || !*toSearch )
        return -1;

    const int patnLen = XMLString::stringLen(pattern);
	if ( !patnLen )
		return -1;

    const XMLCh* srcPtr    = toSearch;
    const XMLCh* patnStart = toSearch;
    int  patnIndex = 0;

    while (*srcPtr)
    {
        if ( !(*srcPtr++ == pattern[patnIndex]))
        {
            patnIndex = 0;
            srcPtr = ++patnStart;
        }
        else
        {
            if (++patnIndex == patnLen)
                // full pattern match found
                return (srcPtr - patnLen - toSearch);

        }
    }

    return -1;
}


unsigned int XMLString::hashN(  const   XMLCh* const    tohash
                                , const unsigned int    n
                                , const unsigned int    hashModulus
                                , MemoryManager* const)
{
    assert(hashModulus);

    unsigned int hashVal = 0;
    if (tohash) {
        const XMLCh* curCh = tohash;
        int i = n;
        while (i--)
        {
            unsigned int top = hashVal >> 24;
            hashVal += (hashVal * 37) + top + (unsigned int)(*curCh);
            curCh++;
        }
    }

    // Divide by modulus
    return hashVal % hashModulus;
}


int XMLString::indexOf(const XMLCh* const toSearch, const XMLCh ch)
{
    if (toSearch)
    {
        const XMLCh* srcPtr = toSearch;
        while (*srcPtr)
        {
            if (ch == *srcPtr)
                return (int)(srcPtr - toSearch);

            srcPtr++;
        }
    }
    return -1;
}


int XMLString::indexOf( const   XMLCh* const    toSearch
                        , const XMLCh           ch
                        , const unsigned int    fromIndex
                        , MemoryManager* const  manager)
{
    const int len = stringLen(toSearch);

    // Make sure the start index is within the XMLString bounds
	if ((int)fromIndex > len-1)
        ThrowXMLwithMemMgr(ArrayIndexOutOfBoundsException, XMLExcepts::Str_StartIndexPastEnd, manager);

    for (int i = (int)fromIndex; i < len; i++)
    {
        if (toSearch[i] == ch)
            return i;
    }
    return -1;
}

int XMLString::lastIndexOf(const XMLCh ch,
                           const XMLCh* const toSearch,
                           const unsigned int toSearchLen)
{
    for (int i = (int)toSearchLen-1; i >= 0; i--)
    {
        if (toSearch[i] == ch)
            return i;
    }
    return -1;
}

int XMLString::lastIndexOf( const   XMLCh* const    toSearch
                            , const XMLCh           ch
                            , const unsigned int    fromIndex
                            , MemoryManager* const  manager)
{
    const int len = stringLen(toSearch);
	if ((int)fromIndex > len-1)
        ThrowXMLwithMemMgr(ArrayIndexOutOfBoundsException, XMLExcepts::Str_StartIndexPastEnd, manager);

    for (int i = (int)fromIndex; i >= 0; i--)
    {
        if (toSearch[i] == ch)
            return i;
    }
    return -1;
}


XMLCh*
XMLString::makeUName(const XMLCh* const pszURI, const XMLCh* const pszName)
{
    //
    //  If there is a URI, then format out the full name in the {uri}name
    //  form. Otherwise, just set it to the same thing as the base name.
    //
    XMLCh* pszRet = 0;
    const unsigned int uriLen = stringLen(pszURI);
    if (uriLen)
    {
        pszRet = new XMLCh[uriLen + stringLen(pszName) + 3];

        XMLCh szTmp[2];
        szTmp[1] = 0;

        szTmp[0] = chOpenCurly;
        copyString(pszRet, szTmp);
        catString(pszRet, pszURI);
        szTmp[0] = chCloseCurly;
        catString(pszRet, szTmp);
        catString(pszRet, pszName);
    }
     else
    {
        pszRet = replicate(pszName);
    }
    return pszRet;
}


bool XMLString::textToBin(const XMLCh* const toConvert, unsigned int& toFill
                          , MemoryManager* const manager)
{
    toFill = 0;

    // If no string, then its a failure
    if ((!toConvert) || (!*toConvert))
        return false;

	XMLCh* trimmedStr = XMLString::replicate(toConvert, manager);
	ArrayJanitor<XMLCh> jan1(trimmedStr, manager);
	XMLString::trim(trimmedStr);
    unsigned int trimmedStrLen = XMLString::stringLen(trimmedStr);

	if ( !trimmedStrLen )
		return false;

	// we don't allow '-' sign
	if (XMLString::indexOf(trimmedStr, chDash, 0, manager) != -1)
		return false;

	//the errno set by previous run is NOT automatically cleared
	errno = 0;

	char *nptr = XMLString::transcode(trimmedStr, manager);
    ArrayJanitor<char> jan2(nptr, manager);

    char *endptr;
	 //
     // REVISIT: conversion of (unsigned long) to (unsigned int)
	 //          may truncate value on IA64
    toFill = (unsigned int) strtoul(nptr, &endptr, 10);

	// check if all chars are valid char
	// check if overflow/underflow occurs
	if ( ( (endptr - nptr) != (int) trimmedStrLen) ||
         (errno == ERANGE)                      )
		return false;

    return true;
}

int XMLString::parseInt(const XMLCh* const toConvert
                     , MemoryManager* const manager)
{
    // If no string, or empty string, then it is a failure
    if ((!toConvert) || (!*toConvert))
        ThrowXMLwithMemMgr(NumberFormatException, XMLExcepts::XMLNUM_null_ptr, manager);

	XMLCh* trimmedStr = XMLString::replicate(toConvert, manager);
	ArrayJanitor<XMLCh> jan1(trimmedStr, manager);
	XMLString::trim(trimmedStr);
    unsigned int trimmedStrLen = XMLString::stringLen(trimmedStr);

	if ( !trimmedStrLen )
        ThrowXMLwithMemMgr(NumberFormatException, XMLExcepts::XMLNUM_null_ptr, manager);

	//the errno set by previous run is NOT automatically cleared
	errno = 0;

	char *nptr = XMLString::transcode(trimmedStr, manager);
    ArrayJanitor<char> jan2(nptr, manager);

    char *endptr;
    long retVal = strtol(nptr, &endptr, 10);

	// check if all chars are valid char
	if ( (endptr - nptr) != (int) trimmedStrLen)
		ThrowXMLwithMemMgr(NumberFormatException, XMLExcepts::XMLNUM_Inv_chars, manager);

	// check if overflow/underflow occurs
    if (errno == ERANGE)
        ThrowXMLwithMemMgr(NumberFormatException, XMLExcepts::Str_ConvertOverflow, manager);

	 //
     // REVISIT: conversion of (long) to (int)
	 //          may truncate value on IA64
	return (int) retVal;
}


void XMLString::trim(XMLCh* const toTrim)
{
    const unsigned int len = stringLen(toTrim);

    unsigned int skip, scrape;
    for (skip = 0; skip < len; skip++)
    {
        if (!XMLChar1_0::isWhitespace(toTrim[skip]))
            break;
    }

    for (scrape = len; scrape > skip; scrape--)
    {
        if (!XMLChar1_0::isWhitespace(toTrim[scrape - 1]))
            break;
    }

    // Cap off at the scrap point
    if (scrape != len)
        toTrim[scrape] = 0;

    if (skip)
    {
        // Copy the chars down
        unsigned int index = 0;
        while (toTrim[skip])
            toTrim[index++] = toTrim[skip++];

        toTrim[index] = 0;
    }
}


void XMLString::upperCase(XMLCh* const toUpperCase)
{
    // Refer this one to the transcoding service
    XMLPlatformUtils::fgTransService->upperCase(toUpperCase);
}

void XMLString::upperCaseASCII(XMLCh* const toUpperCase)
{
    XMLCh* psz1 = toUpperCase;

    if (!psz1)
        return;

    while (*psz1) {
        if (*psz1 >= chLatin_a && *psz1 <= chLatin_z)
            *psz1 = *psz1 - chLatin_a + chLatin_A;

        psz1++;        
    }    
}


void XMLString::lowerCase(XMLCh* const toLowerCase)
{
    // Refer this one to the transcoding service
    XMLPlatformUtils::fgTransService->lowerCase(toLowerCase);
}

void XMLString::lowerCaseASCII(XMLCh* const toLowerCase)
{
    XMLCh* psz1 = toLowerCase;

    if (!psz1)
        return;

    while (*psz1) {
        if (*psz1 >= chLatin_A && *psz1 <= chLatin_Z)
            *psz1 = *psz1 - chLatin_A + chLatin_a;

        psz1++;        
    }    
}

void XMLString::subString(XMLCh* const targetStr, const XMLCh* const srcStr
                          , const int startIndex, const int endIndex
                          , MemoryManager* const manager)
{
    subString(targetStr, srcStr, startIndex, endIndex, stringLen(srcStr), manager);
}

void XMLString::subString(XMLCh* const targetStr, const XMLCh* const srcStr
                          , const int startIndex, const int endIndex
                          , const int srcStrLength
                          , MemoryManager* const manager)
{
    //if (startIndex < 0 || endIndex < 0)
    //    ThrowXMLwithMemMgr(ArrayIndexOutOfBoundsException, XMLExcepts::Str_NegativeIndex);

    if (targetStr == 0)
        ThrowXMLwithMemMgr(IllegalArgumentException, XMLExcepts::Str_ZeroSizedTargetBuf, manager);

    const int copySize = endIndex - startIndex;

    // Make sure the start index is within the XMLString bounds
    if ( startIndex < 0 || startIndex > endIndex || endIndex > srcStrLength)
        ThrowXMLwithMemMgr(ArrayIndexOutOfBoundsException, XMLExcepts::Str_StartIndexPastEnd, manager);

    for (int i= startIndex; i < endIndex; i++) {
        targetStr[i-startIndex] = srcStr[i];
    }

    targetStr[copySize] = 0;
}

BaseRefVectorOf<XMLCh>* XMLString::tokenizeString(const XMLCh*      const   tokenizeSrc
                                               ,  MemoryManager*    const   manager)
{
    XMLCh* orgText = replicate(tokenizeSrc, manager);
    ArrayJanitor<XMLCh> janText(orgText, manager);
    XMLCh* tokenizeStr = orgText;

    RefArrayVectorOf<XMLCh>* tokenStack = new (manager) RefArrayVectorOf<XMLCh>(16, true, manager);

    unsigned int len = stringLen(tokenizeStr);
    unsigned int skip;
    unsigned int index = 0;

    while (index != len) {
        // find the first non-space character
        for (skip = index; skip < len; skip++)
        {
            if (!XMLChar1_0::isWhitespace(tokenizeStr[skip]))
                break;
        }
        index = skip;

        // find the delimiter (space character)
        for (; skip < len; skip++)
        {
            if (XMLChar1_0::isWhitespace(tokenizeStr[skip]))
                break;
        }

        // we reached the end of the string
        if (skip == index)
            break;

        // these tokens are adopted in the RefVector and will be deleted
        // when the vector is deleted by the caller
        XMLCh* token = (XMLCh*) manager->allocate
        (
            (skip+1-index) * sizeof(XMLCh)
        );//new XMLCh[skip+1-index];

        XMLString::subString(token, tokenizeStr, index, skip, len, manager);
        tokenStack->addElement(token);
        index = skip;
    }
    return tokenStack;
}

//
//  This method is called when we get a notation or enumeration type attribute
//  to validate. We have to confirm that the passed value to find is one of
//  the values in the passed list. The list is a space separated string of
//  values to match against.
//
bool XMLString::isInList(const XMLCh* const toFind, const XMLCh* const enumList)
{
    //
    //  We loop through the values in the list via this outer loop. We end
    //  when we hit the end of the enum list or get a match.
    //
    const XMLCh* listPtr = enumList;
    const unsigned int findLen = XMLString::stringLen(toFind);
    while (*listPtr)
    {
        unsigned int testInd;
        for (testInd = 0; testInd < findLen; testInd++)
        {
            //
            //  If they don't match, then reset and try again. Note that
            //  hitting the end of the current item will cause a mismatch
            //  because there can be no spaces in the toFind string.
            //
            if (listPtr[testInd] != toFind[testInd])
                break;
        }

        //
        //  If we went the distance, see if we matched. If we did, the current
        //  list character has to be null or space.
        //
        if (testInd == findLen)
        {
            if ((listPtr[testInd] == chSpace) || !listPtr[testInd])
                return true;
        }

        // Run the list pointer up to the next substring
        while ((*listPtr != chSpace) && *listPtr)
            listPtr++;

        // If we hit the end, then we failed
        if (!*listPtr)
            return false;

        // Else move past the space and try again
        listPtr++;
    }

    // We never found it
    return false;
}

//
// a string is whitespace:replaced, is having no
//    #xD  Carriage Return
//    #xA  Line Feed
//    #x9  TAB
//
bool XMLString::isWSReplaced(const XMLCh* const toCheck)
{
    // If no string, then its a OK
    if (( !toCheck ) || ( !*toCheck ))
        return true;

    const XMLCh* startPtr = toCheck;
    while ( *startPtr )
    {
        if ( ( *startPtr == chCR) ||
             ( *startPtr == chLF) ||
             ( *startPtr == chHTab))
        return false;

        startPtr++;
    }

    return true;
}

//
//   to replace characters listed below to #x20
//    #xD  Carriage Return
//    #xA  Line Feed
//    #x9  TAB
//
void XMLString::replaceWS(XMLCh* const toConvert
                          , MemoryManager* const  manager)
{
    int strLen = XMLString::stringLen(toConvert);
    if (strLen == 0)
        return;

    XMLCh* retBuf = (XMLCh*) manager->allocate
    (
        (strLen+1) * sizeof(XMLCh)
    );//new XMLCh[strLen+1];
    XMLCh* retPtr = &retBuf[0];
    XMLCh* startPtr = toConvert;

    while ( *startPtr )
    {
        if ( ( *startPtr == chCR) ||
             ( *startPtr == chLF) ||
             ( *startPtr == chHTab))
            *retPtr = chSpace;
        else
            *retPtr = *startPtr;

        retPtr++;
        startPtr++;
    }

    retBuf[strLen] = chNull;

    XMLString::moveChars(toConvert, retBuf, strLen);
    manager->deallocate(retBuf);//delete[] retBuf;
    return;
}

//
// a string is whitespace:collapsed, is whitespace::replaced
// and no
//    leading space (#x20)
//    trailing space
//    no contiguous sequences of spaces
//
bool XMLString::isWSCollapsed(const XMLCh* const toCheck)
{
    if (( !toCheck ) || ( !*toCheck ))
        return true;

    // shall be whitespace::replaced first
    if ( !isWSReplaced(toCheck) )
        return false;

    // no leading or trailing space
    if ((*toCheck == chSpace) ||
        (toCheck[XMLString::stringLen(toCheck)-1] == chSpace))
        return false;

    const XMLCh* startPtr = toCheck;
    XMLCh theChar;
    bool  inSpace = false;
    while ( (theChar = *startPtr) != 0 )
    {
        if ( theChar == chSpace)
        {
            if (inSpace)
                return false;
            else
                inSpace = true;
        }
        else
            inSpace = false;

        startPtr++;

    }

    return true;
}

//
// no leading and/or trailing spaces
// no continuous sequences of spaces
//
void XMLString::collapseWS(XMLCh* const toConvert
                           , MemoryManager* const  manager)
{
    // If no string, then its a failure
    if (( !toConvert ) || ( !*toConvert ))
        return;

    // replace whitespace first
    replaceWS(toConvert, manager);

    // remove leading spaces
    const XMLCh* startPtr = toConvert;
    while ( *startPtr == chSpace )
        startPtr++;

    if (!*startPtr)
        return;

    // remove trailing spaces
    const XMLCh* endPtr = toConvert + stringLen(toConvert);
    while (*(endPtr - 1) == chSpace)
        endPtr--;

    //
    //  Work through what remains and chop continuous spaces
    //
    XMLCh* retBuf = (XMLCh*) manager->allocate
    (
        (endPtr - startPtr + 1) * sizeof(XMLCh)
    );//new XMLCh[endPtr - startPtr + 1];
    XMLCh* retPtr = &retBuf[0];
    bool  inSpace = false;
    while (startPtr < endPtr)
    {
        if ( *startPtr == chSpace)
        {
            if (inSpace)
            {
                //discard it;
            }
            else
            {
                inSpace = true;
                *retPtr = chSpace;  //copy the first chSpace
                retPtr++;
            }
        }
        else
        {
            inSpace = false;
            *retPtr = *startPtr;
            retPtr++;
        }

        startPtr++;
    }

    *retPtr = chNull;
    XMLString::moveChars(toConvert, retBuf, stringLen(retBuf)+1); //copy the last chNull as well
    manager->deallocate(retBuf);//delete[] retBuf;
    return;
}

//
// remove whitespace
//
void XMLString::removeWS(XMLCh* const toConvert
                         , MemoryManager* const manager)
{
    // If no string, then its a failure
    if (( !toConvert ) || ( !*toConvert ))
        return;

    XMLCh* retBuf = (XMLCh*) manager->allocate
    (
        (XMLString::stringLen(toConvert) + 1) * sizeof(XMLCh)
    );//new XMLCh[ XMLString::stringLen(toConvert) + 1];
    XMLCh* retPtr = &retBuf[0];
    XMLCh* startPtr = toConvert;

    while (*startPtr)
    {
        if ( ( *startPtr != chCR)    &&
             ( *startPtr != chLF)    &&
             ( *startPtr != chHTab)  &&
             ( *startPtr != chSpace)  )
        {
            *retPtr++ = *startPtr;
        }

        startPtr++;

    }

    *retPtr = chNull;
    XMLString::moveChars(toConvert, retBuf, stringLen(retBuf)+1); //copy the last chNull as well
    manager->deallocate(retBuf);//delete[] retBuf;
    return;
}

void XMLString::removeChar(const XMLCh*     const srcString
                         , const XMLCh&           toRemove
                         ,       XMLBuffer&       dstBuffer)
{
    const XMLCh* pszSrc = srcString;
    XMLCh c;

    dstBuffer.reset();

    while ((c=*pszSrc++)!=0) 
    {
        if (c != toRemove) 
            dstBuffer.append(c);

    }
}

/**
 * Fixes a platform dependent absolute path filename to standard URI form.
 * 1. Windows: fix 'x:' to 'file:///x:' and convert any backslash to forward slash
 * 2. UNIX: fix '/blah/blahblah' to 'file:///blah/blahblah'
 */
void XMLString::fixURI(const XMLCh* const str, XMLCh* const target)
{
    if (!str || !*str)
        return;

    int colonIdx = XMLString::indexOf(str, chColon);

    // If starts with a '/' we assume
    // this is an absolute (UNIX) file path and prefix it with file://
    if (colonIdx == -1 && XMLString::indexOf(str, chForwardSlash) == 0) {
        unsigned index = 0;
        target[index++] = chLatin_f;
        target[index++] = chLatin_i;
        target[index++] = chLatin_l;
        target[index++] = chLatin_e;
        target[index++] = chColon;
        target[index++] = chForwardSlash;
        target[index++] = chForwardSlash;

        // copy the string
        const XMLCh* inPtr = str;
        while (*inPtr)
            target[index++] = *inPtr++;

        target[index] = chNull;
    }
    else if (colonIdx == 1 && XMLString::isAlpha(*str)) {
        // If starts with a driver letter 'x:' we assume
        // this is an absolute (Windows) file path and prefix it with file:///
        unsigned index = 0;
        target[index++] = chLatin_f;
        target[index++] = chLatin_i;
        target[index++] = chLatin_l;
        target[index++] = chLatin_e;
        target[index++] = chColon;
        target[index++] = chForwardSlash;
        target[index++] = chForwardSlash;
        target[index++] = chForwardSlash;

        // copy the string and fix any backward slash
        const XMLCh* inPtr = str;
        while (*inPtr) {
            if (*inPtr == chYenSign ||
                *inPtr == chWonSign ||
                *inPtr == chBackSlash)
                target[index++] = chForwardSlash;
            else
                target[index++] = *inPtr;
            inPtr++;
        }

        // cap it with null
        target[index] = chNull;
    }
    else {
        // not specific case, so just copy the string over
        copyString(target, str);
    }
}

void XMLString::release(char** buf)
{
    delete [] *buf;
    *buf = 0;
}

void XMLString::release(XMLCh** buf)
{
    delete [] *buf;
    *buf = 0;
}

void XMLString::release(XMLByte** buf)
{
    delete [] *buf;
    *buf = 0;
}

void XMLString::release(void** buf, MemoryManager* const manager)
{
    manager->deallocate(*buf);
    *buf = 0;
}

// ---------------------------------------------------------------------------
//  XMLString: Private static methods
// ---------------------------------------------------------------------------
void XMLString::initString(XMLLCPTranscoder* const defToUse,
                           MemoryManager* const manager)
{
    // Store away the default transcoder that we are to use
    gTranscoder = defToUse;

    // Store memory manager
    fgMemoryManager = manager;
}

void XMLString::termString()
{
    // Just clean up our local code page transcoder
    delete gTranscoder;
    gTranscoder = 0;

    // reset memory manager
    fgMemoryManager = 0;
}

XERCES_CPP_NAMESPACE_END
