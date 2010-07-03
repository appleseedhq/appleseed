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

/**
 * @01A D998714.1 V5R2M0    100301   Swan    :Fix error return flags
 * @02A           V5R2M0    200419   jrhansen : support lowercase function
 * @03A D93234.1  V1R1M0    102804   JRHansen :fix transcode overflow 
 * @04A D93234.3  V1R1M0    022505   JRHansen :fix transcode error check 
 * $Id: Iconv400TransService.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/TranscodingException.hpp>
#include "Iconv400TransService.hpp"
#include <string.h>
#include <qlgcase.h>
#include "iconv_cnv.hpp"
#include "iconv_util.hpp"
#include <qusec.h>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/Janitor.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local functions
// ---------------------------------------------------------------------------

//
//  When XMLCh and ICU's UChar are not the same size, we have to do a temp
//  conversion of all strings. These local helper methods make that easier.
//
static UChar* convertToUChar( const   XMLCh* const toConvert
                            , const unsigned int   srcLen = 0
                            , MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager)
{
    const unsigned int actualLen = srcLen
                                   ? srcLen : XMLString::stringLen(toConvert);

    UChar* tmpBuf = (UChar*) manager->allocate
    (
        (srcLen + 1) * sizeof(UChar)
    );//new UChar[srcLen + 1];
    const XMLCh* srcPtr = toConvert;
    UChar* outPtr = tmpBuf;
    while (*srcPtr)
        *outPtr++ = UChar(*srcPtr++);
    *outPtr = 0;

    return tmpBuf;
}

// ---------------------------------------------------------------------------
//  Local, const data
// ---------------------------------------------------------------------------
static const XMLCh gMyServiceId[] =
{
    chLatin_I, chLatin_C, chLatin_O, chLatin_V, chDigit_4, chDigit_0, chDigit_0, chNull
};

// ---------------------------------------------------------------------------
//  IconvTransService: Constructors and Destructor
// ---------------------------------------------------------------------------
Iconv400TransService::Iconv400TransService()
{
    memset((char*)&convertCtlblkUpper,'\0',sizeof(convertCtlblkUpper));
    convertCtlblkUpper.Type_of_Request = 1;
    convertCtlblkUpper.Case_Request = 0;   // upper case
    convertCtlblkUpper.CCSID_of_Input_Data = 13488;

    memset((char*)&convertCtlblkLower,'\0',sizeof(convertCtlblkLower));
    convertCtlblkLower.Type_of_Request = 1;
    convertCtlblkLower.Case_Request = 1;
    convertCtlblkLower.CCSID_of_Input_Data = 13488;
}

Iconv400TransService::~Iconv400TransService()
{
}


// ---------------------------------------------------------------------------
//  Iconv400TransService: The virtual transcoding service API
// ---------------------------------------------------------------------------
int Iconv400TransService::compareIString(const   XMLCh* const    comp1
                                         , const XMLCh* const    comp2)
{
    const XMLCh* psz1 = comp1;
    const XMLCh* psz2 = comp2;

    while (true)
    {

        if (toUnicodeUpper(*psz1) != toUnicodeUpper(*psz2))
            return int(*psz1) - int(*psz2);

        // If either has ended, then they both ended, so equal
        if (!*psz1 || !*psz2)
            break;

        // Move upwards for the next round
        psz1++;
        psz2++;
    }
    return 0;
}


int Iconv400TransService::compareNIString(const  XMLCh* const    comp1
                                          , const XMLCh* const    comp2
                                          , const unsigned int    maxChars)
{
    const XMLCh* psz1 = comp1;
    const XMLCh* psz2 = comp2;


    unsigned int curCount = 0;
    while (true)
    {
        // If an inequality, then return the difference


        // If an inequality, then return difference
        if (toUnicodeUpper(*psz1) != toUnicodeUpper(*psz2))
            return int(*psz1) - int(*psz2);

        // If either ended, then both ended, so equal
        if (!*psz1 || !*psz2)
            break;

        // Move upwards to next chars
        psz1++;
        psz2++;

        //
        //  Bump the count of chars done. If it equals the count then we
        //  are equal for the requested count, so break out and return
        //  equal.
        //
        curCount++;
        if (maxChars == curCount)
            break;
    }
    return 0;
}



const XMLCh* Iconv400TransService::getId() const
{
    return gMyServiceId;
}

bool Iconv400TransService::isSpace(const XMLCh toCheck) const
{
    //   The following are Unicode Space characters
    //
    if ((toCheck == 0x09)
    ||  (toCheck == 0x0A)
    ||  (toCheck == 0x0D)
    ||  (toCheck == 0x20)
    ||  (toCheck == 0xA0)
    ||  ((toCheck >= 0x2000) && (toCheck <= 0x200B))
    ||  (toCheck == 0x3000)
    ||  (toCheck == 0xFEFF))
    {
        return true;
    }
    else return false;
}


XMLLCPTranscoder* Iconv400TransService::makeNewLCPTranscoder()
{
    //
    //  Try to create a default converter. If it fails, return a null pointer
    //  which will basically cause the system to give up because we really can't
    //  do anything without one.
    //
    UErrorCode uerr = U_ZERO_ERROR;
    UConverter* converter = ucnv_open(NULL, &uerr);
    if (!converter)
        return 0;

    // That went ok, so create an Iconv LCP transcoder wrapper and return it
    return new Iconv400LCPTranscoder(converter);
}


bool Iconv400TransService::supportsSrcOfs() const
{
    // This implementation supports source offset information
    return true;
}

void Iconv400TransService::upperCase(XMLCh* const toUpperCase) const
{
    XMLCh* outPtr = toUpperCase;
    while (*outPtr)
    {
        *outPtr = toUnicodeUpper(*outPtr);
        outPtr++;
    }
}

void Iconv400TransService::lowerCase(XMLCh* const toLowerCase) const
{
    XMLCh* outPtr = toLowerCase;
    while (*outPtr)
    {
        *outPtr = toUnicodeLower(*outPtr);
        outPtr++;
    }
}

// ---------------------------------------------------------------------------
//  Iconv400TransService: The virtual transcoding service API
// ---------------------------------------------------------------------------
XMLCh Iconv400TransService::toUnicodeUpper(XMLCh comp1) const
{
    XMLCh chRet;
    struct {
             int bytes_available;
             int bytes_used;
             char exception_id[7];
             char reserved;
             char exception_data[15];
            } error_code;
     error_code.bytes_available = sizeof(error_code);

    long charlen =2;

    QlgConvertCase((char*)&convertCtlblkUpper,
                       (char*)&comp1,
                       (char*)&chRet,
                       (long*)&charlen,
                       (char*)&error_code);
    return chRet;
}

XMLCh Iconv400TransService::toUnicodeLower(XMLCh comp1) const
{
    XMLCh chRet;
    struct {
             int bytes_available;
             int bytes_used;
             char exception_id[7];
             char reserved;
             char exception_data[15];
            } error_code;
     error_code.bytes_available = sizeof(error_code);

    long charlen =2;

    QlgConvertCase((char*)&convertCtlblkLower,
                       (char*)&comp1,
                       (char*)&chRet,
                       (long*)&charlen,
                       (char*)&error_code);
    return chRet;
}

// ---------------------------------------------------------------------------
//  Iconv400TransService: The protected virtual transcoding service API
// ---------------------------------------------------------------------------
XMLTranscoder*
Iconv400TransService::makeNewXMLTranscoder(  const   XMLCh* const            encodingName
                                        ,       XMLTransService::Codes& resValue
                                        , const unsigned int            blockSize
                                        ,       MemoryManager* const    manager)
{
    UErrorCode uerr = U_ZERO_ERROR;
    UConverter* converter = ucnv_openU(encodingName, &uerr);
    if (!converter)
    {
        resValue = XMLTransService::UnsupportedEncoding;
        return 0;
    }
    return new (manager) Iconv400Transcoder(encodingName, converter, blockSize, manager);
}




// ---------------------------------------------------------------------------
//  IconvTranscoder: Constructors and Destructor
// ---------------------------------------------------------------------------
Iconv400Transcoder::Iconv400Transcoder( const XMLCh* const         encodingName
                                      ,       UConverter* const    toAdopt
                                      , const unsigned int         blockSize
                                      ,       MemoryManager* const manager) :

    XMLTranscoder(encodingName, blockSize, manager)
    , fConverter(toAdopt)
    , fFixed(false)
    , fSrcOffsets(0)
{
    // If there is a block size, then allocate our source offset array
    if (blockSize)
        fSrcOffsets = (long*) manager->allocate(blockSize * sizeof(long));//new long[blockSize];

    // Remember if its a fixed size encoding
    fFixed = (ucnv_getMaxCharSize(fConverter) == ucnv_getMinCharSize(fConverter));
}

Iconv400Transcoder::~Iconv400Transcoder()
{
    getMemoryManager()->deallocate(fSrcOffsets);//delete [] fSrcOffsets;

    // If there is a converter, ask Iconv400 to clean it up
    if (fConverter)
    {
        // <TBD> Does this actually delete the structure???
        ucnv_close(fConverter);
        fConverter = 0;
    }
}


// ---------------------------------------------------------------------------
//  Iconv400Transcoder: The virtual transcoder API
// ---------------------------------------------------------------------------
unsigned int
Iconv400Transcoder::transcodeFrom(const  XMLByte* const          srcData
                            , const unsigned int            srcCount
                            ,       XMLCh* const            toFill
                            , const unsigned int            maxChars
                            ,       unsigned int&           bytesEaten
                            ,       unsigned char* const    charSizes)
{
    // If debugging, insure the block size is legal


    // Set up pointers to the start and end of the source buffer
    const XMLByte*  startSrc = srcData;
    const XMLByte*  endSrc = srcData + srcCount;

    //
    //  And now do the target buffer. This works differently according to
    //  whether XMLCh and UChar are the same size or not.
    //
    UChar* startTarget;
    if (sizeof(XMLCh) == sizeof(UChar))
        startTarget = (UChar*)toFill;
     else
        startTarget = (UChar*) getMemoryManager()->allocate
        (
            maxChars * sizeof(UChar)
        );//new UChar[maxChars];
    UChar* orgTarget = startTarget;

    //
    //  Transoode the buffer.  Buffer overflow errors are normal, occuring
    //  when the raw input buffer holds more characters than will fit in
    //  the Unicode output buffer.
    //
    UErrorCode  err = U_ZERO_ERROR;
    ucnv_toUnicode
    (
        fConverter
        , &startTarget
        , startTarget + maxChars
        , (const char**)&startSrc
        , (const char*)endSrc
        , (fFixed ? 0 : (int32_t*)fSrcOffsets)
        , false
        , &err
    );


    if ((err != U_ZERO_ERROR) && (err != U_INDEX_OUTOFBOUNDS_ERROR) &&
	(err != U_BUFFER_OVERFLOW_ERROR))   // @03c
    {
        if (orgTarget != (UChar*)toFill)
            getMemoryManager()->deallocate(orgTarget);//delete [] orgTarget;

        if (fFixed)
        {
            XMLCh tmpBuf[17];
            XMLString::binToText((unsigned int)(*startTarget), tmpBuf, 16, 16, getMemoryManager());
            ThrowXMLwithMemMgr2
            (
                TranscodingException
                , XMLExcepts::Trans_BadSrcCP
                , tmpBuf
                , getEncodingName()
                , getMemoryManager()
            );
        }
         else
        {
            ThrowXMLwithMemMgr(TranscodingException, XMLExcepts::Trans_BadSrcSeq, getMemoryManager());
        }
    }

    // Calculate the bytes eaten and store in caller's param
    bytesEaten = startSrc - srcData;

    // And the characters decoded
    const unsigned int charsDecoded = startTarget - orgTarget;

    //
    //  Translate the array of char offsets into an array of character
    //  sizes, which is what the transcoder interface semantics requires.
    //  If its fixed, then we can optimize it.
    //
    if (fFixed)
    {
        const unsigned char fillSize = (unsigned char)ucnv_getMaxCharSize(fConverter);
        memset(charSizes, fillSize, maxChars);
    }
     else
    {
        //
        //  We have to convert the series of offsets into a series of
        //  sizes. If just one char was decoded, then its the total bytes
        //  eaten. Otherwise, do a loop and subtract out each element from
        //  its previous element.
        //
        if (charsDecoded == 1)
        {
            charSizes[0] = (unsigned char)bytesEaten;
        }
         else
        {
            //  ICU does not return an extra element to allow us to figure
            //  out the last char size, so we have to compute it from the
            //  total bytes used.
            unsigned int index;
            for (index = 0; index < charsDecoded - 1; index++)
            {
                charSizes[index] = (unsigned char)(fSrcOffsets[index + 1]
                                                    - fSrcOffsets[index]);
            }
            if( charsDecoded > 0 ) {
                charSizes[charsDecoded - 1] = (unsigned char)(bytesEaten
                                              - fSrcOffsets[charsDecoded - 1]);
            }
        }
    }

    //
    //  If XMLCh and UChar are not the same size, then we need to copy over
    //  the temp buffer to the new one.
    //
    if (sizeof(UChar) != sizeof(XMLCh))
    {
        XMLCh* outPtr = toFill;
        startTarget = orgTarget;
        for (unsigned int index = 0; index < charsDecoded; index++)
            *outPtr++ = XMLCh(*startTarget++);

        // And delete the temp buffer
        getMemoryManager()->deallocate(orgTarget);//delete [] orgTarget;
    }

    // Return the chars we put into the target buffer
    return charsDecoded;
}


unsigned int
Iconv400Transcoder::transcodeTo( const   XMLCh* const    srcData
                            , const unsigned int    srcCount
                            ,       XMLByte* const  toFill
                            , const unsigned int    maxBytes
                            ,       unsigned int&   charsEaten
                            , const UnRepOpts       options)
{
    //
    //  Get a pointer to the buffer to transcode. If UChar and XMLCh are
    //  the same size here, then use the original. Else, create a temp
    //  one and put a janitor on it.
    //
    const UChar* srcPtr;
    UChar* tmpBufPtr = 0;
    if (sizeof(XMLCh) == sizeof(UChar))
    {
        srcPtr = (const UChar*)srcData;
    }
     else
    {
        tmpBufPtr = convertToUChar(srcData, srcCount, getMemoryManager());
        srcPtr = tmpBufPtr;
    }
    ArrayJanitor<UChar> janTmpBuf(tmpBufPtr, getMemoryManager());

    //
    //  Set the appropriate callback so that it will either fail or use
    //  the rep char. Remember the old one so we can put it back.
    //
    UErrorCode  err = U_ZERO_ERROR;


    //
    //  Ok, lets transcode as many chars as we we can in one shot. The
    //  ICU API gives enough info not to have to do this one char by char.
    //
    XMLByte*        startTarget = toFill;
    const UChar*    startSrc = srcPtr;
    err = U_ZERO_ERROR;
    ucnv_fromUnicode
    (
        fConverter
        , (char**)&startTarget
        , (char*)(startTarget + maxBytes)
        , &startSrc
        , srcPtr + srcCount
        , 0
        , false
        , &err
    );


    if (err)   /*@01A*/
    {
        XMLCh tmpBuf[17];
        XMLString::binToText((unsigned int)*startSrc, tmpBuf, 16, 16, getMemoryManager());
        ThrowXMLwithMemMgr2
        (
            TranscodingException
            , XMLExcepts::Trans_Unrepresentable
            , tmpBuf
            , getEncodingName()
            , getMemoryManager()
        );
    }

    // Fill in the chars we ate from the input
    charsEaten = startSrc - srcPtr;

    // Return the chars we stored
    return startTarget - toFill;
}


bool Iconv400Transcoder::canTranscodeTo(const unsigned int toCheck) const
{
    //
    //  If the passed value is really a surrogate embedded together, then
    //  we need to break it out into its two chars. Else just one. While
    //  we are ate it, convert them to UChar format if required.
    //
    UChar           srcBuf[2];
    unsigned int    srcCount = 1;
    if (toCheck & 0xFFFF0000)
    {
        srcBuf[0] = UChar((toCheck >> 10) + 0xD800);
        srcBuf[1] = UChar(toCheck & 0x3FF) + 0xDC00;
        srcCount++;
    }
     else
    {
        srcBuf[0] = UChar(toCheck);
    }


    // Set upa temp buffer to format into. Make it more than big enough
    char            tmpBuf[64];
    char*           startTarget = tmpBuf;
    const UChar*    startSrc = srcBuf;
    UErrorCode  err = U_ZERO_ERROR;

    ucnv_fromUnicode
    (
        fConverter
        , &startTarget
        , startTarget + 64
        , &startSrc
        , srcBuf + srcCount
        , 0
        , false
        , &err
    );


    return (err==U_ZERO_ERROR);  /*@01A*/
}



// ---------------------------------------------------------------------------
//  IconvLCPTranscoder: Constructors and Destructor
// ---------------------------------------------------------------------------
Iconv400LCPTranscoder::Iconv400LCPTranscoder(UConverter* const toAdopt) :

    fConverter(toAdopt)
{
}

Iconv400LCPTranscoder::~Iconv400LCPTranscoder()
{
    // If there is a converter, ask Iconv to clean it up
    if (fConverter)
    {
        // <TBD> Does this actually delete the structure???
        ucnv_close(fConverter);
        fConverter = 0;
    }
}


// ---------------------------------------------------------------------------
//  Iconv400LCPTranscoder: Constructors and Destructor
// ---------------------------------------------------------------------------
unsigned int Iconv400LCPTranscoder::calcRequiredSize(const XMLCh* const srcText
                                                     , MemoryManager* const manager)
{
    if (!srcText)
        return 0;

    // Lock and attempt the calculation
    UErrorCode err = U_ZERO_ERROR;
    int32_t targetCap;
    {
        XMLMutexLock lockConverter(&fMutex);

        targetCap = ucnv_fromUChars
        (
            fConverter
            , 0
            , 0
            , srcText
            , &err
        );
    }

    if (err != U_BUFFER_OVERFLOW_ERROR)
        return 0;

    return (unsigned int)targetCap;
}

unsigned int Iconv400LCPTranscoder::calcRequiredSize(const char* const srcText
                                                     , MemoryManager* const manager)
{
    if (!srcText)
        return 0;

    int32_t targetCap;
    UErrorCode err = U_ZERO_ERROR;
    {
        XMLMutexLock lockConverter(&fMutex);

        targetCap = ucnv_toUChars
        (
            fConverter
            , 0
            , 0
            , srcText
            , strlen(srcText)
            , &err
        );
    }

    if (err != U_BUFFER_OVERFLOW_ERROR)
        return 0;

    // Subtract one since it includes the terminator space
    return (unsigned int)(targetCap - 1);
}


char* Iconv400LCPTranscoder::transcode(const XMLCh* const toTranscode)
{
    char* retBuf = 0;

    // Check for a couple of special cases
    if (!toTranscode)
        return 0;

    if (!*toTranscode)
    {
        retBuf = new char[1];
        retBuf[0] = 0;
        return retBuf;
    }

    // Caculate a return buffer size not too big, but less likely to overflow
    int32_t targetLen = (int32_t)(u_strlen(toTranscode) * 1.25);

    // Allocate the return buffer
    retBuf = new char[targetLen + 1];

    // Lock now while we call the converter.
    UErrorCode err = U_ZERO_ERROR;
    int32_t targetCap;
    {
        XMLMutexLock lockConverter(&fMutex);

        //Convert the Unicode string to char*
        targetCap = ucnv_fromUChars
        (
            fConverter
            , retBuf
            , targetLen + 1
            , toTranscode
            , &err
        );
    }

    // If targetLen is not enough then buffer overflow might occur
    if (err == U_BUFFER_OVERFLOW_ERROR)
    {
        // Reset the error, delete the old buffer, allocate a new one, and try again
        err = U_ZERO_ERROR;
        delete [] retBuf;
        retBuf = new char[targetCap];

        // Lock again before we retry
        XMLMutexLock lockConverter(&fMutex);
        targetCap = ucnv_fromUChars
        (
            fConverter
            , retBuf
            , targetCap
            , toTranscode
            , &err
        );
    }

    if (U_FAILURE(err))
    {
        delete [] retBuf;
        return 0;
    }

    // Cap it off and return
    retBuf[targetCap] = 0;
    return retBuf;
}

char* Iconv400LCPTranscoder::transcode(const XMLCh* const toTranscode,
                                       MemoryManager* const manager)
{
    char* retBuf = 0;

    // Check for a couple of special cases
    if (!toTranscode)
        return 0;

    if (!*toTranscode)
    {
        retBuf = (char*) manager->allocate(sizeof(char));//new char[1];
        retBuf[0] = 0;
        return retBuf;
    }

    // Caculate a return buffer size not too big, but less likely to overflow
    int32_t targetLen = (int32_t)(u_strlen(toTranscode) * 1.25);

    // Allocate the return buffer
    retBuf = (char*) manager->allocate((targetLen + 1) * sizeof(char));//new char[targetLen + 1];

    // Lock now while we call the converter.
    UErrorCode err = U_ZERO_ERROR;
    int32_t targetCap;
    {
        XMLMutexLock lockConverter(&fMutex);

        //Convert the Unicode string to char*
        targetCap = ucnv_fromUChars
        (
            fConverter
            , retBuf
            , targetLen + 1
            , toTranscode
            , &err
        );
    }

    // If targetLen is not enough then buffer overflow might occur
    if (err == U_BUFFER_OVERFLOW_ERROR)
    {
        // Reset the error, delete the old buffer, allocate a new one, and try again
        err = U_ZERO_ERROR;
        manager->deallocate(retBuf);//delete [] retBuf;
        retBuf = (char*) manager->allocate(targetCap * sizeof(char));//new char[targetCap];

        // Lock again before we retry
        XMLMutexLock lockConverter(&fMutex);
        targetCap = ucnv_fromUChars
        (
            fConverter
            , retBuf
            , targetCap
            , toTranscode
            , &err
        );
    }

    if (U_FAILURE(err))
    {
        manager->deallocate(retBuf);//delete [] retBuf;
        return 0;
    }

    // Cap it off and return
    retBuf[targetCap] = 0;
    return retBuf;
}

XMLCh* Iconv400LCPTranscoder::transcode(const char* const toTranscode)
{
    // Watch for a few pyscho corner cases
    if (!toTranscode)
        return 0;

    XMLCh* retVal = 0;
    if (!*toTranscode)
    {
        retVal = new XMLCh[1];
        retVal[0] = 0;
        return retVal;
    }

    //
    //  Get the length of the string to transcode. The Unicode string will
    //  almost always be no more chars than were in the source, so this is
    //  the best guess as to the storage needed.
    //
    const int32_t srcLen = (int32_t)strlen(toTranscode);
    // Allocate unicode string of equivalent length in unicode bytes
    retVal = new XMLCh[srcLen+1];

    // Now lock while we do these calculations
    UErrorCode err = U_ZERO_ERROR;
    {
        XMLMutexLock lockConverter(&fMutex);

        //
        //  Here we don't know what the target length will be so use 0 and
        //  expect an U_BUFFER_OVERFLOW_ERROR in which case it'd get resolved
        //  by the correct capacity value.
        //
        int32_t targetCap;
        targetCap = ucnv_toUChars
        (
            fConverter
            , retVal
            , srcLen+1
            , toTranscode
            , srcLen
            , &err
        );

        if (err == U_BUFFER_OVERFLOW_ERROR)  // @04c
	{

        err = U_ZERO_ERROR;
	delete [] retVal;  // @04a
        retVal = new XMLCh[targetCap];
        ucnv_toUChars
        (
            fConverter
            , retVal
            , targetCap
            , toTranscode
            , srcLen
            , &err
           );
	 }
   }

    if (U_FAILURE(err))
    {
        // Clean up if we got anything allocated
        delete [] retVal;
        return 0;
    }

    return retVal;
}

XMLCh* Iconv400LCPTranscoder::transcode(const char* const toTranscode,
                                        MemoryManager* const manager)
{
    // Watch for a few pyscho corner cases
    if (!toTranscode)
        return 0;

    XMLCh* retVal = 0;
    if (!*toTranscode)
    {
        retVal = (XMLCh*) manager->allocate(sizeof(XMLCh));//new XMLCh[1];
        retVal[0] = 0;
        return retVal;
    }

    //
    //  Get the length of the string to transcode. The Unicode string will
    //  almost always be no more chars than were in the source, so this is
    //  the best guess as to the storage needed.
    //
    const int32_t srcLen = (int32_t)strlen(toTranscode);
    // Allocate unicode string of equivalent length in unicode bytes
    retVal = (XMLCh*) manager->allocate((srcLen + 1) * sizeof(XMLCh));//new XMLCh[srcLen+1];

    // Now lock while we do these calculations
    UErrorCode err = U_ZERO_ERROR;
    {
        XMLMutexLock lockConverter(&fMutex);

        //
        //  Here we don't know what the target length will be so use 0 and
        //  expect an U_BUFFER_OVERFLOW_ERROR in which case it'd get resolved
        //  by the correct capacity value.
        //
        int32_t targetCap;
        targetCap = ucnv_toUChars
        (
            fConverter
            , retVal
            , srcLen+1
            , toTranscode
            , srcLen
            , &err
        );

        if (err == U_BUFFER_OVERFLOW_ERROR)  // @04c
	{

        err = U_ZERO_ERROR;
        manager->deallocate(retVal);  //@04a
        retVal = (XMLCh*) manager->allocate(targetCap * sizeof(XMLCh));//new XMLCh[targetCap];
        ucnv_toUChars
        (
            fConverter
            , retVal
            , targetCap
            , toTranscode
            , srcLen
            , &err
           );
	 }
   }

    if (U_FAILURE(err))
    {
        // Clean up if we got anything allocated
        manager->deallocate(retVal);//delete [] retVal;
        return 0;
    }

    return retVal;
}


bool Iconv400LCPTranscoder::transcode(const  char* const     toTranscode
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

    // Lock and do the transcode operation
    UErrorCode err = U_ZERO_ERROR;
    const int32_t srcLen = (int32_t)strlen(toTranscode);
    {
        XMLMutexLock lockConverter(&fMutex);
        ucnv_toUChars
        (
            fConverter
            , toFill
            , maxChars + 1
            , toTranscode
            , srcLen
            , &err
        );
    }

    if (U_FAILURE(err))
        return false;

    return true;
}


bool Iconv400LCPTranscoder::transcode(   const   XMLCh* const    toTranscode
                                    ,       char* const     toFill
                                    , const unsigned int    maxChars
                                    , MemoryManager* const  manager)
{
    // Watch for a few psycho corner cases
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


    UErrorCode err = U_ZERO_ERROR;
    int32_t targetCap;
    {
        XMLMutexLock lockConverter(&fMutex);
        targetCap = ucnv_fromUChars
        (
            fConverter
            , toFill
            , maxChars + 1
            , toTranscode
            , &err
        );
    }

    if (U_FAILURE(err))
        return false;

    toFill[targetCap] = 0;
    return true;
}

XERCES_CPP_NAMESPACE_END

