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
 * $Id: CygwinTransService.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/TranscodingException.hpp>
#include <xercesc/util/XMLException.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/RefHashTableOf.hpp>
#include "CygwinTransService.hpp"
#include <windows.h>
#include <stdlib.h>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local, const data
// ---------------------------------------------------------------------------
static const XMLCh gMyServiceId[] =
{
    chLatin_C, chLatin_y, chLatin_g, chLatin_w, chLatin_i, chLatin_n, chNull
};


// Cygwin doesn't support iswspace(), so this table is used by
// CygwinTransService::isSpace() based on a union of Unicode
// Table 6-1 and the ANSI definition of whitespace, arranged
// in order of likely occurrence.

static const XMLCh gWhitespace[] =
{
    0x0020,
    0x00a0,
    0x0009,
    0x000a,
    0x000d,
    0x000b,
    0x000c,
    0x3000,
    0x2000,
    0x2001,
    0x2002,
    0x2003,
    0x2004,
    0x2005,
    0x2006,
    0x2007,
    0x2008,
    0x2009,
    0x200a,
    0x200b,
    0x202f
};

// Used by the kernel32 function LCMapStringW to uppercasify strings
// appropriate to this locale.  Cygwin doesn't support _wcsupr().

static const LCID gLocaleId =
#if defined(CYGWINTRANSCODER_DEFAULT_LOCALE)
    MAKELCID( MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT), SORT_DEFAULT);
#else    // CYGWINTRANSCODER_DYNAMIC_LOCALE
    GetThreadLocale();
#endif



// ---------------------------------------------------------------------------
//  This is the simple CPMapEntry class. It just contains an encoding name
//  and a code page for that encoding.
// ---------------------------------------------------------------------------
class CPMapEntry : public XMemory
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    CPMapEntry
    (
        const   XMLCh* const    encodingName
        , const unsigned int    cpId
        , const unsigned int    ieId
    );

    CPMapEntry
    (
        const   char* const     encodingName
        , const unsigned int    cpId
        , const unsigned int    ieId
    );

    ~CPMapEntry();


    // -----------------------------------------------------------------------
    //  Getter methods
    // -----------------------------------------------------------------------
    const XMLCh* getEncodingName() const;
    const XMLCh* getKey() const;
    unsigned int getWinCP() const;
    unsigned int getIEEncoding() const;


private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    CPMapEntry();
    CPMapEntry(const CPMapEntry&);
    CPMapEntry& operator=(const CPMapEntry&);


    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fEncodingName
    //      This is the encoding name for the code page that this instance
    //      represents.
    //
    //  fCPId
    //      This is the Windows specific code page for the encoding that this
    //      instance represents.
    //
    //  fIEId
    //      This is the IE encoding id. Its not used at this time, but we
    //      go ahead and get it and store it just in case for later.
    // -----------------------------------------------------------------------
    XMLCh*          fEncodingName;
    unsigned int    fCPId;
    unsigned int    fIEId;
};

// ---------------------------------------------------------------------------
//  CPMapEntry: Constructors and Destructor
// ---------------------------------------------------------------------------
CPMapEntry::CPMapEntry( const   char* const     encodingName
                        , const unsigned int    cpId
                        , const unsigned int    ieId) :
    fEncodingName(0)
    , fCPId(cpId)
    , fIEId(ieId)
{
    // Transcode the name to Unicode and store that copy
    const unsigned int srcLen = strlen(encodingName);
    const unsigned int targetLen = ::MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, encodingName, srcLen, NULL, 0);
    fEncodingName = (XMLCh*) XMLPlatformUtils::fgMemoryManager->allocate
    (
        (targetLen + 1) * sizeof(XMLCh)
    );//new XMLCh[targetLen + 1];
    ::MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, encodingName, srcLen, (LPWSTR)fEncodingName, targetLen);
    fEncodingName[targetLen] = 0;

    //
    //  Upper case it because we are using a hash table and need to be
    //  sure that we find all case combinations.
    //
    ::LCMapStringW( gLocaleId, LCMAP_UPPERCASE, (LPCWSTR)fEncodingName, targetLen, (LPWSTR)fEncodingName, targetLen);
}

CPMapEntry::CPMapEntry( const   XMLCh* const    encodingName
                        , const unsigned int    cpId
                        , const unsigned int    ieId) :

    fEncodingName(0)
    , fCPId(cpId)
    , fIEId(ieId)
{
		fEncodingName = XMLString::replicate(encodingName, XMLPlatformUtils::fgMemoryManager);

    //
    //  Upper case it because we are using a hash table and need to be
    //  sure that we find all case combinations.
    //
    unsigned int itsLen = XMLString::stringLen( fEncodingName);
    ::LCMapStringW( gLocaleId, LCMAP_UPPERCASE, (LPCWSTR)fEncodingName, itsLen, (LPWSTR)fEncodingName, itsLen);
}

CPMapEntry::~CPMapEntry()
{
    XMLPlatformUtils::fgMemoryManager->deallocate(fEncodingName);//delete [] fEncodingName;
}


// ---------------------------------------------------------------------------
//  CPMapEntry: Getter methods
// ---------------------------------------------------------------------------
const XMLCh* CPMapEntry::getEncodingName() const
{
    return fEncodingName;
}

unsigned int CPMapEntry::getWinCP() const
{
    return fCPId;
}

unsigned int CPMapEntry::getIEEncoding() const
{
    return fIEId;
}






//---------------------------------------------------------------------------
//
//  class CygwinTransService Implementation ...
//
//---------------------------------------------------------------------------


// ---------------------------------------------------------------------------
//  CygwinTransService: Constructors and Destructor
// ---------------------------------------------------------------------------
CygwinTransService::CygwinTransService()
{
    fCPMap = new RefHashTableOf<CPMapEntry>(109);

    //
    //  Open up the registry key that contains the info we want. Note that,
    //  if this key does not exist, then we just return. It will just mean
    //  that we don't have any support except for intrinsic encodings supported
    //  by the parser itself (and the LCP support of course.
    //
    HKEY charsetKey;
    if (::RegOpenKeyExA
    (
        HKEY_CLASSES_ROOT
        , "MIME\\Database\\Charset"
        , 0
        , KEY_READ
        , &charsetKey))
    {
        return;
    }

    //
    //  Read in the registry keys that hold the code page ids. Skip for now
    //  those entries which indicate that they are aliases for some other
    //  encodings. We'll come back and do a second round for those and look
    //  up the original name and get the code page id.
    //
    //  Note that we have to use A versions here so that this will run on
    //  98, and transcode the strings to Unicode.
    //
    const unsigned int nameBufSz = 1024;
    char nameBuf[nameBufSz + 1];
    unsigned int subIndex = 0;
    unsigned long theSize;
    while (true)
    {
        // Get the name of the next key
        theSize = nameBufSz;
        if (::RegEnumKeyExA
        (
            charsetKey
            , subIndex
            , nameBuf
            , &theSize
            , 0, 0, 0, 0) == ERROR_NO_MORE_ITEMS)
        {
            break;
        }

        // Open this subkey
        HKEY encodingKey;
        if (::RegOpenKeyExA
        (
            charsetKey
            , nameBuf
            , 0
            , KEY_READ
            , &encodingKey))
        {
            XMLPlatformUtils::panic(PanicHandler::Panic_NoTransService);
        }

        //
        //  Lts see if its an alias. If so, then ignore it in this first
        //  loop. Else, we'll add a new entry for this one.
        //
        if (!isAlias(encodingKey))
        {
            //
            //  Lets get the two values out of this key that we are
            //  interested in. There should be a code page entry and an
            //  IE entry.
            //
            unsigned long theType;
            unsigned int CPId;
            unsigned int IEId;

            theSize = sizeof(unsigned int);
            if (::RegQueryValueExA
            (
                encodingKey
                , "Codepage"
                , 0
                , &theType
                , (unsigned char*)&CPId
                , &theSize) != ERROR_SUCCESS)
            {
                XMLPlatformUtils::panic(PanicHandler::Panic_NoTransService);
            }

            //
            //  If this is not a valid Id, and it might not be because its
            //  not loaded on this system, then don't take it.
            //
            if (::IsValidCodePage(CPId))
            {
                theSize = sizeof(unsigned int);
                if (::RegQueryValueExA
                (
                    encodingKey
                    , "InternetEncoding"
                    , 0
                    , &theType
                    , (unsigned char*)&IEId
                    , &theSize) != ERROR_SUCCESS)
                {
                    XMLPlatformUtils::panic(PanicHandler::Panic_NoTransService);
                }

                CPMapEntry* newEntry = new CPMapEntry(nameBuf, CPId, IEId);
                fCPMap->put((void*)newEntry->getEncodingName(), newEntry);
            }
        }

        // And now close the subkey handle and bump the subkey index
        ::RegCloseKey(encodingKey);
        subIndex++;
    }

    //
    //  Now loop one more time and this time we do just the aliases. For
    //  each one we find, we look up that name in the map we've already
    //  built and add a new entry with this new name and the same id
    //  values we stored for the original.
    //
    subIndex = 0;
    char aliasBuf[nameBufSz + 1];
    while (true)
    {
        // Get the name of the next key
        theSize = nameBufSz;
        if (::RegEnumKeyExA
        (
            charsetKey
            , subIndex
            , nameBuf
            , &theSize
            , 0, 0, 0, 0) == ERROR_NO_MORE_ITEMS)
        {
            break;
        }

        // Open this subkey
        HKEY encodingKey;
        if (::RegOpenKeyExA
        (
            charsetKey
            , nameBuf
            , 0
            , KEY_READ
            , &encodingKey))
        {
            XMLPlatformUtils::panic(PanicHandler::Panic_NoTransService);
        }

        //
        //  If its an alias, look up the name in the map. If we find it,
        //  then construct a new one with the new name and the aliased
        //  ids.
        //
        if (isAlias(encodingKey, aliasBuf, nameBufSz))
        {
            const unsigned int srcLen = strlen(aliasBuf);
            const unsigned int targetLen = ::MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, aliasBuf, srcLen, NULL, 0);
            XMLCh* uniAlias = (XMLCh*) XMLPlatformUtils::fgMemoryManager->allocate
            (
                (targetLen + 1) * sizeof(XMLCh)
            );//new XMLCh[targetLen + 1];
            ::MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, aliasBuf, srcLen, (LPWSTR)uniAlias, targetLen);
            uniAlias[targetLen] = 0;
            ::LCMapStringW( gLocaleId, LCMAP_UPPERCASE, (LPCWSTR)uniAlias, targetLen, (LPWSTR)uniAlias, targetLen);

            // Look up the alias name
            CPMapEntry* aliasedEntry = fCPMap->get(uniAlias);
            if (aliasedEntry)
            {
                //
                //  If the name is actually different, then take it.
                //  Otherwise, don't take it. They map aliases that are
                //  just different case.
                //
                if (auxCompareString(uniAlias, aliasedEntry->getEncodingName(), -1L, false))
                {
                    CPMapEntry* newEntry = new CPMapEntry(uniAlias, aliasedEntry->getWinCP(), aliasedEntry->getIEEncoding());
                    fCPMap->put((void*)newEntry->getEncodingName(), newEntry);
                }
            }
            XMLPlatformUtils::fgMemoryManager->deallocate(uniAlias);//delete [] uniAlias;
        }

        // And now close the subkey handle and bump the subkey index
        ::RegCloseKey(encodingKey);
        subIndex++;
    }

    // And close the main key handle
    ::RegCloseKey(charsetKey);

}

CygwinTransService::~CygwinTransService()
{
    delete fCPMap;
}


// ---------------------------------------------------------------------------
//  CygwinTransService: The virtual transcoding service API
// ---------------------------------------------------------------------------
int CygwinTransService::auxCompareString( const XMLCh* const comp1
                                         , const XMLCh* const comp2
                                         , signed long sMaxChars
                                         , const bool ignoreCase)
{
    const XMLCh* args[2] = { comp1, comp2 };
    XMLCh*       firstBuf = NULL;
    XMLCh*       secondBuf = NULL;
    unsigned int len = XMLString::stringLen( comp1);
    unsigned int otherLen = XMLString::stringLen( comp2);
    unsigned int countChar = 0;
    unsigned int maxChars;
    int          theResult = 0;

    // Determine at what string index the comparison stops.
    if ( sMaxChars != -1L )
    {
        len = ( len > (unsigned int)sMaxChars ) ? (unsigned int)sMaxChars : len;
        otherLen = ( otherLen > (unsigned int)sMaxChars ) ? (unsigned int)sMaxChars : otherLen;
        maxChars = ( len > otherLen ) ? otherLen : len;
    }
    else
    {
        // When no Max given must compare terminating NUL to return
        // difference if one string is shorter than the other.

        maxChars = ( len > otherLen ) ? otherLen : len;
        ++maxChars;
    }

    // Handle situation when one argument or the other is NULL
    // by returning +/- string length of non-NULL argument (inferred
    // from XMLString::CompareNString).

    // Obs. Definition of stringLen(XMLCh*) implies NULL ptr and ptr
    // to Empty String are equivalent.  It handles NULL args, BTW.

    if ( !comp1 )
    {
        // Negative because null ptr (c1) less than string (c2).
        return ( 0 - otherLen );
    }

    if ( !comp2 )
    {
        // Positive because string (c1) still greater than null ptr (c2).
        return len;
    }

    // Handle case-insensitive comparison by removing case from string.
    if ( ignoreCase )
    {
        // Copy const parameter strings (plus terminating nul) into locals.
        firstBuf = (XMLCh*) XMLPlatformUtils::fgMemoryManager->allocate
        (
            (++len) * sizeof(XMLCh)
        );//new XMLCh[ ++len];
        secondBuf = (XMLCh*) XMLPlatformUtils::fgMemoryManager->allocate
        (
            (++otherLen) * sizeof(XMLCh)
        );//new XMLCh[ ++otherLen];
        memcpy( firstBuf, comp1, len * sizeof(XMLCh));
        memcpy( secondBuf, comp2, otherLen * sizeof(XMLCh));

        // Then uppercase both strings, losing their case info.
        ::LCMapStringW( gLocaleId, LCMAP_UPPERCASE, (LPWSTR)firstBuf, len, (LPWSTR)firstBuf, len);
        ::LCMapStringW( gLocaleId, LCMAP_UPPERCASE, (LPWSTR)secondBuf, otherLen, (LPWSTR)secondBuf, otherLen);

        // Replace original params in args array with UC ones.
        args[0] = (const XMLCh*)firstBuf;
        args[1] = (const XMLCh*)secondBuf;

        // Not necessary only because they're not used beyond this pt.
        // --len;
        // --otherLen;
    }

    // Strings are equal until proven otherwise.
    while ( ( countChar < maxChars ) && ( !theResult ) )
    {
        theResult = (int)(args[0][countChar]) - (int)(args[1][countChar]);
        ++countChar;
    }

    // Clean-up buffers, equivalent to if ( ignoreCase )
    if ( firstBuf )
    {
        XMLPlatformUtils::fgMemoryManager->deallocate(firstBuf);//delete [] firstBuf;
        XMLPlatformUtils::fgMemoryManager->deallocate(secondBuf);//delete [] secondBuf;
    }

    return theResult;
}


int CygwinTransService::compareIString(  const   XMLCh* const    comp1
                                        , const XMLCh* const    comp2)
{
    return auxCompareString( comp1, comp2, -1L, true);
}


int CygwinTransService::compareNIString( const   XMLCh* const    comp1
                                        , const XMLCh* const    comp2
                                        , const unsigned int    maxChars)
{
    // Promote maxChars to signed long. Avoid any conceivable
    // portability issue from a simple C cast with extension
    // of sign bit when maxChars >= 2^16.  Making that param
    // signed long was necessary for reusability/an out-of-band
    // indicator in CygwinTransService::auxCompareString().

    // Obs. When maxChars == 0, return 0 (strings equal).

    return auxCompareString( comp1, comp2, (maxChars & 0x0000FFFFL), true);
}


const XMLCh* CygwinTransService::getId() const
{
    return gMyServiceId;
}


bool CygwinTransService::isSpace(const XMLCh toCheck) const
{
    unsigned int theCount = 0;
    while ( theCount < (sizeof(gWhitespace) / sizeof(XMLCh)) )
    {
        if ( toCheck == gWhitespace[theCount] )
            return true;

        ++theCount;
    }
    return false;
}


XMLLCPTranscoder* CygwinTransService::makeNewLCPTranscoder()
{
    // Just allocate a new LCP transcoder of our type
    return new CygwinLCPTranscoder;
}


bool CygwinTransService::supportsSrcOfs() const
{
    //
    //  Since the only mechanism we have to translate XML text in this
    //  transcoder basically require us to do work that allows us to support
    //  source offsets, we might as well do it.
    //
    return true;
}


void CygwinTransService::upperCase(XMLCh* const toUpperCase) const
{
    unsigned int itsLen = XMLString::stringLen( toUpperCase);
    ::LCMapStringW( gLocaleId, LCMAP_UPPERCASE, (LPWSTR)toUpperCase, itsLen, (LPWSTR)toUpperCase, itsLen);
}


void CygwinTransService::lowerCase(XMLCh* const toLowerCase) const
{
    unsigned int itsLen = XMLString::stringLen( toLowerCase);
    ::LCMapStringW( gLocaleId, LCMAP_LOWERCASE, (LPWSTR)toLowerCase, itsLen, (LPWSTR)toLowerCase, itsLen);
}


bool CygwinTransService::isAlias(const   HKEY            encodingKey
                    ,       char* const     aliasBuf
                    , const unsigned int    nameBufSz )
{
    unsigned long theType;
    unsigned long theSize = nameBufSz;
    return (::RegQueryValueExA
    (
        encodingKey
        , "AliasForCharset"
        , 0
        , &theType
        , (unsigned char*)aliasBuf
        , &theSize
    ) == ERROR_SUCCESS);
}


XMLTranscoder*
CygwinTransService::makeNewXMLTranscoder(const   XMLCh* const           encodingName
                                        ,       XMLTransService::Codes& resValue
                                        , const unsigned int            blockSize
                                        ,       MemoryManager* const    manager)
{
    const unsigned int upLen = 1024;
    XMLCh upEncoding[upLen + 1];

    //
    //  Get an upper cased copy of the encoding name, since we use a hash
    //  table and we store them all in upper case.
    //
    unsigned int itsLen = XMLString::stringLen( encodingName) + 1;
    memcpy(
        upEncoding
        , encodingName
        , sizeof(XMLCh) * ( ( itsLen > upLen) ? upLen : itsLen)
    );
    upEncoding[upLen] = 0;  // necessary? terminating NUL should've copied.
    upperCase(upEncoding);

    // Now to try to find this guy in the CP map
    CPMapEntry* theEntry = fCPMap->get(upEncoding);

    // If not found, then return a null pointer
    if (!theEntry)
    {
        resValue = XMLTransService::UnsupportedEncoding;
        return 0;
    }

    // We found it, so return a Cygwin transcoder for this encoding
    return new (manager) CygwinTranscoder
    (
        encodingName
        , theEntry->getWinCP()
        , theEntry->getIEEncoding()
        , blockSize
        , manager
    );
}








//---------------------------------------------------------------------------
//
//  class CygwinTranscoder Implementation ...
//
//---------------------------------------------------------------------------


// ---------------------------------------------------------------------------
//  CygwinTranscoder: Constructors and Destructor
// ---------------------------------------------------------------------------
CygwinTranscoder::CygwinTranscoder(const  XMLCh* const    encodingName
                                , const unsigned int    winCP
                                , const unsigned int    ieCP
                                , const unsigned int    blockSize
                                , MemoryManager* const manager) :

    XMLTranscoder(encodingName, blockSize, manager)
    , fIECP(ieCP)
    , fWinCP(winCP)
{
}

CygwinTranscoder::~CygwinTranscoder()
{
}


// ---------------------------------------------------------------------------
//  CygwinTranscoder: The virtual transcoder API
// ---------------------------------------------------------------------------
unsigned int
CygwinTranscoder::transcodeFrom(  const XMLByte* const      srcData
                                , const unsigned int        srcCount
                                ,       XMLCh* const        toFill
                                , const unsigned int        maxChars
                                ,       unsigned int&       bytesEaten
                                ,       unsigned char* const charSizes)
{
    // Get temp pointers to the in and out buffers, and the chars sizes one
    XMLCh*          outPtr = toFill;
    const XMLByte*  inPtr  = srcData;
    unsigned char*  sizesPtr = charSizes;

    // Calc end pointers for each of them
    XMLCh*          outEnd = toFill + maxChars;
    const XMLByte*  inEnd  = srcData + srcCount;

    //
    //  Now loop until we either get our max chars, or cannot get a whole
    //  character from the input buffer.
    //
    bytesEaten = 0;
    while ((outPtr < outEnd) && (inPtr < inEnd))
    {
        //
        //  If we are looking at a leading byte of a multibyte sequence,
        //  then we are going to eat 2 bytes, else 1.
        //
        const unsigned int toEat = ::IsDBCSLeadByteEx(fWinCP, *inPtr) ?
                                    2 : 1;

        // Make sure a whol char is in the source
        if (inPtr + toEat > inEnd)
            break;

        // Try to translate this next char and check for an error
        const unsigned int converted = ::MultiByteToWideChar
        (
            fWinCP
            , MB_PRECOMPOSED | MB_ERR_INVALID_CHARS
            , (const char*)inPtr
            , toEat
            , (LPWSTR)outPtr
            , 1
        );

        if (converted != 1)
        {
            if (toEat == 1)
            {
                XMLCh tmpBuf[17];
                XMLString::binToText((unsigned int)(*inPtr), tmpBuf, 16, 16, getMemoryManager());
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

        // Update the char sizes array for this round
        *sizesPtr++ = toEat;

        // And update the bytes eaten count
        bytesEaten += toEat;

        // And update our in/out ptrs
        inPtr += toEat;
        outPtr++;
    }

    // Return the chars we output
    return (outPtr - toFill);
}


unsigned int
CygwinTranscoder::transcodeTo(const  XMLCh* const   srcData
                            , const unsigned int    srcCount
                            ,       XMLByte* const  toFill
                            , const unsigned int    maxBytes
                            ,       unsigned int&   charsEaten
                            , const UnRepOpts       options)
{
    // Get pointers to the start and end of each buffer
    const XMLCh*    srcPtr = srcData;
    const XMLCh*    srcEnd = srcData + srcCount;
    XMLByte*        outPtr = toFill;
    XMLByte*        outEnd = toFill + maxBytes;

    //
    //  Now loop until we either get our max chars, or cannot get a whole
    //  character from the input buffer.
    //
    //  NOTE: We have to use a loop for this unfortunately because the
    //  conversion API is too dumb to tell us how many chars it converted if
    //  it couldn't do the whole source.
    //
    BOOL usedDef;
    while ((outPtr < outEnd) && (srcPtr < srcEnd))
    {
        //
        //  Do one char and see if it made it.
        const unsigned int bytesStored = ::WideCharToMultiByte
        (
            fWinCP
            , WC_COMPOSITECHECK | WC_SEPCHARS
            , (LPCWSTR)srcPtr
            , 1
            , (char*)outPtr
            , outEnd - outPtr
            , 0
            , &usedDef
        );

        // If we didn't transcode anything, then we are done
        if (!bytesStored)
            break;

        //
        //  If the defaault char was used and the options indicate that
        //  this isn't allowed, then throw.
        //
        if (usedDef && (options == UnRep_Throw))
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

        // Update our pointers
        outPtr += bytesStored;
        srcPtr++;
    }

    // Update the chars eaten
    charsEaten = srcPtr - srcData;

    // And return the bytes we stored
    return outPtr - toFill;
}


bool CygwinTranscoder::canTranscodeTo(const unsigned int toCheck) const
{
    //
    //  If the passed value is really a surrogate embedded together, then
    //  we need to break it out into its two chars. Else just one.
    //
    XMLCh           srcBuf[2];
    unsigned int    srcCount = 1;
    if (toCheck & 0xFFFF0000)
    {
        srcBuf[0] = XMLCh((toCheck >> 10) + 0xD800);
        srcBuf[1] = XMLCh(toCheck & 0x3FF) + 0xDC00;
        srcCount++;
    }
    else
    {
        srcBuf[0] = XMLCh(toCheck);
    }

    //
    //  Use a local temp buffer that would hold any sane multi-byte char
    //  sequence and try to transcode this guy into it.
    //
    char tmpBuf[64];

    BOOL usedDef;
    const unsigned int bytesStored = ::WideCharToMultiByte
    (
        fWinCP
        , WC_COMPOSITECHECK | WC_SEPCHARS
        , (LPCWSTR)srcBuf
        , srcCount
        , tmpBuf
        , 64
        , 0
        , &usedDef
    );

    if (!bytesStored || usedDef)
        return false;

    return true;
}




//---------------------------------------------------------------------------
//
//  class CygwinTranscoder Implementation ...
//
//---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//  CygwinLCPTranscoder: Constructors and Destructor
// ---------------------------------------------------------------------------
CygwinLCPTranscoder::CygwinLCPTranscoder()
{
}

CygwinLCPTranscoder::~CygwinLCPTranscoder()
{
}


// ---------------------------------------------------------------------------
//  CygwinLCPTranscoder: Implementation of the virtual transcoder interface
// ---------------------------------------------------------------------------
unsigned int CygwinLCPTranscoder::calcRequiredSize(const char* const srcText
                                                   , MemoryManager* const /*manager*/)
{
    if (!srcText)
        return 0;

    return ::MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, srcText, -1, NULL, 0);
}


unsigned int CygwinLCPTranscoder::calcRequiredSize(const XMLCh* const srcText
                                                   , MemoryManager* const /*manager*/)
{
    if (!srcText)
        return 0;

    return ::WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)srcText, -1, NULL, 0, NULL, NULL);
}


char* CygwinLCPTranscoder::transcode(const XMLCh* const toTranscode)
{
    if (!toTranscode)
        return 0;

    char* retVal = 0;
    if (*toTranscode)
    {
        // Calc the needed size
        const unsigned int neededLen = ::WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)toTranscode, -1, NULL, 0, NULL, NULL);
        if (neededLen == 0)
            return 0;

        // Allocate a buffer of that size plus one for the null and transcode
        // Returned length of WideCharToMultiByte includes terminating NUL.
        retVal = new char[neededLen+1];
        ::WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)toTranscode, -1, retVal, neededLen+1, NULL, NULL);

        // And cap it off anyway just to make sure
        retVal[neededLen] = 0;
    }
    else
    {
        retVal = new char[1];
        retVal[0] = 0;
    }
    return retVal;
}

char* CygwinLCPTranscoder::transcode(const XMLCh* const toTranscode,
                                     MemoryManager* const manager)
{
    if (!toTranscode)
        return 0;

    char* retVal = 0;
    if (*toTranscode)
    {
        // Calc the needed size
        const unsigned int neededLen = ::WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)toTranscode, -1, NULL, 0, NULL, NULL);
        if (neededLen == 0)
            return 0;

        // Allocate a buffer of that size plus one for the null and transcode
        // Returned length of WideCharToMultiByte includes terminating NUL.
        retVal = (char*) manager->allocate((neededLen+1) * sizeof(char)); //new char[neededLen+1];
        ::WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)toTranscode, -1, retVal, neededLen+1, NULL, NULL);

        // And cap it off anyway just to make sure
        retVal[neededLen] = 0;
    }
    else
    {
        retVal = (char*) manager->allocate(sizeof(char)); //new char[1];
        retVal[0] = 0;
    }
    return retVal;
}


XMLCh* CygwinLCPTranscoder::transcode(const char* const toTranscode)
{
    if (!toTranscode)
        return 0;

    XMLCh* retVal = 0;
    if (*toTranscode)
    {
        // Calculate the buffer size required
        const unsigned int neededLen = ::MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, toTranscode, -1, NULL, 0);
        if (neededLen == 0)
            return 0;

        // Allocate a buffer of that size plus one for the null and transcode
        retVal = new XMLCh[neededLen + 1];
        ::MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, toTranscode, -1, (LPWSTR)retVal, neededLen + 1);

        // Cap it off just to make sure. We are so paranoid!
        retVal[neededLen] = 0;
    }
    else
    {
        retVal = new XMLCh[1];
        retVal[0] = 0;
    }
    return retVal;
}

XMLCh* CygwinLCPTranscoder::transcode(const char* const toTranscode,
                                      MemoryManager* const manager)
{
    if (!toTranscode)
        return 0;

    XMLCh* retVal = 0;
    if (*toTranscode)
    {
        // Calculate the buffer size required
        const unsigned int neededLen = ::MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, toTranscode, -1, NULL, 0);
        if (neededLen == 0)
            return 0;

        // Allocate a buffer of that size plus one for the null and transcode
        retVal = (XMLCh*) manager->allocate((neededLen + 1) * sizeof(XMLCh)); //new XMLCh[neededLen + 1];
        ::MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, toTranscode, -1, (LPWSTR)retVal, neededLen + 1);

        // Cap it off just to make sure. We are so paranoid!
        retVal[neededLen] = 0;
    }
    else
    {
        retVal = (XMLCh*) manager->allocate(sizeof(XMLCh)); //new XMLCh[1];
        retVal[0] = 0;
    }
    return retVal;
}


bool CygwinLCPTranscoder::transcode( const   char* const    toTranscode
                                    ,       XMLCh* const    toFill
                                    , const unsigned int    maxChars
                                    , MemoryManager* const  /*manager*/)
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

    // This one has a fixed size output, so try it and if it fails it fails
    if ( 0 == ::MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, toTranscode, -1, (LPWSTR)toFill, maxChars + 1) )
        return false;
    return true;
}


bool CygwinLCPTranscoder::transcode( const  XMLCh* const    toTranscode
                                    ,       char* const     toFill
                                    , const unsigned int    maxBytes
                                    , MemoryManager* const  /*manager*/)
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

    // This one has a fixed size output, so try it and if it fails it fails
    if ( 0 == ::WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)toTranscode, -1, toFill, maxBytes + 1, NULL, NULL) )
        return false;

    // Cap it off just in case
    toFill[maxBytes] = 0;
    return true;
}


XERCES_CPP_NAMESPACE_END



