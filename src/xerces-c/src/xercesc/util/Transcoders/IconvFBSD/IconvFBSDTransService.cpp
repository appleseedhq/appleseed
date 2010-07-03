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
 * $Id: IconvFBSDTransService.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <ctype.h>

#ifdef XML_USE_LIBICONV
# include <locale.h>
# include <iconv.h>
# include <errno.h>
# include <machine/endian.h>

// ---------------------------------------------------------------------------
// Description of encoding schemas, supported by iconv()
// ---------------------------------------------------------------------------
typedef struct __IconvFBSDEncoding {
    const char*    fSchema;    // schema name
    size_t    fUChSize;    // size of the character
    unsigned int fUBO;        // byte order, relative to the host
} IconvFBSDEncoding;

static const IconvFBSDEncoding    gIconvFBSDEncodings[] = {
    { "ucs-2-internal",        2,    LITTLE_ENDIAN },
    { "ucs2-internal",        2,    LITTLE_ENDIAN },
    { "ucs-4-internal",        4,    LITTLE_ENDIAN },
    { "ucs4-internal",        4,    LITTLE_ENDIAN },
    { "UNICODELITTLE",        2,    LITTLE_ENDIAN },
    { "UNICODEBIG",        2,    BIG_ENDIAN },
    { "iso-10646-ucs-2",    4,    BIG_ENDIAN },
    { "iso-10646-ucs-4",    4,    BIG_ENDIAN },
    /* { "iso-10646-utf-16",    2,    BIG_ENDIAN }, */
    { NULL, 0,     0 }
};

//--------------------------------------------------
// Macro-definitions to translate "native unicode"
// characters <-> XMLCh with different host byte order
// and encoding schemas.

# if BYTE_ORDER == LITTLE_ENDIAN
#  define IXMLCh2WC16(x,w)            \
    *(w) = ((*(x)) >> 8) & 0xFF;        \
    *((w)+1) = (*(x)) & 0xFF
#  define IWC162XMLCh(w,x)    *(x) = ((*(w)) << 8) | (*((w)+1))
#  define XMLCh2WC16(x,w)            \
    *(w) = (*(x)) & 0xFF;            \
    *((w)+1) = ((*(x)) >> 8) & 0xFF
#  define WC162XMLCh(w,x)    *(x) = ((*((w)+1)) << 8) | (*(w))

#  define IXMLCh2WC32(x,w)            \
    *(w) = ((*(x)) >> 24) & 0xFF;        \
    *((w)+1) = ((*(x)) >> 16) & 0xFF;    \
    *((w)+2) = ((*(x)) >> 8) & 0xFF;    \
    *((w)+3) = (*(x)) & 0xFF
#  define IWC322XMLCh(w,x)                \
      *(x) = ((*(w)) << 24) | ((*((w)+1)) << 16) |    \
          ((*((w)+2)) << 8) | (*((w)+3))
#  define XMLCh2WC32(x,w)            \
    *((w)+3) = ((*(x)) >> 24) & 0xFF;    \
    *((w)+2) = ((*(x)) >> 16) & 0xFF;    \
    *((w)+1) = ((*(x)) >> 8) & 0xFF;    \
    *(w) = (*(x)) & 0xFF
#  define WC322XMLCh(w,x)                    \
      *(x) = ((*((w)+3)) << 24) | ((*((w)+2)) << 16) |    \
        ((*((w)+1)) << 8) | (*(w))

# else /* BYTE_ORDER != LITTLE_ENDIAN */

#  define XMLCh2WC16(x,w)            \
    *(w) = ((*(x)) >> 8) & 0xFF;        \
    *((w)+1) = (*(x)) & 0xFF
#  define WC162XMLCh(w,x)    *(x) = ((*(w)) << 8) | (*((w)+1))
#  define IXMLCh2WC16(x,w)            \
    *(w) = (*(x)) & 0xFF;            \
    *((w)+1) = ((*(x)) >> 8) & 0xFF
#  define IWC162XMLCh(w,x)    *(x) = ((*((w)+1)) << 8) | (*(w))

#  define XMLCh2WC32(x,w)            \
    *(w) = ((*(x)) >> 24) & 0xFF;        \
    *((w)+1) = ((*(x)) >> 16) & 0xFF;    \
    *((w)+2) = ((*(x)) >> 8) & 0xFF;    \
    *((w)+3) = (*(x)) & 0xFF
#  define WC322XMLCh(w,x)                \
      *(x) = ((*(w)) << 24) | ((*((w)+1)) << 16) |    \
          ((*((w)+2)) << 8) | (*((w)+3))
#  define IXMLCh2WC32(x,w)            \
    *((w)+3) = ((*(x)) >> 24) & 0xFF;    \
    *((w)+2) = ((*(x)) >> 16) & 0xFF;    \
    *((w)+1) = ((*(x)) >> 8) & 0xFF;    \
    *(w) = (*(x)) & 0xFF
#  define IWC322XMLCh(w,x)                    \
      *(x) = ((*((w)+3)) << 24) | ((*((w)+2)) << 16) |    \
        ((*((w)+1)) << 8) | (*(w))
# endif /* BYTE_ORDER == LITTLE_ENDIAN */

#else /* !XML_USE_LIBICONV */

# if __FreeBSD_cc_version > 430000
#  include <wchar.h>
# else
#  define wint_t XMLCh
# endif

#endif /* XML_USE_LIBICONV */

#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/TranscodingException.hpp>
#include "IconvFBSDTransService.hpp"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#if !defined(APP_NO_THREADS)
#include <xercesc/util/Mutexes.hpp>
#endif

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local, const data
// ---------------------------------------------------------------------------
static const unsigned int    gTempBuffArraySize = 4096;
static const XMLCh          gMyServiceId[] =
{
    chLatin_I, chLatin_C, chLatin_o, chLatin_n, chLatin_v, chNull
};


// ---------------------------------------------------------------------------
//  Local methods
// ---------------------------------------------------------------------------
static unsigned int getWideCharLength(const XMLCh* const src)
{
    if (!src)
        return 0;

    unsigned int len = 0;
    const XMLCh* pTmp = src;
    while (*pTmp++)
        len++;
    return len;
}

#ifndef XML_USE_LIBICONV

// ---------------------------------------------------------------------------
// FreeBSD got the wide-characters support since 4.0 version. But (at least
// up to the 4.4) this support differs from "others" in that the xxstoyys()
// does not handle the NULL-dest argument properly. So the custom functions
// are provided.
// ---------------------------------------------------------------------------

#define __TMP_ARRAY_SIZE__      4

static size_t fbsd_wcstombs(char *dest, const wchar_t *src, size_t n)
{
    char        tarr[ __TMP_ARRAY_SIZE__ + 1 ];
    size_t      len = 0, lent = 0;
    char*       ptr;
    size_t      slen;
    wchar_t*    wptr;

    if (dest)
        return ::wcstombs(dest, src, n);
    if (!src)
        return 0;
    for (wptr = (wchar_t *) src, slen = 0; *wptr; wptr++, slen++);
    if (slen == 0)
        return 0;
    wptr = (wchar_t *) src;
    ptr = dest;
    while ( (len = ::wcstombs(tarr, wptr, __TMP_ARRAY_SIZE__)) > 0 ) {
        wptr += len;
        lent += len;
    }
    if (len == (unsigned) -1)
        return 0;
    return lent;
}

static size_t fbsd_mbstowcs(wchar_t *dest, const char *src, size_t n)
{
    wchar_t     tarr[ __TMP_ARRAY_SIZE__ + 1 ];
    size_t      len = 0, lent = 0;
    char*       ptr;

    if (dest)
        return ::mbstowcs(dest, src, n);
    ptr = (char*) src;
    if (!src || strlen(src) == 0)
        return 0;
    while ( (len = ::mbstowcs(tarr, ptr, __TMP_ARRAY_SIZE__)) > 0 ) {
        ptr += len;
        lent += len;
    }
    if (len == (unsigned) -1)
        return 0;
    return lent;
}

static wint_t fbsd_towupper(wint_t ch)
{
    if (ch <= 0x7F)
    return toupper(ch);
    unsigned char    buf[16];
    wchar_t    wc = wchar_t(ch);
    wcstombs((char*)buf, &wc, 16);
    return toupper(*buf);
}

static wint_t fbsd_towlower(wint_t ch)
{
    if (ch <= 0x7F)
    return tolower(ch);
    unsigned char    buf[16];
    wchar_t    wc = wchar_t(ch);
    wcstombs((char *)buf, &wc, 16);
    return tolower(*buf);
}

#else /* XML_USE_LIBICONV */

#if !defined(APP_NO_THREADS)
// Iconv() access syncronization point
static XMLMutex    *gIconvMutex = NULL;
#  define ICONV_LOCK    XMLMutexLock lockConverter(gIconvMutex);

#else /* APP_NO_THREADS */

# define ICONV_LOCK

#endif /* !APP_NO_THREADS */

//----------------------------------------------------------------------------
// There is implementation of the libiconv for FreeBSD (available through the
// ports collection). The following is a wrapper around the iconv().
//----------------------------------------------------------------------------

IconvFBSDCD::IconvFBSDCD ()
    : fUChSize(0), fUBO(LITTLE_ENDIAN),
      fCDTo((iconv_t)-1), fCDFrom((iconv_t)-1)
{
}

IconvFBSDCD::IconvFBSDCD ( iconv_t    cd_from,
               iconv_t    cd_to,
               size_t    uchsize,
               unsigned int    ubo )
    : fUChSize(uchsize), fUBO(ubo),
      fCDTo(cd_to), fCDFrom(cd_from)
{
    if (fCDFrom == (iconv_t) -1 || fCDTo == (iconv_t) -1) {
    XMLPlatformUtils::panic (PanicHandler::Panic_NoTransService);
    }
}

IconvFBSDCD::~IconvFBSDCD()
{
}

// Convert "native unicode" character into XMLCh
void    IconvFBSDCD::mbcToXMLCh (const char *mbc, XMLCh *toRet) const
{
    if (fUBO == LITTLE_ENDIAN) {
        if (fUChSize == sizeof(XMLCh))
            *toRet = *((XMLCh*) mbc);
        else if (fUChSize == 2) {
            WC162XMLCh( mbc, toRet );
        } else {
            WC322XMLCh( mbc, toRet );
        }
    } else {
        if (fUChSize == 2) {
            IWC162XMLCh( mbc, toRet );
        } else {
            IWC322XMLCh( mbc, toRet );
        }
    }
}

// Convert XMLCh into "native unicode" character
void    IconvFBSDCD::xmlChToMbc (XMLCh xch, char *mbc) const
{
    if (fUBO == LITTLE_ENDIAN) {
        if (fUChSize == sizeof(XMLCh)) {
            memcpy (mbc, &xch, fUChSize);
            return;
        }
        if (fUChSize == 2) {
            XMLCh2WC16( &xch, mbc );
        } else {
            XMLCh2WC32( &xch, mbc );
        }
    } else {
        if (fUChSize == 2) {
            IXMLCh2WC16( &xch, mbc );
        } else {
            IXMLCh2WC32( &xch, mbc );
        }
    }
}

// Return uppercase equivalent for XMLCh
XMLCh    IconvFBSDCD::toUpper (const XMLCh ch) const
{
    if (ch <= 0x7F)
        return toupper(ch);

    char    wcbuf[fUChSize * 2];
    xmlChToMbc (ch, wcbuf);

    char    tmpArr[4];
    char*    ptr = wcbuf;
    size_t    len = fUChSize;
    char    *pTmpArr = tmpArr;
    size_t    bLen = 2;

    ICONV_LOCK;
    if (::iconv (fCDTo, (const char**) &ptr, &len,
         &pTmpArr, &bLen) == (size_t) -1)
    return 0;
    tmpArr[1] = toupper (*((unsigned char *)tmpArr));
    *tmpArr = tmpArr[1];
    len = 1;
    pTmpArr = wcbuf;
    bLen = fUChSize;
    ptr = tmpArr;
    if (::iconv (fCDFrom, (const char **)&ptr, &len,
         &pTmpArr, &bLen) == (size_t) -1)
    return 0;
    mbcToXMLCh (wcbuf, (XMLCh*) &ch);
    return ch;
}

// Return lowercase equivalent for XMLCh
XMLCh    IconvFBSDCD::toLower (const XMLCh ch) const
{
    if (ch <= 0x7F)
        return tolower(ch);

    char    wcbuf[fUChSize * 2];
    xmlChToMbc (ch, wcbuf);

    char    tmpArr[4];
    char*    ptr = wcbuf;
    size_t    len = fUChSize;
    char    *pTmpArr = tmpArr;
    size_t    bLen = 2;

    ICONV_LOCK;
    if (::iconv (fCDTo, (const char**) &ptr, &len,
         &pTmpArr, &bLen) == (size_t) -1)
    return 0;
    tmpArr[1] = tolower (*((unsigned char*)tmpArr));
    *tmpArr = tmpArr[1];
    len = 1;
    pTmpArr = wcbuf;
    bLen = fUChSize;
    ptr = tmpArr;
    if (::iconv (fCDFrom, (const char **)&ptr, &len,
         &pTmpArr, &bLen) == (size_t) -1)
    return 0;
    mbcToXMLCh (wcbuf, (XMLCh*) &ch);
    return ch;
}

// Check if passed characters belongs to the :space: class
bool    IconvFBSDCD::isSpace(const XMLCh toCheck) const
{
    if (toCheck <= 0x7F)
        return isspace(toCheck);

    char    wcbuf[fUChSize * 2];
    char    tmpArr[4];

    xmlChToMbc (toCheck, wcbuf);
    char*    ptr = wcbuf;
    size_t    len = fUChSize;
    char    *pTmpArr = tmpArr;
    size_t    bLen = 2;

    {
        ICONV_LOCK;
        if (::iconv (fCDTo, (const char**) &ptr, &len,
                 &pTmpArr, &bLen) == (size_t) -1)
            return 0;
    }
    return isspace(*tmpArr);
}

// Fill array of XMLCh characters with data, supplyed in the array
// of "native unicode" characters.
XMLCh*    IconvFBSDCD::mbsToXML
(
    const char*        mbs_str
    ,       size_t    mbs_cnt
    ,       XMLCh*    xml_str
    ,       size_t    xml_cnt
) const
{
    if (mbs_str == NULL || mbs_cnt == 0 || xml_str == NULL || xml_cnt == 0)
        return NULL;
    size_t    cnt = (mbs_cnt < xml_cnt) ? mbs_cnt : xml_cnt;
    if (fUBO == LITTLE_ENDIAN) {
    if (fUChSize == sizeof(XMLCh)) {
        // null-transformation
        memcpy (xml_str, mbs_str, fUChSize * cnt);
        return xml_str;
    }
    if (fUChSize == 2)
        for (size_t i = 0; i < cnt; i++, mbs_str += fUChSize) {
            WC162XMLCh( mbs_str, xml_str + i);
        }
    else
        for (size_t i = 0; i < cnt; i++, mbs_str += fUChSize) {
            WC322XMLCh( mbs_str, xml_str + i );
        }
    } else {
        if (fUChSize == 2)
            for (size_t i = 0; i < cnt; i++, mbs_str += fUChSize) {
                IWC162XMLCh( mbs_str, xml_str + i );
            }
        else
            for (size_t i = 0; i < cnt; i++, mbs_str += fUChSize) {
                IWC322XMLCh( mbs_str, xml_str + i );
            }
    }
    return xml_str;
}

// Fill array of "native unicode" characters with data, supplyed
// in the array of XMLCh characters.
char*    IconvFBSDCD::xmlToMbs
(
    const XMLCh*    xml_str
    ,      size_t    xml_cnt
    ,      char*        mbs_str
    ,      size_t    mbs_cnt
) const
{
    if (mbs_str == NULL || mbs_cnt == 0 || xml_str == NULL || xml_cnt == 0)
        return NULL;
    size_t    cnt = (mbs_cnt < xml_cnt) ? mbs_cnt : xml_cnt;
    char    *toReturn = mbs_str;
    if (fUBO == LITTLE_ENDIAN) {
        if (fUChSize == sizeof(XMLCh)) {
            // null-transformation
            memcpy (mbs_str, xml_str, fUChSize * cnt);
            return toReturn;
        }
        if (fUChSize == 2)
            for (size_t i = 0; i < cnt; i++, mbs_str += fUChSize, xml_str++) {
                XMLCh2WC16( xml_str, mbs_str );
            }
        else
            for (size_t i = 0; i < cnt; i++, mbs_str += fUChSize, xml_str++) {
                XMLCh2WC32( xml_str, mbs_str );
            }
    } else {
        if (fUChSize == 2)
            for (size_t i = 0; i < cnt; i++, mbs_str += fUChSize, xml_str++) {
                IXMLCh2WC16( xml_str, mbs_str );
            }
        else
            for (size_t i = 0; i < cnt; i++, mbs_str += fUChSize, xml_str++) {
                IXMLCh2WC32( xml_str, mbs_str );
            }
    }
    return toReturn;
}

size_t    IconvFBSDCD::iconvFrom ( const char    *fromPtr,
                 size_t        *fromLen,
                 char        **toPtr,
                 size_t        toLen ) const
{
    ICONV_LOCK;
    return ::iconv (fCDFrom, &fromPtr, fromLen, toPtr, &toLen);
}

size_t    IconvFBSDCD::iconvTo ( const char    *fromPtr,
                   size_t        *fromLen,
                   char        **toPtr,
                   size_t        toLen ) const
{
    ICONV_LOCK;
    return ::iconv (fCDTo, &fromPtr, fromLen, toPtr, &toLen);
}

#endif /* !XML_USE_LIBICONV */


// ---------------------------------------------------------------------------
//  IconvFBSDTransService: Constructors and Destructor
// ---------------------------------------------------------------------------

IconvFBSDTransService::IconvFBSDTransService()
#ifndef XML_USE_LIBICONV
{}
#else  /* XML_USE_LIBICONV */
    : IconvFBSDCD(), fUnicodeCP(0)
{
#if !defined(APP_NO_THREADS)
    // Create global lock object
    if (gIconvMutex == NULL) {
        gIconvMutex = new XMLMutex;
        if (gIconvMutex == NULL)
            XMLPlatformUtils::panic (PanicHandler::Panic_NoTransService);
    }
#endif

    // Try to obtain local (host) characterset from the setlocale
    // and through the environment. Do not call setlocale(LC_*, "")!
    // Using an empty string instead of NULL, will modify the libc
    // behavior.
    //
    char* fLocalCP = setlocale (LC_CTYPE, NULL);
    if (fLocalCP == NULL || *fLocalCP == 0 ||
        strcmp (fLocalCP, "C") == 0 ||
        strcmp (fLocalCP, "POSIX") == 0) {
      fLocalCP = getenv ("LC_ALL");
      if (fLocalCP == NULL) {
        fLocalCP = getenv ("LC_CTYPE");
        if (fLocalCP == NULL)
          fLocalCP = getenv ("LANG");
      }
    }

    if (fLocalCP == NULL || *fLocalCP == 0 ||
        strcmp (fLocalCP, "C") == 0 ||
        strcmp (fLocalCP, "POSIX") == 0)
        fLocalCP = "iso-8859-1";    // fallback locale
    else {
        char    *ptr = strchr (fLocalCP, '.');
        if (ptr == NULL)
            fLocalCP = "iso-8859-1";    // fallback locale
        else
            fLocalCP = ptr + 1;
    }

    // Select the native unicode characters encoding schema
    const IconvFBSDEncoding    *eptr;
    // first - try to use the schema with character size, equil to XMLCh
    for (eptr = gIconvFBSDEncodings; eptr->fSchema; eptr++) {
        if (eptr->fUChSize != sizeof(XMLCh))
            continue;
        ICONV_LOCK;
        // try to create conversion descriptor
        iconv_t    cd_to = iconv_open(fLocalCP, eptr->fSchema);
        if (cd_to == (iconv_t)-1)
            continue;
        iconv_t    cd_from = iconv_open(eptr->fSchema, fLocalCP);
        if (cd_to == (iconv_t)-1) {
            iconv_close (cd_to);
            continue;
        }
        // got it
        setUChSize(eptr->fUChSize);
        setUBO(eptr->fUBO);
        setCDTo(cd_to);
        setCDFrom(cd_from);
        fUnicodeCP = eptr->fSchema;
        break;
    }
    if (fUnicodeCP == NULL)
        // try to use any known schema
        for (eptr = gIconvFBSDEncodings; eptr->fSchema; eptr++) {
            // try to create conversion descriptor
            ICONV_LOCK;
            iconv_t    cd_to = iconv_open(fLocalCP, eptr->fSchema);
            if (cd_to == (iconv_t)-1)
            continue;
            iconv_t    cd_from = iconv_open(eptr->fSchema, fLocalCP);
            if (cd_to == (iconv_t)-1) {
            iconv_close (cd_to);
            continue;
        }
        // got it
        setUChSize(eptr->fUChSize);
        setUBO(eptr->fUBO);
        setCDTo(cd_to);
        setCDFrom(cd_from);
        fUnicodeCP = eptr->fSchema;
        break;
    }

    if (fUnicodeCP == NULL || cdTo() == (iconv_t)-1 || cdFrom() == (iconv_t)-1)
    XMLPlatformUtils::panic (PanicHandler::Panic_NoTransService);
}
#endif /* XML_USE_LIBICONV */

IconvFBSDTransService::~IconvFBSDTransService()
{
#ifdef XML_USE_LIBICONV
    if (cdTo() != (iconv_t) -1) {
        iconv_close (cdTo());
        setCDTo ((iconv_t)-1);
    }
    if (cdFrom() != (iconv_t) -1) {
        iconv_close (cdFrom());
        setCDFrom ((iconv_t)-1);
    }
#endif /* XML_USE_LIBICONV */
}

// ---------------------------------------------------------------------------
//  IconvFBSDTransService: The virtual transcoding service API
// ---------------------------------------------------------------------------
int IconvFBSDTransService::compareIString(const XMLCh* const    comp1
                                        , const XMLCh* const    comp2)
{
    const XMLCh* cptr1 = comp1;
    const XMLCh* cptr2 = comp2;

#ifndef XML_USE_LIBICONV

    while ( (*cptr1 != 0) && (*cptr2 != 0) ) {
        wint_t wch1 = fbsd_towupper(*cptr1);
        wint_t wch2 = fbsd_towupper(*cptr2);
        if (wch1 != wch2)
            break;
        cptr1++;
        cptr2++;
    }
    return (int) ( fbsd_towupper(*cptr1) - fbsd_towupper(*cptr2) );

#else /* XML_USE_LIBICONV */

    XMLCh    c1 = toUpper(*cptr1);
    XMLCh    c2 = toUpper(*cptr2);
    while ( (*cptr1 != 0) && (*cptr2 != 0) ) {
        if (c1 != c2)
            break;
        c1 = toUpper(*(++cptr1));
        c2 = toUpper(*(++cptr2));

    }
    return (int) ( c1 - c2 );

#endif /* !XML_USE_LIBICONV */
}


int IconvFBSDTransService::compareNIString(const XMLCh* const    comp1
                                         , const XMLCh* const    comp2
                                         , const unsigned int    maxChars)
{
    unsigned int  n = 0;
    const XMLCh* cptr1 = comp1;
    const XMLCh* cptr2 = comp2;

#ifndef XML_USE_LIBICONV

    while (true && maxChars)
    {
        wint_t wch1 = fbsd_towupper(*cptr1);
        wint_t wch2 = fbsd_towupper(*cptr2);

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

#else /* XML_USE_LIBICONV */

    while (true && maxChars)
    {
        XMLCh    c1 = toUpper(*cptr1);
        XMLCh    c2 = toUpper(*cptr2);

        if (c1 != c2)
            return (int) (c1 - c2);

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

#endif /* !XML_USE_LIBICONV */

    return 0;
}


const XMLCh* IconvFBSDTransService::getId() const
{
    return gMyServiceId;
}


bool IconvFBSDTransService::isSpace(const XMLCh toCheck) const
{
#ifndef XML_USE_LIBICONV
    if (toCheck <= 0x7F)
        return isspace(toCheck);
    char buf[16];
    wchar_t    wc = wchar_t(toCheck);
    wcstombs( buf, &wc, 16 );
    return (isspace(*buf) != 0);
#else /* XML_USE_LIBICONV */
    return IconvFBSDCD::isSpace(toCheck);
#endif /* !XML_USE_LIBICONV */
}


XMLLCPTranscoder* IconvFBSDTransService::makeNewLCPTranscoder()
{
    // Just allocate a new transcoder of our type
#ifndef XML_USE_LIBICONV
    return new IconvFBSDLCPTranscoder;
#else /* XML_USE_LIBICONV */
    return new IconvFBSDLCPTranscoder (cdFrom(), cdTo(), uChSize(), UBO());
#endif /* !XML_USE_LIBICONV */
}

bool IconvFBSDTransService::supportsSrcOfs() const
{
    return true;
}

// ---------------------------------------------------------------------------
//  IconvFBSDTransService: The protected virtual transcoding service API
// ---------------------------------------------------------------------------
XMLTranscoder*
IconvFBSDTransService::makeNewXMLTranscoder
(
    const    XMLCh* const    encodingName
    ,    XMLTransService::Codes&    resValue
    , const     unsigned int    blockSize
    ,       MemoryManager* const    manager
)
{
#ifndef XML_USE_LIBICONV
    //
    //  NOTE: We don't use the block size here
    //
    //  This is a minimalist transcoding service, that only supports a local
    //  default transcoder. All named encodings return zero as a failure,
    //  which means that only the intrinsic encodings supported by the parser
    //  itself will work for XML data.
    //
    resValue = XMLTransService::UnsupportedEncoding;
    return 0;
#else /* XML_USE_LIBICONV */

    resValue = XMLTransService::UnsupportedEncoding;
    IconvFBSDTranscoder    *newTranscoder = NULL;

    char    *encLocal = XMLString::transcode(encodingName, manager);
    iconv_t    cd_from, cd_to;

    {
        ICONV_LOCK;
        cd_from = iconv_open (fUnicodeCP, encLocal);
        if (cd_from == (iconv_t)-1) {
            resValue = XMLTransService::SupportFilesNotFound;
            if (encLocal)
            manager->deallocate(encLocal);//delete [] encLocal;
            return NULL;
        }
        cd_to = iconv_open (encLocal, fUnicodeCP);
        if (cd_to == (iconv_t)-1) {
            resValue = XMLTransService::SupportFilesNotFound;
            iconv_close (cd_from);
            if (encLocal)
            manager->deallocate(encLocal);//delete [] encLocal;
            return NULL;
        }
        newTranscoder = new (manager) IconvFBSDTranscoder (encodingName,
                             blockSize,
                             cd_from, cd_to,
                             uChSize(), UBO(),
                             manager);
    }
    if (newTranscoder)
        resValue = XMLTransService::Ok;
    if (encLocal)
        manager->deallocate(encLocal);//delete [] encLocal;
    return newTranscoder;

#endif /* !XML_USE_LIBICONV */
}

void IconvFBSDTransService::upperCase(XMLCh* const toUpperCase) const
{
    XMLCh* outPtr = toUpperCase;
    while (*outPtr)
    {
#ifndef XML_USE_LIBICONV
        *outPtr = fbsd_towupper(*outPtr);
#else /* XML_USE_LIBICONV */
        *outPtr = toUpper(*outPtr);
#endif /* !XML_USE_LIBICONV */
        outPtr++;
    }
}

void IconvFBSDTransService::lowerCase(XMLCh* const toLowerCase) const
{
    XMLCh* outPtr = toLowerCase;
    while (*outPtr)
    {
#ifndef XML_USE_LIBICONV
        *outPtr = fbsd_towlower(*outPtr);
#else /* XML_USE_LIBICONV */
        *outPtr = toLower(*outPtr);
#endif /* !XML_USE_LIBICONV */
        outPtr++;
    }
}

// ---------------------------------------------------------------------------
//  IconvFBSDLCPTranscoder: The virtual transcoder API
// ---------------------------------------------------------------------------
unsigned int
IconvFBSDLCPTranscoder::calcRequiredSize (const char* const srcText
                                          , MemoryManager* const manager)
{
    if (!srcText)
        return 0;

#ifndef XML_USE_LIBICONV

    unsigned int retVal = fbsd_mbstowcs(NULL, srcText, 0);
    if (retVal == ~0)
        return 0;
    return retVal;

#else /* XML_USE_LIBICONV */

    size_t      len, srcLen;
    len = srcLen = strlen(srcText);
    if (len == 0)
        return 0;

    char    tmpWideArr[gTempBuffArraySize];
    size_t    totalLen = 0;

    for (;;) {
        char        *pTmpArr = tmpWideArr;
        const char    *ptr = srcText + srcLen - len;
        size_t    rc = iconvFrom(ptr, &len, &pTmpArr, gTempBuffArraySize);
        if (rc == (size_t) -1 && errno != E2BIG) {
            ThrowXMLwithMemMgr(TranscodingException, XMLExcepts::Trans_BadSrcSeq, manager);
            /* return 0; */
        }
        rc = pTmpArr - (char *) tmpWideArr;
        totalLen += rc;
        if (rc == 0 || len == 0)
            break;
    }
    return totalLen / uChSize();

#endif /* XML_USE_LIBICONV */
}


unsigned int
IconvFBSDLCPTranscoder::calcRequiredSize(const XMLCh* const srcText
                                         , MemoryManager* const manager)
{
    if (!srcText)
        return 0;
    unsigned int  wLent = getWideCharLength(srcText);
    if (wLent == 0)
        return 0;

#ifndef XML_USE_LIBICONV

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
        wideCharBuf[i] = srcText[i];
    wideCharBuf[wLent] = 0x00;

    const unsigned int retVal = fbsd_wcstombs(NULL, wideCharBuf, 0);

    if (allocatedArray)
        manager->deallocate(allocatedArray);//delete [] allocatedArray;
    if (retVal == ~0)
        return 0;
    return retVal;

#else /* XML_USE_LIBICONV */

    char    tmpWBuff[gTempBuffArraySize];
    char    *wBuf = 0;
    char    *wBufPtr = 0;
    size_t      len = wLent * uChSize();
    if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER) {
        if (len > gTempBuffArraySize) {
            wBufPtr = (char*) manager->allocate
            (
                len * sizeof(char)
            );//new char[len];
            if (wBufPtr == NULL)
            return 0;
            wBuf = wBufPtr;
        } else
            wBuf = tmpWBuff;
        xmlToMbs (srcText, wLent, wBuf, wLent);
    } else
    wBuf = (char *) srcText;

    char    tmpBuff[gTempBuffArraySize];
    size_t    totalLen = 0;
    char    *srcEnd = wBuf + wLent * uChSize();

    for (;;) {
        char        *pTmpArr = tmpBuff;
        const char    *ptr = srcEnd - len;
        size_t    rc = iconvTo(ptr, &len, &pTmpArr, gTempBuffArraySize);
        if (rc == (size_t) -1 && errno != E2BIG) {
            if (wBufPtr)
            manager->deallocate(wBufPtr);//delete [] wBufPtr;
            ThrowXMLwithMemMgr(TranscodingException, XMLExcepts::Trans_BadSrcSeq, manager);
            /* return 0; */
        }
        rc = pTmpArr - tmpBuff;
        totalLen += rc;
        if (rc == 0 || len == 0)
            break;
    }
    if (wBufPtr)
        manager->deallocate(wBufPtr);//delete [] wBufPtr;
    return totalLen;

#endif /* !XML_USE_LIBICONV */
}


char* IconvFBSDLCPTranscoder::transcode(const XMLCh* const toTranscode)
{
    if (!toTranscode)
        return 0;

    char* retVal = 0;
    if (*toTranscode) {
        unsigned int  wLent = getWideCharLength(toTranscode);

#ifndef XML_USE_LIBICONV

        wchar_t       tmpWideCharArr[gTempBuffArraySize];
        wchar_t*      allocatedArray = 0;
        wchar_t*      wideCharBuf = 0;

        if (wLent >= gTempBuffArraySize)
            wideCharBuf = allocatedArray = new wchar_t[wLent + 1];
        else
            wideCharBuf = tmpWideCharArr;

        for (unsigned int i = 0; i < wLent; i++)
            wideCharBuf[i] = toTranscode[i];
        wideCharBuf[wLent] = 0x00;

        // Calc the needed size.
        const size_t neededLen = fbsd_wcstombs(NULL, wideCharBuf, 0);
        if (neededLen == -1) {
            if (allocatedArray)
                delete [] allocatedArray;
            return 0;
        }

        retVal = new char[neededLen + 1];
        fbsd_wcstombs(retVal, wideCharBuf, neededLen);
        if (allocatedArray)
            delete [] allocatedArray;
        retVal[neededLen] = 0;

#else /* XML_USE_LIBICONV */

        // Calc needed size.
        const size_t neededLen = calcRequiredSize (toTranscode);
        if (neededLen == 0)
            return 0;
        // allocate output buffer
        retVal = new char[neededLen + 1];
        if (retVal == NULL)
            return 0;
        // prepare the original
        char    tmpWBuff[gTempBuffArraySize];
        char    *wideCharBuf = 0;
        char    *wBufPtr = 0;
        size_t  len = wLent * uChSize();

        if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER) {
            if (len > gTempBuffArraySize) {
            wBufPtr = new char[len];
            if (wBufPtr == NULL)
                return 0;
            wideCharBuf = wBufPtr;
            } else
            wideCharBuf = tmpWBuff;
            xmlToMbs (toTranscode, wLent, wideCharBuf, wLent);
        } else
            wideCharBuf = (char *) toTranscode;

        // perform conversion
        wLent *= uChSize();
        char    *ptr = retVal;
        size_t  tmpwLent = wLent;
        size_t  rc = iconvTo(wideCharBuf, &tmpwLent, &ptr, neededLen);
        if (rc == (size_t)-1) {
            if (wBufPtr)
            delete [] wBufPtr;
            return 0;
        }
        if (wBufPtr)
            delete [] wBufPtr;
        retVal[neededLen] = 0;

#endif /* !XML_USE_LIBICONV */

    } else {
        retVal = new char[1];
        if (retVal == NULL)
            return 0;
        retVal[0] = 0;
    }
    return retVal;
}

char* IconvFBSDLCPTranscoder::transcode(const XMLCh* const toTranscode,
                                        MemoryManager* const manager)
{
    if (!toTranscode)
        return 0;

    char* retVal = 0;
    if (*toTranscode) {
        unsigned int  wLent = getWideCharLength(toTranscode);

#ifndef XML_USE_LIBICONV

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
            wideCharBuf[i] = toTranscode[i];
        wideCharBuf[wLent] = 0x00;

        // Calc the needed size.
        const size_t neededLen = fbsd_wcstombs(NULL, wideCharBuf, 0);
        if (neededLen == -1) {
            if (allocatedArray)
                manager->deallocate(allocatedArray);//delete [] allocatedArray;
            return 0;
        }

        retVal = (char*) manager->allocate((neededLen + 1) * sizeof(char));//new char[neededLen + 1];
        fbsd_wcstombs(retVal, wideCharBuf, neededLen);
        if (allocatedArray)
            manager->deallocate(allocatedArray);//delete [] allocatedArray;
        retVal[neededLen] = 0;

#else /* XML_USE_LIBICONV */

        // Calc needed size.
        const size_t neededLen = calcRequiredSize (toTranscode, manager);
        if (neededLen == 0)
            return 0;
        // allocate output buffer
        retVal = (char*) manager->allocate((neededLen + 1) * sizeof(char));//new char[neededLen + 1];
        if (retVal == NULL)
            return 0;
        // prepare the original
        char    tmpWBuff[gTempBuffArraySize];
        char    *wideCharBuf = 0;
        char    *wBufPtr = 0;
        size_t  len = wLent * uChSize();

        if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER) {
            if (len > gTempBuffArraySize) {
            wBufPtr = (char*) manager->allocate(len * sizeof(char));//new char[len];
            if (wBufPtr == NULL) {
                manager->deallocate(retVal);
                return 0;
            }
            wideCharBuf = wBufPtr;
            } else
            wideCharBuf = tmpWBuff;
            xmlToMbs (toTranscode, wLent, wideCharBuf, wLent);
        } else
            wideCharBuf = (char *) toTranscode;

        // perform conversion
        wLent *= uChSize();
        char    *ptr = retVal;
        size_t  tmpwLent = wLent;
        size_t  rc = iconvTo(wideCharBuf, &tmpwLent, &ptr, neededLen);
        if (rc == (size_t)-1) {
            if (wBufPtr)
                manager->deallocate(wBufPtr);//delete [] wBufPtr;
            return 0;
        }
        if (wBufPtr)
            manager->deallocate(wBufPtr);//delete [] wBufPtr;
        retVal[neededLen] = 0;

#endif /* !XML_USE_LIBICONV */

    } else {
        retVal = (char*) manager->allocate(sizeof(char));//new char[1];
        if (retVal == NULL)
            return 0;
        retVal[0] = 0;
    }
    return retVal;
}


bool IconvFBSDLCPTranscoder::transcode( const   XMLCh* const    toTranscode
                    , char* const        toFill
                    , const unsigned int    maxBytes
                    , MemoryManager* const  manager)
{
    // Watch for a couple of pyscho corner cases
    if (!toTranscode || !maxBytes) {
        toFill[0] = 0;
        return true;
    }
    if (!*toTranscode) {
        toFill[0] = 0;
        return true;
    }

    unsigned int  wLent = getWideCharLength(toTranscode);
    if (wLent > maxBytes)
        wLent = maxBytes;

#ifndef XML_USE_LIBICONV

    wchar_t       tmpWideCharArr[gTempBuffArraySize];
    wchar_t*      allocatedArray = 0;
    wchar_t*      wideCharBuf = 0;

    if (maxBytes >= gTempBuffArraySize)
        wideCharBuf = allocatedArray = (wchar_t*) manager->allocate
        (
            (maxBytes + 1) * sizeof(wchar_t)
        );//new wchar_t[maxBytes + 1];
    else
        wideCharBuf = tmpWideCharArr;

    for (unsigned int i = 0; i < wLent; i++)
        wideCharBuf[i] = toTranscode[i];
    wideCharBuf[wLent] = 0x00;

    // Ok, go ahead and try the transcoding. If it fails, then ...
    size_t mblen = fbsd_wcstombs(toFill, wideCharBuf, maxBytes);
    if (mblen == -1) {
        if (allocatedArray)
            manager->deallocate(allocatedArray);//delete [] allocatedArray;
        return false;
    }
    if (allocatedArray)
        manager->deallocate(allocatedArray);//delete [] allocatedArray;

    // Cap it off just in case
    toFill[mblen] = 0;
#else /* XML_USE_LIBICONV */

    // Fill the "unicode" string
    char    tmpWBuff[gTempBuffArraySize];
    char    *wideCharBuf = 0;
    char    *wBufPtr = 0;
    size_t  len = wLent * uChSize();

    if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER) {
        if (len > gTempBuffArraySize) {
            wBufPtr = (char*) manager->allocate
            (
                len * sizeof(char)
            );//new char[len];
            if (wBufPtr == NULL)
                return 0;
            wideCharBuf = wBufPtr;
         }
         else
             wideCharBuf = tmpWBuff;
             xmlToMbs (toTranscode, wLent, wideCharBuf, wLent);
     }
     else
         wideCharBuf = (char *) toTranscode;

    // Ok, go ahead and try the transcoding. If it fails, then ...
    char    *ptr = toFill;
    size_t rc = iconvTo(wideCharBuf, &len, &ptr, maxBytes);
    if (rc == (size_t)-1) {
        if (wBufPtr)
           manager->deallocate(wBufPtr);//delete [] wBufPtr;
        return false;
    }
    if (wBufPtr)
        manager->deallocate(wBufPtr);//delete [] wBufPtr;

    *ptr = 0;
#endif /* !XML_USE_LIBICONV */

    return true;
}



XMLCh* IconvFBSDLCPTranscoder::transcode(const char* const toTranscode)
{
    if (!toTranscode)
        return 0;

    XMLCh* retVal = 0;
    if (*toTranscode) {
        const unsigned int wLent = calcRequiredSize(toTranscode);
        if (wLent == 0) {
            retVal = new XMLCh[1];
            retVal[0] = 0;
            return retVal;
        }

#ifndef XML_USE_LIBICONV

        wchar_t       tmpWideCharArr[gTempBuffArraySize];
        wchar_t*      allocatedArray = 0;
        wchar_t*      wideCharBuf = 0;

        if (wLent >= gTempBuffArraySize)
            wideCharBuf = allocatedArray = new wchar_t[wLent + 1];
        else
            wideCharBuf = tmpWideCharArr;

        fbsd_mbstowcs(wideCharBuf, toTranscode, wLent);
        retVal = new XMLCh[wLent + 1];
        if (retVal == NULL) {
            if (allocatedArray)
                delete [] allocatedArray;
            return NULL;
        }
        for (unsigned int i = 0; i < wLent; i++)
            retVal[i] = (XMLCh) wideCharBuf[i];
        retVal[wLent] = 0x00;
        if (allocatedArray)
            delete [] allocatedArray;

#else /* XML_USE_LIBICONV */

        char    tmpWBuff[gTempBuffArraySize];
        char    *wideCharBuf = 0;
        char    *wBufPtr = 0;
        size_t  len = wLent * uChSize();

        retVal = new XMLCh[wLent + 1];
        if (retVal == NULL)
            return NULL;
        if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER) {
            if (len > gTempBuffArraySize) {
                wBufPtr = new char[len];
                if (wBufPtr == NULL)
                    return 0;
                wideCharBuf = wBufPtr;
            } else
                wideCharBuf = tmpWBuff;
        } else
            wideCharBuf = (char *) retVal;

        size_t    flen = strlen(toTranscode);
        char    *ptr = wideCharBuf;
        size_t    rc = iconvFrom(toTranscode, &flen, &ptr, len);
        if (rc == (size_t) -1) {
            if (wBufPtr)
            delete [] wBufPtr;
            return NULL;
        }
        if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER)
            mbsToXML (wideCharBuf, wLent, retVal, wLent);
        if (wBufPtr)
            delete [] wBufPtr;
        retVal[wLent] = 0x00;

#endif /* !XML_USE_LIBICONV */

    }
    else {
        retVal = new XMLCh[1];
        if (retVal == NULL )
            return 0;
        retVal[0] = 0;
    }
    return retVal;
}

XMLCh* IconvFBSDLCPTranscoder::transcode(const char* const toTranscode,
                                         MemoryManager* const manager)
{
    if (!toTranscode)
        return 0;

    XMLCh* retVal = 0;
    if (*toTranscode) {
        const unsigned int wLent = calcRequiredSize(toTranscode, manager);
        if (wLent == 0) {
            retVal = (XMLCh*) manager->allocate(sizeof(XMLCh));//new XMLCh[1];
            retVal[0] = 0;
            return retVal;
        }

#ifndef XML_USE_LIBICONV

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

        fbsd_mbstowcs(wideCharBuf, toTranscode, wLent);
        retVal = (XMLCh*) manager->allocate((wLent + 1) * sizeof(XMLCh));//new XMLCh[wLent + 1];
        if (retVal == NULL) {
            if (allocatedArray)
                manager->deallocate(allocatedArray);//delete [] allocatedArray;
            return NULL;
        }
        for (unsigned int i = 0; i < wLent; i++)
            retVal[i] = (XMLCh) wideCharBuf[i];
        retVal[wLent] = 0x00;
        if (allocatedArray)
            manager->deallocate(allocatedArray);//delete [] allocatedArray;

#else /* XML_USE_LIBICONV */

        char    tmpWBuff[gTempBuffArraySize];
        char    *wideCharBuf = 0;
        char    *wBufPtr = 0;
        size_t  len = wLent * uChSize();

        retVal = (XMLCh*) manager->allocate((wLent + 1) * sizeof(XMLCh));//new XMLCh[wLent + 1];
        if (retVal == NULL)
            return NULL;
        if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER) {
            if (len > gTempBuffArraySize) {
                wBufPtr = (char*) manager->allocate(len * sizeof(char));//new char[len];
                if (wBufPtr == NULL)
                    return 0;
                wideCharBuf = wBufPtr;
            } else
                wideCharBuf = tmpWBuff;
        } else
            wideCharBuf = (char *) retVal;

        size_t    flen = strlen(toTranscode);
        char    *ptr = wideCharBuf;
        size_t    rc = iconvFrom(toTranscode, &flen, &ptr, len);
        if (rc == (size_t) -1) {
            if (wBufPtr)
            manager->deallocate(wBufPtr);//delete [] wBufPtr;
            return NULL;
        }
        if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER)
            mbsToXML (wideCharBuf, wLent, retVal, wLent);
        if (wBufPtr)
            manager->deallocate(wBufPtr);//delete [] wBufPtr;
        retVal[wLent] = 0x00;

#endif /* !XML_USE_LIBICONV */

    }
    else {
        retVal = (XMLCh*) manager->allocate(sizeof(XMLCh));//new XMLCh[1];
        if (retVal == NULL )
            return 0;
        retVal[0] = 0;
    }
    return retVal;
}


bool IconvFBSDLCPTranscoder::transcode(const   char* const    toTranscode
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

    size_t wLent = calcRequiredSize(toTranscode);
    if (wLent > maxChars)
        wLent = maxChars;

#ifndef XML_USE_LIBICONV

    wchar_t       tmpWideCharArr[gTempBuffArraySize];
    wchar_t*      allocatedArray = 0;
    wchar_t*      wideCharBuf = 0;

    if (maxChars >= gTempBuffArraySize)
        wideCharBuf = allocatedArray = (wchar_t*) manager->allocate
        (
            (wLent + 1) * sizeof(wchar_t)
        );//new wchar_t[wLent + 1];
    else
        wideCharBuf = tmpWideCharArr;

    if (fbsd_mbstowcs(wideCharBuf, toTranscode, wLent) == -1) {
        if (allocatedArray)
            manager->deallocate(allocatedArray);//delete [] allocatedArray;
        return false;
    }
    for (unsigned int i = 0; i < wLent; i++)
        toFill[i] = (XMLCh) wideCharBuf[i];
    if (allocatedArray)
        manager->deallocate(allocatedArray);//delete [] allocatedArray;

#else /* XML_USE_LIBICONV */

    char    tmpWBuff[gTempBuffArraySize];
    char    *wideCharBuf = 0;
    char    *wBufPtr = 0;
    size_t    len = wLent * uChSize();

    if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER) {
    if (len > gTempBuffArraySize) {
        wBufPtr = (char*) manager->allocate
        (
            len * sizeof(char)
        );//new char[len];
        if (wBufPtr == NULL)
        return 0;
        wideCharBuf = wBufPtr;
    } else
        wideCharBuf = tmpWBuff;
    } else
    wideCharBuf = (char *) toFill;

    size_t    flen = strlen(toTranscode); // wLent;
    char    *ptr = wideCharBuf;
    size_t    rc = iconvFrom(toTranscode, &flen, &ptr, len);
    if (rc == (size_t)-1) {
        if (wBufPtr)
            manager->deallocate(wBufPtr);//delete [] wBufPtr;
        return false;
    }

    if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER)
    mbsToXML (wideCharBuf, wLent, toFill, wLent);
    if (wBufPtr)
        manager->deallocate(wBufPtr);//delete [] wBufPtr;

#endif /* !XML_USE_LIBICONV */

    toFill[wLent] = 0x00;
    return true;
}


// ---------------------------------------------------------------------------
//  IconvFBSDLCPTranscoder: Constructors and Destructor
// ---------------------------------------------------------------------------

#ifdef XML_USE_LIBICONV

IconvFBSDLCPTranscoder::IconvFBSDLCPTranscoder (iconv_t        cd_from,
                        iconv_t        cd_to,
                        size_t        uchsize,
                        unsigned int    ubo)
    : IconvFBSDCD (cd_from, cd_to, uchsize, ubo)
{
}

#endif /* XML_USE_LIBICONV */


#ifndef XML_USE_LIBICONV

IconvFBSDLCPTranscoder::IconvFBSDLCPTranscoder()
{
}

#endif /* !XML_USE_LIBICONV */

IconvFBSDLCPTranscoder::~IconvFBSDLCPTranscoder()
{
}


#ifdef XML_USE_LIBICONV

// ---------------------------------------------------------------------------
//  IconvFBSDTranscoder: Constructors and Destructor
// ---------------------------------------------------------------------------
IconvFBSDTranscoder::IconvFBSDTranscoder (const    XMLCh* const    encodingName
                      , const unsigned int    blockSize
                      ,    iconv_t        cd_from
                      ,    iconv_t        cd_to
                      ,    size_t        uchsize
                      ,    unsigned int    ubo
                      , MemoryManager* const manager
    )
    : XMLTranscoder(encodingName, blockSize, manager)
    , IconvFBSDCD (cd_from, cd_to, uchsize, ubo)
{
}

IconvFBSDTranscoder::~IconvFBSDTranscoder()
{
    ICONV_LOCK;
    if (cdTo() != (iconv_t)-1) {
        iconv_close (cdTo());
        setCDTo ((iconv_t)-1);
    }
    if (cdFrom() != (iconv_t)-1) {
        iconv_close (cdFrom());
        setCDFrom ((iconv_t)-1);
    }
}

// ---------------------------------------------------------------------------
//  IconvFBSDTranscoder: Implementation of the virtual transcoder API
// ---------------------------------------------------------------------------
unsigned int    IconvFBSDTranscoder::transcodeFrom
(
    const   XMLByte* const          srcData
    , const unsigned int            srcCount
    ,       XMLCh* const            toFill
    , const unsigned int            maxChars
    ,       unsigned int&           bytesEaten
    ,       unsigned char* const    charSizes )
{
    // Transcode TO XMLCh
    const char*  startSrc = (const char*) srcData;
    const char*  endSrc = (const char*) srcData + srcCount;

    char    tmpWBuff[gTempBuffArraySize];
    char    *startTarget = 0;
    char    *wBufPtr = 0;
    size_t    len = maxChars * uChSize();

    if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER) {
        if (len > gTempBuffArraySize) {
            wBufPtr = (char*) getMemoryManager()->allocate
            (
                len * sizeof(char)
            );//new char[len];
            if (wBufPtr == NULL)
            return 0;
            startTarget = wBufPtr;
        } else
            startTarget = tmpWBuff;
    } else
    startTarget = (char *) toFill;

    // Do character-by-character transcoding
    char    *orgTarget = startTarget;
    size_t    srcLen = srcCount;
    size_t    prevSrcLen = srcLen;
    unsigned int toReturn = 0;
    bytesEaten = 0;
    for (size_t cnt = 0; cnt < maxChars && srcLen; cnt++) {
        size_t    rc = iconvFrom(startSrc, &srcLen, &orgTarget, uChSize());
        if (rc == (size_t)-1) {
            if (errno != E2BIG || prevSrcLen == srcLen) {
                if (wBufPtr)
                    getMemoryManager()->deallocate(wBufPtr);//delete [] wBufPtr;
                ThrowXMLwithMemMgr(TranscodingException, XMLExcepts::Trans_BadSrcSeq, getMemoryManager());
            }
        }
        charSizes[cnt] = prevSrcLen - srcLen;
        prevSrcLen = srcLen;
        bytesEaten += charSizes[cnt];
        startSrc = endSrc - srcLen;
        toReturn++;
    }
    if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER)
        mbsToXML (startTarget, toReturn, toFill, toReturn);
    if (wBufPtr)
        getMemoryManager()->deallocate(wBufPtr);//delete [] wBufPtr;
    return toReturn;
}

unsigned int    IconvFBSDTranscoder::transcodeTo
(
    const   XMLCh* const    srcData
    , const unsigned int    srcCount
    ,       XMLByte* const    toFill
    , const unsigned int    maxBytes
    ,       unsigned int&    charsEaten
    , const UnRepOpts        options )
{
    // Transcode FROM XMLCh
    char    tmpWBuff[gTempBuffArraySize];
    char    *startSrc = tmpWBuff;
    char    *wBufPtr = 0;
    size_t    len = srcCount * uChSize();

    if (uChSize() != sizeof(XMLCh) || UBO() != BYTE_ORDER) {
        if (len > gTempBuffArraySize) {
            wBufPtr = (char*) getMemoryManager()->allocate
            (
                len * sizeof(char)
            );//new char[len];
            if (wBufPtr == NULL)
                return 0;
            startSrc = wBufPtr;
        } else
            startSrc = tmpWBuff;
        xmlToMbs (srcData, srcCount, startSrc, srcCount);
    } else
        startSrc = (char *) srcData;

    char*    startTarget = (char *) toFill;
    size_t    srcLen = len;
    size_t    rc = iconvTo (startSrc, &srcLen, &startTarget, maxBytes);
    if (rc == (size_t)-1 && errno != E2BIG) {
        if (wBufPtr)
            getMemoryManager()->deallocate(wBufPtr);//delete [] wBufPtr;
        ThrowXMLwithMemMgr(TranscodingException, XMLExcepts::Trans_BadSrcSeq, getMemoryManager());
    }
    charsEaten = srcCount - srcLen / uChSize();
    if (wBufPtr)
        getMemoryManager()->deallocate(wBufPtr);//delete [] wBufPtr;
    return startTarget - (char *)toFill;
}

bool        IconvFBSDTranscoder::canTranscodeTo
(
    const unsigned int toCheck
)   const
{
    //
    //  If the passed value is really a surrogate embedded together, then
    //  we need to break it out into its two chars. Else just one.
    //
    char        srcBuf[2 * uChSize()];
    unsigned int    srcCount = 1;
    if (toCheck & 0xFFFF0000) {
        XMLCh    ch1 = (toCheck >> 10) + 0xD800;
        XMLCh    ch2 = toCheck & 0x3FF + 0xDC00;
        xmlToMbs(&ch1, 1, srcBuf, 1);
        xmlToMbs(&ch2, 1, srcBuf + uChSize(), 1);
        srcCount++;
    } else
        xmlToMbs((const XMLCh*) &toCheck, 1, srcBuf, 1);
    size_t    len = srcCount * uChSize();
    char    tmpBuf[64];
    char*    pTmpBuf = tmpBuf;

    size_t    rc = iconvTo( srcBuf, &len, &pTmpBuf, 64);
    return (rc != (size_t)-1) && (len == 0);
}

#endif /* XML_USE_LIBICONV */

XERCES_CPP_NAMESPACE_END
