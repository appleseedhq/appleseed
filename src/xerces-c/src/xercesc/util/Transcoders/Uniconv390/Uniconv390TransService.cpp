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
 * $Id: Uniconv390TransService.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/Janitor.hpp>
#include <xercesc/util/TranscodingException.hpp>
#include <xercesc/util/RefHashTableOf.hpp>
#include <xercesc/util/RefVectorOf.hpp>
#include <xercesc/util/Transcoders/Uniconv390/XML88591Transcoder390.hpp>
#include <xercesc/util/Transcoders/Uniconv390/XMLASCIITranscoder390.hpp>
#include <xercesc/util/XMLChTranscoder.hpp>
#include <xercesc/util/Transcoders/Uniconv390/XMLEBCDICTranscoder390.hpp>
#include <xercesc/util/XMLUCS4Transcoder.hpp>
#include <xercesc/util/Transcoders/Uniconv390/XMLIBM1047Transcoder390.hpp>
#include <xercesc/util/Transcoders/Uniconv390/XMLIBM1140Transcoder390.hpp>
#include <xercesc/util/Transcoders/Uniconv390/XMLUTF8Transcoder390.hpp>
#include <xercesc/util/XMLUTF16Transcoder.hpp>
#include <xercesc/util/Transcoders/Uniconv390/XMLWin1252Transcoder390.hpp>
#include <xercesc/util/TransENameMap.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/Transcoders/ICU/ICUTransService.hpp>
#include "Uniconv390TransService.hpp"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <langinfo.h>
#include <locale.h>
#include <xercesc/util/regx/XMLUniCharacter.hpp>

#include <xercesc/util/XML88591Transcoder.hpp>
#include <xercesc/util/XMLASCIITranscoder.hpp>
#include <xercesc/util/XMLChTranscoder.hpp>
#include <xercesc/util/XMLEBCDICTranscoder.hpp>
#include <xercesc/util/XMLIBM1047Transcoder.hpp>
#include <xercesc/util/XMLIBM1140Transcoder.hpp>
#include <xercesc/util/XMLUCS4Transcoder.hpp>
#include <xercesc/util/XMLUTF8Transcoder.hpp>
#include <xercesc/util/XMLUTF16Transcoder.hpp>
#include <xercesc/util/XMLWin1252Transcoder.hpp>
#include <unicode/uchar.h>

XERCES_CPP_NAMESPACE_BEGIN

// debug printfs.... I'll take these out after function test.
/*
#define DBGPRINTF1(a) {}
#define DBGPRINTF2(a,b) {}
#define DBGPRINTF3(a,b,c) {}
#define DBGPRINTF4(a,b,c,d) {}
#define DBGPRINTF5(a,b,c,d,e) {}
#define DBGPRINTF6(a,b,c,d,e,f) {}
#define DBGPRINTF7(a,b,c,d,e,f,g) {}
*/
#define DBGPRINTF1(a) {if (gViewDebug) printf(a);}
#define DBGPRINTF2(a,b) {if (gViewDebug) printf(a,b);}
#define DBGPRINTF3(a,b,c) {if (gViewDebug) printf(a,b,c);}
#define DBGPRINTF4(a,b,c,d) {if (gViewDebug) printf(a,b,c,d);}
#define DBGPRINTF5(a,b,c,d,e) {if (gViewDebug) printf(a,b,c,d,e);}
#define DBGPRINTF6(a,b,c,d,e,f) {if (gViewDebug) printf(a,b,c,d,e,f);}
#define DBGPRINTF7(a,b,c,d,e,f,g) {if (gViewDebug) printf(a,b,c,d,e,f,g);}

// ---------------------------------------------------------------------------
//  Local, const data
// ---------------------------------------------------------------------------
static const XMLCh gMyServiceId[] =
{
    chLatin_U, chLatin_N, chLatin_I, chLatin_C, chLatin_O, chLatin_N, chLatin_V, chNull
};

// These will hold the environment variable settings.
bool gViewTranscoder;
static bool gViewDebug;
static int gForceTranscode;
#define NO_FORCE 0
#define MUST_USE_ICU  1
#define MUST_USE_UNICONV  2
#define MUST_USE_ICU_SRC_OFFS  3

// ---------------------------------------------------------------------------
//  Local functions
// ---------------------------------------------------------------------------

// This is a local service routine to figure out the number of characters (not bytes)
// in a unicode string.
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

// This is a local service routine to open a transcoder to/from unicode.
static uniconvconverter * addConverter(const char* const EncodingName
                             ,XMLTransService::Codes& resValue)
{
DBGPRINTF1("Add converter\n");
   uniconvconverter *tconv = new uniconvconverter;
   tconv->fIconv390DescriptorFrom = uniconv_open("UCS-2",EncodingName);
   if (tconv->fIconv390DescriptorFrom <= (uniconv_t)(0)) {
DBGPRINTF2("uniconv_open from failed rc=%d\n",(int)tconv->fIconv390DescriptorFrom);
      resValue = XMLTransService::UnsupportedEncoding;
      delete tconv;
      return 0;
   }
   tconv->fIconv390DescriptorTo = uniconv_open(EncodingName,"UCS-2");
   if (tconv->fIconv390DescriptorTo <= (uniconv_t)(0)) {
DBGPRINTF2("uniconv_open to failed rc=%d\n",(int)tconv->fIconv390DescriptorTo);
      resValue = XMLTransService::UnsupportedEncoding;
      uniconv_close(tconv->fIconv390DescriptorFrom);
      delete tconv;
      return 0;
   }
   return tconv;
}

// This is a local service routine to close the transcoders.
static void removeConverter(uniconvconverter* const converter)
{
DBGPRINTF1("remove converter\n");
   if (converter) {
      uniconv_close(converter->fIconv390DescriptorFrom);
      uniconv_close(converter->fIconv390DescriptorTo);
      delete converter;
   }
}

// ***************************************************************************
// ***************************************************************************
// ***************************************************************************
// ***************************************************************************
// *************** Uniconv390TransService Class ******************************
// ***************************************************************************
// ***************************************************************************
// ***************************************************************************
// ***************************************************************************
// ---------------------------------------------------------------------------
//  Uniconv390TransService: Constructor and Destructor
// ---------------------------------------------------------------------------
Uniconv390TransService::Uniconv390TransService()
{

   char * myenviron = getenv("_IXM_FORCE_CONVERSION");
   gForceTranscode = NO_FORCE;
   if ( !strcmp(myenviron,"USE_ICU") )
      gForceTranscode = MUST_USE_ICU;
   else if ( !strcmp(myenviron,"USE_NATIVE") )
      gForceTranscode = MUST_USE_UNICONV;
   else if ( !strcmp(myenviron,"USE_ICU_SRC_OFFS") )
      gForceTranscode = MUST_USE_ICU_SRC_OFFS;
  DBGPRINTF3("FORCE PARM=%s %d\n",myenviron,gForceTranscode);

// If we are forcing ICU to be used for transcoding then we also should
// force it to be used for case conversions.
if ((gForceTranscode == MUST_USE_ICU) ||
    (gForceTranscode == MUST_USE_ICU_SRC_OFFS)) {
   fCaseConverter = new uniconvcaseconverter;
   fCaseConverter->ftoupperhand=UNICONV_ERROR;
   fCaseConverter->ftolowerhand=UNICONV_ERROR;
}
else {
   fCaseConverter = new uniconvcaseconverter;
   fCaseConverter->ftoupperhand=UNICONV_NOHANDLE;
   fCaseConverter->ftolowerhand=UNICONV_NOHANDLE;
}

   if ((gForceTranscode == MUST_USE_ICU) |
       (gForceTranscode == MUST_USE_ICU_SRC_OFFS)) {
         XMLPlatformUtils::fgTransService2 = new ICUTransService;
   }

   gViewTranscoder = false;
   if ( !strcmp(getenv("_IXM_VIEW_CONVERSION"),"YES") )
      gViewTranscoder = true;

   gViewDebug = false;
   if ( !strcmp(getenv("_IXM_DEBUG_CONVERSION"),"YES") )
      gViewDebug = true;
}

Uniconv390TransService::~Uniconv390TransService()
{
   if ( (fCaseConverter->ftoupperhand!=UNICONV_NOHANDLE) &&
        (fCaseConverter->ftoupperhand!=UNICONV_ERROR) ) {
      uniconv_toupper_close(fCaseConverter->ftoupperhand);
      fCaseConverter->ftoupperhand=UNICONV_NOHANDLE;
   }
   if ( (fCaseConverter->ftolowerhand!=UNICONV_NOHANDLE) &&
        (fCaseConverter->ftolowerhand!=UNICONV_ERROR) ) {
      uniconv_tolower_close(fCaseConverter->ftolowerhand);
      fCaseConverter->ftolowerhand=UNICONV_NOHANDLE;
   }
   if (fCaseConverter) {
      delete fCaseConverter;
      fCaseConverter=0;
   }
   if (XMLPlatformUtils::fgTransService2) {
    delete  XMLPlatformUtils::fgTransService2;
   }
}

// ---------------------------------------------------------------------------
//  Uniconv390TransService: The virtual transcoding service API
// ---------------------------------------------------------------------------
int Uniconv390TransService::compareIString(const   XMLCh* const    comp1
                                    , const XMLCh* const    comp2)
{

//char localname1[500];
//XMLString::transcode(comp1,localname1,400);
//char localname2[500];
//XMLString::transcode(comp2,localname2,400);
//DBGPRINTF3("comparing %s %s \n",localname1,localname2);
//printf("toupper handle=%x\n",fCaseConverter->ftoupperhand);

   if (fCaseConverter->ftoupperhand!=UNICONV_ERROR) {
      const XMLCh* psz1 = comp1;
      const XMLCh* psz2 = comp2;
      XMLCh tmp1;
      XMLCh tmp2;

      XMLMutexLock lockcaser(&fCaseConverter->fcaseMutex);
      if (fCaseConverter->ftoupperhand==UNICONV_NOHANDLE) {
         fCaseConverter->ftoupperhand=uniconv_toupper_open();
      }

      unsigned int curCount = 0;
      while (fCaseConverter->ftoupperhand!=UNICONV_ERROR)
      {
         tmp1 = uniconv_caseit(fCaseConverter->ftoupperhand,*psz1);
         if (errno==0)
            tmp2 = uniconv_caseit(fCaseConverter->ftoupperhand,*psz2);
         if (errno) {
            uniconv_toupper_close(fCaseConverter->ftoupperhand);
            fCaseConverter->ftoupperhand=UNICONV_ERROR;
            break;
         }

         //
         //  If an inequality, then return the difference.
         //
         if (tmp1 != tmp2)
            return int(*psz1) - int(*psz2);

         // If either has ended, then they both ended, so equal
         if (!*psz1 || !*psz2)
            break;

         // Move upwards for the next round
         psz1++;
         psz2++;
      }
   }
   // check if unicode services does not support upper casing again, then call ICU.
   if (fCaseConverter->ftoupperhand == UNICONV_ERROR) {
      if (!XMLPlatformUtils::fgTransService2) {
         XMLMutexLock lockConverter(&fCaseConverter->fcaseMutex);
         if (!XMLPlatformUtils::fgTransService2) {
            XMLPlatformUtils::fgTransService2 = new ICUTransService;
         }
      }
      return XMLPlatformUtils::fgTransService2->compareIString(comp1,comp2);
   }
   return 0;
}


int Uniconv390TransService::compareNIString(const  XMLCh* const    comp1
                                    , const XMLCh* const    comp2
                                    , const unsigned int    maxChars)
{
//char localname1[500];
//XMLString::transcode(comp1,localname1,400);
//char localname2[500];
//XMLString::transcode(comp2,localname2,400);
//DBGPRINTF3("comparing NI %s %s \n",localname1,localname2);
//printf("toupper handle=%x\n",fCaseConverter->ftoupperhand);
//printf("!!!***comparing NI %s %s\n",localname1,localname2);

   if (fCaseConverter->ftoupperhand!=UNICONV_ERROR) {
      const XMLCh* psz1 = comp1;
      const XMLCh* psz2 = comp2;
      XMLCh tmp1;
      XMLCh tmp2;

      XMLMutexLock lockcaser(&fCaseConverter->fcaseMutex);
      if (fCaseConverter->ftoupperhand==UNICONV_NOHANDLE) {
         fCaseConverter->ftoupperhand=uniconv_toupper_open();
      }

      unsigned int curCount = 0;
      while (fCaseConverter->ftoupperhand!=UNICONV_ERROR) {
         tmp1 = uniconv_caseit(fCaseConverter->ftoupperhand,*psz1);
         if (errno==0)
            tmp2 = uniconv_caseit(fCaseConverter->ftoupperhand,*psz2);
         if (errno) {
            uniconv_toupper_close(fCaseConverter->ftoupperhand);
            fCaseConverter->ftoupperhand=UNICONV_ERROR;
            break;
         }

         //
         //  If an inequality, then return the difference.
         //
         if (tmp1 != tmp2)
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
   }
   // check if unicode services does not support upper casing, then call ICU.
   if (fCaseConverter->ftoupperhand == UNICONV_ERROR) {
      if (!XMLPlatformUtils::fgTransService2) {
         XMLMutexLock lockConverter(&fCaseConverter->fcaseMutex);
         if (!XMLPlatformUtils::fgTransService2) {
            XMLPlatformUtils::fgTransService2 = new ICUTransService;
         }
      }
      return XMLPlatformUtils::fgTransService2->compareNIString(comp1,comp2,maxChars);
   }
   return 0;
}


const XMLCh* Uniconv390TransService::getId() const
{
   return gMyServiceId;
}

bool Uniconv390TransService::isSpace(const XMLCh toCheck) const
{
	/* copied from ICU */
    if ((toCheck == 0x09)
    ||  (toCheck == 0x0A)
    ||  (toCheck == 0x0D))
    {
        return true;
    }
    return (u_isspace(UChar(toCheck)) != 0);
}

bool Uniconv390TransService::supportsSrcOfs() const
{
   if (gForceTranscode == MUST_USE_ICU_SRC_OFFS)
      return true;
    else
      return false;
}

void Uniconv390TransService::upperCase(XMLCh* const toUpperCase) const
{

//char localname1[500];
//XMLString::transcode(toUpperCase,localname1,400);
//DBGPRINTF2("upper casing %s \n",localname1);
//printf("toupper handle=%x\n",fCaseConverter->ftoupperhand);

   if (fCaseConverter->ftoupperhand!=UNICONV_ERROR) {
      XMLCh* outPtr = toUpperCase;

      XMLMutexLock lockcaser(&fCaseConverter->fcaseMutex);
      if (fCaseConverter->ftoupperhand==UNICONV_NOHANDLE) {
         fCaseConverter->ftoupperhand=uniconv_toupper_open();
      }

      unsigned int curCount = 0;
      while ((fCaseConverter->ftoupperhand!=UNICONV_ERROR) && (*outPtr) ) {
         XMLCh tmp = uniconv_caseit(fCaseConverter->ftoupperhand,*outPtr);
         if (errno) {
            uniconv_toupper_close(fCaseConverter->ftoupperhand);
            fCaseConverter->ftoupperhand=UNICONV_ERROR;
            break;
         }
         *outPtr = tmp;
         outPtr++;
      }
   }
   if (fCaseConverter->ftoupperhand==UNICONV_ERROR) {
     if (!XMLPlatformUtils::fgTransService2) {
          XMLMutexLock lockConverter(&fCaseConverter->fcaseMutex);
          if (!XMLPlatformUtils::fgTransService2) {
             XMLPlatformUtils::fgTransService2 = new ICUTransService;
          }
     }
     return XMLPlatformUtils::fgTransService2->upperCase(toUpperCase);
   }
}

void Uniconv390TransService::lowerCase(XMLCh* const toLowerCase) const
{
//char localname1[500];
//XMLString::transcode(toLowerCase,localname1,400);
//DBGPRINTF2("lower casing %s \n",localname1);
//printf("tolower handle=%x\n",fCaseConverter->ftolowerhand);
//printf("!!!***Lower casing function called: %s \n",localname1);



   if (fCaseConverter->ftolowerhand!=UNICONV_ERROR) {
      XMLCh* outPtr = toLowerCase;

      XMLMutexLock lockcaser(&fCaseConverter->fcaseMutex);
      if (fCaseConverter->ftolowerhand==UNICONV_NOHANDLE) {
         fCaseConverter->ftolowerhand=uniconv_tolower_open();
      }

      unsigned int curCount = 0;
      while ((fCaseConverter->ftolowerhand!=UNICONV_ERROR) && (*outPtr) ) {
         XMLCh tmp = uniconv_caseit(fCaseConverter->ftolowerhand,*outPtr);
         if (errno) {
            uniconv_tolower_close(fCaseConverter->ftolowerhand);
            fCaseConverter->ftolowerhand=UNICONV_ERROR;
            break;
         }
         *outPtr = tmp;
         outPtr++;
      }
   }
   if (fCaseConverter->ftolowerhand==UNICONV_ERROR) {
     if (!XMLPlatformUtils::fgTransService2) {
        XMLMutexLock lockConverter(&fCaseConverter->fcaseMutex);
        if (!XMLPlatformUtils::fgTransService2) {
           XMLPlatformUtils::fgTransService2 = new ICUTransService;
        }
     }
     return XMLPlatformUtils::fgTransService2->lowerCase(toLowerCase);
   }
}

XMLLCPTranscoder* Uniconv390TransService::makeNewLCPTranscoder()
{
   XMLTransService::Codes resValue;
DBGPRINTF2("makeNewLCPTranscoder() localencoding=%s \n",nl_langinfo(CODESET));
   // USS default code page is IBM-1047
   if ((gForceTranscode == MUST_USE_ICU) ||
       (gForceTranscode == MUST_USE_ICU_SRC_OFFS)) {
      if (gViewTranscoder)
         printf("IXM1004I LCP - Using ICU - %s\n",nl_langinfo(CODESET));
      if (!XMLPlatformUtils::fgTransService2) {
         XMLMutexLock lockConverter(&fCaseConverter->fcaseMutex);
         if (!XMLPlatformUtils::fgTransService2) {
            XMLPlatformUtils::fgTransService2 = new ICUTransService;
         }
      }
      fLCPTranscoder = XMLPlatformUtils::fgTransService2->makeNewLCPTranscoder();
   } else {
      char codepage[32];
      sprintf(codepage,"%s-s390", nl_langinfo(CODESET));
      uniconvconverter *tconv=addConverter(codepage,resValue);
      DBGPRINTF3("gForce=%d,tconv=%x\n",gForceTranscode,(int)tconv);

      if (tconv) {
         if (gViewTranscoder)
            printf("IXM1005I LCP - Using Unicode Services - %s\n",nl_langinfo(CODESET));
         fLCPTranscoder = new Uniconv390LCPTranscoder(tconv);
      } else {
         if (gForceTranscode != MUST_USE_UNICONV) {
            if (gViewTranscoder)
               printf("IXM1006I LCP - Using ICU - %s\n",nl_langinfo(CODESET));
            if (!XMLPlatformUtils::fgTransService2) {
               XMLMutexLock lockConverter(&fCaseConverter->fcaseMutex);
               if (!XMLPlatformUtils::fgTransService2) {
                  XMLPlatformUtils::fgTransService2 = new ICUTransService;
               }
            }
            fLCPTranscoder = XMLPlatformUtils::fgTransService2->makeNewLCPTranscoder();
         }
      }
   }
   return fLCPTranscoder;
}

// ---------------------------------------------------------------------------
//  Uniconv390TransService: The protected virtual transcoding service API
// ---------------------------------------------------------------------------
XMLTranscoder* Uniconv390TransService::
makeNewXMLTranscoder(const  XMLCh* const            encodingName
                    ,       XMLTransService::Codes& resValue
                    , const unsigned int            blockSize
                    ,       MemoryManager* const    manager)
{
char * localname = XMLString::transcode(encodingName, manager);
ArrayJanitor<char> janText((char*)localname, manager);
DBGPRINTF3("makeNewXMLTranscoder() encoding=%s blocksize=%d\n",localname,blockSize);

   if ((gForceTranscode == MUST_USE_ICU) ||
       (gForceTranscode == MUST_USE_ICU_SRC_OFFS)) {
      if (gViewTranscoder)
         printf("IXM1001I XML - Using ICU - %s\n",localname);
      if (!XMLPlatformUtils::fgTransService2) {
         XMLMutexLock lockConverter(&fCaseConverter->fcaseMutex);
         if (!XMLPlatformUtils::fgTransService2) {
            XMLPlatformUtils::fgTransService2 = new ICUTransService;
         }
      }
      return XMLPlatformUtils::fgTransService2->makeNewXMLTranscoder(encodingName,resValue,blockSize, manager);
   }

   uniconvconverter *tconv=addConverter(localname,resValue);

   if (tconv == 0) {
      DBGPRINTF1("uniconv failed!!!!!!!!\n");
      if (gForceTranscode == MUST_USE_UNICONV)
         return 0;
      else {
         if (gViewTranscoder)
            printf("IXM1002I XML - Using ICU - %s\n",localname);
         if (!XMLPlatformUtils::fgTransService2) {
            XMLMutexLock lockConverter(&fCaseConverter->fcaseMutex);
            if (!XMLPlatformUtils::fgTransService2) {
               XMLPlatformUtils::fgTransService2 = new ICUTransService;
            }
         }
         return XMLPlatformUtils::fgTransService2->makeNewXMLTranscoder(encodingName,resValue,blockSize, manager);
      }
   }

   if (gViewTranscoder)
      printf("IXM1003I XML - Using Unicode Services - %s\n",localname);
   return new (manager) Uniconv390Transcoder(encodingName, tconv, blockSize, manager);
}
// ---------------------------------------------------------------------------
//  Uniconv390TransService: Hidden Init Method
//
//  This is called by OS390 platform utils during startup.
// ---------------------------------------------------------------------------
void Uniconv390TransService::initTransService()
{
    // Need to first check if the translate instruction is supported
    // by this processor ... if it is use the new intrinsics.

    if (((*(int*) 200) & 0x00001000) != 0)
       {
       //
       //  A stupid way to increment the fCurCount inside the RefVectorOf
       //
       for (unsigned int i = 0; i < XMLRecognizer::Encodings_Count; i++)
           gMappingsRecognizer->addElement(0);

       //
       //  Add in the magical mapping for the native XMLCh transcoder. This
       //  is used for internal entities.
       //
       gMappingsRecognizer->setElementAt(new ENameMapFor<XMLChTranscoder>(XMLUni::fgXMLChEncodingString), XMLRecognizer::XERCES_XMLCH);
       gMappings->put((void*)XMLUni::fgXMLChEncodingString, new ENameMapFor<XMLChTranscoder>(XMLUni::fgXMLChEncodingString));

       //
       //  Add in our mappings for ASCII.
       //
       gMappingsRecognizer->setElementAt(new ENameMapFor<XMLASCIITranscoder390>(XMLUni::fgUSASCIIEncodingString), XMLRecognizer::US_ASCII);
       gMappings->put((void*)XMLUni::fgUSASCIIEncodingString, new ENameMapFor<XMLASCIITranscoder390>(XMLUni::fgUSASCIIEncodingString));
       gMappings->put((void*)XMLUni::fgUSASCIIEncodingString2, new ENameMapFor<XMLASCIITranscoder390>(XMLUni::fgUSASCIIEncodingString2));
       gMappings->put((void*)XMLUni::fgUSASCIIEncodingString3, new ENameMapFor<XMLASCIITranscoder390>(XMLUni::fgUSASCIIEncodingString3));
       gMappings->put((void*)XMLUni::fgUSASCIIEncodingString4, new ENameMapFor<XMLASCIITranscoder390>(XMLUni::fgUSASCIIEncodingString4));


       //
       //  Add in our mappings for UTF-8
       //
       gMappingsRecognizer->setElementAt(new ENameMapFor<XMLUTF8Transcoder390>(XMLUni::fgUTF8EncodingString), XMLRecognizer::UTF_8);
       gMappings->put((void*)XMLUni::fgUTF8EncodingString, new ENameMapFor<XMLUTF8Transcoder390>(XMLUni::fgUTF8EncodingString));
       gMappings->put((void*)XMLUni::fgUTF8EncodingString2, new ENameMapFor<XMLUTF8Transcoder390>(XMLUni::fgUTF8EncodingString2));

       //
       //  Add in our mappings for Latin1
       //
       gMappings->put((void*)XMLUni::fgISO88591EncodingString, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString2, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString2));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString3, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString3));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString4, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString4));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString5, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString5));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString6, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString6));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString7, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString7));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString8, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString8));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString9, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString9));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString10, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString10));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString11, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString11));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString12, new ENameMapFor<XML88591Transcoder390>(XMLUni::fgISO88591EncodingString12));

       //
       //  Add in our mappings for UTF-16 and UCS-4, little endian
       //
       bool swapped = false;

       #if defined(ENDIANMODE_BIG)
       swapped = true;
       #endif
       gMappingsRecognizer->setElementAt(new EEndianNameMapFor<XMLUTF16Transcoder>(XMLUni::fgUTF16LEncodingString, swapped), XMLRecognizer::UTF_16L);
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16LEncodingString,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16LEncodingString
               , swapped
           )
       );

       gMappings->put
       (
   		(void*)XMLUni::fgUTF16LEncodingString2,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16LEncodingString2
               , swapped
           )
       );

       gMappingsRecognizer->setElementAt(new EEndianNameMapFor<XMLUCS4Transcoder>(XMLUni::fgUCS4LEncodingString, swapped), XMLRecognizer::UCS_4L);
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4LEncodingString,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4LEncodingString
               , swapped
           )
       );

       gMappings->put
       (
   		(void*)XMLUni::fgUCS4LEncodingString2,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4LEncodingString2
               , swapped
           )
       );

       //
       //  Add in our mappings for UTF-16 and UCS-4, big endian
       //
       swapped = false;
       #if defined(ENDIANMODE_LITTLE)
       swapped = true;
       #endif
       gMappingsRecognizer->setElementAt(new EEndianNameMapFor<XMLUTF16Transcoder>(XMLUni::fgUTF16BEncodingString, swapped), XMLRecognizer::UTF_16B);
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16BEncodingString,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16BEncodingString
               , swapped
           )
       );

       gMappings->put
       (
   		(void*)XMLUni::fgUTF16BEncodingString2,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16BEncodingString2
               , swapped
           )
       );

       gMappingsRecognizer->setElementAt(new EEndianNameMapFor<XMLUCS4Transcoder>(XMLUni::fgUCS4BEncodingString, swapped), XMLRecognizer::UCS_4B);
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4BEncodingString,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4BEncodingString
               , swapped
           )
       );

       gMappings->put
       (
   		(void*)XMLUni::fgUCS4BEncodingString2,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4BEncodingString2
               , swapped
           )
       );

       //
       //  Add in our mappings for UTF-16 and UCS-4 which does not indicate endian
       //  assumes the same endian encoding as the OS
       //

       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString2,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString2
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString3,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString3
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString4,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString4
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString5,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString5
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString6,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString6
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString7,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString7
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4EncodingString,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4EncodingString
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4EncodingString2,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4EncodingString2
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4EncodingString3,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4EncodingString3
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4EncodingString4,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4EncodingString4
               , false
           )
       );

       //
       //  Add in our mappings for IBM037, and the one alias we support for
       //  it, which is EBCDIC-CP-US.
       //
       gMappingsRecognizer->setElementAt(new ENameMapFor<XMLEBCDICTranscoder390>(XMLUni::fgEBCDICEncodingString), XMLRecognizer::EBCDIC);
       gMappings->put((void*)XMLUni::fgIBM037EncodingString, new ENameMapFor<XMLEBCDICTranscoder390>(XMLUni::fgIBM037EncodingString));
       gMappings->put((void*)XMLUni::fgIBM037EncodingString2, new ENameMapFor<XMLEBCDICTranscoder390>(XMLUni::fgIBM037EncodingString2));


       //hhe
       gMappings->put((void*)XMLUni::fgIBM1047EncodingString, new ENameMapFor<XMLIBM1047Transcoder390>(XMLUni::fgIBM1047EncodingString));
       gMappings->put((void*)XMLUni::fgIBM1047EncodingString2, new ENameMapFor<XMLIBM1047Transcoder390>(XMLUni::fgIBM1047EncodingString2));

       //
       //  Add in our mappings for IBM037 with Euro update, i.e. IBM1140. It
       //  has alias IBM01140, the one suggested by IANA
       //
       gMappings->put((void*)XMLUni::fgIBM1140EncodingString, new ENameMapFor<XMLIBM1140Transcoder390>(XMLUni::fgIBM1140EncodingString));
       gMappings->put((void*)XMLUni::fgIBM1140EncodingString2, new ENameMapFor<XMLIBM1140Transcoder390>(XMLUni::fgIBM1140EncodingString2));
       gMappings->put((void*)XMLUni::fgIBM1140EncodingString3, new ENameMapFor<XMLIBM1140Transcoder390>(XMLUni::fgIBM1140EncodingString3));
       gMappings->put((void*)XMLUni::fgIBM1140EncodingString4, new ENameMapFor<XMLIBM1140Transcoder390>(XMLUni::fgIBM1140EncodingString4));

       //
       //  Add in our mappings for Windows-1252. We don't have any aliases for
       //  this one, so there is just one mapping.
       //
       gMappings->put((void*)XMLUni::fgWin1252EncodingString, new ENameMapFor<XMLWin1252Transcoder390>(XMLUni::fgWin1252EncodingString));
    } // use new intrinsics
    else // use old intrinsics
    {
       //
       //  A stupid way to increment the fCurCount inside the RefVectorOf
       //
       for (unsigned int i = 0; i < XMLRecognizer::Encodings_Count; i++)
           gMappingsRecognizer->addElement(0);

       //
       //  Add in the magical mapping for the native XMLCh transcoder. This
       //  is used for internal entities.
       //
       gMappingsRecognizer->setElementAt(new ENameMapFor<XMLChTranscoder>(XMLUni::fgXMLChEncodingString), XMLRecognizer::XERCES_XMLCH);
       gMappings->put((void*)XMLUni::fgXMLChEncodingString, new ENameMapFor<XMLChTranscoder>(XMLUni::fgXMLChEncodingString));

       //
       //  Add in our mappings for ASCII.
       //
       gMappingsRecognizer->setElementAt(new ENameMapFor<XMLASCIITranscoder>(XMLUni::fgUSASCIIEncodingString), XMLRecognizer::US_ASCII);
       gMappings->put((void*)XMLUni::fgUSASCIIEncodingString, new ENameMapFor<XMLASCIITranscoder>(XMLUni::fgUSASCIIEncodingString));
       gMappings->put((void*)XMLUni::fgUSASCIIEncodingString2, new ENameMapFor<XMLASCIITranscoder>(XMLUni::fgUSASCIIEncodingString2));
       gMappings->put((void*)XMLUni::fgUSASCIIEncodingString3, new ENameMapFor<XMLASCIITranscoder>(XMLUni::fgUSASCIIEncodingString3));
       gMappings->put((void*)XMLUni::fgUSASCIIEncodingString4, new ENameMapFor<XMLASCIITranscoder>(XMLUni::fgUSASCIIEncodingString4));


       //
       //  Add in our mappings for UTF-8
       //
       gMappingsRecognizer->setElementAt(new ENameMapFor<XMLUTF8Transcoder>(XMLUni::fgUTF8EncodingString), XMLRecognizer::UTF_8);
       gMappings->put((void*)XMLUni::fgUTF8EncodingString, new ENameMapFor<XMLUTF8Transcoder>(XMLUni::fgUTF8EncodingString));
       gMappings->put((void*)XMLUni::fgUTF8EncodingString2, new ENameMapFor<XMLUTF8Transcoder>(XMLUni::fgUTF8EncodingString2));

       //
       //  Add in our mappings for Latin1
       //
       gMappings->put((void*)XMLUni::fgISO88591EncodingString, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString2, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString2));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString3, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString3));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString4, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString4));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString5, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString5));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString6, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString6));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString7, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString7));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString8, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString8));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString9, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString9));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString10, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString10));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString11, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString11));
       gMappings->put((void*)XMLUni::fgISO88591EncodingString12, new ENameMapFor<XML88591Transcoder>(XMLUni::fgISO88591EncodingString12));

       //
       //  Add in our mappings for UTF-16 and UCS-4, little endian
       //
       bool swapped = false;

       #if defined(ENDIANMODE_BIG)
       swapped = true;
       #endif
       gMappingsRecognizer->setElementAt(new EEndianNameMapFor<XMLUTF16Transcoder>(XMLUni::fgUTF16LEncodingString, swapped), XMLRecognizer::UTF_16L);
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16LEncodingString,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16LEncodingString
               , swapped
           )
       );

       gMappings->put
       (
   		(void*)XMLUni::fgUTF16LEncodingString2,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16LEncodingString2
               , swapped
           )
       );

       gMappingsRecognizer->setElementAt(new EEndianNameMapFor<XMLUCS4Transcoder>(XMLUni::fgUCS4LEncodingString, swapped), XMLRecognizer::UCS_4L);
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4LEncodingString,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4LEncodingString
               , swapped
           )
       );

       gMappings->put
       (
   		(void*)XMLUni::fgUCS4LEncodingString2,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4LEncodingString2
               , swapped
           )
       );

       //
       //  Add in our mappings for UTF-16 and UCS-4, big endian
       //
       swapped = false;
       #if defined(ENDIANMODE_LITTLE)
       swapped = true;
       #endif
       gMappingsRecognizer->setElementAt(new EEndianNameMapFor<XMLUTF16Transcoder>(XMLUni::fgUTF16BEncodingString, swapped), XMLRecognizer::UTF_16B);
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16BEncodingString,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16BEncodingString
               , swapped
           )
       );

       gMappings->put
       (
   		(void*)XMLUni::fgUTF16BEncodingString2,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16BEncodingString2
               , swapped
           )
       );

       gMappingsRecognizer->setElementAt(new EEndianNameMapFor<XMLUCS4Transcoder>(XMLUni::fgUCS4BEncodingString, swapped), XMLRecognizer::UCS_4B);
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4BEncodingString,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4BEncodingString
               , swapped
           )
       );

       gMappings->put
       (
   		(void*)XMLUni::fgUCS4BEncodingString2,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4BEncodingString2
               , swapped
           )
       );

       //
       //  Add in our mappings for UTF-16 and UCS-4 which does not indicate endian
       //  assumes the same endian encoding as the OS
       //

       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString2,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString2
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString3,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString3
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString4,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString4
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString5,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString5
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString6,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString6
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUTF16EncodingString7,
           new EEndianNameMapFor<XMLUTF16Transcoder>
           (
               XMLUni::fgUTF16EncodingString7
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4EncodingString,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4EncodingString
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4EncodingString2,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4EncodingString2
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4EncodingString3,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4EncodingString3
               , false
           )
       );
       gMappings->put
       (
   		(void*)XMLUni::fgUCS4EncodingString4,
           new EEndianNameMapFor<XMLUCS4Transcoder>
           (
               XMLUni::fgUCS4EncodingString4
               , false
           )
       );

       //
       //  Add in our mappings for IBM037, and the one alias we support for
       //  it, which is EBCDIC-CP-US.
       //
       gMappingsRecognizer->setElementAt(new ENameMapFor<XMLEBCDICTranscoder>(XMLUni::fgEBCDICEncodingString), XMLRecognizer::EBCDIC);
       gMappings->put((void*)XMLUni::fgIBM037EncodingString, new ENameMapFor<XMLEBCDICTranscoder>(XMLUni::fgIBM037EncodingString));
       gMappings->put((void*)XMLUni::fgIBM037EncodingString2, new ENameMapFor<XMLEBCDICTranscoder>(XMLUni::fgIBM037EncodingString2));


       //hhe
       gMappings->put((void*)XMLUni::fgIBM1047EncodingString, new ENameMapFor<XMLIBM1047Transcoder>(XMLUni::fgIBM1047EncodingString));
       gMappings->put((void*)XMLUni::fgIBM1047EncodingString2, new ENameMapFor<XMLIBM1047Transcoder>(XMLUni::fgIBM1047EncodingString2));

       //
       //  Add in our mappings for IBM037 with Euro update, i.e. IBM1140. It
       //  has alias IBM01140, the one suggested by IANA
       //
       gMappings->put((void*)XMLUni::fgIBM1140EncodingString, new ENameMapFor<XMLIBM1140Transcoder>(XMLUni::fgIBM1140EncodingString));
       gMappings->put((void*)XMLUni::fgIBM1140EncodingString2, new ENameMapFor<XMLIBM1140Transcoder>(XMLUni::fgIBM1140EncodingString2));
       gMappings->put((void*)XMLUni::fgIBM1140EncodingString3, new ENameMapFor<XMLIBM1140Transcoder>(XMLUni::fgIBM1140EncodingString3));
       gMappings->put((void*)XMLUni::fgIBM1140EncodingString4, new ENameMapFor<XMLIBM1140Transcoder>(XMLUni::fgIBM1140EncodingString4));

       //
       //  Add in our mappings for Windows-1252. We don't have any aliases for
       //  this one, so there is just one mapping.
       //
       gMappings->put((void*)XMLUni::fgWin1252EncodingString, new ENameMapFor<XMLWin1252Transcoder>(XMLUni::fgWin1252EncodingString));
    } // use old intrinsics

}//end initTransService()


// ***************************************************************************
// ***************************************************************************
// ***************************************************************************
// ***************************************************************************
// *************** Uniconv390Transcoder Class ********************************
// ***************************************************************************
// ***************************************************************************
// ***************************************************************************
// ***************************************************************************

// ---------------------------------------------------------------------------
//  Uniconv390Transcoder: Constructors and Destructor
// ---------------------------------------------------------------------------
Uniconv390Transcoder::Uniconv390Transcoder(const  XMLCh* const        encodingName
                            ,        uniconvconverter_t * const   toAdopt
                            , const unsigned int        blockSize
                            , MemoryManager* const manager) :
    XMLTranscoder(encodingName, blockSize, manager)
    , fConverter(toAdopt)
{
}

Uniconv390Transcoder::~Uniconv390Transcoder()
{
   // If there is a converter, clean it up
   if (fConverter) {
      removeConverter(fConverter);
      fConverter=0;
   }
}


// ---------------------------------------------------------------------------
//  Uniconv390Transcoder: The virtual transcoder API
// ---------------------------------------------------------------------------
// ignore  charSizes since that is used to generate character offsets.
unsigned int
Uniconv390Transcoder::transcodeFrom(const  XMLByte* const          srcData
                            , const unsigned int            srcCount
                            ,       XMLCh* const            toFill
                            , const unsigned int            maxChars
                            ,       unsigned int&           bytesEaten
                            ,       unsigned char* const    charSizes)
{
   unsigned int countIn = 0;
   unsigned int countOut = 0;
DBGPRINTF2("Uniconv390Transcoder::transcodeFrom bytes=%d\n",srcCount);
   int retCode;
   char *tmpInPtr = (char *) srcData;
   char *tmpOutPtr = (char *) toFill;
   unsigned int inByteLeft = srcCount;
   unsigned int outByteLeft = maxChars*sizeof(XMLCh);
   { // locking scope
      XMLMutexLock lockConverter(&fConverter->fMutex);
      retCode = uniconv(fConverter->fIconv390DescriptorFrom, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
   }
DBGPRINTF5("Uniconv390Transcoder::transcodeFrom iconv finished, rc=%d inleft=%d outleft=%d errno=%d \n",retCode,inByteLeft,outByteLeft,errno);
   if ( (retCode < 0 ) && (errno != E2BIG) ) {
      return 0;
   }

   // Give back the counts of eaten and transcoded
   bytesEaten = srcCount-inByteLeft;
   return maxChars-outByteLeft/sizeof(XMLCh);
}

// The returned int is really supposed to be the number of bytes, not chars!
unsigned int
Uniconv390Transcoder::transcodeTo( const   XMLCh* const    srcData
                            , const unsigned int    srcCount
                            ,       XMLByte* const  toFill
                            , const unsigned int    maxBytes
                            ,       unsigned int&   charsEaten
                            , const UnRepOpts       options)
{
   unsigned int countIn = 0;
   unsigned int countOut = 0;
DBGPRINTF2("Uniconv390Transcoder::transcodeTo bytes=%d\n",srcCount);

   int retCode;
   char *tmpInPtr = (char *) srcData;
   char *tmpOutPtr = (char *) toFill;
   unsigned int inByteLeft = srcCount*sizeof(XMLCh);
   unsigned int outByteLeft = maxBytes;
   { // locking scope
      XMLMutexLock lockConverter(&fConverter->fMutex);
      retCode = uniconv(fConverter->fIconv390DescriptorTo, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
   }
   if ( (retCode < 0) && (errno != E2BIG) ) {
      return 0;
   }

   // Give back the counts of eaten and transcoded
   charsEaten = srcCount-inByteLeft/sizeof(XMLCh);
   return maxBytes-outByteLeft;
}


bool Uniconv390Transcoder::canTranscodeTo(const unsigned int toCheck) const
{
   int retCode;
DBGPRINTF1("Uniconv390Transcoder::canTranscodeTo\n");
//printf("!!!***Uniconv390Transcoder::canTranscodeTo\n");
   //
   //  If the passed value is really a surrogate embedded together, then
   //  we need to break it out into its two chars. Else just one. While
   //  we are ate it, convert them to UChar format if required.
   //
   XMLCh          srcBuf[2];
   unsigned int    srcCount = 1;
   if (toCheck & 0xFFFF0000) {
      srcBuf[0] = XMLCh((toCheck >> 10) + 0xD800);
      srcBuf[1] = XMLCh(toCheck & 0x3FF) + 0xDC00;
      srcCount++;
   } else {
      srcBuf[0] = XMLCh(toCheck);
   }

   // Set up a temp buffer to format into. Make it more than big enough
   char  tmpBuf[16];

   char *tmpInPtr = (char *) srcBuf;
   char *tmpOutPtr = (char *) tmpBuf;
   unsigned int inByteLeft = srcCount*sizeof(XMLCh);
   unsigned int outByteLeft = 16;

   { // locking scope
      XMLMutexLock lockConverter(&fConverter->fMutex);
      retCode = uniconv(fConverter->fIconv390DescriptorTo, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
   }
   if ( (retCode < 0) && (errno != E2BIG) ) {
      return false;
   }

   return true;
}


// ***************************************************************************
// ***************************************************************************
// ***************************************************************************
// ***************************************************************************
// *************** Uniconv390LCPTranscoder Class *****************************
// ***************************************************************************
// ***************************************************************************
// ***************************************************************************
// ***************************************************************************


// ---------------------------------------------------------------------------
//  Uniconv390LCPTranscoder: Constructor and Destructor
// ---------------------------------------------------------------------------
Uniconv390LCPTranscoder::Uniconv390LCPTranscoder( uniconvconverter_t* const toAdopt) :

    fConverter(toAdopt)
{
}

Uniconv390LCPTranscoder::~Uniconv390LCPTranscoder()
{
    // If there is a converter, clean it up
   if (fConverter) {
      removeConverter(fConverter);
      fConverter=0;
   }
}


// ---------------------------------------------------------------------------
//  Uniconv390LCPTranscoder: calcRequiredSize
//
// The only way I can find to reliably determine the exact required size is to actually
// transcode the string and see how long it is. Fortunately, this is only done as a last
// ditch effort so it should only be used very rarely (if at all).
// ---------------------------------------------------------------------------
unsigned int Uniconv390LCPTranscoder::calcRequiredSize(const XMLCh* const srcText
                                                       , MemoryManager* const manager)
{
DBGPRINTF1("Uniconv390LCPTranscoder::calcRequiredSize(const XMLCh* const srcText)  \n");
//printf("!!!***Uniconv390LCPTranscoder::calcRequiredSize(const XMLCh* const srcText)  \n");
   int thesize=0;
   if (!srcText)
      return 0;
   if (!*srcText)
      return 0;

   char * result = transcode(srcText, manager);
   if (result) {
      thesize = strlen(result);
      manager->deallocate(result);//delete [] result;
   }
   return thesize;
}

unsigned int Uniconv390LCPTranscoder::calcRequiredSize(const char* const srcText
                                                       , MemoryManager* const manager)
{
DBGPRINTF1("Uniconv390LCPTranscoder::calcRequiredSize(const char* const srcText)  \n");
//printf("!!!***Uniconv390LCPTranscoder::calcRequiredSize(const char* const srcText)  \n");
   int thesize=0;
   if (!srcText)
      return 0;
   if (!*srcText)
      return 0;

   XMLCh * result = transcode(srcText, manager);
   if (result) {
      thesize = getWideCharLength(result);
      manager->deallocate(result);//delete [] result;
   }

DBGPRINTF2("Uniconv390LCPTranscoder::calcRequiredSize(const char* const srcText) %d  \n",thesize);
   return thesize;
}


// ---------------------------------------------------------------------------
//  Uniconv390LCPTranscoder: transcode
//
// Now what follows are various methods to transcode to/from unicode.
// ---------------------------------------------------------------------------
char* Uniconv390LCPTranscoder::transcode(const XMLCh* const toTranscode)
{
//printf("Uniconv390LCPTranscoder::transcode(const XMLCh* const toTranscode) ");
//printf("transcode handle=%x\n",fConverter->fIconv390DescriptorTo);
   if (!toTranscode)
      return 0;

   char* retVal = 0;
    // find out the length of the source and use this as an estimate for the needed buffer length.
   unsigned int  wLent = getWideCharLength(toTranscode);
   if (wLent == 0) {
      retVal = new char[1];
      retVal[0] = 0;
      return retVal;
   }
   retVal = new char[wLent * 2 + 1]; // get double just to be sure.
   while (true) {
      int retCode;
      char *tmpInPtr = (char*) toTranscode;
      char *tmpOutPtr = (char*) retVal;
      unsigned int inByteLeft = wLent*sizeof(XMLCh);
      unsigned int outByteLeft = wLent*sizeof(XMLCh);
//printf("!!!transcode len=%d\n",wLent);

      { // Locking scope
         XMLMutexLock lockConverter(&fConverter->fMutex);
         retCode = uniconv(fConverter->fIconv390DescriptorTo, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
      }
//printf("!!!transcode uniconv finished rc=%d errno=%d\n",retCode,errno);
      // If the data does not fit into our estimation of the buffer size, then delete the buffer,
      // double the estimated length and try again.
      if ( ((retCode < 0) && (errno == E2BIG)) || (outByteLeft == 0) ) {
//printf("!!!Uniconv390LCPTranscoder::transcode(const XMLCh* const toTranscode):Retrying with a bigger buffer.......\n");
         delete [] retVal;
         wLent*=2;
         retVal = new char[wLent*sizeof(XMLCh) + 1];
      }
      // If uniconv doesn't complete for any other reason, then return failure.
      else if (retCode < 0) {
         return 0;
      }
      // it was successful so break out of the loop
      else {
         *tmpOutPtr = 0x00;
         break;
      }
   }
//printf("Uniconv390LCPTranscoder::transcode(const XMLCh* const toTranscode):%s\n",retVal);
   return retVal;
}

char* Uniconv390LCPTranscoder::transcode(const XMLCh* const toTranscode,
                                         MemoryManager* const manager)
{
//printf("Uniconv390LCPTranscoder::transcode(const XMLCh* const toTranscode) ");
//printf("transcode handle=%x\n",fConverter->fIconv390DescriptorTo);
   if (!toTranscode)
      return 0;

   char* retVal = 0;
    // find out the length of the source and use this as an estimate for the needed buffer length.
   unsigned int  wLent = getWideCharLength(toTranscode);
   if (wLent == 0) {
      retVal = (char*) manager->allocate(sizeof(char));//new char[1];
      retVal[0] = 0;
      return retVal;
   }
   retVal = (char*) manager->allocate((wLent * 2 + 1) * sizeof(char));//new char[wLent * 2 + 1]; // get double just to be sure.
   while (true) {
      int retCode;
      char *tmpInPtr = (char*) toTranscode;
      char *tmpOutPtr = (char*) retVal;
      unsigned int inByteLeft = wLent*sizeof(XMLCh);
      unsigned int outByteLeft = wLent*sizeof(XMLCh);
//printf("!!!transcode len=%d\n",wLent);

      { // Locking scope
         XMLMutexLock lockConverter(&fConverter->fMutex);
         retCode = uniconv(fConverter->fIconv390DescriptorTo, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
      }
//printf("!!!transcode uniconv finished rc=%d errno=%d\n",retCode,errno);
      // If the data does not fit into our estimation of the buffer size, then delete the buffer,
      // double the estimated length and try again.
      if ( ((retCode < 0) && (errno == E2BIG)) || (outByteLeft == 0) ) {
//printf("!!!Uniconv390LCPTranscoder::transcode(const XMLCh* const toTranscode):Retrying with a bigger buffer.......\n");
         manager->deallocate(retVal);//delete [] retVal;
         wLent*=2;
         retVal = (char*) manager->allocate
         (
             (wLent*sizeof(XMLCh) + 1) * sizeof(char)
         );//new char[wLent*sizeof(XMLCh) + 1];
      }
      // If uniconv doesn't complete for any other reason, then return failure.
      else if (retCode < 0) {
         return 0;
      }
      // it was successful so break out of the loop
      else {
         *tmpOutPtr = 0x00;
         break;
      }
   }
//printf("Uniconv390LCPTranscoder::transcode(const XMLCh* const toTranscode):%s\n",retVal);
   return retVal;
}

XMLCh* Uniconv390LCPTranscoder::transcode(const char* const toTranscode)
{
DBGPRINTF2("Uniconv390LCPTranscoder::transcode(const char* const toTranscode):%s \n",toTranscode);
//printf("transcode handle=%x\n",fConverter->fIconv390DescriptorFrom);
   if (!toTranscode)
      return 0;

   XMLCh* retVal = 0;
   const unsigned int len = strlen(toTranscode);
   retVal = new XMLCh[len + 1]; // +1 is for the null terminator!
   if (len == 0) {
      retVal[0] = 0;
      return retVal;
   }

   int retCode;
   char *tmpInPtr = (char*) toTranscode;
   char *tmpOutPtr = (char*) retVal;
   unsigned int inByteLeft = len;
   unsigned int outByteLeft = len*sizeof(XMLCh);
   { // locking scope
      XMLMutexLock lockConverter(&fConverter->fMutex);
      retCode = uniconv(fConverter->fIconv390DescriptorFrom, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
   }
   // Because we check the length in the beginning, and we make sure the output buffer
   // is big enough, uniconv should complete the transcoding. If it doesn't for any reason, then
   // return failure.
   if (retCode < 0) {
      delete [] retVal;
      return 0;
   }
   *tmpOutPtr = 0x00;
   *(tmpOutPtr+1) = 0x00;
   return retVal;
}

XMLCh* Uniconv390LCPTranscoder::transcode(const char* const toTranscode,
                                          MemoryManager* const manager)
{
DBGPRINTF2("Uniconv390LCPTranscoder::transcode(const char* const toTranscode):%s \n",toTranscode);
//printf("transcode handle=%x\n",fConverter->fIconv390DescriptorFrom);
   if (!toTranscode)
      return 0;

   XMLCh* retVal = 0;
   const unsigned int len = strlen(toTranscode);
   retVal = (XMLCh*) manager->allocate((len + 1) * sizeof(XMLCh));//new XMLCh[len + 1]; // +1 is for the null terminator!
   if (len == 0) {
      retVal[0] = 0;
      return retVal;
   }

   int retCode;
   char *tmpInPtr = (char*) toTranscode;
   char *tmpOutPtr = (char*) retVal;
   unsigned int inByteLeft = len;
   unsigned int outByteLeft = len*sizeof(XMLCh);
   { // locking scope
      XMLMutexLock lockConverter(&fConverter->fMutex);
      retCode = uniconv(fConverter->fIconv390DescriptorFrom, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
   }
   // Because we check the length in the beginning, and we make sure the output buffer
   // is big enough, uniconv should complete the transcoding. If it doesn't for any reason, then
   // return failure.
   if (retCode < 0) {
      manager->deallocate(retVal);//delete [] retVal;
      return 0;
   }
   *tmpOutPtr = 0x00;
   *(tmpOutPtr+1) = 0x00;
   return retVal;
}


bool Uniconv390LCPTranscoder::transcode(const  char* const     toTranscode
                                ,       XMLCh* const    toFill
                                , const unsigned int    maxChars
                                , MemoryManager* const  manager)
{
DBGPRINTF1("Uniconv390LCPTranscoder::transcode(const  char* const     toTranscode, etc.... \n");
//printf("transcode handle=%x\n",fConverter->fIconv390DescriptorFrom);
    // Check for a couple of psycho corner cases
   if (!toTranscode || !maxChars) {
      toFill[0] = 0;
      return true;
   }

   unsigned int  Lent = strlen(toTranscode);
   if (Lent == 0) {
      toFill[0] = 0;
      return true;
   }

   int retCode;
   char *tmpInPtr = (char*) toTranscode;
   char *tmpOutPtr = (char*) toFill;
   unsigned int inByteLeft = Lent;
   unsigned int outByteLeft = maxChars*2;
   { // locking scope
      XMLMutexLock lockConverter(&fConverter->fMutex);
      retCode = uniconv(fConverter->fIconv390DescriptorFrom, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
   }
   // Because we check the length in the beginning, and the caller makes sure that the output buffer
   // is big enough, uniconv should complete the transcoding. If it doesn't for any reason, then
   // return failure.
   if (retCode < 0) {
      return false;
   }
   *tmpOutPtr = 0x00;
   *(tmpOutPtr+1) = 0x00;
   return true;
}


bool Uniconv390LCPTranscoder::transcode(   const   XMLCh* const    toTranscode
                                    ,       char* const     toFill
                                    , const unsigned int    maxBytes
                                    , MemoryManager* const  manager)
{
DBGPRINTF1("Uniconv390LCPTranscoder::transcode(const  XMLCh* const     toTranscode, etc.... \n");
//printf("transcode handle=%x\n",fConverter->fIconv390DescriptorTo);
   // Watch for a couple of pyscho corner cases
   if (!toTranscode || !maxBytes) {
      toFill[0] = 0;
      return true;
   }
   //-------------------
   unsigned int  wLent = getWideCharLength(toTranscode);
   if (wLent == 0) {
      toFill[0] = 0;
      return true;
   }
   int retCode;
   char *tmpInPtr = (char*) toTranscode;
   char *tmpOutPtr = (char*) toFill;
   unsigned int inByteLeft = wLent*sizeof(XMLCh);
   unsigned int outByteLeft = maxBytes;
   { // locking scope
      XMLMutexLock lockConverter(&fConverter->fMutex);
      retCode = uniconv(fConverter->fIconv390DescriptorTo, &tmpInPtr, &inByteLeft, &tmpOutPtr, &outByteLeft);
   }

   // Because we check the length in the beginning, and the caller makes sure that the output buffer
   // is big enough, uniconv should complete the transcoding. If it doesn't for any reason, then
   // return failure.
   if (retCode < 0) {
      return false;
   }
   *tmpOutPtr = 0x00;
   return true;
}

XERCES_CPP_NAMESPACE_END
