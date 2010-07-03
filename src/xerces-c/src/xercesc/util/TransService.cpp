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
 * $Id: TransService.cpp 568078 2007-08-21 11:43:25Z amassari $
 */
// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/Janitor.hpp>
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
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/EncodingValidator.hpp>
#include <xercesc/util/XMLRegisterCleanup.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/TransENameMap.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local, static data
//
//  gStrictIANAEncoding
//      A flag to control whether strict IANA encoding names checking should
//      be done
//
// ---------------------------------------------------------------------------
static bool gStrictIANAEncoding = false;
RefHashTableOf<ENameMap>* XMLTransService::gMappings = 0;
RefVectorOf<ENameMap> * XMLTransService::gMappingsRecognizer = 0;
static XMLRegisterCleanup mappingsCleanup;
static XMLRegisterCleanup mappingsRecognizerCleanup;

// -----------------------------------------------------------------------
//  Notification that lazy data has been deleted
// -----------------------------------------------------------------------
void XMLTransService::reinitMappings() {
    delete gMappings;    // The contents of the gMappings hash table are owned by
    gMappings = 0;       //   the it, and so will be deleted by gMapping's destructor.
}

void XMLTransService::reinitMappingsRecognizer() {
    delete XMLTransService::gMappingsRecognizer;
    gMappingsRecognizer = 0;
}

// ---------------------------------------------------------------------------
//  XMLTransService: Constructors and destructor
// ---------------------------------------------------------------------------
XMLTransService::XMLTransService()
{
    if (!gMappings) {
        RefHashTableOf<ENameMap>* t = new RefHashTableOf<ENameMap>(103);

        if (XMLPlatformUtils::compareAndSwap((void **)&gMappings, t, 0) != 0)
        {
            delete t;
        }
        else
        {
            mappingsCleanup.registerCleanup(reinitMappings);
        }
    }

    if (!gMappingsRecognizer) {
        RefVectorOf<ENameMap>* t = new RefVectorOf<ENameMap>(XMLRecognizer::Encodings_Count);

        if (XMLPlatformUtils::compareAndSwap((void **)&gMappingsRecognizer, t, 0) != 0)
        {
            delete t;
        }
        else
        {
            mappingsRecognizerCleanup.registerCleanup(reinitMappingsRecognizer);
        }
    }
}

XMLTransService::~XMLTransService()
{
}

// ---------------------------------------------------------------------------
//	Allow user specific encodings to be added to the mappings table.
//	Should be called after platform init
// ---------------------------------------------------------------------------
void XMLTransService::addEncoding(const XMLCh* const encoding,
								  ENameMap* const ownMapping)
{
    gMappings->put((void *) encoding, ownMapping);
}

// ---------------------------------------------------------------------------
//  XMLTransService: Non-virtual API
// ---------------------------------------------------------------------------
XMLTranscoder*
XMLTransService::makeNewTranscoderFor(  const   char* const             encodingName
                                        ,       XMLTransService::Codes& resValue
                                        , const unsigned int            blockSize
                                        ,       MemoryManager* const    manager)
{
    XMLCh* tmpName = XMLString::transcode(encodingName, manager);
    ArrayJanitor<XMLCh> janName(tmpName, manager);

    return makeNewTranscoderFor(tmpName, resValue, blockSize, manager);
}

XMLTranscoder*
XMLTransService::makeNewTranscoderFor(  const   XMLCh* const            encodingName
                                        ,       XMLTransService::Codes& resValue
                                        , const unsigned int            blockSize
                                        ,       MemoryManager* const    manager)
{
    //
    // If strict IANA encoding flag is set, validate encoding name
    //
    if (gStrictIANAEncoding)
    {
        if (!EncodingValidator::instance()->isValidEncoding(encodingName))
        {
            resValue = XMLTransService::UnsupportedEncoding;
            return 0;
        }
    }

    //
    //  First try to find it in our list of mappings to intrinsically
    //  supported encodings. We have to upper case the passed encoding
    //  name because we use a hash table and we stored all our mappings
    //  in all uppercase.
    //
    const unsigned int bufSize = 2048;
    XMLCh upBuf[bufSize + 1];
    if (!XMLString::copyNString(upBuf, encodingName, bufSize))
    {
        resValue = XMLTransService::InternalFailure;
        return 0;
    }
    XMLString::upperCaseASCII(upBuf);
    ENameMap* ourMapping = gMappings->get(upBuf);

    // If we found it, then call the factory method for it
    if (ourMapping)
	{		
       XMLTranscoder* temp = ourMapping->makeNew(blockSize, manager);
       resValue = temp ? XMLTransService::Ok : XMLTransService::InternalFailure;
       return temp;
    }

    //
    //  It wasn't an intrinsic and it wasn't disallowed, so pass it on
    //  to the trans service to see if he can make anything of it.
    //

    XMLTranscoder* temp =  makeNewXMLTranscoder(encodingName, resValue, blockSize, manager);

    // if successful, set resValue to OK
    // if failed, the makeNewXMLTranscoder has already set the proper failing resValue
    if (temp) resValue =  XMLTransService::Ok;

    return temp;

}


XMLTranscoder*
XMLTransService::makeNewTranscoderFor(  XMLRecognizer::Encodings        encodingEnum
                                        ,       XMLTransService::Codes& resValue
                                        , const unsigned int            blockSize
                                        ,       MemoryManager* const    manager)
{
    //
    // We can only make transcoder if the passed encodingEnum is under this range
    //
    if (encodingEnum < XMLRecognizer::Encodings_Min || encodingEnum > XMLRecognizer::Encodings_Max) {
        resValue = XMLTransService::InternalFailure;
        return 0;
    }

    ENameMap* ourMapping = gMappingsRecognizer->elementAt(encodingEnum);

    // If we found it, then call the factory method for it
    if (ourMapping)	{		
       XMLTranscoder* temp = ourMapping->makeNew(blockSize, manager);
       resValue = temp ? XMLTransService::Ok : XMLTransService::InternalFailure;
       return temp;
    }
    else {
        XMLTranscoder* temp =  makeNewXMLTranscoder(XMLRecognizer::nameForEncoding(encodingEnum, manager), resValue, blockSize, manager);

        // if successful, set resValue to OK
        // if failed, the makeNewXMLTranscoder has already set the proper failing resValue
        if (temp) resValue =  XMLTransService::Ok;

        return temp;
    }

}


// ---------------------------------------------------------------------------
//  XMLTransTransService: Hidden Init Method
//
//  This is called by platform utils during startup.
// ---------------------------------------------------------------------------
void XMLTransService::initTransService()
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

}

// ---------------------------------------------------------------------------
//  XMLTransService: IANA encoding setting
// ---------------------------------------------------------------------------
void XMLTransService::strictIANAEncoding(const bool newState)
{
    gStrictIANAEncoding = newState;
}

bool XMLTransService::isStrictIANAEncoding()
{
    return gStrictIANAEncoding;
}

// ---------------------------------------------------------------------------
//  XMLTranscoder: Public Destructor
// ---------------------------------------------------------------------------
XMLTranscoder::~XMLTranscoder()
{
    fMemoryManager->deallocate(fEncodingName);//delete [] fEncodingName;
}


// ---------------------------------------------------------------------------
//  XMLTranscoder: Hidden Constructors
// ---------------------------------------------------------------------------
XMLTranscoder::XMLTranscoder(const  XMLCh* const    encodingName
                            , const unsigned int    blockSize
                            , MemoryManager* const  manager) :
      fBlockSize(blockSize)
    , fEncodingName(0)    
    , fMemoryManager(manager)
{
    fEncodingName = XMLString::replicate(encodingName, fMemoryManager);
}


// ---------------------------------------------------------------------------
//  XMLTranscoder: Protected helpers
// ---------------------------------------------------------------------------
void XMLTranscoder::checkBlockSize(const unsigned int)
{
//    if (toCheck > fBlockSize)
//        ThrowXML(TranscodingException, XMLExcepts::Trans_BadBlockSize);
}


// ---------------------------------------------------------------------------
//  XMLLCPTranscoder: Public Destructor
// ---------------------------------------------------------------------------
XMLLCPTranscoder::XMLLCPTranscoder()
{
}


// ---------------------------------------------------------------------------
//  XMLLCPTranscoder: Hidden Constructors
// ---------------------------------------------------------------------------
XMLLCPTranscoder::~XMLLCPTranscoder()
{
}

XERCES_CPP_NAMESPACE_END
