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
 * $Id: Uniconv390TransService.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef UNICONV390TRANSSERVICE_HPP
#define UNICONV390TRANSSERVICE_HPP

#include <xercesc/util/Mutexes.hpp>
#include <xercesc/util/TransService.hpp>
#include <xercesc/util/Transcoders/ICU/ICUTransService.hpp>
#include "uniconv.h"

XERCES_CPP_NAMESPACE_BEGIN

typedef struct uniconvconverter {
   XMLMutex               fMutex;
   uniconv_t                fIconv390DescriptorFrom;
   uniconv_t                fIconv390DescriptorTo;
} uniconvconverter_t;

typedef struct uniconvcaseconverter {
   XMLMutex  fcaseMutex;
   uniconv_t ftoupperhand;
   uniconv_t ftolowerhand;
} uniconvcaseconverter_t;

class XMLUTIL_EXPORT Uniconv390TransService : public XMLTransService
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    Uniconv390TransService();
    ~Uniconv390TransService();


    // -----------------------------------------------------------------------
    //  Implementation of the virtual transcoding service API
    // -----------------------------------------------------------------------
    virtual int compareIString
    (
        const   XMLCh* const    comp1
        , const XMLCh* const    comp2
    );

    virtual int compareNIString
    (
        const   XMLCh* const    comp1
        , const XMLCh* const    comp2
        , const unsigned int    maxChars
    );

    virtual const XMLCh* getId() const;

    virtual bool isSpace(const XMLCh toCheck) const;

    virtual XMLLCPTranscoder* makeNewLCPTranscoder();

    virtual bool supportsSrcOfs() const;

    virtual void upperCase(XMLCh* const toUpperCase) const;
    virtual void lowerCase(XMLCh* const toLowerCase) const;

protected :
    // -----------------------------------------------------------------------
    //  Protected virtual methods
    // -----------------------------------------------------------------------
    virtual XMLTranscoder* makeNewXMLTranscoder
    (
        const   XMLCh* const            encodingName
        ,       XMLTransService::Codes& resValue
        , const unsigned int            blockSize
        ,       MemoryManager* const    manager
    );

    virtual void initTransService();

private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    Uniconv390TransService(const Uniconv390TransService&);
    Uniconv390TransService& operator=(const Uniconv390TransService&);
    ICUTransService * fICUService;
    XMLLCPTranscoder* fLCPTranscoder;
    uniconvcaseconverter_t *fCaseConverter;
};



class XMLUTIL_EXPORT Uniconv390Transcoder : public XMLTranscoder
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    Uniconv390Transcoder
    (
        const   XMLCh* const        encodingName
        ,        uniconvconverter_t* const   toAdopt
        , const unsigned int        blockSize
        , MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager
    );
    ~Uniconv390Transcoder();


    // -----------------------------------------------------------------------
    //  Implementation of the virtual transcoder interface
    // -----------------------------------------------------------------------
    virtual unsigned int transcodeFrom
    (
        const   XMLByte* const          srcData
        , const unsigned int            srcCount
        ,       XMLCh* const            toFill
        , const unsigned int            maxChars
        ,       unsigned int&           bytesEaten
        ,       unsigned char* const    charSizes
    );

    virtual unsigned int transcodeTo
    (
        const   XMLCh* const    srcData
        , const unsigned int    srcCount
        ,       XMLByte* const  toFill
        , const unsigned int    maxBytes
        ,       unsigned int&   charsEaten
        , const UnRepOpts       options
    );

    virtual bool canTranscodeTo
    (
        const   unsigned int    toCheck
    )   const;



private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    Uniconv390Transcoder();
    Uniconv390Transcoder(const Uniconv390Transcoder&);
    Uniconv390Transcoder& operator=(const Uniconv390Transcoder&);

    ICUTranscoder * fICUTranscoder;

    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fConverter
    //      This is a pointer to the converter structure that this transcoder
    //      uses.
    //
    // -----------------------------------------------------------------------
    uniconvconverter_t *fConverter;
};


class XMLUTIL_EXPORT Uniconv390LCPTranscoder : public XMLLCPTranscoder
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    Uniconv390LCPTranscoder( uniconvconverter_t* const toAdopt);
    ~Uniconv390LCPTranscoder();


    // -----------------------------------------------------------------------
    //  Implementation of the virtual transcoder interface
    // -----------------------------------------------------------------------
    virtual unsigned int calcRequiredSize(const char* const srcText
        , MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager);

    virtual unsigned int calcRequiredSize(const XMLCh* const srcText
        , MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager);

    virtual char* transcode(const XMLCh* const toTranscode);
    virtual char* transcode(const XMLCh* const toTranscode,
                            MemoryManager* const manager);

    virtual XMLCh* transcode(const char* const toTranscode);
    virtual XMLCh* transcode(const char* const toTranscode,
                             MemoryManager* const manager);

    virtual bool transcode
    (
        const   char* const     toTranscode
        ,       XMLCh* const    toFill
        , const unsigned int    maxChars
        , MemoryManager* const  manager = XMLPlatformUtils::fgMemoryManager
    );

    virtual bool transcode
    (
        const   XMLCh* const    toTranscode
        ,       char* const     toFill
        , const unsigned int    maxChars
        , MemoryManager* const  manager = XMLPlatformUtils::fgMemoryManager
    );



private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    Uniconv390LCPTranscoder();
    Uniconv390LCPTranscoder(const Uniconv390LCPTranscoder&);
    Uniconv390LCPTranscoder& operator=(const Uniconv390LCPTranscoder&);

    ICULCPTranscoder * fICULCPTranscoder;

    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fConverter
    //      This is a pointer to the converter structure that this transcoder
    //      uses.
    // -----------------------------------------------------------------------
    uniconvconverter_t *fConverter;
};

XERCES_CPP_NAMESPACE_END

#endif
