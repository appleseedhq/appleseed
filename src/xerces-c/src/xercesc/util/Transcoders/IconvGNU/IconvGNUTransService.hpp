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
 * $Id: IconvGNUTransService.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ICONVGNUTRANSSERVICE_HPP
#define ICONVGNUTRANSSERVICE_HPP

#include <xercesc/util/TransService.hpp>


#include <iconv.h>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Libiconv wrapper (low-level conversion utilities collection)
// ---------------------------------------------------------------------------

class XMLUTIL_EXPORT IconvGNUWrapper
{
public:
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    IconvGNUWrapper
    (
  iconv_t		cd_from,
  iconv_t		cd_to,
  size_t		uchsize,
  unsigned int	ubo
    );
    virtual ~IconvGNUWrapper();

    // Convert "native unicode" character into XMLCh
    void	mbcToXMLCh (const char *mbc, XMLCh *toRet) const;

    // Convert XMLCh into "native unicode" character
    void	xmlChToMbc (XMLCh xch, char *mbc) const;

    // Return uppercase equivalent for XMLCh
    XMLCh 	toUpper (const XMLCh ch) const;

    // Return uppercase equivalent for XMLCh
    XMLCh 	toLower (const XMLCh ch) const;

    // Check if passed characters belongs to the :space: class
    virtual bool isSpace(const XMLCh toCheck) const;

    // Fill array of XMLCh characters with data, supplyed in the array
    // of "native unicode" characters.
    XMLCh*	mbsToXML (
  const char*	mbs_str,
  size_t		mbs_cnt,
  XMLCh*		xml_str,
  size_t		xml_cnt
    ) const;


    // Fill array of "native unicode" characters with data, supplyed
    // in the array of XMLCh characters.
    char*	xmlToMbs
    (
  const XMLCh*	xml_str,
  size_t		xml_cnt,
  char*		mbs_str,
  size_t		mbs_cnt
    ) const;

    // Wrapper aroung the iconv() for transcoding from the local charset
    size_t	iconvFrom
    (
  const char	*fromPtr,
  size_t		*fromLen,
  char		**toPtr,
  size_t		toLen
    ) const;

    // Wrapper aroung the iconv() for transcoding to the local charset
    size_t	iconvTo
    (
  const char	*fromPtr,
  size_t		*fromLen,
  char		**toPtr,
  size_t		toLen
    ) const;

    // Private data accessors
    inline iconv_t	cdTo () const { return fCDTo; }
    inline iconv_t	cdFrom () const { return fCDFrom; }
    inline size_t	uChSize () const { return fUChSize; }
    inline unsigned int	UBO () const { return fUBO; }

protected:

    // Hiden defaull constructor
    IconvGNUWrapper();

    // Private data accessors
    inline void	setCDTo (iconv_t cd) { fCDTo = cd; }
    inline void	setCDFrom (iconv_t cd) { fCDFrom = cd; }
    inline void	setUChSize (size_t sz) { fUChSize = sz; }
    inline void	setUBO (unsigned int u) { fUBO = u; }

private:
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    IconvGNUWrapper(const IconvGNUWrapper&);
    IconvGNUWrapper& operator=(const IconvGNUWrapper&);

    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fCDTo
    //	    Characterset conversion descriptor TO the local-host encoding
    //  fCDFrom
    //	    Characterset conversion descriptor FROM the local-host encoding
    //  fUChSize
    //      Sizeof the "native unicode" character in bytes
    //  fUBO
    //      "Native unicode" characters byte order
    // -----------------------------------------------------------------------
    size_t	fUChSize;
    unsigned int fUBO;
    iconv_t	fCDTo;
    iconv_t	fCDFrom;
};



// ---------------------------------------------------------------------------
//  FreeBSD-specific Transcoding Service implementation
// ---------------------------------------------------------------------------

class XMLUTIL_EXPORT IconvGNUTransService : public XMLTransService, IconvGNUWrapper
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    IconvGNUTransService();
    ~IconvGNUTransService();


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
    virtual void lowerCase(XMLCh* const toUpperCase) const;

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


private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    IconvGNUTransService(const IconvGNUTransService&);
    IconvGNUTransService& operator=(const IconvGNUTransService&);


    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fUnicodeCP
    //      Unicode encoding schema name
    // -----------------------------------------------------------------------
    const char*	fUnicodeCP;

};


//----------------------------------------------------------------------------
// Implementation of the transcoders for arbitrary input characterset is
// supported ONLY through libiconv interface
//----------------------------------------------------------------------------

class XMLUTIL_EXPORT IconvGNUTranscoder : public XMLTranscoder, IconvGNUWrapper
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    IconvGNUTranscoder(const	XMLCh* const	encodingName
  		, const unsigned int	blockSize
  		,	iconv_t		cd_from
  		,	iconv_t		cd_to
  		,	size_t		uchsize
  		,	unsigned int	ubo
        , MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager
    );
    ~IconvGNUTranscoder();


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
        const   XMLCh* const	srcData
        , const unsigned int	srcCount
        ,       XMLByte* const	toFill
        , const unsigned int	maxBytes
        ,       unsigned int&	charsEaten
        , const UnRepOpts	options
    );

    virtual bool canTranscodeTo
    (
        const   unsigned int	toCheck
    )   const;

private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    IconvGNUTranscoder();
    IconvGNUTranscoder(const IconvGNUTranscoder&);
    IconvGNUTranscoder& operator=(const IconvGNUTranscoder&);
};


// ---------------------------------------------------------------------------
//  GNU-specific XMLCh <-> local (host) characterset transcoder
// ---------------------------------------------------------------------------

class XMLUTIL_EXPORT IconvGNULCPTranscoder : public XMLLCPTranscoder, IconvGNUWrapper
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------

    IconvGNULCPTranscoder
    (
  iconv_t		from,
  iconv_t		to,
  size_t		uchsize,
  unsigned int	ubo
    );

protected:
    IconvGNULCPTranscoder();	// Unimplemented

public:

    ~IconvGNULCPTranscoder();


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

    virtual bool transcode
    (
        const   XMLCh* const    toTranscode
        ,       char* const     toFill
        , const unsigned int    maxBytes
        , MemoryManager* const  manager = XMLPlatformUtils::fgMemoryManager
    );

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


private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    IconvGNULCPTranscoder(const IconvGNULCPTranscoder&);
    IconvGNULCPTranscoder& operator=(const IconvGNULCPTranscoder&);
};

XERCES_CPP_NAMESPACE_END

#endif /* ICONVGNUTRANSSERVICE */

