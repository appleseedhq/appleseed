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

#include "TypeInfo.hpp"

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLException.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/validators/schema/SchemaSymbols.hpp>

#if defined(XERCES_NEW_IOSTREAMS)
#include <iostream>
#else
#include <iostream.h>
#endif


#define DOMTYPEINFOTEST(info, type, uri, line) \
    tmp = XMLString::equals(info->getName(), type) && XMLString::equals(info->getNamespace(), uri);\
    if(!tmp) { \
      XERCES_STD_QUALIFIER cerr << "DOMTypeInfo test failed at line, " << line << "\nExpected values : typename '" << XMLString::transcode((XMLCh*)type) << "', uri '" << XMLString::transcode((XMLCh*)uri); \
      XERCES_STD_QUALIFIER cerr << "'\nActual values   : typename '"; \
      if(info->getName())   \
        XERCES_STD_QUALIFIER cerr << XMLString::transcode(info->getName());  \
      else \
        XERCES_STD_QUALIFIER cerr << "(null)"; \
      XERCES_STD_QUALIFIER cerr << "', uri '"; \
      if(info->getNamespace())   \
        XERCES_STD_QUALIFIER cerr << XMLString::transcode(info->getNamespace()); \
      else \
        XERCES_STD_QUALIFIER cerr << "(null)"; \
      XERCES_STD_QUALIFIER cerr << "'\n" << XERCES_STD_QUALIFIER endl; \
      passed = false; \
    }

bool tmp;

// ---------------------------------------------------------------------------
//  This is a simple class that lets us do easy (though not terribly efficient)
//  trancoding of char* data to XMLCh data.
// ---------------------------------------------------------------------------
class XStr
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    XStr(const char* const toTranscode)
    {
        // Call the private transcoding method
        fUnicodeForm = XMLString::transcode(toTranscode);
    }

    ~XStr()
    {
        XMLString::release(&fUnicodeForm);
    }


    // -----------------------------------------------------------------------
    //  Getter methods
    // -----------------------------------------------------------------------
    const XMLCh* unicodeForm() const
    {
        return fUnicodeForm;
    }

private :
    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fUnicodeForm
    //      This is the Unicode XMLCh format of the string.
    // -----------------------------------------------------------------------
    XMLCh*   fUnicodeForm;
};

#define X(str) XStr(str).unicodeForm()


//  This is a simple class that lets us do easy (though not terribly efficient)
//  trancoding of XMLCh data to local code page for display.
// ---------------------------------------------------------------------------
class StrX
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    StrX(const XMLCh* const toTranscode)
    {
        // Call the private transcoding method
        fLocalForm = XMLString::transcode(toTranscode);
    }

    ~StrX()
    {
        XMLString::release(&fLocalForm);
    }


    // -----------------------------------------------------------------------
    //  Getter methods
    // -----------------------------------------------------------------------
    const char* localForm() const
    {
        return fLocalForm;
    }

private :
    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fLocalForm
    //      This is the local code page form of the string.
    // -----------------------------------------------------------------------
    char*   fLocalForm;
};

#define StrX(str) StrX(str).localForm()

TypeInfo::TypeInfo() {
    try
    {
        XMLPlatformUtils::Initialize();
    }

    catch(const XMLException &toCatch)
    {
        XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n"
             << "  Exception message:"
             << StrX(toCatch.getMessage()) << XERCES_STD_QUALIFIER endl;
    }
    parser = 0;
}

TypeInfo::~TypeInfo() {
    XMLPlatformUtils::Terminate();
}

bool TypeInfo::testInBuiltTypesOnAttributes(bool DTDPresent) {

    bool passed = true;
    DOMElement *testEle = findElement(X("attrTest"));

    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("attrTestType"), X(""), __LINE__);

    DOMAttr *testAttr;

    testAttr = testEle->getAttributeNodeNS(0, X("anySimpleType"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_ANYSIMPLETYPE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("string"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("boolean"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_BOOLEAN, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("decimal"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_DECIMAL, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("float"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_FLOAT, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("double"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_DOUBLE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("duration"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_DURATION, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("dateTime"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_DATETIME, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("time"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_TIME, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("date"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_DATE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("gYearMonth"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_YEARMONTH, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("gYear"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_YEAR, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("gMonthDay"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_MONTHDAY, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("gDay"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_DAY, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("gMonth"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_MONTH, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("hexBinary"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_HEXBINARY, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("base64Binary"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_BASE64BINARY, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("anyURI"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_ANYURI, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("QName"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_QNAME, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);


    testAttr = testEle->getAttributeNodeNS(0, X("normalizedString"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_NORMALIZEDSTRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("token"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_TOKEN, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("language"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_LANGUAGE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("NMTOKEN"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), XMLUni::fgNmTokenString, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("NMTOKENS"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), XMLUni::fgNmTokensString, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("Name"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_NAME, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("NCName"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_NCNAME, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("ID"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), XMLUni::fgIDString, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("IDREF"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), XMLUni::fgIDRefString, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("IDREFS"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), XMLUni::fgIDRefsString, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);


    if(DTDPresent) {
        testAttr = testEle->getAttributeNodeNS(0, X("ENTITY"));
        DOMTYPEINFOTEST(testAttr->getTypeInfo(), XMLUni::fgEntityString, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

        testAttr = testEle->getAttributeNodeNS(0, X("ENTITIES"));
        DOMTYPEINFOTEST(testAttr->getTypeInfo(), XMLUni::fgEntitiesString, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);
    }

    testAttr = testEle->getAttributeNodeNS(0, X("integer"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_INTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("nonPositiveInteger"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_NONPOSITIVEINTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("negativeInteger"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_NEGATIVEINTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("long"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_LONG, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("int"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_INT, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("short"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_SHORT, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("byte"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_BYTE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("nonNegativeInteger"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_NONNEGATIVEINTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("unsignedLong"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_ULONG, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("unsignedInt"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_UINT, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("unsignedShort"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_USHORT, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("unsignedByte"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_UBYTE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("positiveInteger"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_POSITIVEINTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    //couple of defaulted ones
    testAttr = testEle->getAttributeNodeNS(0, X("defaultString"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(0, X("defaultInt"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_INTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    //ns attr
    testAttr = testEle->getAttributeNodeNS(X("http://www.w3.org/2000/xmlns/"), X("prefix"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_ANYURI, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(X("http://www.w3.org/2001/XMLSchema-instance"), X("noNamespaceSchemaLocation"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_ANYURI, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    return passed;
}


bool TypeInfo::testInBuiltTypesOnElements() {

    bool passed = true;
    DOMNode *docEle = doc->getDocumentElement();

    //the eleTest element.
    DOMElement *testEle = findElement(X("eleTest"));

    testEle = (DOMElement *)testEle->getFirstChild()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);
    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_BOOLEAN, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_DECIMAL, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_FLOAT, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_DOUBLE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_DURATION, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_DATETIME, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_TIME, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_DATE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_YEARMONTH, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_YEAR, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_MONTHDAY, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_DAY, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_MONTH, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_HEXBINARY, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_BASE64BINARY, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_ANYURI, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_QNAME, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_NORMALIZEDSTRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_TOKEN, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_LANGUAGE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), XMLUni::fgNmTokenString, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), XMLUni::fgNmTokensString, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_NAME, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_NCNAME, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_INTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_NONPOSITIVEINTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_NEGATIVEINTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_LONG, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_INT, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_SHORT, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_BYTE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_NONNEGATIVEINTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_ULONG, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_UINT, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_USHORT, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_UBYTE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_POSITIVEINTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgATTVAL_ANYTYPE, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    return passed;
}

bool TypeInfo::testSimpleDerived() {

    bool passed = true;
    DOMNode *docEle = doc->getDocumentElement();

    //element simpleDerTest
    DOMElement *testEle = (DOMElement *)docEle->getFirstChild()->getNextSibling()->getNextSibling()
        ->getNextSibling()->getNextSibling()->getNextSibling();

    DOMAttr *testAtt = testEle->getAttributeNodeNS(0, X("decimalDerived"));
    DOMTYPEINFOTEST(testAtt->getTypeInfo(), X("decimalDerivedType"), X(""),  __LINE__);

    testAtt = testEle->getAttributeNodeNS(0, X("stringDerived"));
    DOMTYPEINFOTEST(testAtt->getTypeInfo(), X("stringDerivedType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getFirstChild()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("decimalDerivedType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("stringDerivedType"), X(""), __LINE__);

    return passed;
}

bool TypeInfo::testComplexTypes() {

    bool passed = true;
    DOMNode *docEle = doc->getDocumentElement();

    //element complexTest
    DOMElement *testEle = findElement(X("complexTest"));
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("complexTestType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("complexDerTestType"), X(""), __LINE__);
    return passed;
}


bool TypeInfo::testUnions() {

    bool passed = true;
    DOMNode *docEle = doc->getDocumentElement();
    DOMAttr *testAttr;

    //element unionTest
    DOMElement *testEle = findElement(X("unionTest"));

    testEle = (DOMElement *)testEle->getFirstChild()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("decimalDerivedType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("stringDerivedType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("decimal"), SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("string"), SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    testAttr = testEle->getAttributeNodeNS(0, X("testAttr"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("decimalDerivedType"), X(""), __LINE__);
    testAttr = testEle->getAttributeNodeNS(0, X("testAttr2"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("stringDerivedType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    testAttr = testEle->getAttributeNodeNS(0, X("testAttr"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("stringDerivedType"), X(""), __LINE__);
    testAttr = testEle->getAttributeNodeNS(0, X("testAttr2"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("decimalDerivedType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    testAttr = testEle->getAttributeNodeNS(0, X("testAttr"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("decimal"), SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);
    testAttr = testEle->getAttributeNodeNS(0, X("testAttr2"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("string"), SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    testAttr = testEle->getAttributeNodeNS(0, X("testAttr"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("string"), SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);
    testAttr = testEle->getAttributeNodeNS(0, X("testAttr2"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("decimal"), SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    return passed;

}

bool TypeInfo::testAnonymous() {

    bool passed = true;
    DOMNode *docEle = doc->getDocumentElement();
    DOMAttr *testAttr;


    //element anonymousTest
    DOMElement *testEle = findElement(X("anonymousTest"));
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("__AnonC1"), X(""), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("partNum"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("__AnonS7"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getFirstChild()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("__AnonS2"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("__AnonS4"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("__AnonS6"), X(""), __LINE__);

    return passed;
}


bool TypeInfo::testXsiTypes() {

    bool passed = true;
    DOMNode *docEle = doc->getDocumentElement();

    //element xsiTypeTest
    DOMElement *testEle = findElement(X("xsiTypeTest"));

    testEle = (DOMElement *)testEle->getFirstChild()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("base"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("level1"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("level2"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("baseComplex"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("level1Complex"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("level2Complex"), X(""), __LINE__);

    return passed;
}



bool TypeInfo::testAnys() {

    bool passed = true;
    DOMNode *docEle = doc->getDocumentElement();
    DOMAttr *testAttr;

    //element anyTestPartial
    DOMElement *testEle = findElement(X("anyTestPartial"));
    DOMElement *back = testEle;

    testAttr = testEle->getAttributeNodeNS(X("http://www.w3.org/1999/xhtml"), X("attr2"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), 0, 0, __LINE__);


    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("anyTestPartialType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getFirstChild()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), 0, 0, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), 0, 0, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);


    //element anyTest
    testEle = (DOMElement *)back->getNextSibling()->getNextSibling();
    back = testEle;

    testAttr = testEle->getAttributeNodeNS(X("http://www.secondSchema"), X("attr1"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(X("http://www.secondSchema"), X("attr2"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_INTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(X("http://www.secondSchema"), X("attr3"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(X("http://www.secondSchema"), X("attr4"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_INTEGER, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("anyTestType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getFirstChild()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), 0, 0, __LINE__);

    //anyTestAttr1
    testEle = (DOMElement *)back->getNextSibling()->getNextSibling();
    back = testEle;

    testAttr = testEle->getAttributeNodeNS(X("http://www.secondSchema"), X("attr5"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), 0, 0, __LINE__);

    //anyTestAttr2
    testEle = (DOMElement *)back->getNextSibling()->getNextSibling();
    back = testEle;

    testAttr = testEle->getAttributeNodeNS(X("http://www.secondSchema"), X("attr5"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), 0, 0, __LINE__);

    //anyTestMissing
    testEle = (DOMElement *)back->getNextSibling()->getNextSibling();
    back = testEle;
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("anyTestType"), X(""), __LINE__);
    return passed;
}



bool TypeInfo::testInvaild() {

    bool passed = true;
    DOMNode *docEle = doc->getDocumentElement();
    DOMAttr *testAttr;


    DOMTYPEINFOTEST(((DOMElement *)docEle)->getTypeInfo(), X("rootType"), X(""), __LINE__);

    //element invalidTest
    DOMElement *testEle = findElement(X("invalidTest"));

    testAttr = testEle->getAttributeNodeNS(X(""), X("simple"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("stringDerivedType"), X(""), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("invalid"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), 0, 0, __LINE__);

    testEle = (DOMElement *)testEle->getFirstChild()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("stringDerivedType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("baseComplex"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    // this is a "number" of type "base" but it has a xsi:type="stringDerivedType"
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("stringDerivedType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("baseComplex"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("uType"), X(""), __LINE__);
    //an undeclared element does not have anon value. Test this here
    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), 0, 0, __LINE__);

    DOMElement *testEle2 = (DOMElement *)testEle->getFirstChild()->getNextSibling();
    DOMTYPEINFOTEST(testEle2->getTypeInfo(), 0, 0, __LINE__);
    
    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("attrOnlyType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("attrOnlyType"), X(""), __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("prohibitedAttrType"), X(""), __LINE__);

    return passed;
}


bool TypeInfo::compareDOMTypeInfo(const DOMTypeInfo *info, const XMLCh* type, const XMLCh* uri) {
    return XMLString::equals(info->getName(), type) && XMLString::equals(info->getNamespace(), uri);
}


bool TypeInfo::testDTD() {

    bool passed = true;
    DOMElement *testEle = doc->getDocumentElement();
    DOMAttr *testAttr;

    DOMTYPEINFOTEST(testEle->getTypeInfo(), X(""), X(""), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("cdata"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("CDATA"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("enum"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("ENUMERATION"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("id"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("ID"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("idRef"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("IDREF"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("idRefs"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("IDREFS"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("nmToken"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("NMTOKEN"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("nmTokenDefault"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("NMTOKEN"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("nmTokenDefault2"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("NMTOKEN"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("nmTokens"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("NMTOKENS"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("entity"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("ENTITY"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("entities"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("ENTITIES"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("notation"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("NOTATION"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("noDecl"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("CDATA"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    testAttr = testEle->getAttributeNode(X("xmlns:foo"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), X("CDATA"), X("http://www.w3.org/TR/REC-xml"), __LINE__);

    return passed;

}

bool TypeInfo::combinedTest() {
    bool passed = true;
    DOMNode *docEle = doc->getDocumentElement();
    DOMAttr *testAttr;

    DOMElement *testEle = doc->getDocumentElement();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), X("rootType"), X(""), __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("attBoth"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("attSchema"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testAttr = testEle->getAttributeNodeNS(X(""), X("attDTD"));
    DOMTYPEINFOTEST(testAttr->getTypeInfo(), 0, 0, __LINE__);

    testEle = (DOMElement *)testEle->getFirstChild()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), SchemaSymbols::fgDT_STRING, SchemaSymbols::fgURI_SCHEMAFORSCHEMA, __LINE__);

    testEle = (DOMElement *)testEle->getNextSibling()->getNextSibling();
    DOMTYPEINFOTEST(testEle->getTypeInfo(), 0, 0, __LINE__);

    return passed;
}


DOMElement* TypeInfo::findElement(const XMLCh *name) const {

    DOMNode *toTest = doc->getDocumentElement()->getFirstChild();

    while(!XMLString::equals(toTest->getNodeName(), name)) {
        toTest = toTest->getNextSibling();
    }

    return (DOMElement *)toTest;
}

int main(int argc, char **argv)
{
    bool passed = true;

    TypeInfo ti;

    try {
        //first the combined DTD/Schema scanner
        ti.parser = new XercesDOMParser;
        ti.parser->setValidationScheme(XercesDOMParser::Val_Auto);
        ti.parser->setCreateSchemaInfo(true);
        ti.parser->setDoNamespaces(true);
        ti.parser->setDoSchema(true);
        ti.parser->parse("data/TypeInfo.xml");
        ti.doc = ti.parser->getDocument();
    }
    catch (...) {
        XERCES_STD_QUALIFIER cerr << "parsing data/TypeInfo.xml failed at line" <<  __LINE__ << XERCES_STD_QUALIFIER endl;
        delete ti.parser;
        return false;
    }

    // test only if we got a doc
    if (ti.doc) {
        passed &= ti.testInBuiltTypesOnAttributes(true);
        passed &= ti.testInBuiltTypesOnElements();
        passed &= ti.testSimpleDerived();
        passed &= ti.testComplexTypes();
        passed &= ti.testUnions();
        passed &= ti.testAnonymous();
        passed &= ti.testXsiTypes();
        passed &= ti.testAnys();
        passed &= ti.testInvaild();
    }
    else
        XERCES_STD_QUALIFIER cout << "DOMTypeInfo test at line " << __LINE__ << "was not carried out" << XERCES_STD_QUALIFIER endl;

    delete ti.parser;

    //lets do the same for the just schema scanner
    try {
        ti.parser = new XercesDOMParser;
        ti.parser->setValidationScheme(XercesDOMParser::Val_Auto);
        ti.parser->setCreateSchemaInfo(true);
        ti.parser->setDoNamespaces(true);
        ti.parser->setDoSchema(true);
        ti.parser->useScanner(X("SGXMLScanner"));
        ti.parser->parse("data/TypeInfoNoDTD.xml");
        ti.doc = ti.parser->getDocument();
    }
    catch (...) {
        XERCES_STD_QUALIFIER cerr << "parsing data/TypeInfoNoDTD.xml failed at line" <<  __LINE__ << XERCES_STD_QUALIFIER endl;
        delete ti.parser;
        return false;
    }

    // test only if we got a doc
    if (ti.doc) {
        passed &= ti.testInBuiltTypesOnAttributes(false);
        passed &= ti.testInBuiltTypesOnElements();
        passed &= ti.testSimpleDerived();
        passed &= ti.testComplexTypes();
        passed &= ti.testUnions();
        passed &= ti.testAnonymous();
        passed &= ti.testXsiTypes();
        passed &= ti.testAnys();
        passed &= ti.testInvaild();
    }
    else
        XERCES_STD_QUALIFIER cout << "DOMTypeInfo test at line " << __LINE__ << "was not carried out" << XERCES_STD_QUALIFIER endl;

    delete ti.parser;


    //now default for DTD
    try {
        ti.parser = new XercesDOMParser;
        ti.parser->setValidationScheme(XercesDOMParser::Val_Auto);
        ti.parser->setCreateSchemaInfo(true);
        ti.parser->parse("data/TypeInfoJustDTD.xml");
        ti.doc = ti.parser->getDocument();
    }
    catch (...) {
        XERCES_STD_QUALIFIER cerr << "parsing data/TypeInfoJustDTD.xml failed at line" <<  __LINE__ << XERCES_STD_QUALIFIER endl;
        delete ti.parser;
        return false;
    }

    // test only if we got a doc
    if (ti.doc) {
        passed &= ti.testDTD();
    }
    else
        XERCES_STD_QUALIFIER cout << "DOMTypeInfo test at line " << __LINE__ << "was not carried out" << XERCES_STD_QUALIFIER endl;

    delete ti.parser;


    //and specific scanner
    try {
        ti.parser = new XercesDOMParser;
        ti.parser->setValidationScheme(XercesDOMParser::Val_Auto);
        ti.parser->setCreateSchemaInfo(true);
        ti.parser->useScanner(X("DGXMLScanner"));
        ti.parser->parse("data/TypeInfoJustDTD.xml");
        ti.doc = ti.parser->getDocument();
    }
    catch (...) {
        XERCES_STD_QUALIFIER cerr << "parsing data/TypeInfoJustDTD.xml failed at line" <<  __LINE__ << XERCES_STD_QUALIFIER endl;
        delete ti.parser;
        return false;
    }

    // test only if we got a doc
    if (ti.doc) {
        passed &=  ti.testDTD();
    }
    else
        XERCES_STD_QUALIFIER cout << "DOMTypeInfo test at line " << __LINE__ << "was not carried out" << XERCES_STD_QUALIFIER endl;

    delete ti.parser;

    try {
        ti.parser = new XercesDOMParser;
        ti.parser->setValidationScheme(XercesDOMParser::Val_Auto);
        ti.parser->setCreateSchemaInfo(true);
        ti.parser->setDoNamespaces(true);
        ti.parser->setDoSchema(true);
        ti.parser->parse("data/combined.xml");
        ti.doc = ti.parser->getDocument();
    }
    catch (...) {
        XERCES_STD_QUALIFIER cerr << "parsing data/combined.xml failed at line" <<  __LINE__ << XERCES_STD_QUALIFIER endl;
        delete ti.parser;
        return false;
    }

    // test only if we got a doc
    if (ti.doc) {
        passed &= ti.combinedTest();
    }
    else
        XERCES_STD_QUALIFIER cout << "DOMTypeInfo test at line " << __LINE__ << "was not carried out" << XERCES_STD_QUALIFIER endl;

    delete ti.parser;

    if (!passed) {
        XERCES_STD_QUALIFIER cerr << "test failed" << XERCES_STD_QUALIFIER endl;
        return 4;
    }

    XERCES_STD_QUALIFIER cerr << "Test Run Successfully" << XERCES_STD_QUALIFIER endl;
    return 0;
}
