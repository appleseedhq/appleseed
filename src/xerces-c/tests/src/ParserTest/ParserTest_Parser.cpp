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
 * $Id: ParserTest_Parser.cpp 470088 2006-11-01 20:35:12Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/RefVectorOf.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/internal/XMLReader.hpp>
#include <xercesc/internal/XMLScanner.hpp>
#include <xercesc/framework/XMLAttr.hpp>
#include <xercesc/framework/XMLNotationDecl.hpp>
#include <xercesc/framework/XMLValidator.hpp>
#include <xercesc/validators/DTD/DTDAttDef.hpp>
#include <xercesc/validators/DTD/DTDElementDecl.hpp>
#include <xercesc/validators/DTD/DTDEntityDecl.hpp>
#include "ParserTest.hpp"

#include <stdlib.h>
#include <ctype.h>



// ---------------------------------------------------------------------------
//  Local functions
// ---------------------------------------------------------------------------
static int attrComp(const void* elemOne, const void* elemTwo)
{
    return XMLString::compareString
    (
        (*(XMLAttr**)elemOne)->getName()
        , (*(XMLAttr**)elemTwo)->getName()
    );
}


// ---------------------------------------------------------------------------
//  TestParser: Constructors and Destructor
// ---------------------------------------------------------------------------
TestParser::TestParser() :

    fDoNamespaces(false)
    , fInExtSubset(false)
    , fInsideRoot(false)
    , fIntDTDOutput(false)
    , fNestingLevel(0)
    , fOutputType(OutputType_None)
    , fShowErrLoc(false)
    , fShowWarnings(false)
    , fSurrogate(0)
{
}

TestParser::~TestParser()
{
}


// ---------------------------------------------------------------------------
//  TestParser: The document handler interfaces
// ---------------------------------------------------------------------------
void TestParser::docCharacters( const   XMLCh* const    chars
                                , const unsigned int    length
                                , const bool            cdataSection)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got CHARS:\n    Bytes: "
             << length << ", CDATA?: "
             << (cdataSection ? "Yes" : "No")
             << "\n"
             << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if ((fOutputType == OutputType_JCCanon)
          ||  (fOutputType == OutputType_SunCanon))
    {
        showChars(chars, length);
    }
     else if (fOutputType == OutputType_XML)
    {
        if (cdataSection)
            XERCES_STD_QUALIFIER cout << "<![CDATA[";
        showChars(chars, length);
        if (cdataSection)
            XERCES_STD_QUALIFIER cout << "]]>";
    }
}

void TestParser::docComment(const XMLCh* const comment)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got document COMMENT:\n    "
             << "Text: \"" << StrX(comment) << "\"\n"
             << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType == OutputType_XML)
    {
        XERCES_STD_QUALIFIER cout << "<!--";
        showString(comment);
        XERCES_STD_QUALIFIER cout << "-->";
    }
}

void TestParser::docPI( const   XMLCh* const    target
                        , const XMLCh* const    data)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got document PI:\n     "
             << "Target: \"" << target << '"';

        if (XMLString::stringLen(data))
            XERCES_STD_QUALIFIER cout << ", Data: \"" << StrX(data) << "\"\n";

        XERCES_STD_QUALIFIER cout << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if ((fOutputType == OutputType_XML)
          ||  (fOutputType == OutputType_JCCanon)
          ||  (fOutputType == OutputType_SunCanon))
    {
        XERCES_STD_QUALIFIER cout << "<?";
        showString(target);
        XERCES_STD_QUALIFIER cout << " ";
        if (XMLString::stringLen(data))
            XERCES_STD_QUALIFIER cout << StrX(data);
        XERCES_STD_QUALIFIER cout << "?>";
    }
}


void TestParser::endDocument()
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got ENDDOCUMENT:\n"
             << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType == OutputType_SunCanon)
    {
        XERCES_STD_QUALIFIER cout << "\r\n";
    }
}

void TestParser::endElement(const   XMLElementDecl& elemDecl
                            , const unsigned int    uriId
                            , const bool            isRoot)
{
    if (fOutputType == OutputType_Debug)
    {
        if (fDoNamespaces)
        {
            XMLBuffer bufURI;
            fScanner->getURIText(uriId, bufURI);

            XERCES_STD_QUALIFIER cout << "Got ENDELEMENT:\n    Name: "
                 << "{" << StrX(bufURI.getRawBuffer()) << "}"
                 << StrX(elemDecl.getBaseName())
                 << XERCES_STD_QUALIFIER endl;
        }
         else
        {
            XERCES_STD_QUALIFIER cout << "Got ENDELEMENT:\n    Name: "
                 << StrX(elemDecl.getFullName()) << XERCES_STD_QUALIFIER endl;
        }

        XERCES_STD_QUALIFIER cout << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if ((fOutputType == OutputType_XML)
          ||  (fOutputType == OutputType_JCCanon)
          ||  (fOutputType == OutputType_SunCanon))
    {
        XERCES_STD_QUALIFIER cout << "</";
        showString(elemDecl.getFullName());
        XERCES_STD_QUALIFIER cout << ">";
    }

    // Clear the flag that says we're now inside the root
    if (isRoot)
        fInsideRoot = false;
}

void TestParser::endEntityReference(const XMLEntityDecl& entDecl)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got ENDENTITYREF:\n    "
             << "Name: " << StrX(entDecl.getName()) << "\n" << XERCES_STD_QUALIFIER endl;
    }
}

void TestParser::ignorableWhitespace(const  XMLCh* const    chars
                                    , const unsigned int    length
                                    , const bool            cdataSection)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got WHITESPACE:\n    Bytes: "
             << length << ", CDATA?: "
             << (cdataSection ? "Yes" : "No")
             << "\n"
             << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType == OutputType_XML)
    {
        if (cdataSection)
            XERCES_STD_QUALIFIER cout << "<![CDATA[";
        showChars(chars, length);
        if (cdataSection)
            XERCES_STD_QUALIFIER cout << "]]>";
    }
     else if ((fOutputType == OutputType_JCCanon)
          ||  (fOutputType == OutputType_SunCanon))
    {
        if (!fInsideRoot)
            return;
        showChars(chars, length);
    }
}

void TestParser::resetDocument()
{
    if (fOutputType == OutputType_Debug)
        XERCES_STD_QUALIFIER cout << "Got RESETDOCUMENT:\n" << XERCES_STD_QUALIFIER endl;
}

void TestParser::startDocument()
{
    if (fOutputType == OutputType_Debug)
        XERCES_STD_QUALIFIER cout << "Got STARTDOCUMENT:\n" << XERCES_STD_QUALIFIER endl;
}

void
TestParser::startElement(const  XMLElementDecl&         elemDecl
                        , const unsigned int            uriId
                        , const XMLCh* const            prefixName
                        , const RefVectorOf<XMLAttr>&   attrList
                        , const unsigned int            attCount
                        , const bool                    isEmpty
                        , const bool                    isRoot)
{
    // Set the flag that says we're now inside the root, if its not empty
    if (isRoot && !isEmpty)
        fInsideRoot = true;

    if (fOutputType == OutputType_Debug)
    {
        XMLBuffer bufURI;
        if (fDoNamespaces)
        {
            fScanner->getURIText(uriId, bufURI);
            XERCES_STD_QUALIFIER cout << "Got STARTELEMENT:\n    "
                 << " Name: {" << StrX(bufURI.getRawBuffer()) << "}"
                 << StrX(elemDecl.getBaseName())
                 << ", AttCount: " << attCount
                 << ", Empty?: "
                 << (isEmpty ? "yes" : "no")
                 << "\n";
        }
         else
        {
            XERCES_STD_QUALIFIER cout << "Got STARTELEMENT:\n    Name: "
                 << StrX(elemDecl.getFullName())
                 << ", AttCount: " << attCount
                 << ", Empty?: "
                 << (isEmpty ? "yes" : "no")
                 << "\n";
        }

        XERCES_STD_QUALIFIER cout << "    SrcOfs: " << fScanner->getSrcOffset() << "\n";

        // If any attributes, then show them
        if (attCount)
        {
            XERCES_STD_QUALIFIER cout << "    Attrs: ";
            for (unsigned int attInd = 0; attInd < attCount; attInd++)
            {
                const XMLAttr* curAttr = attrList.elementAt(attInd);

                if (fDoNamespaces)
                {
                    fScanner->getURIText(curAttr->getURIId(), bufURI);
                    XERCES_STD_QUALIFIER cout << "Name=" << "{" << StrX(bufURI.getRawBuffer())
                         << "}" << StrX(curAttr->getName());
                }
                 else
                {
                    XERCES_STD_QUALIFIER cout << "Name=" << StrX(curAttr->getQName());
                }

                if (curAttr->getSpecified())
                    XERCES_STD_QUALIFIER cout << " (Explicit)  ";
                else
                    XERCES_STD_QUALIFIER cout << " (Defaulted) ";

                XERCES_STD_QUALIFIER cout << "Value=" << StrX(curAttr->getValue()) << "\n"
                     << "           ";
            }
        }

        XERCES_STD_QUALIFIER cout << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType == OutputType_XML)
    {
        XERCES_STD_QUALIFIER cout << "<";
        showString(elemDecl.getFullName());

        if (attCount)
        {
            XERCES_STD_QUALIFIER cout << " ";

            for (unsigned int index = 0; index < attCount; index++)
            {
                const XMLAttr* curAttr = attrList.elementAt(index);
                showString(curAttr->getQName());
                XERCES_STD_QUALIFIER cout << "=\"";
                showString(curAttr->getValue());
                XERCES_STD_QUALIFIER cout << "\"";

                if (index < attCount-1)
                    XERCES_STD_QUALIFIER cout << " ";
            }
        }

        if (isEmpty)
            XERCES_STD_QUALIFIER cout << "/>";
        else
            XERCES_STD_QUALIFIER cout << ">";
    }
     else if ((fOutputType == OutputType_JCCanon)
          ||  (fOutputType == OutputType_SunCanon))
    {
        XERCES_STD_QUALIFIER cout << "<";
        showString(elemDecl.getFullName());

        if (attCount)
        {
            XERCES_STD_QUALIFIER cout << " ";

            //
            //  Get a list of attribute pointers. The canonical output
            //  format requires sorted attributes. If we aren't doing
            //  canonical output, then we don't sort it, but we still use
            //  the array.
            //
            const XMLAttr** attrTmp = new const XMLAttr*[attCount];
            unsigned int index;
            for (index = 0; index < attCount; index++)
                attrTmp[index] = attrList.elementAt(index);

            if (attCount > 1)
                qsort(attrTmp, attCount, sizeof(XMLAttr*), attrComp);

            for (index = 0; index < attCount; index++)
            {
                const XMLAttr* curAttr = attrTmp[index];
                showString(curAttr->getQName());
                XERCES_STD_QUALIFIER cout << "=\"";
                showString(curAttr->getValue());
                XERCES_STD_QUALIFIER cout << "\"";

                if (index < attCount-1)
                    XERCES_STD_QUALIFIER cout << " ";
            }
            delete [] attrTmp;
        }

        if (isEmpty)
        {
            XERCES_STD_QUALIFIER cout << "></";
            showString(elemDecl.getFullName());
            XERCES_STD_QUALIFIER cout << ">";
        }
         else
        {
            XERCES_STD_QUALIFIER cout << ">";
        }
    }
}

void TestParser::startEntityReference(const XMLEntityDecl& entDecl)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got STARTENTITY:\n    "
             << "Name: " << StrX(entDecl.getName()) << "\n" << XERCES_STD_QUALIFIER endl;
    }
}

void TestParser::XMLDecl(const  XMLCh* const    versionStr
                        , const XMLCh* const    encodingStr
                        , const XMLCh* const    standaloneStr
                        , const XMLCh* const    autoEncStr)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got XMLDECL:\n    "
             << "Version:\"" << StrX(versionStr) << "\""
             << " Encoding:\"" << StrX(encodingStr) << "\""
             << " Standalone:\"" << StrX(standaloneStr) << "\""
             << " Auto Encoding:\"" << StrX(autoEncStr) << "\""
             << "\n"
             << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType == OutputType_XML)
    {
        XERCES_STD_QUALIFIER cout << "<?xml";

        if (XMLString::stringLen(versionStr))
            XERCES_STD_QUALIFIER cout << " version=\"" << StrX(versionStr) << '"';

        if (XMLString::stringLen(encodingStr))
            XERCES_STD_QUALIFIER cout << " encoding=\"" << StrX(encodingStr) << '"';

        if (XMLString::stringLen(standaloneStr))
            XERCES_STD_QUALIFIER cout  << " standlone=\"" << StrX(standaloneStr) << '"';

        XERCES_STD_QUALIFIER cout << " ?>";
    }
}


// -----------------------------------------------------------------------
//  TestParser: The DocTypeHandler interface
// -----------------------------------------------------------------------
void TestParser::attDef(const   DTDElementDecl& elemDecl
                        , const DTDAttDef&      attDef
                        , const bool            ignoring)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got ATTDEF:\n    "
             << "Name: " << StrX(attDef.getFullName())
             << ", Type: "
             << StrX(XMLAttDef::getAttTypeString(attDef.getType()))
             << ", DefType: "
             << StrX(XMLAttDef::getDefAttTypeString(attDef.getDefaultType()));

        if (XMLString::stringLen(attDef.getValue()))
            XERCES_STD_QUALIFIER cout << ", Value: \"" << StrX(attDef.getValue()) << '"';

        XERCES_STD_QUALIFIER cout << "\n    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fInExtSubset)
            return;

        if (fIntDTDOutput)
        {
            XERCES_STD_QUALIFIER cout << StrX(attDef.getFullName()) << " ";
            if (attDef.getType() == XMLAttDef::Enumeration)
            {
                XERCES_STD_QUALIFIER cout << '(';
                StrX tmpStr(attDef.getEnumeration());
                const char* curCh = tmpStr.localForm();
                while (*curCh)
                {
                    while (!isspace(*curCh) && *curCh)
                        XERCES_STD_QUALIFIER cout << *curCh++;

                    if (*curCh)
                    {
                        XERCES_STD_QUALIFIER cout << '|';
                        curCh++;
                    }
                }
                XERCES_STD_QUALIFIER cout << ')';
            }
             else
            {
                XERCES_STD_QUALIFIER cout << StrX(XMLAttDef::getAttTypeString(attDef.getType()));
            }

            if (XMLString::stringLen(attDef.getValue()))
                XERCES_STD_QUALIFIER cout << " \"" << StrX(attDef.getValue()) << '"';

            if (attDef.getDefaultType() != XMLAttDef::Default)
            {
                XERCES_STD_QUALIFIER cout << " "
                     << StrX(XMLAttDef::getDefAttTypeString(attDef.getDefaultType()));
            }
        }
    }
}

void TestParser::doctypeComment(const XMLCh* const comment)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got DTD COMMENT:\n    "
             << "Text: \"" << StrX(comment) << "\"\n"
             << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fInExtSubset)
            return;

        if (fIntDTDOutput)
            XERCES_STD_QUALIFIER cout << "<!--" << StrX(comment) << "-->";
    }
}

void TestParser::doctypeDecl(const  DTDElementDecl& elemDecl
                            , const XMLCh* const    publicId
                            , const XMLCh* const    systemId
                            , const bool            hasIntSubset)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got DOCTYPE:\n    "
                << "Root: " << StrX(elemDecl.getFullName());

        if (XMLString::stringLen(publicId))
            XERCES_STD_QUALIFIER cout << ", PUBLIC: " << StrX(publicId);

        if (XMLString::stringLen(systemId))
            XERCES_STD_QUALIFIER cout << ", SYSTEM: " << StrX(systemId);

        XERCES_STD_QUALIFIER cout << "\n    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fIntDTDOutput)
        {
            XERCES_STD_QUALIFIER cout << "<!DOCTYPE " << StrX(elemDecl.getFullName());
            showIds(publicId, systemId);

            if (!hasIntSubset)
                XERCES_STD_QUALIFIER cout << ">";
        }
    }
}

void TestParser::doctypePI( const   XMLCh* const    target
                            , const XMLCh* const    data)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got DTD PI:\n     "
                << "Target: \"" << StrX(target) << '"';

        if (XMLString::stringLen(data))
            XERCES_STD_QUALIFIER cout << ", Data: \"" << StrX(data) << '"';

        XERCES_STD_QUALIFIER cout << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fInExtSubset)
            return;

        if (fIntDTDOutput)
        {
            XERCES_STD_QUALIFIER cout << "<?" << target;
            if (XMLString::stringLen(data))
                XERCES_STD_QUALIFIER cout << " " << StrX(data);
            XERCES_STD_QUALIFIER cout << "?>";
        }
    }
}

void TestParser::doctypeWhitespace( const   XMLCh* const    chars
                                    , const unsigned int    length)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got DTD Spaces:\n    Bytes: "
             << length << "\n"
             << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fInExtSubset)
            return;

        if (fIntDTDOutput)
            showChars(chars, length);
    }
}

void TestParser::elementDecl(const  DTDElementDecl&     decl
                            , const bool                isIgnored)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got ELEMENT DECL:\n    "
             << "Name: " << StrX(decl.getFullName());

        if (isIgnored)
            XERCES_STD_QUALIFIER cout << " (Ignored)";

        XERCES_STD_QUALIFIER cout << ", Content: "
             << StrX(decl.getFormattedContentModel())
             << "\n    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fInExtSubset)
            return;

        if (fIntDTDOutput)
        {
            XERCES_STD_QUALIFIER cout << "<!ELEMENT " << StrX(decl.getFullName()) << " "
                 << StrX(decl.getFormattedContentModel())
                 << ">";
        }
    }
}

void TestParser::endAttList(const DTDElementDecl& elemDecl)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got ENDATTLIST:\n    "
             << "Name: " << StrX(elemDecl.getFullName()) << "\n"
             << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fInExtSubset)
            return;

        if (fIntDTDOutput)
            XERCES_STD_QUALIFIER cout << ">";
    }
}

void TestParser::endIntSubset()
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got ENDINTSUBSET\n"
             << "    SrcOfs: " << fScanner->getSrcOffset()
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fIntDTDOutput)
            XERCES_STD_QUALIFIER cout << "]>";
    }
}

void TestParser::endExtSubset()
{
    fInExtSubset = false;
    if (fOutputType == OutputType_Debug)
        XERCES_STD_QUALIFIER cout << "Got ENDEXTSUBSET\n" << XERCES_STD_QUALIFIER endl;
}

void TestParser::entityDecl(const   DTDEntityDecl&  entityDecl
                            , const bool            isPEDecl
                            , const bool            isIgnored)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got ENTITYDECL:\n    "
             << "Name: " << StrX(entityDecl.getName())
             << (isPEDecl ? " [Parameter Entity]" : " [General Entity]")
             << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fInExtSubset)
            return;

        if (fIntDTDOutput)
        {
            XERCES_STD_QUALIFIER cout << "<!ENTITY ";
            if (isPEDecl)
                XERCES_STD_QUALIFIER cout << "% ";
            XERCES_STD_QUALIFIER cout << StrX(entityDecl.getName());

            if (entityDecl.isExternal())
                showIds(entityDecl.getPublicId(), entityDecl.getSystemId());
             else
                XERCES_STD_QUALIFIER cout << " \"" << StrX(entityDecl.getValue()) << "\"";

            XERCES_STD_QUALIFIER cout << ">";
        }
    }
}

void TestParser::resetDocType()
{
    if (fOutputType == OutputType_Debug)
        XERCES_STD_QUALIFIER cout << "Got RESETDOCTYPE:\n" << XERCES_STD_QUALIFIER endl;
}

void TestParser::notationDecl(  const   XMLNotationDecl&    notDecl
                                , const bool                isIgnored)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got NOTATIONDECL:\n    "
             << "Name: " << StrX(notDecl.getName())
             << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fInExtSubset)
            return;

        if (fIntDTDOutput)
        {
            XERCES_STD_QUALIFIER cout << "<!NOTATION " << StrX(notDecl.getName()) << " ";

            if (!XMLString::stringLen(notDecl.getSystemId()))
                XERCES_STD_QUALIFIER cout << "PUBLIC ";
            else
                XERCES_STD_QUALIFIER cout << "SYSTEM ";

            if (XMLString::stringLen(notDecl.getPublicId()))
                XERCES_STD_QUALIFIER cout << "\"" << StrX(notDecl.getPublicId()) << '"';

            if (XMLString::stringLen(notDecl.getSystemId()))
                XERCES_STD_QUALIFIER cout << " \"" << StrX(notDecl.getSystemId()) << '"';

            XERCES_STD_QUALIFIER cout << ">";
        }
    }
}

void TestParser::startAttList(const DTDElementDecl& elemDecl)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got STARTATTLIST:\n    "
                << "Name: " << StrX(elemDecl.getFullName())
                << "\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fInExtSubset)
            return;

        if (fIntDTDOutput)
            XERCES_STD_QUALIFIER cout << "<!ATTLIST " << StrX(elemDecl.getFullName());
    }
}

void TestParser::startIntSubset()
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got STARTINTSUBSET\n" << XERCES_STD_QUALIFIER endl;
    }
     else if (fOutputType != OutputType_None)
    {
        if (fIntDTDOutput)
            XERCES_STD_QUALIFIER cout << " [";
    }
}

void TestParser::startExtSubset()
{
    fInExtSubset = true;
    if (fOutputType == OutputType_Debug)
        XERCES_STD_QUALIFIER cout << "Got STARTEXTSUBSET\n" << XERCES_STD_QUALIFIER endl;
}

void TestParser::TextDecl(  const   XMLCh* const    versionStr
                            , const XMLCh* const    encodingStr)
{
    if (fOutputType == OutputType_Debug)
    {
        XERCES_STD_QUALIFIER cout << "Got TEXTDECL:\n    ";

        if (XMLString::stringLen(versionStr))
            XERCES_STD_QUALIFIER cout << "Version: " << StrX(versionStr);

        if (XMLString::stringLen(encodingStr))
            XERCES_STD_QUALIFIER cout << "Encoding: " << StrX(encodingStr);

        XERCES_STD_QUALIFIER cout << "\n" << XERCES_STD_QUALIFIER endl;
    }
}


// ---------------------------------------------------------------------------
//  TestParser: Implementation of the XMLErrorReporter interface
// ---------------------------------------------------------------------------
void TestParser::error( const   unsigned int                errCode
                        , const XMLCh* const                msgDomain
                        , const XMLErrorReporter::ErrTypes  type
                        , const XMLCh* const                text
                        , const XMLCh* const                systemId
                        , const XMLCh* const                publicId
                        , const unsigned int                lineNum
                        , const unsigned int                colNum)
{
    //
    //  If we are in 'show error loc' mode, then we do a special, condensed
    //  display of error location info. Else we fall through and do the
    //  normal one for human consumption.
    //
    if (fShowErrLoc)
    {
        // We only do fatal and validity errors in this case
        if (type == XMLErrorReporter::ErrType_Warning)
            return;

        //
        //  We want to display the entity name, but not the whole path, since
        //  this output is for regression testing and has to be compared
        //  against previous runs potentitally on other machines.
        //
        const XMLCh* entName = systemId;
        int ofs = XMLString::lastIndexOf(systemId, chForwardSlash);
        if (ofs == -1)
            ofs = XMLString::lastIndexOf(systemId, chBackSlash);
        if (ofs != -1)
            entName = &systemId[ofs + 1];

        XERCES_STD_QUALIFIER cout << lineNum << "/" << colNum
                << ":" << StrX(entName)
                << " - " << StrX(text)
                << XERCES_STD_QUALIFIER endl;
        return;
    }

    // If its a warning and we are not showing warnings, then get out
    if ((type == XMLErrorReporter::ErrType_Warning) && !fShowWarnings)
        return;

    const char* typeStr = "?";
    if (type == XMLErrorReporter::ErrType_Fatal)
        typeStr = "ERROR";
    else if (type == XMLErrorReporter::ErrType_Warning)
        typeStr = "WARNING";
    else if (type == XMLErrorReporter::ErrType_Error)
        typeStr = "VALIDITY";

    // Output the error heading and the error type string
    XERCES_STD_QUALIFIER cout << "\nError: (" << typeStr;

    // If we have either id, display them
    if (XMLString::stringLen(systemId))
         XERCES_STD_QUALIFIER cout << ", System Id: " << StrX(systemId);

    if (XMLString::stringLen(publicId))
        XERCES_STD_QUALIFIER cout << ", Public Id: " << StrX(publicId);

    // Display the position information
    XERCES_STD_QUALIFIER cout << ", Line/Col: " << lineNum << "/" << colNum
         << ")\n";

    // And finally the error text
    XERCES_STD_QUALIFIER cout << StrX(text) << XERCES_STD_QUALIFIER endl;
}

void TestParser::resetErrors()
{
    if (fOutputType == OutputType_Debug)
        XERCES_STD_QUALIFIER cout << "Got RESETERRORS:\n" << XERCES_STD_QUALIFIER endl;
}


// ---------------------------------------------------------------------------
//  TestParser: Private helpers
// ---------------------------------------------------------------------------
void TestParser::showChars( const   XMLCh* const    chars
                            , const unsigned int    length)
{
    static const XMLByte FirstByteMark[7] =
    {
        0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC
    };

    const bool doCan = ((fOutputType == OutputType_SunCanon)
                        || (fOutputType == OutputType_JCCanon));

    unsigned int index = 0;
    while (index < length)
    {
        //
        //  Convert the char to UTF-8 format. This will generate multiple
        //  short chars to display. For each one, call the showChar()
        //  method to display it.
        //
        XMLUInt32 tmpVal = chars[index++];

        if ((tmpVal >= 0xD800) && (tmpVal <= 0xDBFF))
        {
            if (index == length)
            {
                XERCES_STD_QUALIFIER cout << "Missing trailing surrogate\n" << XERCES_STD_QUALIFIER endl;
                break;
            }

            tmpVal = ((tmpVal - 0xD800) << 10)
                     + ((chars[index] - 0xDC00) + 0x10000);
            index++;
        }

        // Figure out how many bytes we'll kick out
        unsigned int outBytes;
        if (tmpVal < 0x80)
            outBytes = 1;
        else if (tmpVal < 0x800)
            outBytes = 2;
        else if (tmpVal < 0x10000)
            outBytes = 3;
        else if (tmpVal < 0x200000)
            outBytes = 4;
        else if (tmpVal < 0x4000000)
            outBytes = 5;
        else
        {
            outBytes = 6;
            if (tmpVal & 0x80000000)
            {
                outBytes = 2;
                tmpVal = 0xFFFD;
            }
        }

        // Get the chars into a temp buffer in the right order
        char tmpOutChars[6];
        unsigned int outIndex = outBytes;
        switch(outBytes)
        {
            case 6 : tmpOutChars[--outIndex] = char((tmpVal | 0x80) & 0xBF);
                     tmpVal >>= 6;
            case 5 : tmpOutChars[--outIndex] = char((tmpVal | 0x80) & 0xBF);
                     tmpVal >>= 6;
            case 4 : tmpOutChars[--outIndex] = char((tmpVal | 0x80) & 0xBF);
                     tmpVal >>= 6;
            case 3 : tmpOutChars[--outIndex] = char((tmpVal | 0x80) & 0xBF);
                     tmpVal >>= 6;
            case 2 : tmpOutChars[--outIndex] = char((tmpVal | 0x80) & 0xBF);
                     tmpVal >>= 6;
            case 1 : tmpOutChars[--outIndex] = char(tmpVal | FirstByteMark[outBytes]);
        }

        // And spit them out
        for (outIndex = 0; outIndex < outBytes; outIndex++)
            showChar(tmpOutChars[outIndex], doCan);
    }
}


void TestParser::showChar(const char toShow, const bool doCan)
{
    if (doCan)
    {
        if (toShow == chLF)
            XERCES_STD_QUALIFIER cout << "&#10;";
        else if (toShow == chHTab)
            XERCES_STD_QUALIFIER cout << "&#9;";
        else if (toShow == chCR)
            XERCES_STD_QUALIFIER cout << "&#13;";
        else if (toShow == chSingleQuote)
            XERCES_STD_QUALIFIER cout << "&apos;";
        else if (toShow == chAmpersand)
            XERCES_STD_QUALIFIER cout << "&amp;";
        else if (toShow == chDoubleQuote)
            XERCES_STD_QUALIFIER cout << "&quot;";
        else if (toShow == chOpenAngle)
            XERCES_STD_QUALIFIER cout << "&lt;";
        else if (toShow == chCloseAngle)
            XERCES_STD_QUALIFIER cout << "&gt;";
        else
            XERCES_STD_QUALIFIER cout << toShow;
    }
     else
    {
        XERCES_STD_QUALIFIER cout << toShow;
    }
}

void
TestParser::showIds(const XMLCh* const publicId, const XMLCh* const systemId)
{
    if (XMLString::stringLen(publicId) || XMLString::stringLen(systemId))
    {
        if (!XMLString::stringLen(publicId))
        {
            XERCES_STD_QUALIFIER cout << " SYSTEM '" << StrX(systemId) << "'";
        }
         else
        {
            XERCES_STD_QUALIFIER cout << " PUBLIC '" << StrX(publicId) << "'";
            if (systemId)
                XERCES_STD_QUALIFIER cout << " '" << StrX(systemId) << "'";
        }
    }
}


void TestParser::showString(const XMLCh* const toShow)
{
    showChars(toShow, XMLString::stringLen(toShow));
}
