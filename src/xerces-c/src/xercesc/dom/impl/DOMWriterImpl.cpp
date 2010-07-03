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
 * $Id: DOMWriterImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOMWriterImpl.hpp"
#include "DOMErrorImpl.hpp"
#include "DOMLocatorImpl.hpp"
#include "DOMImplementationImpl.hpp"

#include <xercesc/framework/MemBufFormatTarget.hpp>

#include <xercesc/util/TransService.hpp>
#include <xercesc/util/TranscodingException.hpp>
#include <xercesc/util/Janitor.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLMsgLoader.hpp>
#include <xercesc/dom/StDOMNode.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>

XERCES_CPP_NAMESPACE_BEGIN


// ---------------------------------------------------------------------------
//  Local const data
//
// ---------------------------------------------------------------------------

static const int INVALID_FEATURE_ID               = -1;
static const int CANONICAL_FORM_ID                = 0x0;
static const int DISCARD_DEFAULT_CONTENT_ID       = 0x1;
static const int ENTITIES_ID                      = 0x2;
static const int FORMAT_PRETTY_PRINT_ID           = 0x3;
static const int NORMALIZE_CHARACTERS_ID          = 0x4;
static const int SPLIT_CDATA_SECTIONS_ID          = 0x5;
static const int VALIDATION_ID                    = 0x6;
static const int WHITESPACE_IN_ELEMENT_CONTENT_ID = 0x7;
static const int BYTE_ORDER_MARK_ID               = 0x8;
static const int XML_DECLARATION                 = 0x9;

//    feature                      true                       false
// ================================================================================
//canonical-form                 [optional] Not Supported     [required] (default)
//discard-default-content        [required] (default)         [required]
//entity                         [required] (default)         [optional]
//format-pretty-print            [optional] Partially Supported [required] (default)
//normalize-characters           [optional] Not Supported     [required] (default)
//split-cdata-sections           [required] (default)         [required]
//validation                     [optional] Not Supported     [required] (default)
//whitespace-in-element-content  [requierd] (default)         [optional] Not Supported
//

//
// Each feature has 2 entries in this array,
// the first for "true",
// the second for "false".
//
static const bool  featuresSupported[] = {
    false, true,  // canonical-form
    true,  true,  // discard-default-content
    true,  true,  // entity
    true,  true,  // format-pretty-print
    false, true,  // normalize-characters
    true,  true,  // split-cdata-sections
    false, true,  // validation
    true,  false, // whitespace-in-element-content
    true,  true,   // byte-order-mark
    true,  true   // xml-declaration
};

// default end-of-line sequence
static const XMLCh  gEOLSeq[] =
{
    chLF, chNull
};

//UTF-8
static const XMLCh  gUTF8[] =
{
    chLatin_U, chLatin_T, chLatin_F, chDash, chDigit_8, chNull
};

//</
static const XMLCh  gEndElement[] =
{
    chOpenAngle, chForwardSlash, chNull
};

//?>
static const XMLCh  gEndPI[] =
{
    chQuestion, chCloseAngle, chNull
};

//<?
static const XMLCh  gStartPI[] =
{
    chOpenAngle, chQuestion, chNull
};

//<?xml version="
static const XMLCh  gXMLDecl_VersionInfo[] =
{
    chOpenAngle, chQuestion, chLatin_x,     chLatin_m,  chLatin_l,  chSpace,
    chLatin_v,   chLatin_e,  chLatin_r,     chLatin_s,  chLatin_i,  chLatin_o,
    chLatin_n,   chEqual,    chDoubleQuote, chNull
};

static const XMLCh gXMLDecl_ver10[] =
{
    chDigit_1, chPeriod, chDigit_0, chNull
};

//encoding="
static const XMLCh  gXMLDecl_EncodingDecl[] =
{
    chLatin_e,  chLatin_n,  chLatin_c,  chLatin_o,      chLatin_d, chLatin_i,
    chLatin_n,  chLatin_g,  chEqual,    chDoubleQuote,  chNull
};

//" standalone="
static const XMLCh  gXMLDecl_SDDecl[] =
{
    chLatin_s, chLatin_t, chLatin_a,   chLatin_n,    chLatin_d,   chLatin_a,
    chLatin_l, chLatin_o, chLatin_n,   chLatin_e,    chEqual,     chDoubleQuote,
    chNull
};

//"
static const XMLCh  gXMLDecl_separator[] =
{
    chDoubleQuote, chSpace, chNull
};

//?>
static const XMLCh  gXMLDecl_endtag[] =
{
    chQuestion, chCloseAngle,  chNull
};

//<![CDATA[
static const XMLCh  gStartCDATA[] =
{
    chOpenAngle, chBang,    chOpenSquare, chLatin_C, chLatin_D,
    chLatin_A,   chLatin_T, chLatin_A,    chOpenSquare, chNull
};

//]]>
static const XMLCh  gEndCDATA[] =
{
//    chCloseSquare, chCloseAngle, chCloseAngle, chNull  // test only: ]>>
      chCloseSquare, chCloseSquare, chCloseAngle, chNull
};

static const int offset = XMLString::stringLen(gEndCDATA);

//<!--
static const XMLCh  gStartComment[] =
{
    chOpenAngle, chBang, chDash, chDash, chNull
};

//-->
static const XMLCh  gEndComment[] =
{
    chDash, chDash, chCloseAngle, chNull
};

//<!DOCTYPE
static const XMLCh  gStartDoctype[] =
{
    chOpenAngle, chBang,    chLatin_D, chLatin_O, chLatin_C, chLatin_T,
    chLatin_Y,   chLatin_P, chLatin_E, chSpace,   chNull
};

//PUBLIC "
static const XMLCh  gPublic[] =
{
    chLatin_P, chLatin_U, chLatin_B,     chLatin_L, chLatin_I,
    chLatin_C, chSpace,   chDoubleQuote, chNull
};

//SYSTEM "
static const XMLCh  gSystem[] =
{
    chLatin_S, chLatin_Y, chLatin_S,     chLatin_T, chLatin_E,
    chLatin_M, chSpace,   chDoubleQuote, chNull
};

//<!ENTITY
static const XMLCh  gStartEntity[] =
{
    chOpenAngle, chBang,    chLatin_E, chLatin_N, chLatin_T, chLatin_I,
    chLatin_T,   chLatin_Y, chSpace,   chNull
};

//NDATA "
static const XMLCh  gNotation[] =
{
    chLatin_N, chLatin_D,     chLatin_A, chLatin_T, chLatin_A,
    chSpace,   chDoubleQuote, chNull
};

//Feature
static const XMLCh  gFeature[] =
{
    chLatin_F, chLatin_e, chLatin_a, chLatin_t, chLatin_u, chLatin_r,
    chLatin_e, chSpace,   chNull
};

// Can not be set to
static const XMLCh  gCantSet[] =
{
    chSpace,   chLatin_C, chLatin_a, chLatin_n, chSpace, chLatin_n, chLatin_o,
    chLatin_t, chSpace,   chLatin_b, chLatin_e, chSpace, chLatin_s,
    chLatin_e, chLatin_t, chSpace,   chLatin_t, chLatin_o, chSpace, chNull
};

static const XMLCh  gTrue[] =
{
    chSingleQuote, chLatin_t, chLatin_r, chLatin_u, chLatin_e,
    chSingleQuote, chLF,      chNull
};

static const XMLCh  gFalse[] =
{
    chSingleQuote, chLatin_f, chLatin_a, chLatin_l, chLatin_s,
    chLatin_e,     chSingleQuote, chLF, chNull
};

static const XMLByte  BOM_utf16be[] = {(XMLByte)0xFE, (XMLByte)0xFF, (XMLByte) 0};
static const XMLByte  BOM_utf16le[] = {(XMLByte)0xFF, (XMLByte)0xFE, (XMLByte) 0};
static const XMLByte  BOM_ucs4be[]  = {(XMLByte)0x00, (XMLByte)0x00, (XMLByte)0xFE, (XMLByte)0xFF, (XMLByte) 0};
static const XMLByte  BOM_ucs4le[]  = {(XMLByte)0xFF, (XMLByte)0xFE, (XMLByte)0x00, (XMLByte)0x00, (XMLByte) 0};

static bool lineFeedInTextNodePrinted = false;
static int  lastWhiteSpaceInTextNode = 0;

//
// Notification of the error though error handler
//
// The application may instruct the engine to abort serialization
// by returning "false".
//
// REVISIT: update the locator ctor once the line#, col#, uri and offset
// are available from DOM3 core
//
// REVISIT: use throwing exception to abort serialization is an interesting
// thing here, since the serializer is a recusive function, we
// can't use return, obviously. However we may have multiple try/catch
// along its way going back to writeNode(). So far we don't come up with a
// "short-cut" to go "directly" back.
//
#define  TRY_CATCH_THROW(action, forceToRethrow)                     \
fFormatter->setUnRepFlags(XMLFormatter::UnRep_Fail);                 \
try                                                                  \
{                                                                    \
    action;                                                          \
}                                                                    \
catch(TranscodingException const &e)                                 \
{                                                                    \
    if ( !reportError(nodeToWrite                                    \
                    , DOMError::DOM_SEVERITY_FATAL_ERROR             \
                    , e.getMessage())                       ||       \
          forceToRethrow)                                            \
        throw e;                                                       \
}

DOMWriterImpl::~DOMWriterImpl()
{
    fMemoryManager->deallocate(fEncoding);//delete [] fEncoding;
    fMemoryManager->deallocate(fNewLine);//delete [] fNewLine;
    delete fNamespaceStack;
    // we don't own/adopt error handler and filter
}

DOMWriterImpl::DOMWriterImpl(MemoryManager* const manager)
:fFeatures(0)
,fEncoding(0)
,fNewLine(0)
,fErrorHandler(0)
,fFilter(0)
,fDocumentVersion(XMLUni::fgVersion1_0)
,fEncodingUsed(0)
,fNewLineUsed(0)
,fFormatter(0)
,fErrorCount(0)
,fCurrentLine(0)
,fNamespaceStack(0)
,fMemoryManager(manager)
{
    fNamespaceStack=new (fMemoryManager) RefVectorOf< RefHashTableOf<XMLCh> >(0,true, fMemoryManager);

    //
    // set features to default setting
    //
    setFeature(CANONICAL_FORM_ID,                false);
    setFeature(DISCARD_DEFAULT_CONTENT_ID,       true );
    setFeature(ENTITIES_ID,                      true );
    setFeature(FORMAT_PRETTY_PRINT_ID,           false);
    setFeature(NORMALIZE_CHARACTERS_ID,          false);
    setFeature(SPLIT_CDATA_SECTIONS_ID,          true );
    setFeature(VALIDATION_ID,                    false);
    setFeature(WHITESPACE_IN_ELEMENT_CONTENT_ID, true );
    setFeature(BYTE_ORDER_MARK_ID,               false);
    setFeature(XML_DECLARATION,                 true );

}

bool DOMWriterImpl::canSetFeature(const XMLCh* const featName
                                  , bool               state) const
{
    int featureId = INVALID_FEATURE_ID;
    return checkFeature(featName, false, featureId) ? canSetFeature(featureId, state) : false;
}

void DOMWriterImpl::setFeature(const XMLCh* const featName
                             , bool               state)
{
    int featureId = INVALID_FEATURE_ID;
    checkFeature(featName, true, featureId);

    if (!canSetFeature(featureId, state))
    {
        XMLCh  tmpbuf[256];
        unsigned int strLen = XMLString::stringLen(gFeature) +
                              XMLString::stringLen(featName) +
                              XMLString::stringLen(gCantSet) +
                              XMLString::stringLen(gFalse);

        XMLString::copyString(tmpbuf, gFeature);
        if (strLen < 256)
        {
            XMLString::catString(tmpbuf, featName);
        }
        else
        {
            // truncate the featurename to fit into the 256 buffer
            XMLString::copyNString(tmpbuf+XMLString::stringLen(gFeature),
                                   featName, 200);
        }
        XMLString::catString(tmpbuf, gCantSet);
        XMLString::catString(tmpbuf, state? gTrue : gFalse);
        throw DOMException(DOMException::NOT_SUPPORTED_ERR, tmpbuf, fMemoryManager);
    }
    else
        setFeature(featureId, state);

    //
    // canonical-form and format-pretty-print can not be both set to true
    // meaning set canonical-form true will automatically set
    // format-pretty-print to false and vise versa.
    //
    if ((featureId == CANONICAL_FORM_ID) && state)
        setFeature(FORMAT_PRETTY_PRINT_ID, false);

    if ((featureId == FORMAT_PRETTY_PRINT_ID) && state)
        setFeature(CANONICAL_FORM_ID, false);

    return;
}

bool DOMWriterImpl::getFeature(const XMLCh* const featName) const
{
    int featureId = INVALID_FEATURE_ID;
    checkFeature(featName, true, featureId);
    return getFeature(featureId);
}

// we don't check the validity of the encoding set
void DOMWriterImpl::setEncoding(const XMLCh* const encoding)
{
    fMemoryManager->deallocate(fEncoding);//delete [] fEncoding;
    fEncoding = XMLString::replicate(encoding, fMemoryManager);
}

const XMLCh* DOMWriterImpl::getEncoding() const
{
    return fEncoding;
}

void DOMWriterImpl::setNewLine(const XMLCh* const newLine)
{
    fMemoryManager->deallocate(fNewLine);//delete [] fNewLine;
    fNewLine = XMLString::replicate(newLine, fMemoryManager);
}

const XMLCh* DOMWriterImpl::getNewLine() const
{
    return fNewLine;
}

void DOMWriterImpl::setErrorHandler(DOMErrorHandler *errorHandler)
{
    fErrorHandler = errorHandler;
}

DOMErrorHandler* DOMWriterImpl::getErrorHandler() const
{
    return fErrorHandler;
}

void DOMWriterImpl::setFilter(DOMWriterFilter *filter)
{
    fFilter = filter;
}

DOMWriterFilter* DOMWriterImpl::getFilter() const
{
    return fFilter;
}

//
//
//
bool DOMWriterImpl::writeNode(XMLFormatTarget* const destination
                            , const DOMNode         &nodeToWrite)
{
    //init session vars
    initSession(&nodeToWrite);

    try
    {
        fFormatter = new (fMemoryManager) XMLFormatter(fEncodingUsed
                                     ,fDocumentVersion
                                     ,destination
                                     ,XMLFormatter::NoEscapes
                                     ,XMLFormatter::UnRep_CharRef
                                     ,fMemoryManager);
    }
    catch (const TranscodingException& e)
    {
        reportError(&nodeToWrite, DOMError::DOM_SEVERITY_FATAL_ERROR, e.getMessage());
        return false;
    }

    try
    {
        Janitor<XMLFormatter> janName(fFormatter);
        processNode(&nodeToWrite);
        destination->flush();
    }

    //
    // The serialize engine (processNode) throws an exception to abort
    // serialization if
    //
    //   . A fatal error occurs which renters the output ill-formed, or
    //   . Instructed by the application's error handler
    //
    catch (const TranscodingException&)
    {
        destination->flush();
        return false;
    }

    catch (const XMLDOMMsg::Codes)
    {
        destination->flush();
        return false;
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch (...)
    {        
        destination->flush();
        throw;
    }

    //
    // true if node was successfully serialized and
    // false in case a failure occured and the
    // failure wasn't canceled by the error handler.
    //
    return ((fErrorCount == 0)? true : false);
}

//
// We don't throw DOMSTRING_SIZE_ERR since we are no longer
// using DOMString.
//
XMLCh* DOMWriterImpl::writeToString(const DOMNode &nodeToWrite)
{
    MemBufFormatTarget  destination(1023, fMemoryManager);
    bool retVal;

    // XMLCh is unicode, assume fEncoding as UTF-16
    XMLCh* tempEncoding = fEncoding;
    fEncoding = (XMLCh*) XMLUni::fgUTF16EncodingString;

    try
    {
        retVal = writeNode(&destination, nodeToWrite);
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch (...)
    {
        //
        // there is a possibility that memeory allocation
        // exception thrown in XMLBuffer class
        //
        fEncoding = tempEncoding;
        return 0;
    }

    fEncoding = tempEncoding;
    return (retVal ? XMLString::replicate((XMLCh*) destination.getRawBuffer(), fMemoryManager) : 0);
}

void DOMWriterImpl::initSession(const DOMNode* const nodeToWrite)
{

/**
 * The encoding to use when writing is determined as follows:
 *     If the encoding attribute has been set, that value will be used.
 *     If the encoding attribute is null or empty,
 *            but the item to be written, or
 *            the owner document specified encoding (ie. the "actualEncoding"
 *            from the document) that value will be used.
 *     If neither of the above provides an encoding name, a default encoding of
 *            "UTF-8" will be used.
 */
    fEncodingUsed = gUTF8;

    if (fEncoding && *fEncoding)
    {
        fEncodingUsed = fEncoding;
    }
    else
    {
        const DOMDocument *docu = (nodeToWrite->getNodeType() == DOMNode::DOCUMENT_NODE)?
                            (const DOMDocument*)nodeToWrite : nodeToWrite->getOwnerDocument();
        if (docu)
        {
            const XMLCh* tmpEncoding = docu->getEncoding();

            if ( tmpEncoding && *tmpEncoding)
            {
                fEncodingUsed = tmpEncoding;
            }
            else
            {
                tmpEncoding = docu->getActualEncoding();

                if ( tmpEncoding && *tmpEncoding)
                {
                    fEncodingUsed = tmpEncoding;
                }
            }
        }
    }

    /**
     *  The end-of-line sequence of characters to be used in the XML being
     *  written out. The only permitted values are these:
     *     . null
     *
     *  Use a default end-of-line sequence. DOM implementations should choose
     * the default to match the usual convention for text files in the
     * environment being used. Implementations must choose a default
     * sequence that matches one of those allowed by  2.11 "End-of-Line
     * Handling".
     *
     *    CR    The carriage-return character (#xD)
     *    CR-LF The carriage-return and line-feed characters (#xD #xA)
     *    LF    The line-feed character (#xA)
     *
     *  The default value for this attribute is null
     */
    fNewLineUsed = (fNewLine && *fNewLine)? fNewLine : gEOLSeq;

    /**
     *  get Document Version
     */
    const DOMDocument *docu = (nodeToWrite->getNodeType() == DOMNode::DOCUMENT_NODE)?
                              (const DOMDocument*)nodeToWrite : nodeToWrite->getOwnerDocument();
    if (docu)
    {
        fDocumentVersion = docu->getVersion();
    }

    fErrorCount = 0;
}

//
// Characters not representable in output encoding,
//
// 1. CHARACTER DATA (outside of markup)                --- no error
//    ordinary character  -> numeric character reference
//    '<' and '&'         -> &lt; and &amp;
//
// 2. Within MARKUP, but outside of attributes
//    reported as an error                                 --- ERROR
//    markup:
//           start tag                                  done
//           end tag                                    done
//           empty element tag                          done
//           entity references                          done
//           character references    // REVISIT
//           comments                                   done
//           CDATA section delimiters                   done, done
//           document type declarartions                done
//           processing instructions (PI)               done
//
// 3. With in ATTRIBUTE
//    -> numeric character reference
//    no quotes                        -> in quotes
//    with quotes, no apostrophe       -> in apostrophe
//    with quotes and apostrophe       -> in quotes and &quot;
//
// 4. CDATA sections
//    "split_cdata_section"  true                      --- char ref
//                           false                     ---      ERROR
//
// ---------------------------------------------------------------------------
//  Stream out a DOM node, and, recursively, all of its children. This
//  function is the heart of writing a DOM tree out as XML source. Give it
//  a document node and it will do the whole thing.
// ---------------------------------------------------------------------------

void DOMWriterImpl::processNode(const DOMNode* const nodeToWrite, int level)
{

    // Get the name and value out for convenience
    const XMLCh*    nodeName = nodeToWrite->getNodeName();
    const XMLCh*    nodeValue = nodeToWrite->getNodeValue();
    unsigned long   lent = XMLString::stringLen(nodeValue);

    switch (nodeToWrite->getNodeType())
    {
    case DOMNode::TEXT_NODE:
        {
            if (checkFilter(nodeToWrite) != DOMNodeFilter::FILTER_ACCEPT)
                break;

            if (getFeature(FORMAT_PRETTY_PRINT_ID))
            {
                lineFeedInTextNodePrinted = false;
                lastWhiteSpaceInTextNode = 0;

                if(XMLString::isAllWhiteSpace(nodeValue))
                {
                    // skips whitespace-only text nodes unless whitespace-in-element is set.
                    if (!getFeature(WHITESPACE_IN_ELEMENT_CONTENT_ID))
                    {
                        break;
                    }
                    else        
                    {
                        //
                        // we need to trace if newline(s) have been printed out
                        // to avoid generate extra newline for pretty printing,
                        // as well as the number of whitespaces after the last
                        // newline character to do indentation properly.
                        //
                        int pos = XMLString::lastIndexOf(nodeValue, chLF);
                        if (-1 != pos)
                        {
                            lineFeedInTextNodePrinted = true;
                            lastWhiteSpaceInTextNode = lent - pos;
                        }
                        else
                        {
                            // for those platforms using chCR alone as
                            // a newline character
                            pos = XMLString::lastIndexOf(nodeValue, chCR);
                            if (-1 != pos)
                            {
                                lineFeedInTextNodePrinted = true;
                                lastWhiteSpaceInTextNode = lent - pos;
                            }
                        }
                    }
                }
            }

            setURCharRef();      // character data
            fFormatter->formatBuf(nodeValue, lent, XMLFormatter::CharEscapes);
            break;
        }

    case DOMNode::PROCESSING_INSTRUCTION_NODE:
        {
            if (checkFilter(nodeToWrite) != DOMNodeFilter::FILTER_ACCEPT)
                break;

            if(level == 1)
                printNewLine();

            printNewLine();
            printIndent(level);

            TRY_CATCH_THROW
            (
                *fFormatter << XMLFormatter::NoEscapes << gStartPI << nodeName;
                if (lent > 0)
                {
                    *fFormatter << chSpace << nodeValue;
                }
                *fFormatter << gEndPI;
                ,true
            )
            break;
        }

    case DOMNode::DOCUMENT_NODE: // Not to be shown to Filter
        {

            // output BOM if needed
            processBOM();          
            
            setURCharRef();
            const DOMDocument *docu = (const DOMDocument*)nodeToWrite;
        
            //[23] XMLDecl      ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
            //[24] VersionInfo  ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
            //[80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName
            //[32] SDDecl       ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
            //

            if (getFeature(XML_DECLARATION)) {
                const XMLCh* versionNo = (docu->getVersion()) ? docu->getVersion() : gXMLDecl_ver10;
                *fFormatter << gXMLDecl_VersionInfo << versionNo << gXMLDecl_separator;

                // use the encoding resolved in initSession()
                *fFormatter << gXMLDecl_EncodingDecl << fEncodingUsed << gXMLDecl_separator;

                const XMLCh* st = (docu->getStandalone())? XMLUni::fgYesString : XMLUni::fgNoString;
                *fFormatter << gXMLDecl_SDDecl << st << gXMLDecl_separator;
                
                *fFormatter << gXMLDecl_endtag;
            }

            DOMNodeSPtr child = nodeToWrite->getFirstChild();
            while( child != 0)
            {
                processNode(child, level);
                child = child->getNextSibling();
            }
            printNewLine();
            break;
        }

    case DOMNode::DOCUMENT_FRAGMENT_NODE:
        {
           
            setURCharRef();

            DOMNode *child = nodeToWrite->getFirstChild();
            while( child != 0)
            {
                processNode(child, level);
                child = child->getNextSibling();
            }
            printNewLine();
            break;
        }

    case DOMNode::ELEMENT_NODE:
        {
            DOMNodeFilter::FilterAction filterAction = checkFilter(nodeToWrite);

            if ( filterAction == DOMNodeFilter::FILTER_REJECT)
                break;

            if (!lineFeedInTextNodePrinted)
            {
                if(level == 1)
                    printNewLine();

                printNewLine();
            }
            else
            {
                lineFeedInTextNodePrinted = false;
            }

            printIndent(level);

            //track the line number the current node begins on
            int nodeLine = fCurrentLine;

            // add an entry in the namespace stack
            RefHashTableOf<XMLCh>* namespaceMap=NULL;

            if ( filterAction == DOMNodeFilter::FILTER_ACCEPT)
            {
                //           this element    attributes   child elements
                // accept        yes             yes           yes
                // skip          no              no            yes
                //
                TRY_CATCH_THROW
                (
                // The name has to be representable without any escapes
                    *fFormatter  << XMLFormatter::NoEscapes
                                 << chOpenAngle << nodeName;
                    ,true
                )

                // Output any attributes on this element
                setURCharRef();
                DOMNamedNodeMap *attributes = nodeToWrite->getAttributes();
                int attrCount = attributes->getLength();

                // check if the namespace for the current node is already defined
                const XMLCh* prefix = nodeToWrite->getPrefix();
                const XMLCh* uri = nodeToWrite->getNamespaceURI();
                if(uri && uri[0])
                {
                    if(prefix==0 || prefix[0]==0)
                        prefix=XMLUni::fgZeroLenString;
                    bool bPrefixDeclared=false;
                    for(int i=fNamespaceStack->size()-1;i>=0;i--)
                    {
                        RefHashTableOf<XMLCh>* curNamespaceMap=fNamespaceStack->elementAt(i);
                        const XMLCh* thisUri=curNamespaceMap->get((void*)prefix);
                        if(thisUri)
                        {
                            // the prefix has been declared: check if it binds to the correct namespace, otherwise, redeclare it
                            if(XMLString::equals(thisUri,nodeToWrite->getNamespaceURI()))
                                bPrefixDeclared=true;
                            break;
                        }
                    }
                    if(!bPrefixDeclared)
                    {
                        if(namespaceMap==NULL)
                        {
                            namespaceMap=new (fMemoryManager) RefHashTableOf<XMLCh>(12, false, fMemoryManager);
                            fNamespaceStack->addElement(namespaceMap);
                        }
                        namespaceMap->put((void*)prefix,(XMLCh*)nodeToWrite->getNamespaceURI());
                        *fFormatter  << XMLFormatter::NoEscapes
                                     << chSpace << XMLUni::fgXMLNSString;
                        if(!XMLString::equals(prefix,XMLUni::fgZeroLenString))
                            *fFormatter  << chColon << prefix;
                        *fFormatter  << chEqual << chDoubleQuote
                                     << XMLFormatter::AttrEscapes
                                     << nodeToWrite->getNamespaceURI()
                                     << XMLFormatter::NoEscapes
                                     << chDoubleQuote;
                    }
                }

                bool discard = getFeature(DISCARD_DEFAULT_CONTENT_ID);
                for (int i = 0; i < attrCount; i++)
                {
                    DOMAttrSPtr  attribute = (DOMAttr*)attributes->item(i);

                    // Not to be shown to Filter

                    //
                    //"discard-default-content"
                    //    true
                    //    [required] (default)
                    //    Use whatever information available to the implementation
                    //  (i.e. XML schema, DTD, the specified flag on Attr nodes,
                    //  and so on) to decide what attributes and content should be
                    //  discarded or not.
                    //  Note that the specified flag on Attr nodes in itself is
                    //  not always reliable, it is only reliable when it is set
                    //  to false since the only case where it can be set to false
                    //  is if the attribute was created by the implementation.
                    //  The default content won't be removed if an implementation
                    //  does not have any information available.
                    //    false
                    //    [required]
                    //    Keep all attributes and all content.
                    //
                    if (discard && !((DOMAttr*)attribute )->getSpecified())
                        continue;
                    //
                    //  Again the name has to be completely representable. But the
                    //  attribute can have refs and requires the attribute style
                    //  escaping.
                    //

                    // if this attribute is a namespace declaration, add it to the namespace map for the current level
                    const XMLCh* ns = attribute->getNamespaceURI();
                    if (ns != 0 )
                    {
                        if(XMLString::equals(ns, XMLUni::fgXMLNSURIName)) 
                        {
                            if(namespaceMap==NULL)
                            {
                                namespaceMap=new (fMemoryManager) RefHashTableOf<XMLCh>(12, false, fMemoryManager);
                                fNamespaceStack->addElement(namespaceMap);
                            }
			                const XMLCh* nsPrefix = attribute->getLocalName();
                            if(XMLString::equals(attribute->getNodeName(),XMLUni::fgXMLNSString))
								nsPrefix = XMLUni::fgZeroLenString;
							if(namespaceMap->containsKey((void*)nsPrefix))
								continue;
                            namespaceMap->put((void*)attribute->getLocalName(),(XMLCh*)attribute->getNodeValue());
                        }
                        else if(!XMLString::equals(ns, XMLUni::fgXMLURIName)) 
                        {
                            // check if the namespace for the current node is already defined
                            const XMLCh* prefix = attribute->getPrefix();
                            if(prefix && prefix[0])
                            {
                                bool bPrefixDeclared=false;
                                for(int i=fNamespaceStack->size()-1;i>=0;i--)
                                {
                                    RefHashTableOf<XMLCh>* curNamespaceMap=fNamespaceStack->elementAt(i);
                                    const XMLCh* thisUri=curNamespaceMap->get((void*)prefix);
                                    if(thisUri)
                                    {
                                        // the prefix has been declared: check if it binds to the correct namespace, otherwise, redeclare it
                                        if(XMLString::equals(thisUri,attribute->getNamespaceURI()))
                                            bPrefixDeclared=true;
                                        break;
                                    }
                                }
                                if(!bPrefixDeclared)
                                {
                                    if(namespaceMap==NULL)
                                    {
                                        namespaceMap=new (fMemoryManager) RefHashTableOf<XMLCh>(12, false, fMemoryManager);
                                        fNamespaceStack->addElement(namespaceMap);
                                    }
                                    namespaceMap->put((void*)prefix,(XMLCh*)attribute->getNamespaceURI());
                                    *fFormatter  << XMLFormatter::NoEscapes
                                                 << chSpace << XMLUni::fgXMLNSString << chColon << prefix
                                                 << chEqual << chDoubleQuote
                                                 << XMLFormatter::AttrEscapes
                                                 << attribute->getNamespaceURI()
                                                 << XMLFormatter::NoEscapes
                                                 << chDoubleQuote;
                                }
                            }
                        }
                    }
                    *fFormatter  << XMLFormatter::NoEscapes
                                 << chSpace << attribute->getNodeName()
                                 << chEqual << chDoubleQuote
                                 << XMLFormatter::AttrEscapes;
                    if (getFeature(ENTITIES_ID))
                    {
                        DOMNodeSPtr child = attribute->getFirstChild();
                        while( child != 0)
                        {
                            if(child->getNodeType()==DOMNode::TEXT_NODE)
                                *fFormatter  << child->getNodeValue();
                            else if(child->getNodeType()==DOMNode::ENTITY_REFERENCE_NODE)
                                *fFormatter << XMLFormatter::NoEscapes 
                                            << chAmpersand << child->getNodeName() << chSemiColon 
                                            << XMLFormatter::AttrEscapes;
                            child = child->getNextSibling();
                        }
                    }
                    else
                        *fFormatter  << attribute->getNodeValue();
                    *fFormatter  << XMLFormatter::NoEscapes
                                 << chDoubleQuote;
                } // end of for
            } // end of FILTER_ACCEPT

            level++;

            // FILTER_SKIP may start from here

            //
            //  Test for the presence of children, which includes both
            //  text content and nested elements.
            //
            DOMNodeSPtr child = nodeToWrite->getFirstChild();
            if (child != 0)
            {
                // There are children. Close start-tag, and output children.
                // No escapes are legal here
                if (filterAction == DOMNodeFilter::FILTER_ACCEPT)
                    *fFormatter << XMLFormatter::NoEscapes << chCloseAngle;

                while( child != 0)
                {
                    processNode(child, level);
                    child = child->getNextSibling();
                }

                level--;

                if (filterAction == DOMNodeFilter::FILTER_ACCEPT)
                {
                    //if we are not on the same line as when we started
                    //this node then print a new line and indent
                    if(nodeLine != fCurrentLine)
                    {
                        if (!lineFeedInTextNodePrinted)
                        {
                            printNewLine();
                        }
                        else
                        {
                            lineFeedInTextNodePrinted = false;
                        }

                        if(nodeLine != fCurrentLine && level == 0)
                            printNewLine();

                        printIndent(level);
                    }
                    TRY_CATCH_THROW
                    (
                         *fFormatter << XMLFormatter::NoEscapes << gEndElement
                                     << nodeName << chCloseAngle;
                        ,true
                    )

                }
            }
            else
            {
                level--;

                //
                //  There were no children. Output the short form close of
                //  the element start tag, making it an empty-element tag.
                //
                if (filterAction == DOMNodeFilter::FILTER_ACCEPT)
                {
                    TRY_CATCH_THROW
                    (
                        *fFormatter << XMLFormatter::NoEscapes << chForwardSlash << chCloseAngle;
                       , true
                    )
                }
            }

            // remove the namespace map at this level
            if(namespaceMap!=NULL)
                fNamespaceStack->removeLastElement();

            break;
        }
    case DOMNode::ATTRIBUTE_NODE:
        {
            if (checkFilter(nodeToWrite) != DOMNodeFilter::FILTER_ACCEPT)
                break;

            const XMLCh* localName = nodeToWrite->getLocalName();

            // check if this is a DOM Level 1 Node
            if(localName == 0)
                *fFormatter  << XMLFormatter::NoEscapes
                             << nodeToWrite->getNodeName();
            else
                *fFormatter  << XMLFormatter::NoEscapes
                             << chOpenCurly << nodeToWrite->getNamespaceURI() 
                             << chCloseCurly << localName;
            *fFormatter  << chEqual << chDoubleQuote
                         << XMLFormatter::AttrEscapes;
            if (getFeature(ENTITIES_ID))
            {
                DOMNodeSPtr child = nodeToWrite->getFirstChild();
                while( child != 0)
                {
                    if(child->getNodeType()==DOMNode::TEXT_NODE)
                        *fFormatter  << child->getNodeValue();
                    else if(child->getNodeType()==DOMNode::ENTITY_REFERENCE_NODE)
                        *fFormatter << XMLFormatter::NoEscapes 
                                    << chAmpersand << child->getNodeName() << chSemiColon 
                                    << XMLFormatter::AttrEscapes;
                    child = child->getNextSibling();
                }
            }
            else
                *fFormatter  << nodeValue;
            *fFormatter  << XMLFormatter::NoEscapes
                         << chDoubleQuote;

            break;
        }
    case DOMNode::ENTITY_REFERENCE_NODE:
        {
            //"entities"
            //true
            //[required] (default)
            //Keep EntityReference and Entity nodes in the document.

            //false
            //[optional]
            //Remove all EntityReference and Entity nodes from the document,
            //       putting the entity expansions directly in their place.
            //       Text nodes are into "normal" form.
            //Only EntityReference nodes to non-defined entities are kept in the document.

            if (checkFilter(nodeToWrite) != DOMNodeFilter::FILTER_ACCEPT)
                break;

            if (getFeature(ENTITIES_ID))
            {
                TRY_CATCH_THROW
                (
                    *fFormatter << XMLFormatter::NoEscapes << chAmpersand
                                << nodeName << chSemiColon;
                    , true
                )
            }
            else
            {
                // check if the referenced entity is defined or not
                if (nodeToWrite->getOwnerDocument()->getDoctype()->getEntities()->getNamedItem(nodeName))
                {
                    DOMNodeSPtr child;
                    for (child = nodeToWrite->getFirstChild();
                    child != 0;
                    child = child->getNextSibling())
                    {
                        processNode(child, level);
                    }
                }
                else
                {
                    TRY_CATCH_THROW
                   (
                        *fFormatter<<XMLFormatter::NoEscapes<<chAmpersand<<nodeName<<chSemiColon;
                        , true
                    )
                }
            }
            break;
        }

        //
        //  feature:split_cdata_sections     occurence of ]]>   unrep-char
        //  ===============================================================
        //          true                        split            split
        //          false                       fails            fails
        //
    case DOMNode::CDATA_SECTION_NODE:
        {
            if (checkFilter(nodeToWrite) != DOMNodeFilter::FILTER_ACCEPT)
                break;

            if(level == 1)
                printNewLine();

            printNewLine();
            printIndent(level);

            if (getFeature(SPLIT_CDATA_SECTIONS_ID))
            {
                // it is fairly complicated and we process this
                // in a separate function.
                procCdataSection(nodeValue, nodeToWrite, level);
            }
            else
            {
                // search for "]]>", the node value is not supposed to have this
                if (XMLString::patternMatch((XMLCh* const) nodeValue, gEndCDATA) != -1)
                {
                    reportError(nodeToWrite, DOMError::DOM_SEVERITY_FATAL_ERROR, XMLDOMMsg::Writer_NestedCDATA);
                }

                TRY_CATCH_THROW
                (
                    // transcoder throws exception for unrep chars
                    *fFormatter << XMLFormatter::NoEscapes << gStartCDATA << nodeValue << gEndCDATA;
                   , true
                )
            }

            break;
        }

    case DOMNode::COMMENT_NODE:
        {
            if (checkFilter(nodeToWrite) != DOMNodeFilter::FILTER_ACCEPT)
                break;

            if(level == 1)
                printNewLine();

            printNewLine();
            printIndent(level);

            TRY_CATCH_THROW
            (
                *fFormatter << XMLFormatter::NoEscapes << gStartComment
                << nodeValue << gEndComment;
                , true
            )
            break;
        }

    case DOMNode::DOCUMENT_TYPE_NODE:  // Not to be shown to Filter
        {
            const DOMDocumentType *doctype = (const DOMDocumentType *)nodeToWrite;

            fFormatter->setEscapeFlags(XMLFormatter::NoEscapes);

            printNewLine();
            printIndent(level);

            TRY_CATCH_THROW
            (
                *fFormatter << gStartDoctype << nodeName;

                const XMLCh  *id = doctype->getPublicId();
                if (id && *id)
                {
                    *fFormatter << chSpace << gPublic << id << chDoubleQuote;

                    id = doctype->getSystemId();
                    if (id && *id)
                    {
                        *fFormatter << chSpace << chDoubleQuote << id << chDoubleQuote;
                    }
                    else
                    {
                        //
                        // 4.2.2 External Entities
                        // [Definition: If the entity is not internal,
                        //           it is an external entity, declared as follows:]
                        // External Entity Declaration
                        // [75] ExternalID ::= 'SYSTEM' S SystemLiteral
                        //                   | 'PUBLIC' S PubidLiteral S SystemLiteral
                        //
                        reportError(nodeToWrite, DOMError::DOM_SEVERITY_FATAL_ERROR, XMLDOMMsg::Writer_NotRecognizedType);
                        // systemLiteral not found
                    }
                }
                else
                {
                    id = doctype->getSystemId();
                    if (id && *id)
                    {
                        *fFormatter << chSpace << gSystem << id << chDoubleQuote;
                    }
                }

                id = doctype->getInternalSubset();
                if (id && *id)
                {
                    *fFormatter << chSpace << chOpenSquare << id << chCloseSquare;
                }

                *fFormatter << chCloseAngle;
                , true

            ) // end of TRY_CATCH_THROW

            break;
        }

    case DOMNode::ENTITY_NODE:  // Not to be shown to Filter
        {
            //
            // REVISIT: how does the feature "entities" impact
            // entity node?
            //
            printNewLine();
            printIndent(level);

            fFormatter->setEscapeFlags(XMLFormatter::NoEscapes);
            *fFormatter << gStartEntity    << nodeName;

            const XMLCh * id = ((const DOMEntity*)nodeToWrite)->getPublicId();
            if (id)
                *fFormatter << gPublic << id << chDoubleQuote;

            id = ((const DOMEntity*)nodeToWrite)->getSystemId();
            if (id)
                *fFormatter << gSystem << id << chDoubleQuote;

            id = ((const DOMEntity*)nodeToWrite)->getNotationName();
            if (id)
                *fFormatter << gNotation << id << chDoubleQuote;

            *fFormatter << chCloseAngle;

            break;
        }

    default:
        /***
            This is an implementation specific behaviour, we abort if a user derived class has not dealt with
            this node type.
         ***/
        {
            if(!customNodeSerialize(nodeToWrite, level)) {
                reportError(nodeToWrite, DOMError::DOM_SEVERITY_FATAL_ERROR, XMLDOMMsg::Writer_NotRecognizedType);
                // UnreognizedNodeType;
            }
        }

        break;
    }

}

bool DOMWriterImpl::customNodeSerialize(const DOMNode* const, int) {
    return false;
}

//
//
DOMNodeFilter::FilterAction DOMWriterImpl::checkFilter(const DOMNode* const node) const
{
    if (!fFilter ||
        ((fFilter->getWhatToShow() & (1 << (node->getNodeType() - 1))) == 0))
        return DOMNodeFilter::FILTER_ACCEPT;

    //
    // if and only if there is a filter, and it is interested
    // in the node type, then we pass the node to the filter
    // for examination
    //
    return (DOMNodeFilter::FilterAction) fFilter->acceptNode(node);
}


bool DOMWriterImpl::checkFeature(const XMLCh* const featName
                               , bool               toThrow
                               , int&               featureId) const
{
    // check for null and/or empty feature name
    if (!featName || !*featName)
    {
        if (toThrow)
            throw DOMException(DOMException::NOT_FOUND_ERR, 0, fMemoryManager);

        return false;
    }

    featureId = INVALID_FEATURE_ID;

    if (XMLString::equals(featName, XMLUni::fgDOMWRTCanonicalForm))
        featureId = CANONICAL_FORM_ID;
    else if (XMLString::equals(featName, XMLUni::fgDOMWRTDiscardDefaultContent))
        featureId = DISCARD_DEFAULT_CONTENT_ID;
    else if (XMLString::equals(featName, XMLUni::fgDOMWRTEntities))
        featureId = ENTITIES_ID;
    else if (XMLString::equals(featName, XMLUni::fgDOMWRTFormatPrettyPrint))
        featureId = FORMAT_PRETTY_PRINT_ID;
    else if (XMLString::equals(featName, XMLUni::fgDOMWRTNormalizeCharacters))
        featureId = NORMALIZE_CHARACTERS_ID;
    else if (XMLString::equals(featName, XMLUni::fgDOMWRTSplitCdataSections))
        featureId = SPLIT_CDATA_SECTIONS_ID;
    else if (XMLString::equals(featName, XMLUni::fgDOMWRTValidation))
        featureId = VALIDATION_ID;
    else if (XMLString::equals(featName, XMLUni::fgDOMWRTWhitespaceInElementContent))
        featureId = WHITESPACE_IN_ELEMENT_CONTENT_ID;
    else if (XMLString::equals(featName, XMLUni::fgDOMWRTBOM))
        featureId = BYTE_ORDER_MARK_ID;
    else if (XMLString::equals(featName, XMLUni::fgDOMXMLDeclaration))
        featureId = XML_DECLARATION;


    //feature name not resolvable
    if (featureId == INVALID_FEATURE_ID)
    {
        if (toThrow)
            throw DOMException(DOMException::NOT_FOUND_ERR, featName, fMemoryManager);

        return false;
    }

    return true;
}

bool DOMWriterImpl::reportError(const DOMNode* const    errorNode
                              , DOMError::ErrorSeverity errorType
                              , const XMLCh*   const    errorMsg)
{
    bool toContinueProcess = true;   // default value for no error handler

    if (fErrorHandler)
    {
        DOMLocatorImpl  locator(0, 0, (DOMNode* const) errorNode, 0, 0);
        DOMErrorImpl    domError(errorType , errorMsg, &locator);
        toContinueProcess = fErrorHandler->handleError(domError);
    }

    if (errorType != DOMError::DOM_SEVERITY_WARNING)
        fErrorCount++;

    return toContinueProcess;
}

bool DOMWriterImpl::reportError(const DOMNode* const    errorNode
                              , DOMError::ErrorSeverity errorType
                              , XMLDOMMsg::Codes        toEmit)
{
    const unsigned int msgSize = 1023;
    XMLCh errText[msgSize + 1];

    DOMImplementationImpl::getMsgLoader4DOM()->loadMsg(toEmit, errText, msgSize);

    bool toContinueProcess = true;   // default value for no error handler

    if (fErrorHandler)
    {
        DOMLocatorImpl  locator(0, 0, (DOMNode* const) errorNode, 0, 0);
        DOMErrorImpl    domError(errorType , errText, &locator);
        toContinueProcess = fErrorHandler->handleError(domError);
    }

    if (errorType != DOMError::DOM_SEVERITY_WARNING)
        fErrorCount++;

    if (errorType == DOMError::DOM_SEVERITY_FATAL_ERROR || !toContinueProcess)
        throw toEmit;

    return toContinueProcess;
}

//
//
//
void DOMWriterImpl::procCdataSection(const XMLCh*   const nodeValue
                                   , const DOMNode* const nodeToWrite
                                   , int level)
{
    /***
     * Append a ']]>' at the end
     */
    int len = XMLString::stringLen(nodeValue);
    XMLCh* repNodeValue = (XMLCh*) fMemoryManager->allocate
    (
        (len + offset + 1) * sizeof(XMLCh)
    );//new XMLCh [len + offset + 1];
    XMLString::copyString(repNodeValue, nodeValue);
    XMLString::catString(repNodeValue, gEndCDATA);
    ArrayJanitor<XMLCh>  jName(repNodeValue, fMemoryManager);

    XMLCh* curPtr  = (XMLCh*) repNodeValue;
    XMLCh* nextPtr = 0;
    int    endTagPos = -1;

    bool   endTagFound = true;

    while (endTagFound)
    {
        endTagPos = XMLString::patternMatch(curPtr, gEndCDATA);
        if (endTagPos != -1)
        {
            nextPtr = curPtr + endTagPos + offset;  // skip the ']]>'
            *(curPtr + endTagPos) = chNull;         //nullify the first ']'
            if (endTagPos != len)
                reportError(nodeToWrite, DOMError::DOM_SEVERITY_WARNING, XMLDOMMsg::Writer_NestedCDATA);
            len = len - endTagPos - offset;
        }
        else
        {
            endTagFound = false;
        }

        /***
            to check ]]>]]>
        ***/
        if (endTagPos == 0)
        {
            printNewLine();
            printIndent(level);
            TRY_CATCH_THROW
            (
                *fFormatter << XMLFormatter::NoEscapes << gStartCDATA << gEndCDATA;
                , true
            )
        }
        else
        {
            procUnrepCharInCdataSection(curPtr, nodeToWrite, level);
        }

        if (endTagFound)
        {
            *(nextPtr - offset) = chCloseSquare;   //restore the first ']'
            curPtr = nextPtr;
        }
    }

    return;
}

//
//
//
void DOMWriterImpl::procUnrepCharInCdataSection(const XMLCh*   const nodeValue
                                              , const DOMNode* const nodeToWrite
                                              , int level)
{
    //
    //  We have to check each character and see if it could be represented.
    //  As long as it can, we just keep up with where we started and how
    //  many chars we've checked. When we hit an unrepresentable one, we
    //  stop, transcode everything we've collected, then start handling
    //  the unrepresentables via char refs. We repeat this until we get all
    //  the chars done.
    //
    const XMLCh*    srcPtr = nodeValue;
    const XMLCh*    endPtr = nodeValue +  XMLString::stringLen(nodeValue);

    // Set up the common part of the buffer that we build char refs into
    XMLCh tmpBuf[32];
    tmpBuf[0] = chAmpersand;
    tmpBuf[1] = chPound;
    tmpBuf[2] = chLatin_x;

    while (srcPtr < endPtr)
    {
        const XMLCh* tmpPtr = srcPtr;
        while (tmpPtr < endPtr)
        {
            if (fFormatter->getTranscoder()->canTranscodeTo(*tmpPtr))
                tmpPtr++;
            else
                break;
        }

        if (tmpPtr > srcPtr)
        {
            printNewLine();
            printIndent(level);
            TRY_CATCH_THROW
           (
                *fFormatter << XMLFormatter::NoEscapes << gStartCDATA;
                , true
            )

            // We got at least some chars that can be done normally
            fFormatter->formatBuf
            (
                srcPtr
                , tmpPtr - srcPtr
                , XMLFormatter::NoEscapes
                , XMLFormatter::UnRep_Fail
            );

            TRY_CATCH_THROW
            (
                *fFormatter << XMLFormatter::NoEscapes << gEndCDATA;
                , true
            )

            // Update the source pointer to our new spot
            srcPtr = tmpPtr;
        }
        else
        {
            //
            //  We hit something unrepresentable. So continue forward doing
            //  char refs until we hit something representable again or the
            //  end of input.
            //

            // one warning for consective unrep chars
            reportError(nodeToWrite, DOMError::DOM_SEVERITY_WARNING, XMLDOMMsg::Writer_NotRepresentChar);

            while (srcPtr < endPtr)
            {
                // Build a char ref for the current char
                XMLString::binToText(*srcPtr, &tmpBuf[3], 8, 16, fMemoryManager);
                const unsigned int bufLen = XMLString::stringLen(tmpBuf);
                tmpBuf[bufLen] = chSemiColon;
                tmpBuf[bufLen+1] = chNull;

                // And now call recursively back to our caller to format this
                fFormatter->formatBuf
                (
                    tmpBuf
                    , bufLen + 1
                    , XMLFormatter::NoEscapes
                    , XMLFormatter::UnRep_Fail
                );

                // Move up the source pointer and break out if needed
                srcPtr++;
                if (fFormatter->getTranscoder()->canTranscodeTo(*srcPtr))
                    break;
            }
        }
    }
}

void DOMWriterImpl::processNode(const DOMNode* const nodeToWrite)
{
    processNode(nodeToWrite, 0);
}

bool DOMWriterImpl::canSetFeature(const int featureId
                                       , bool      val) const
{
    return featuresSupported[2*featureId + (val? 0: 1)];
}

void DOMWriterImpl::printNewLine()
{
    if (getFeature(FORMAT_PRETTY_PRINT_ID))
    {
        fCurrentLine++;
        *fFormatter << fNewLineUsed;
    }
}

void DOMWriterImpl::printIndent(int level) const
{
    if (getFeature(FORMAT_PRETTY_PRINT_ID))
    {
        if (lastWhiteSpaceInTextNode)
        {
            level -= lastWhiteSpaceInTextNode/2; // two chSpaces equals one indent level
            lastWhiteSpaceInTextNode = 0;
            // if lastWhiteSpaceInTextNode/2 is greater than level, then
            // it means too many spaces have been written to the
            // output stream and we can no longer indent properly
        }

        for(int i = 0; i < level; i++)
            *fFormatter << chSpace << chSpace;
    }
}

void DOMWriterImpl::release()
{
    DOMWriterImpl* writer = (DOMWriterImpl*) this;
    delete writer;
}

void DOMWriterImpl::processBOM()
{
    // if the feature is not set, don't output bom
    if (!getFeature(BYTE_ORDER_MARK_ID))
        return;

    if ((XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16LEncodingString)  == 0) ||
        (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16LEncodingString2) == 0)  )
    {
        fFormatter->writeBOM(BOM_utf16le, 2);
    }
    else if ((XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16BEncodingString)  == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16BEncodingString2) == 0)  )
    {
        fFormatter->writeBOM(BOM_utf16be, 2);
    }
    else if ((XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16EncodingString)  == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16EncodingString2) == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16EncodingString3) == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16EncodingString4) == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16EncodingString5) == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16EncodingString6) == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUTF16EncodingString7) == 0)  ) 
    {
#if defined(ENDIANMODE_LITTLE)
            fFormatter->writeBOM(BOM_utf16le, 2);
#elif defined(ENDIANMODE_BIG)
            fFormatter->writeBOM(BOM_utf16be, 2);
#endif
    }
    else if ((XMLString::compareIStringASCII(fEncoding, XMLUni::fgUCS4LEncodingString)  == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUCS4LEncodingString2) == 0)  )
    {
        fFormatter->writeBOM(BOM_ucs4le, 4);
    }
    else if ((XMLString::compareIStringASCII(fEncoding, XMLUni::fgUCS4BEncodingString)  == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUCS4BEncodingString2) == 0)  )
    {
        fFormatter->writeBOM(BOM_ucs4be, 4);
    }
    else if ((XMLString::compareIStringASCII(fEncoding, XMLUni::fgUCS4EncodingString)  == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUCS4EncodingString2) == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUCS4EncodingString3) == 0) ||
             (XMLString::compareIStringASCII(fEncoding, XMLUni::fgUCS4EncodingString4) == 0)  )
    {
#if defined(ENDIANMODE_LITTLE)
        fFormatter->writeBOM(BOM_ucs4le, 4);
#elif defined(ENDIANMODE_BIG)
        fFormatter->writeBOM(BOM_ucs4be, 4);
#endif
    }

    return;

}

XERCES_CPP_NAMESPACE_END

