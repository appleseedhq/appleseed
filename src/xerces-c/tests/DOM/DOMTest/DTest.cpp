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
 * $Log$
 * Revision 1.42  2004/09/08 13:57:02  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.41  2004/09/02 15:11:42  cargilld
 * Add OutOfMemoryException block to tests.
 *
 * Revision 1.40  2004/03/02 13:53:50  amassari
 * Added test for bug# 26919
 *
 * Revision 1.39  2003/02/05 18:55:19  tng
 * [Bug 11915] Utility for freeing memory.
 *
 * Revision 1.38  2003/01/29 20:04:09  gareth
 * testing for DOMTypeInfo
 *
 * Revision 1.37  2003/01/03 17:09:02  tng
 * delete the parser when done, avoid memory leak report with the test case
 *
 * Revision 1.36  2002/11/21 22:12:08  tng
 * fix typo where isID should be isId
 *
 * Revision 1.35  2002/11/21 14:24:39  gareth
 * Tests added for isId, setIdAttribute, setIdAttributeNS, setIdAttributeNode
 *
 * Revision 1.34  2002/11/12 17:52:01  tng
 * Test update: do not issue "Test Run Successfully" if there was an error.
 *
 * Revision 1.33  2002/09/23 21:00:14  tng
 * DOM L3: fix to isDefaultNamespace.  Patch from Gareth Reakes.
 *
 * Revision 1.32  2002/09/23 20:09:23  tng
 * DOM L3: Test baseURI with different parser's setting.
 *
 * Revision 1.31  2002/09/23 18:27:48  tng
 * DOM L3: Test baseURI.   Added by Gareth Reakes and Thomas Ford.
 *
 * Revision 1.30  2002/08/21 20:59:11  tng
 * release the cloned document.
 *
 * Revision 1.29  2002/08/19 19:56:08  tng
 * DOM L3: test DOMNode::isDefaultNamespace.   Added by Gareth Reakes.
 *
 * Revision 1.28  2002/08/16 19:22:29  tng
 * Test DOM L3 lookupNamespacePrefix, lookupNamespaceURI support.   Added by Gareth Reakes.
 *
 * Revision 1.27  2002/08/16 16:03:02  tng
 * [Bug 11360] Release user data using handler.
 *
 * Revision 1.26  2002/08/16 13:49:56  tng
 * [Bug 11360] Release user data using handler.
 *
 * Revision 1.25  2002/08/09 20:21:21  tng
 * Test DOM L3 compareTreePosition.
 *
 * Revision 1.24  2002/07/04 15:35:15  tng
 * DOM L3: Test DOMDocument::renameNode
 *
 * Revision 1.23  2002/06/27 18:42:16  tng
 * DOM L3: Test DOMNode::isSameNode and DOMNode::isEqualNode
 *
 * Revision 1.22  2002/06/25 16:22:52  tng
 * DOM L3: use release()
 *
 * Revision 1.21  2002/06/12 18:31:17  tng
 * DOM L3: test the DOMUserDataHandler and set/getUserData
 *
 * Revision 1.20  2002/06/03 20:51:21  tng
 * DOM L3: Add DOMImplementationRegistry and DOMImplementationSource
 *
 * Revision 1.19  2002/05/21 18:50:16  tng
 * Test case update: modify to use the latest DOM interface
 *
 * Revision 1.6  2002/04/01 21:04:00  tng
 * According to DOM spec, setNodeValue by default is no-op.
 *
 * Revision 1.5  2002/03/14 21:59:29  tng
 * Run methods test[NodeType] in the IDOMTest and other fixes.
 *
 * Revision 1.4  2002/02/01 22:44:24  peiyongz
 * sane_include
 *
 * Revision 1.3  2001/12/07 01:48:27  tng
 * [Bug 1959] setNodeValue throws exception when spec specifies NOP.
 *
 * Revision 1.2  2001/11/23 16:16:52  tng
 * Elimiate compiler warning Warning: String literal converted to char* in initialization.
 *
 * Revision 1.1  2001/08/09 19:28:47  tng
 * Port test case DOMTest to  IDOMTest
 *
 */



/**
 * This class tests methods for XML DOM implementation
 * DOMException errors are tested by calls to DOMExceptionsTest from: Main, docBuilder...
 *
 */

#include <stdio.h>
#include "DTest.h"
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLException.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>

#define EXCEPTIONSTEST(operation, expectedException, resultFlag, testNum) \
    {                                                               \
        try                                                         \
        {                                                           \
            operation;                                              \
            fprintf(stderr, "Exceptions Test # %d: no Exception thrown->\n", testNum); \
        }                                                           \
        catch (DOMException &e)                                 \
        {                                                           \
            if (e.code != expectedException) {                      \
                fprintf(stderr, "Exceptions Test # %d: wrong DOMException thrown->\n", \
                    testNum);                                       \
                resultFlag = false;                                 \
            }                                                       \
        }                                                           \
        catch (...)                                                 \
        {                                                           \
            fprintf(stderr, "Exceptions Test # %d: unknown exception thrown->\n",    \
                testNum);                                           \
            resultFlag = false;                                     \
        }                                                           \
    }

#define USERDATAHANDLERTEST(userhandler, uoperation, ukey, udata, usrc, udst, uline) \
    if (userhandler.getCurrentType() != uoperation) {\
        fprintf(stderr, "DOMUserDataHandler::handler's operationType does not work in line %i\n", uline); \
        OK = false; \
    } \
    if (XMLString::compareString(userhandler.getCurrentKey(), ukey)) {\
        fprintf(stderr, "DOMUserDataHandler::handler's key does not work in line %i\n", uline); \
        OK = false; \
    } \
    if (userhandler.getCurrentData() != udata) {\
        fprintf(stderr, "DOMUserDataHandler::handler's data does not work in line %i\n", uline); \
        OK = false; \
    } \
    if (userhandler.getCurrentSrc() != usrc) {\
        fprintf(stderr, "DOMUserDataHandler::handler's src does not work in line %i\n", uline); \
        OK = false; \
    } \
    if (userhandler.getCurrentDst() != udst) {\
        fprintf(stderr, "DOMUserDataHandler::handler's dst does not work in line %i\n", uline); \
        OK = false; \
    }


#define LOOKUPDEFAULTNSTEST(thisNode, uri, pass, line) \
    if(thisNode->isDefaultNamespace(uri)) { \
        if(!pass) { \
            fprintf(stderr, "DOMNode::isDefaultNamespace returned true in line %i\n", line); \
            OK = false; \
        } \
    } \
    else { \
        if(pass) { \
            fprintf(stderr, "DOMNode::isDefaultNamespace returned false in line %i\n", line); \
            OK = false; \
        } \
    }


#define LOOKUPNSTEST(thisNode, prefix, uri, pass, line) \
    prefixResult = XMLString::compareString(thisNode->lookupNamespacePrefix(uri, false), prefix); \
    prefixResult2 = XMLString::compareString(thisNode->lookupNamespacePrefix(uri, true), prefix); \
    uriResult = XMLString::compareString(thisNode->lookupNamespaceURI(prefix), uri); \
    if(pass) { \
        if(prefixResult != 0) { \
        fprintf(stderr, "DOMNode::lookupNamespacePrefix does not work in line %i\n", line); \
        OK = false; \
        } \
        if(prefixResult2 != 0) { \
        fprintf(stderr, "DOMNode::lookupNamespacePrefix does not work in line %i\n", line); \
        OK = false; \
        } \
            if(uriResult != 0) { \
        fprintf(stderr, "DOMNode::lookupNamespaceURI does not work in line %i\n", line); \
        OK = false;\
            } \
        } \
    else { \
        if(prefixResult == 0) { \
        fprintf(stderr, "DOMNode::lookupNamespacePrefix does not work in line %i\n", line); \
        OK = false; \
        } \
        if(prefixResult2 == 0) { \
        fprintf(stderr, "DOMNode::lookupNamespacePrefix does not work in line %i\n", line); \
        OK = false; \
        } \
            if(uriResult == 0) { \
        fprintf(stderr, "DOMNode::lookupNamespaceURI does not work in line %i\n", line); \
        OK = false; \
            } \
        } \

int prefixResult;
int prefixResult2;
int uriResult;

#define COMPARETREEPOSITIONTEST(thisNode, otherNode, position, line) \
    myposition = thisNode->compareTreePosition(otherNode); \
    if (position == DOMNode::TREE_POSITION_DISCONNECTED) {  \
        if ((myposition & DOMNode::TREE_POSITION_DISCONNECTED) != 0) {\
            fprintf(stderr, "DOMNode::compareTreePosition does not work in line %i\n", line); \
            OK = false; \
        } \
    } \
    else if ((myposition & position) == 0) {\
        fprintf(stderr, "DOMNode::compareTreePosition does not work in line %i\n", line); \
        OK = false; \
    }

// temp position for compareTreePosition
short myposition;

//temp XMLCh String Buffer
XMLCh tempStr[4000];
XMLCh tempStr2[4000];
XMLCh tempStr3[4000];
XMLCh tempStr4[4000];
XMLCh tempStr5[4000];

//DOMUserDataHandler
myUserDataHandler userhandler;

DOMElement*                 DOMTest::testElementNode;
DOMAttr*                    DOMTest::testAttributeNode;
DOMText*                    DOMTest::testTextNode;
DOMCDATASection*            DOMTest::testCDATASectionNode;
DOMEntityReference*         DOMTest::testEntityReferenceNode;
DOMEntity*                  DOMTest::testEntityNode;
DOMProcessingInstruction*   DOMTest::testProcessingInstructionNode;
DOMComment*                 DOMTest::testCommentNode;
DOMDocument*                DOMTest::testDocumentNode;
DOMDocumentType*            DOMTest::testDocumentTypeNode;
DOMDocumentFragment*        DOMTest::testDocumentFragmentNode;
DOMNotation*                DOMTest::testNotationNode;

/**
 *
 *
 */

DOMTest::DOMTest()
{
};


/**
 *
 * @return DOMDocument
 *
 */
DOMDocument* DOMTest::createDocument() {
    XMLCh coreStr[100];
    XMLString::transcode("Core",coreStr,99);

    DOMImplementation* impl = DOMImplementationRegistry::getDOMImplementation(coreStr);
    return impl->createDocument();
};


/**
 *
 * @return DOMDocumentType
 * @param name XMLCh*
 *
 */
DOMDocumentType* DOMTest::createDocumentType(DOMDocument* doc, XMLCh* name) {
    return doc->createDocumentType(name);    //Replace with a DOMDocumentType* creator
};


/**
 *
 * @return org.w3c.dom.DOMEntity
 * @param doc org.w3c.dom.DOMDocument
 * @param name XMLCh*
 *
 */
DOMEntity* DOMTest::createEntity(DOMDocument* doc, XMLCh* name) {
    return doc->createEntity(name);
};



/**
 *
 * @return org.w3c.dom.DOMNotation
 * @param doc org.w3c.dom.DOMDocument
 * @param name XMLCh*
 *
 */
DOMNotation* DOMTest::createNotation(DOMDocument* doc, XMLCh* name) {
    return doc->createNotation(name);
};


/**
 * This method builds test documents for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 * @param name document's name
 * @param type document's type
 *
 */
bool DOMTest::docBuilder(DOMDocument* document, XMLCh* nameIn)
{
    XMLCh* name = XMLString::replicate(nameIn);

    DOMDocument* doc = document;
    bool OK = true;

    //name + "FirstElement"
    XMLString::transcode("FirstElement", tempStr2, 3999);
    XMLString::copyString(tempStr, name);
    XMLString::catString(tempStr, tempStr2);

    DOMElement* docFirstElement = doc->createElement(tempStr);
    doc->appendChild(docFirstElement);

    //name + "FirstElement", name + "firstElement"
    XMLString::catString(tempStr, name);
    XMLString::transcode("FirstElement", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    XMLString::catString(tempStr2, name);
    XMLString::transcode("firstElement", tempStr3, 3999);
    XMLString::catString(tempStr2, tempStr3);
    docFirstElement->setAttribute(tempStr, tempStr2);
    DOMAttr* docFirstElementAttr = docFirstElement->getAttributeNode(tempStr);

    //name + "TargetProcessorChannel" + "This is " + doc->getNodeName() + "'s processing instruction");
    XMLString::copyString(tempStr, name);
    XMLString::transcode("TargetProcessorChannel", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    XMLString::transcode("This is ", tempStr2, 3999);
    XMLString::catString(tempStr2, doc->getNodeName());
    XMLString::transcode("'s processing instruction", tempStr3, 3999);
    XMLString::catString(tempStr2, tempStr3);

    DOMProcessingInstruction* docProcessingInstruction = doc->createProcessingInstruction(tempStr, tempStr2);
    docFirstElement->appendChild(docProcessingInstruction);

    //name + "TestBody"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("TestBody", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMElement* docBody = doc->createElement(tempStr);
    docFirstElement->appendChild(docBody);

    //name + "BodyLevel21"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel21", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMElement* docBodyLevel21 = doc->createElement(tempStr);

    //name + "BodyLevel22"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel22", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMElement* docBodyLevel22 = doc->createElement(tempStr);

    //name + "BodyLevel23"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel23", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMElement* docBodyLevel23 = doc->createElement(tempStr);

    //name + "BodyLevel24"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel24", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMElement* docBodyLevel24 = doc->createElement(tempStr);

    docBody->appendChild(docBodyLevel21);
    docBody->appendChild(docBodyLevel22);
    docBody->appendChild(docBodyLevel23);
    docBody->appendChild(docBodyLevel24);

    //name + "BodyLevel31"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel31", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMElement* docBodyLevel31 = doc->createElement(tempStr);

    //name + "BodyLevel32"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel32", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMElement* docBodyLevel32 = doc->createElement(tempStr);

    //name + "BodyLevel33"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel33", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMElement* docBodyLevel33 = doc->createElement(tempStr);

    //name + "BodyLevel34"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel34", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMElement* docBodyLevel34 = doc->createElement(tempStr);

    docBodyLevel21->appendChild(docBodyLevel31);
    docBodyLevel21->appendChild(docBodyLevel32);
    docBodyLevel22->appendChild(docBodyLevel33);
    docBodyLevel22->appendChild(docBodyLevel34);

    //name + "BodyLevel31'sChildTextNode11"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel31'sChildTextNode11", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMText* docTextNode11 = doc->createTextNode(tempStr);

    //name + "BodyLevel31'sChildTextNode12"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel31'sChildTextNode12", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMText* docTextNode12 = doc->createTextNode(tempStr);

    //name + "BodyLevel31'sChildTextNode13"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("BodyLevel31'sChildTextNode13", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMText* docTextNode13 = doc->createTextNode(tempStr);

    //name + "TextNode2"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("TextNode2", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMText* docTextNode2 = doc->createTextNode(tempStr);

    //name + "TextNode3"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("TextNode3", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMText* docTextNode3 = doc->createTextNode(tempStr);

    //name + "TextNode4"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("TextNode4", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMText* docTextNode4 = doc->createTextNode(tempStr);

    docBodyLevel31->appendChild(docTextNode11);
    docBodyLevel31->appendChild(docTextNode12);
    docBodyLevel31->appendChild(docTextNode13);
    docBodyLevel32->appendChild(docTextNode2);
    docBodyLevel33->appendChild(docTextNode3);
    docBodyLevel34->appendChild(docTextNode4);

    //"<![CDATA[<greeting>Hello, world!</greeting>]]>"
    XMLString::transcode("<![CDATA[<greeting>Hello, world!</greeting>]]>", tempStr, 3999);
    DOMCDATASection* docCDATASection = doc->createCDATASection(tempStr);
    docBodyLevel23->appendChild(docCDATASection);

    //"This should be a comment of some kind "
    XMLString::transcode("This should be a comment of some kind ", tempStr, 3999);
    DOMComment* docComment = doc->createComment(tempStr);

    //Test compareTreePosition before append
    COMPARETREEPOSITIONTEST(docFirstElementAttr, docComment, DOMNode::TREE_POSITION_DISCONNECTED, __LINE__);

    docBodyLevel23->appendChild(docComment);

    //"ourEntityNode"
    XMLString::transcode("ourEntityNode", tempStr, 3999);
    DOMEntityReference* docReferenceEntity = doc->createEntityReference(tempStr);
    docBodyLevel24->appendChild(docReferenceEntity);

    DOMTest make;

    //"ourNotationNode"
    XMLString::transcode("ourNotationNode", tempStr, 3999);
    DOMNotation* docNotation = make.createNotation(doc, tempStr);
    DOMNode*  abc1 = doc->getFirstChild();
    DOMDocumentType* docType = (DOMDocumentType*) abc1;
    DOMNode* rem = docType->getNotations()->setNamedItem(docNotation);
    if (rem)
        rem->release();


//***********Do some quick compareTreePosition tests
//The tree now looks like
//
// docFirstElement (has docFirstElementAttr)
//      |
//      |_ docProcessInstruction
//      |
//      |_ docBody
//            |
//            |_ docBodyLevel21
//            |         |
//            |         |_ docBodyLevel31
//            |         |        |
//            |         |        |_ docTextNode11
//            |         |        |
//            |         |        |_ docTextNode12
//            |         |        |
//            |         |        |_ docTextNode13
//            |         |
//            |         |_ docBodyLevel32
//            |                  |
//            |                  |_ docTextNode2
//            |
//            |_ docBodyLevel22
//            |         |
//            |         |_ docBodyLevel33
//            |         |        |
//            |         |        |_ docTextNode3
//            |         |
//            |         |
//            |         |_ docBodyLevel34
//            |                  |
//            |                  |_ docTextNode4
//            |
//            |_ docBodyLevel23
//            |         |
//            |         |_ docCDATASection
//            |         |
//            |         |_ docComment
//            |
//            |_ docBodyLevel24
//                      |
//                      |_ docReferenceEntity
//

    COMPARETREEPOSITIONTEST(docProcessingInstruction, docBody, DOMNode::TREE_POSITION_FOLLOWING, __LINE__);
    COMPARETREEPOSITIONTEST(docBodyLevel24, docProcessingInstruction, DOMNode::TREE_POSITION_PRECEDING, __LINE__);
    COMPARETREEPOSITIONTEST(docBodyLevel23, docBodyLevel21, DOMNode::TREE_POSITION_PRECEDING, __LINE__);
    COMPARETREEPOSITIONTEST(docBodyLevel21, docTextNode11, DOMNode::TREE_POSITION_DESCENDANT, __LINE__);
    COMPARETREEPOSITIONTEST(docCDATASection, docFirstElement, DOMNode::TREE_POSITION_ANCESTOR, __LINE__);
    COMPARETREEPOSITIONTEST(docFirstElement, docFirstElement, DOMNode::TREE_POSITION_SAME_NODE, __LINE__);
    COMPARETREEPOSITIONTEST(docFirstElement, docFirstElement, DOMNode::TREE_POSITION_EQUIVALENT, __LINE__);
    COMPARETREEPOSITIONTEST(docReferenceEntity, docFirstElement, DOMNode::TREE_POSITION_ANCESTOR, __LINE__);
    COMPARETREEPOSITIONTEST(docFirstElementAttr, docFirstElement, DOMNode::TREE_POSITION_PRECEDING, __LINE__);
    COMPARETREEPOSITIONTEST(docFirstElementAttr, docProcessingInstruction, DOMNode::TREE_POSITION_FOLLOWING, __LINE__);
    COMPARETREEPOSITIONTEST(docProcessingInstruction, docFirstElementAttr, DOMNode::TREE_POSITION_PRECEDING, __LINE__);
    COMPARETREEPOSITIONTEST(docFirstElementAttr, doc, DOMNode::TREE_POSITION_PRECEDING, __LINE__);
    COMPARETREEPOSITIONTEST(doc, docFirstElementAttr, DOMNode::TREE_POSITION_FOLLOWING, __LINE__);
    COMPARETREEPOSITIONTEST(docBodyLevel21, docBodyLevel22, DOMNode::TREE_POSITION_FOLLOWING, __LINE__);

    COMPARETREEPOSITIONTEST(docNotation, docFirstElement, DOMNode::TREE_POSITION_DISCONNECTED, __LINE__);


    //now do some lookupNamespaceURI and lookupNamespacePrefix
    //first lets add some attributes
    XMLString::transcode("http://www.w3.org/2000/xmlns/", tempStr, 3999);
    XMLString::transcode("xmlns:pre1", tempStr2, 3999);
    XMLString::transcode("pre1URI", tempStr3, 3999);
    XMLString::transcode("pre1", tempStr4, 3999);

    DOMAttr *attr1 = doc->createAttributeNS(tempStr, tempStr2);
    attr1->setValue(tempStr3);
    docFirstElement->setAttributeNodeNS(attr1);


    LOOKUPNSTEST(docProcessingInstruction, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel24, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel23, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel21, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel31, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel32, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docCDATASection, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docFirstElement, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docReferenceEntity, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docFirstElementAttr, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(doc, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docNotation, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(docTextNode2, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docTextNode4, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docComment, tempStr4, tempStr3, true, __LINE__);

    XMLString::transcode("xmlns:pre2", tempStr2, 3999);
    XMLString::transcode("pre2URI", tempStr3, 3999);
    XMLString::transcode("pre2", tempStr4, 3999);

    DOMAttr *attr2 = doc->createAttributeNS(tempStr, tempStr2);
    attr2->setValue(tempStr3);
    docBodyLevel21->setAttributeNodeNS(attr2);


    LOOKUPNSTEST(docProcessingInstruction, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(docBodyLevel24, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(docBodyLevel23, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(docBodyLevel21, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel31, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel32, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docCDATASection, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(docFirstElement, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(docReferenceEntity, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(docFirstElementAttr, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(doc, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(docNotation, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(docComment, tempStr4, tempStr3, false, __LINE__);
    LOOKUPNSTEST(docTextNode2, tempStr4, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docTextNode4, tempStr4, tempStr3, false, __LINE__);


    XMLString::transcode("xmlns", tempStr2, 3999);
    XMLString::transcode("default", tempStr3, 3999);
    XMLString::transcode("", tempStr4, 3999);


    DOMAttr *attr3 = doc->createAttributeNS(tempStr, tempStr2);
    attr3->setValue(tempStr3);
    docFirstElement->setAttributeNodeNS(attr3);

    LOOKUPNSTEST(docProcessingInstruction, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel24, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel23, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel21, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel31, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docBodyLevel32, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docCDATASection, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docFirstElement, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docReferenceEntity, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docFirstElementAttr, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(doc, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docComment, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docTextNode2, 0, tempStr3, true, __LINE__);
    LOOKUPNSTEST(docTextNode4, 0, tempStr3, true, __LINE__);

    //this has to be done separately because negative lookup is the same as default ns lookup!!!

    prefixResult = XMLString::compareString(docNotation->lookupNamespacePrefix(tempStr3, false), 0);
    prefixResult2 = XMLString::compareString(docNotation->lookupNamespacePrefix(tempStr3, true), 0);

    uriResult = XMLString::compareString(docNotation->lookupNamespaceURI(0), 0);
    if(prefixResult != 0) {
        fprintf(stderr, "DOMNode::lookupNamespacePrefix does not work in line %i\n", __LINE__);
        OK = false;
    }
    if(prefixResult2 != 0) {
        fprintf(stderr, "DOMNode::lookupNamespacePrefix does not work in line %i\n", __LINE__);
        OK = false;
    }
    if(uriResult != 0) {
        fprintf(stderr, "DOMNode::lookupNamespacePrefix does not work in line %i\n", __LINE__);
        OK = false;
    }

    XMLString::transcode("notset", tempStr3, 3999);

    LOOKUPDEFAULTNSTEST(docProcessingInstruction, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel24, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel23, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel21, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel31, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel32, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docCDATASection, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docFirstElement, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docReferenceEntity, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docFirstElementAttr, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(doc, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docNotation, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docComment, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docTextNode2, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docTextNode4, tempStr3, false, __LINE__);


    XMLString::transcode("default", tempStr3, 3999);

    LOOKUPDEFAULTNSTEST(docProcessingInstruction, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel24, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel23, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel21, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel31, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel32, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docCDATASection, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docFirstElement, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docReferenceEntity, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docFirstElementAttr, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(doc, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docNotation, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docComment, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docTextNode2, tempStr3, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docTextNode4, tempStr3, false, __LINE__);

    //remove the xmlns attr
    docFirstElement->removeAttributeNode(attr3);

    LOOKUPDEFAULTNSTEST(docProcessingInstruction, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel24, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel23, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel21, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel31, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docBodyLevel32, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docCDATASection, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docFirstElement, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docReferenceEntity, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docFirstElementAttr, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(doc, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docNotation, 0, false, __LINE__);
    LOOKUPDEFAULTNSTEST(docComment, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docTextNode2, 0, true, __LINE__);
    LOOKUPDEFAULTNSTEST(docTextNode4, 0, true, __LINE__);

    XMLString::transcode("someSpecialURI", tempStr3, 3999);
    XMLString::transcode("newNameForEle", tempStr4, 3999);
    DOMElement *ele = doc->createElementNS(tempStr3, tempStr4);
    docFirstElement->insertBefore(ele, docFirstElement->getFirstChild());

    // test for bug# 26919
    docFirstElement->insertBefore(docFirstElement->getFirstChild(), docFirstElement->getFirstChild());

    //a test for lookup when xmlns is not set so we take the fact that there is no prefix to be confimation
    LOOKUPDEFAULTNSTEST(ele, tempStr3, true, __LINE__);

    docFirstElement->removeAttributeNode(attr1);
    docBodyLevel21->removeAttributeNode(attr2);
    docFirstElement->removeChild(ele);

//***********Following are for errorTests
    DOMDocumentFragment* docDocFragment = doc->createDocumentFragment();

    //name + "docTextNode3"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("docTextNode3", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMText* docNode3 = doc->createTextNode(tempStr);

    //name + "docTextNode4"
    XMLString::copyString(tempStr, name);
    XMLString::transcode("docTextNode4", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    DOMText* docNode4 = doc->createTextNode(tempStr);

    //"ourEntityNode"
    XMLString::transcode("ourEntityNode", tempStr, 3999);
    DOMNode*   abc2 =  doc->getDoctype()->getEntities()->getNamedItem(tempStr);  // Get the DOMEntity* node
    DOMEntity* docEntity = (DOMEntity*) abc2;
    DOMNode*   abc3 = doc->getFirstChild(); // Get the DOMDocumentType* node
    DOMDocumentType* docDocType = (DOMDocumentType*) abc3;
    DOMNode*   abc4 = doc->getLastChild()->getLastChild()->getLastChild()->getFirstChild();
    DOMEntityReference* entityReferenceText = (DOMEntityReference*) abc4;

    //"entityReferenceText information"
    XMLString::transcode("entityReferenceText information", tempStr, 3999);
    DOMText* entityReferenceText2 = doc->createTextNode(tempStr);
//************************************************ ERROR TESTS
    DOMTest tests;

    EXCEPTIONSTEST(document->appendChild(docBody), DOMException::HIERARCHY_REQUEST_ERR, OK,  1);

    EXCEPTIONSTEST(document->appendChild(docBody), DOMException::HIERARCHY_REQUEST_ERR, OK, 2);
    EXCEPTIONSTEST(docNode3->appendChild(docNode4), DOMException::HIERARCHY_REQUEST_ERR, OK, 3);
    // EXCEPTIONSTEST(doc->insertBefore(docEntity, docFirstElement), DOMException::HIERARCHY_REQUEST_ERR, OK, 4);
    EXCEPTIONSTEST(doc->replaceChild(docCDATASection, docFirstElement), DOMException::HIERARCHY_REQUEST_ERR, OK, 5);

    //"This shouldn't work!"
    XMLString::transcode("entityReferenceText information", tempStr, 3999);

    // The following setNodeValue tests are not invalid
    // According to DOM spec, if the node value is defined to be null in the DOM spec, setting it has no effect.
    // Only those node type that are supposed to have node value, exception will be raised if the node is readonly.
    // EXCEPTIONSTEST(docFirstElement->setNodeValue(tempStr), DOMException::NO_MODIFICATION_ALLOWED_ERR, OK, 6);
    // EXCEPTIONSTEST(docReferenceEntity->setNodeValue(tempStr), DOMException::NO_MODIFICATION_ALLOWED_ERR, OK, 7);
    // EXCEPTIONSTEST(docEntity->setNodeValue(tempStr), DOMException::NO_MODIFICATION_ALLOWED_ERR, OK, 8);
    // EXCEPTIONSTEST(doc->setNodeValue(tempStr), DOMException::NO_MODIFICATION_ALLOWED_ERR, OK, 9);
    // EXCEPTIONSTEST(docDocType->setNodeValue(tempStr), DOMException::NO_MODIFICATION_ALLOWED_ERR, OK, 10);
    // EXCEPTIONSTEST(docDocFragment->setNodeValue(tempStr), DOMException::NO_MODIFICATION_ALLOWED_ERR, OK, 11);
    // EXCEPTIONSTEST(docNotation->setNodeValue(tempStr), DOMException::NO_MODIFICATION_ALLOWED_ERR, OK, 12);
    EXCEPTIONSTEST(docReferenceEntity->appendChild(entityReferenceText2 ), DOMException::NO_MODIFICATION_ALLOWED_ERR , OK, 13);
    EXCEPTIONSTEST(docBodyLevel32->insertBefore(docTextNode11,docBody ), DOMException::NOT_FOUND_ERR, OK, 14);
    EXCEPTIONSTEST(docBodyLevel32->removeChild(docFirstElement), DOMException::NOT_FOUND_ERR, OK, 15);
    EXCEPTIONSTEST(docBodyLevel32->replaceChild(docTextNode11,docFirstElement ), DOMException::NOT_FOUND_ERR, OK, 16);

    XMLString::release(&name);

    return OK;


//!! Throws a NOT_FOUND_ERR ********

     // docBodyLevel32->getAttributes()->removeNamedItem(testAttribute->getName());    16  // To test removeNamedItem

};  //END OF DOCBUILDER



/**
 * @param document org.w3c.dom.DOMDocument
 */
void DOMTest::findTestNodes(DOMDocument* document) {
    DOMNode* node = document;
    int nodeCount = 0;

    // Walk the tree until you find and assign all node types needed that exist.
    while (node != 0 && nodeCount < 12)
    {

        switch (node->getNodeType())
    {
        case DOMNode::ELEMENT_NODE :
            if (testElementNode == 0) {testElementNode = (DOMElement*)node; nodeCount++;}
            break;
        case DOMNode::ATTRIBUTE_NODE :
            if (testAttributeNode == 0) {testAttributeNode = (DOMAttr*)node; nodeCount++;}
            break;
        case DOMNode::TEXT_NODE :
            if (testTextNode == 0) {testTextNode = (DOMText*)node; nodeCount++;}
            break;
        case DOMNode::CDATA_SECTION_NODE :
            if (testCDATASectionNode == 0) {testCDATASectionNode = (DOMCDATASection*)node; nodeCount++;}
            break;
        case DOMNode::ENTITY_REFERENCE_NODE :
            if (testEntityReferenceNode == 0) {testEntityReferenceNode = (DOMEntityReference*)node; nodeCount++;}
            break;
        case DOMNode::ENTITY_NODE :
            if (testEntityNode == 0) {testEntityNode = (DOMEntity*)node; nodeCount++;}
            break;
        case DOMNode::PROCESSING_INSTRUCTION_NODE :
            if (testProcessingInstructionNode == 0) {testProcessingInstructionNode = (DOMProcessingInstruction*)node; nodeCount++;}
            break;
        case DOMNode::COMMENT_NODE :
            if (testCommentNode == 0) {testCommentNode = (DOMComment*)node; nodeCount++;}
            break;
        case DOMNode::DOCUMENT_TYPE_NODE :
            if (testDocumentTypeNode == 0) {testDocumentTypeNode = (DOMDocumentType*)node; nodeCount++;}
            break;
        case DOMNode::DOCUMENT_FRAGMENT_NODE :
            if (testDocumentFragmentNode == 0) {testDocumentFragmentNode = (DOMDocumentFragment*)node; nodeCount++;}
            break;
        case DOMNode::NOTATION_NODE :
            if (testNotationNode == 0) {testNotationNode = (DOMNotation*)node; nodeCount++;}
            break;
        case DOMNode::DOCUMENT_NODE :
            if (testDocumentNode == 0) {testDocumentNode = (DOMDocument*)node; nodeCount++;}
            break;
        default:
            ;
    }// End of switch

    }   // End of while
};


/**
 * @param document org.w3c.dom.DOMDocument
 */
void DOMTest::findTestNodes(DOMNode* node) {
    DOMTest test;
    DOMNode*  kid;
    // Walk the tree until you find and assign all node types needed that exist.


    if (node->getFirstChild() != 0)
    {
        kid = node->getFirstChild();
        test.findTestNodes(kid);
    }


    if (node->getNextSibling() != 0)
    {
        kid = node->getNextSibling();
        test.findTestNodes(kid);
    }


    switch (node->getNodeType())
    {
        case DOMNode::ELEMENT_NODE :
            if (testElementNode == 0) {testElementNode = (DOMElement*)node; }
            break;
        case DOMNode::ATTRIBUTE_NODE :
            if (testAttributeNode == 0) {testAttributeNode = (DOMAttr*)node; }
            break;
        case DOMNode::TEXT_NODE :
            if (testTextNode == 0) {testTextNode = (DOMText*)node; }
            break;
        case DOMNode::CDATA_SECTION_NODE :
            if (testCDATASectionNode == 0) {testCDATASectionNode = (DOMCDATASection*)node; }
            break;
        case DOMNode::ENTITY_REFERENCE_NODE :
            if (testEntityReferenceNode == 0) {testEntityReferenceNode = (DOMEntityReference*)node;}
            break;
        case DOMNode::ENTITY_NODE :
            if (testEntityNode == 0) {testEntityNode = (DOMEntity*)node;}
            break;
        case DOMNode::PROCESSING_INSTRUCTION_NODE :
            if (testProcessingInstructionNode == 0) {testProcessingInstructionNode = (DOMProcessingInstruction*)node;}
            break;
        case DOMNode::COMMENT_NODE :
            if (testCommentNode == 0) {testCommentNode = (DOMComment*)node;}
            break;
        case DOMNode::DOCUMENT_TYPE_NODE :
            if (testDocumentTypeNode == 0) {testDocumentTypeNode = (DOMDocumentType*)node; }
            break;
        case DOMNode::DOCUMENT_FRAGMENT_NODE :
            if (testDocumentFragmentNode == 0) {testDocumentFragmentNode = (DOMDocumentFragment*)node;}
            break;
        case DOMNode::NOTATION_NODE :
            if (testNotationNode == 0) {testNotationNode = (DOMNotation*)node;}
            break;
        case DOMNode::DOCUMENT_NODE :
            if (testDocumentNode == 0) {testDocumentNode = (DOMDocument*)node;}
            break;
        default:
            ;
    }// End of switch
};//End of class

/**
 *
 *
 */
int main(int argc, char **argv)
 {
     bool OK = true;

     {
         //  Nest entire test in an inner block.
         //     Reference counting should recover all document
         //     storage when this block exits.

         DOMTest test;
         try {
             XMLPlatformUtils::Initialize();
         }
         catch (const XMLException& toCatch) {
             char *pMessage = XMLString::transcode(toCatch.getMessage());
             fprintf(stderr, "Error during initialization! \n  %s \n", pMessage);
             XMLString::release(&pMessage);
             return -1;
         }

         long avgTime = 0;
         long startTime = 0;//****************Time the whole thing for efficiency of DOM implementation

         // for (int i=0; i< 1000; i++)
         // {
         // AH Revisit  //  startTime = System.currentTimeMillis();
         //     if(!OK)
         //     break;

         DOMDocument* d = test.createDocument();

         XMLString::transcode("testDocument1", tempStr, 3999);
         DOMDocumentType* docDocType = test.createDocumentType(d,tempStr);
         d->appendChild(docDocType);

         XMLString::transcode("ourEntityNode", tempStr, 3999);
         DOMEntity* docEntity = test.createEntity( d, tempStr);
         //Build a branch for entityReference tests
         // DOMText* entityChildText = d.createTextNode("entityChildText information"); //
         // docEntity->appendChild(entityChildText);
         // docDocType->getEntities()->setNamedItem(docEntity);

         XMLString::transcode("d", tempStr3, 3999);
         OK = test.docBuilder(d, tempStr3);

         test.findTestNodes((DOMNode*)d);

         OK = test.testAttr(d);
         OK = test.testCDATASection(d);
         OK = test.testCharacterData(d);
         OK = test.testChildNodeList(d);
         OK = test.testComment(d);
         OK = test.testDeepNodeList(d);
         OK = test.testDocument(d);
         OK = test.testDocumentFragment(d);
         OK = test.testDocumentType(d);
         OK = test.testDOMImplementation(d);
         OK = test.testElement(d);
//         OK = test.testEntity(d);  // Can not test entities;  only parser can create them.
         OK = test.testEntityReference(d);
         OK = test.testNode(d);
         OK = test.testNotation(d);
         OK = test.testPI(d);
         OK = test.testText(d);
         OK = test.testDOMerrors(d);

         // Null out the static object references in class DOMTest,
         // which will recover their storage.
         DOMTest::testElementNode = 0;
         DOMTest::testAttributeNode = 0;
         DOMTest::testTextNode = 0;
         DOMTest::testCDATASectionNode = 0;
         DOMTest::testEntityReferenceNode = 0;
         DOMTest::testEntityNode = 0;
         DOMTest::testProcessingInstructionNode = 0;
         DOMTest::testCommentNode = 0;
         DOMTest::testDocumentNode = 0;
         DOMTest::testDocumentTypeNode = 0;
         DOMTest::testDocumentFragmentNode = 0;
         DOMTest::testNotationNode = 0;

        // we couldn't really test the user data handler call as the userhandler is already
        // deleted when the release() is done, but still set it to test the code internally
        d->setUserData(tempStr, (void*) tempStr, &userhandler);
        d->release();

        // Test baseURI. BaseURI is set on nodes at parse time so we
        // cannot use the docBuilder document above

        //Setup parser

        XercesDOMParser *parser = new XercesDOMParser;
        parser->setValidationScheme(XercesDOMParser::Val_Never);
        parser->setDoNamespaces(true);
        parser->setDoSchema(true);
        parser->setCreateEntityReferenceNodes(true);

        OK = test.testBaseURI(parser);

        parser->setCreateEntityReferenceNodes(false);
        OK = test.testBaseURI(parser);

        parser->setDoNamespaces(false);
        parser->setDoSchema(false);
        OK = test.testBaseURI(parser);

        parser->setCreateEntityReferenceNodes(true);
        OK = test.testBaseURI(parser);

        delete parser;
    };

    XMLPlatformUtils::Terminate();

    if (!OK) {
        printf("Test Failed\n");
        return 4;
    }

    printf("Test Run Successfully\n");

    return 0;
};


/**
 * This method tests DOMAttr* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testAttr(DOMDocument* document)
{
    DOMNode* node;
    DOMAttr* attributeNode;
    bool T = true;
    bool F = false;
    bool OK = true;
// For debugging*****   printf("\n          testAttr's outputs:\n\n");

    XMLString::transcode("testAttribute", tempStr, 3999);
    DOMAttr* testAttribute = document->createAttribute(tempStr);

    XMLString::transcode("testAttribute's value", tempStr, 3999);
    testAttribute->setValue(tempStr);
    node = document->getDocumentElement(); // node gets first element

    // ((DOMElement*)node)->setAttributeNode(testAttribute);
    // attributeNode = ((DOMElement*)node)->getAttributeNode("testAttribute");
    DOMElement* el = (DOMElement*)node;
    DOMNode* rem = el->setAttributeNode(testAttribute);
    if (rem)
        rem->release();

    XMLString::transcode("testAttribute", tempStr, 3999);
    attributeNode = el->getAttributeNode(tempStr);

    //Test compareTreePosition, the equivalent case here
    XMLString::transcode("dFirstElementdFirstElement", tempStr2, 3999);
    DOMAttr* docFirstElementAttr = el->getAttributeNode(tempStr2);

    COMPARETREEPOSITIONTEST(docFirstElementAttr, attributeNode, DOMNode::TREE_POSITION_EQUIVALENT, __LINE__);

    // Test the name and other data
    if (XMLString::compareString(tempStr, attributeNode->getName()))
    {
        fprintf(stderr, "Warning!!! DOMAttr's 'getName' method failed to work properly!\n");
        OK = false;
    }

    XMLString::transcode("testAttribute's value", tempStr, 3999);
    if (XMLString::compareString(tempStr, attributeNode->getNodeValue()))
    {
        fprintf(stderr, "Warning!!! DOMAttr's 'getNodeValue' method failed to work properly!\n");
        OK = false;
    }
    if (! T ==attributeNode->getSpecified())
    {
        fprintf(stderr, "Warning!!! DOMAttr's 'getSpecified' method failed to work properly!\n");
        OK = false;
    }

    if (XMLString::compareString(tempStr, attributeNode->getValue()))
    {
        fprintf(stderr, "Warning!!! DOMAttr's 'getValue' method failed to work properly!\n");
        OK = false;
    }


    XMLString::transcode("Reset Value", tempStr, 3999);
    attributeNode->setNodeValue(tempStr);   /// LEAK!!!!!
    if (XMLString::compareString(tempStr, attributeNode->getNodeValue()))
    {
        fprintf(stderr, "Warning!!! DOMAttr's 'setNodeValue' method failed to work properly!\n");
        OK = false;
    }

    attributeNode->setValue(XMLUni::fgZeroLenString);
    if (XMLString::compareString(XMLUni::fgZeroLenString, attributeNode->getValue()))
    {
        fprintf(stderr, "Warning!!! DOMAttr's 'setValue' to '0' method failed to work properly!\n");
        OK = false;
    }

    XMLString::transcode("Another value ", tempStr, 3999);
    attributeNode->setValue(tempStr);
    if (XMLString::compareString(tempStr, attributeNode->getValue()))
    {
        fprintf(stderr, "Warning!!! DOMAttr's 'setValue' method failed to work properly!");
        OK = false;
    }

    node = attributeNode->cloneNode(T);

    // Check nodes for equality, both their name and value or lack thereof
    bool cloneOK = true;
    if (XMLString::compareString(node->getNodeName(), attributeNode->getNodeName()))
        cloneOK = false;
    if (node->getNodeValue() == 0 &&
        attributeNode->getNodeValue() != 0)
    {
        cloneOK = false;
    }

    if (node->getNodeValue() != 0 && attributeNode->getNodeValue() == 0)
    {
        cloneOK = false;
    };

    if (node->getNodeValue() != 0 && attributeNode->getNodeValue() != 0)
    {
        if (XMLString::compareString(node->getNodeValue(),attributeNode->getNodeValue()))
            cloneOK = false;
    }


/*
    if (! (node->getNodeName(), attributeNode->getNodeName()) &&         // Compares node names for equality
          (node->getNodeValue() != 0 && attributeNode->getNodeValue() != 0)  // Checks to make sure each node has a value node
        ?  node->getNodeValue(), attributeNode->getNodeValue())          // If both have value nodes test those value nodes for equality
        : (node->getNodeValue() == 0 && attributeNode->getNodeValue() == 0)))// If one node doesn't have a value node make sure both don't
*/
    if (cloneOK == false)
        {
            fprintf(stderr, "'cloneNode' did not clone the Attribute node correctly\n");
            OK = false;
        }
        // Deep clone test comparison is in testNode & testDocument

//************************************************* ERROR TESTS
    DOMTest tests;
//!! Throws HIERARCHY_REQUEST_ERR ****************
    //  doc->getDocumentElement()->appendChild(attributeNode);

//!! Throws a NOT_FOUND_ERR ********
    //  attribute2 = doc->createAttribute("testAttribute2");
    //  doc->getDocumentElement()->removeAttributeNode(attribute2);

//!! Throws an INUSE_ATTRIBUTE_ERR ******
    //  DOMElement* element = (DOMElement*)doc->getLastChild()->getLastChild();
    //  element->setAttributeNode(testAttribute );// Tests setNamedItem which generates error through justSetNamedItem.


    // Test the user data
    // Test simple set and get
    DOMAttr* userTest = testAttribute;
    DOMElement*  userFirst = el;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (userFirst->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(userFirst)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test renameNode
    XMLString::transcode("http://nsa", tempStr4, 3999);
    XMLString::transcode("aa", tempStr5, 3999);
    XMLString::transcode("pnsa:aa", tempStr3, 3999);

    // create the attribute
    DOMAttr* renameTestAttribute = document->createAttribute(tempStr5);
    DOMAttr* renameTestAttributeNS = document->createAttributeNS(tempStr4, tempStr3);

    // create the owner element and append the attribute node
    DOMElement* renameTestElement = document->createElement(tempStr5);
    renameTestElement->setAttributeNode(renameTestAttribute);
    renameTestElement->setAttributeNode(renameTestAttributeNS);

    // set up the userdata
    renameTestAttribute->setUserData(tempStr5, (void*) document, &userhandler);
    renameTestAttributeNS->setUserData(tempStr4, (void*) document, 0);

    // set up the node value
    renameTestAttribute->setNodeValue(tempStr5);
    renameTestAttributeNS->setNodeValue(tempStr4);

    XMLString::transcode("http://nsb", tempStr, 3999);
    XMLString::transcode("bb", tempStr2, 3999);
    XMLString::transcode("pnsb:bb", tempStr3, 3999);

    // start the rename tests
    // rename the NS Attribute
    DOMAttr* renameTest = (DOMAttr*) document->renameNode(renameTestAttributeNS, tempStr, tempStr3);
    // test the name
    if (XMLString::compareString(tempStr, renameTest->getNamespaceURI()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr2, renameTest->getLocalName()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr3, renameTest->getNodeName()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the child / nodevalue
    if (XMLString::compareString(tempStr4, renameTest->getNodeValue()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr4, renameTest->getFirstChild()->getNodeValue()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the owner element
    if (!renameTestElement->getAttributeNodeNS(tempStr, tempStr2)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestElement->getAttributeNodeNS(tempStr4, tempStr5)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test user data
    void* renamedocument = renameTest->getUserData(tempStr4);
    if (document != renamedocument) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test isSame and isEqual
    if (!renameTestAttributeNS->isEqualNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (!renameTestAttributeNS->isSameNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }


    // rename the Attribute (null namespace)
    renameTest = (DOMAttr*) document->renameNode(renameTestAttribute, 0, tempStr2);
    // test the name
    if (renameTest->getNamespaceURI())
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTest->getLocalName())
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr2, renameTest->getNodeName()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the child / nodevalue
    if (XMLString::compareString(tempStr5, renameTest->getNodeValue()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr5, renameTest->getFirstChild()->getNodeValue()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the owner element
    if (!renameTestElement->getAttributeNode(tempStr2)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestElement->getAttributeNode(tempStr5)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test user data
    renamedocument = renameTest->getUserData(tempStr5);
    if (document != renamedocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }
    // test isSame and isEqual
    if (!renameTestAttribute->isEqualNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (!renameTestAttribute->isSameNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }


    // rename the Attribute (with namespace)
    renameTest = (DOMAttr*) document->renameNode(renameTestAttribute, tempStr, tempStr3);
    // test the name
    if (XMLString::compareString(tempStr, renameTest->getNamespaceURI()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr2, renameTest->getLocalName()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr3, renameTest->getNodeName()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the child / nodevalue
    if (XMLString::compareString(tempStr5, renameTest->getNodeValue()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr5, renameTest->getFirstChild()->getNodeValue()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestAttribute->getFirstChild())
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the owner element
    if (!renameTestElement->getAttributeNodeNS(tempStr, tempStr2)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestElement->getAttributeNodeNS(0, tempStr2)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestElement->getAttributeNode(tempStr2)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test user data
    renamedocument = renameTest->getUserData(tempStr5);
    if (document != renamedocument) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test userdatahandler
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_RENAMED, tempStr5, document, renameTestAttribute, renameTest, __LINE__);
    // test isSame and isEqual
    // a new node is created here, so both isSame and isEqual are not compared
    if (renameTestAttribute->isEqualNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestAttribute->isSameNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }



    //isID tests

    XMLString::transcode("http://nsa", tempStr4, 3999);
    XMLString::transcode("aa", tempStr5, 3999);

    DOMAttr *idAtt = document->createAttributeNS(tempStr4, tempStr5);
    testElementNode->setAttributeNode(idAtt);


    if(idAtt->isId()) {
        fprintf(stderr, "isID failed in line %i\n", __LINE__);
        OK = false;
    }

    testElementNode->setIdAttributeNode(idAtt);

    if(!idAtt->isId()) {
        fprintf(stderr, "isID failed in line %i\n", __LINE__);
        OK = false;
    }
    //clean up
    testElementNode->removeAttributeNode(idAtt);

    if (! OK)
        printf("\n*****The DOMAttr* method calls listed above failed, all others worked correctly.*****\n");
    return OK;

};




/**
 * This method tests DOMCDATASection* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testCDATASection(DOMDocument* document)
{
    DOMNode* node;
    DOMNode* node2;
    bool T = true;
    bool OK = true;
// For debugging*****   printf("\n          testCDATASection's outputs:\n");
    XMLString::transcode("dBodyLevel23", tempStr, 3999);
    node = document->getDocumentElement()->getElementsByTagName(tempStr)->item(0)->getFirstChild(); // node gets DOMCDATASection* node

    node2 = node->cloneNode(T);//*****?

    // Check nodes for equality, both their name and value or lack thereof
    if (!(!XMLString::compareString(node->getNodeName(), node2->getNodeName()) &&        // Compares node names for equality
          (node->getNodeValue() != 0 && node2->getNodeValue() != 0)     // Checks to make sure each node has a value node
        ? !XMLString::compareString(node->getNodeValue(), node2->getNodeValue())         // If both have value nodes test those value nodes for equality
        : (node->getNodeValue() == 0 && node2->getNodeValue() == 0)))   // If one node doesn't have a value node make sure both don't
    {
        fprintf(stderr, "'cloneNode' did not clone the DOMCDATASection* node correctly\n");
        OK = false;
    }
    // Deep clone test comparison is in testNode & testDocument


    // Test the user data
    // Test simple set and get
    DOMCDATASection* userTest = (DOMCDATASection*) node;
    DOMCDATASection*  userFirst = (DOMCDATASection*) node2;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (userFirst->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(document)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 204 );

    if (! OK)
        printf("\n*****The DOMCDATASection* method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DOMCharacterData methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testCharacterData(DOMDocument* document)
{
    DOMCharacterData* charData;
    XMLCh resetData[3999];
    bool OK = true;
// For debugging*****   printf("\n          testCharacterData's outputs:\n");
    XMLString::transcode("dBodyLevel31", tempStr, 3999);
    DOMNode*  abc1 = document->getDocumentElement()->getElementsByTagName(tempStr)->item(0)->getFirstChild(); // charData gets textNode11

    charData = (DOMCharacterData *) abc1;

    XMLString::transcode("dBodyLevel31'sChildTextNode11", tempStr, 3999);
    if (XMLString::compareString(tempStr, charData->getData()))
    {
        fprintf(stderr, "Warning!!! DOMCharacterData's 'getData' failed to work properly!\n This may corrupt other DOMCharacterData tests!!!*****\n");
        OK = false;
    }

    XMLString::copyString(resetData,charData->getData());
    //  printf("This node's original data is: " + charData->getData());

    XMLString::transcode(" This is new data for this node", tempStr2, 3999);

    XMLString::copyString(tempStr, charData->getData());
    XMLString::catString(tempStr, tempStr2);

    charData->appendData(tempStr2);


    if (XMLString::compareString(tempStr, charData->getData()))
    {
        fprintf(stderr, "Warning!!! DOMCharacterData's 'appendData' failed to work properly!\n");
        OK = false;
    }
    //  printf("This node's appended data is: " + charData->getData());

    XMLString::transcode("dBodyLevel", tempStr, 3999);
    charData->deleteData(10, 100);
    if (XMLString::compareString(tempStr, charData->getData()))
    {
        fprintf(stderr, "Warning!!! DOMCharacterData's 'deleteData' failed to work properly!\n");
        OK = false;
    }
    //  printf("This node's partially deleted data is: " + charData->getData());

    unsigned int length = 10;
    if (!(length == charData->getLength()))
    {
        fprintf(stderr, "Warning!!! DOMCharacterData's 'getLength' failed to work properly!\n");
        OK = false;
    }
    //  printf("This node's data length is: " + charData->getLength());

    XMLString::transcode("dBody' This is data inserted into this node'Level", tempStr, 3999);
    XMLString::transcode("' This is data inserted into this node'", tempStr2, 3999);
    charData->insertData(5, tempStr2);
    if (XMLString::compareString(tempStr, charData->getData()))
    {
        fprintf(stderr, "Warning!!! DOMCharacterData's 'insertData' failed to work properly!\n");
        OK = false;
    }
    //  printf("This node's updated with insert data is: " + charData->getData());

    XMLString::transcode("dBody' This is ' replacement data'ted into this node'Level", tempStr, 3999);
    XMLString::transcode("' replacement data'", tempStr2, 3999);
    charData->replaceData(15, 10, tempStr2);
    if (XMLString::compareString(tempStr, charData->getData()))
    {
        fprintf(stderr, "Warning!!! DOMCharacterData's 'replaceData' failed to work properly!\n");
        OK = false;
    }
    //  printf("This node's updated with replacement data is: " +charData->getData());

    XMLString::transcode("New data A123456789B123456789C123456789D123456789E123456789", tempStr, 3999);
    charData->setData(tempStr);
    if (XMLString::compareString(tempStr, charData->getData()))
    {
        fprintf(stderr, "Warning!!! DOMCharacterData's 'setData' failed to work properly!");
        OK = false;
    }
    //  printf("This node's new data via setData: " + charData->getData());

    XMLString::transcode("123456789D123456789E123456789", tempStr, 3999);
    if (XMLString::compareString(tempStr, charData->substringData(30, 30)))
    {
        fprintf(stderr, "Warning!!! DOMCharacterData's 'substringData' failed to work properly!\n");
        OK = false;
    }
    //  printf("Using subString 30,30 you get:");  charData->substringData(30,30)).print();

    XMLString::transcode("New data A123456789B12345", tempStr, 3999);
    if (XMLString::compareString(tempStr, charData->substringData(0, 25)))
    {
        fprintf(stderr, "Warning!!! DOMCharacterData's 'substringData' failed to work properly!\n");
        OK = false;
    }
    //  printf("Using subString 0,25 you get: ");   charData->substringData(0,25)).print();

//************************************************* ERROR TESTS
    DOMTest tests;   // What is this for?  'tests' is never used.

//!! Throws INDEX_SIZE_ERR ********************
    EXCEPTIONSTEST(charData->deleteData(-1, 5), DOMException::INDEX_SIZE_ERR, OK, 101 );
    // Test 102 is not an error because the -1 parameter is an unsigned value, and counts
    //   that exceed the length of the string are allowed.
//    EXCEPTIONSTEST(charData->deleteData(2, -1), DOMException::INDEX_SIZE_ERR, OK, 102 );
    EXCEPTIONSTEST(charData->deleteData(100, 5), DOMException::INDEX_SIZE_ERR, OK,103 );

//can't set negative unsigned int in c++ compiler

  //  EXCEPTIONSTEST(charData->insertData(-1, "Stuff inserted"), DOMException::INDEX_SIZE_ERR, OK, 104 );
    XMLString::transcode("Stuff inserted", tempStr, 3999);
    EXCEPTIONSTEST(charData->insertData(100, tempStr), DOMException::INDEX_SIZE_ERR, OK, 105 );

  //  EXCEPTIONSTEST(charData->replaceData(-1, 5, "Replacement stuff") , DOMException::INDEX_SIZE_ERR, OK, 106 );
    XMLString::transcode("Replacement stuff", tempStr, 3999);
    EXCEPTIONSTEST(charData->replaceData(100, 5 ,tempStr), DOMException::INDEX_SIZE_ERR, OK, 107 );
  //  EXCEPTIONSTEST(charData->replaceData(2, -1, "Replacement stuff"), DOMException::INDEX_SIZE_ERR,  OK, 108 );

    EXCEPTIONSTEST(charData->substringData(-1, 5), DOMException::INDEX_SIZE_ERR, OK, 109 );
    EXCEPTIONSTEST(charData->substringData(100, 5), DOMException::INDEX_SIZE_ERR, OK, 110 );
 //   EXCEPTIONSTEST(charData->substringData(2, -1), DOMException::INDEX_SIZE_ERR, OK, 111 );


    // Test the user data
    // Test simple set and get
    DOMCharacterData* userTest = charData;
    DOMDocument*  userFirst = document;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (userFirst->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(userFirst)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 205 );

    if (!OK)
        printf("\n*****The DOMCharacterData method calls listed above failed, all others worked correctly.*****\n");
    charData->setData(resetData); // reset node to original data
    return OK;
};




/**
 * This method tests ChildNodeList methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testChildNodeList(DOMDocument* document)
{
    DOMNode* node;
    DOMNode* node2;
    bool OK = true;
// For debugging*****   printf("\n          testChildNodeList's outputs:\n");
    node = document->getDocumentElement()->getLastChild(); // node gets doc's testBody element

    if (!(node->getChildNodes()->getLength()== 4))
        OK = false;
    node2 = node->getChildNodes()->item(2);
    XMLString::transcode("dBodyLevel23", tempStr, 3999);
    if (XMLString::compareString(tempStr, node2->getNodeName()))
        OK = false;

    if (!OK)
        printf("\n*****The ChildNodeList method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DOMComment* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testComment(DOMDocument* document)
{
    DOMNode* node;
    DOMNode* node2;
    bool T = true;
    bool OK = true;
// For debugging*****   printf("\n          testComment's outputs:\n");
    XMLString::transcode("dBodyLevel31", tempStr, 3999);
    node = document->getDocumentElement()->getElementsByTagName(tempStr)->item(0)->getFirstChild(); // node gets textNode11
    node2 = node->cloneNode(T);
    // Check nodes for equality, both their name and value or lack thereof
    if (!(!XMLString::compareString(node->getNodeName(), node2->getNodeName()) &&        // Compares node names for equality
          (node->getNodeValue() != 0 && node2->getNodeValue() != 0)     // Checks to make sure each node has a value node
        ? !XMLString::compareString(node->getNodeValue(), node2->getNodeValue())         // If both have value nodes test those value nodes for equality
        : (node->getNodeValue() == 0 && node2->getNodeValue() == 0)))   // If one node doesn't have a value node make sure both don't
    {
        fprintf(stderr, "'cloneNode' did not clone the DOMComment* node correctly\n");
        OK = false;
    }
    // Deep clone test comparison is in testNode & testDocument

    // Test the user data
    // Test simple set and get
    DOMComment* userTest = (DOMComment*) node;
    DOMComment*  userFirst = (DOMComment*) node2;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (userFirst->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(document)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 206 );

    if (!OK)
        printf("\n*****The DOMComment* method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DeepNodeList methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testDeepNodeList(DOMDocument* document)
{
    DOMNode* node;
    DOMNode* node2;
    bool OK = true;
// For debugging*****   printf("\n          testDeepNodeList's outputs:\n\n");
    node = document->getLastChild()->getLastChild(); // node gets docBody element
//  DOMElement* el = (DOMElement*)node;
//  DOMNodeList nl = el->getElementsByTagName("*");
//  int len = nl->getLength();
//  if (len != 8)
    XMLString::transcode("*",tempStr, 3999);
    if (!(8 == ((DOMElement*) node)->getElementsByTagName(tempStr)->getLength()))
        {
            printf ("Warning!!! DeepNodeList's 'getLength' failed to work properly!\n");
            OK = false;
        }
    node2 = ((DOMElement*) node)->getElementsByTagName(tempStr)->item(2); //This also runs through 'nextMatchingElementAfter"

    XMLString::transcode("dBodyLevel32", tempStr, 3999);
    if (XMLString::compareString(tempStr, node2->getNodeName()))
        {
            printf ("Warning!!! DeepNodeList's 'item' (or DOMElement's 'getElementsBy TagName)failed to work properly!\n");
            OK = false;
        }
    node2 = document->getLastChild();
    XMLString::transcode("dTestBody", tempStr, 3999);
    if (XMLString::compareString(tempStr, ((DOMElement*) node2)->getElementsByTagName(tempStr)->item(0)->getNodeName()))//This also runs through 'nextMatchingElementAfter"
        {
            printf ("Warning!!! DeepNodeList's 'item' (or DOMElement's 'getElementsBy TagName)failed to work properly!\n");
            OK = false;
        }

    if (!OK)
        printf("\n*****The DeepNodeList method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DOMDocument* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 *
 **** ALL DOMDocument* create methods are run in docBuilder except createAttribute which is in testAttribute**
 */
bool DOMTest::testDocument(DOMDocument* document)
{
    DOMTest make;
    DOMDocumentFragment* docFragment, *docFragment2;
    DOMElement* newElement;
    DOMNode* node, *node2;

    const char* elementNames[] =  {"dFirstElement", "dTestBody", "dBodyLevel21","dBodyLevel31","dBodyLevel32",
                   "dBodyLevel22","dBodyLevel33","dBodyLevel34","dBodyLevel23","dBodyLevel24"};
    const char* newElementNames[] = {"dFirstElement", "dTestBody", "dBodyLevel22","dBodyLevel33","dBodyLevel34","dBodyLevel23"};


    bool result;
    bool OK = true;
// For debugging*****   printf("\n          testDocument's outputs:\n \n");

    XMLString::transcode("testDocument1", tempStr, 3999);
    DOMDocumentType* checkDocType =  make.createDocumentType(document,tempStr);
    DOMDocumentType* docType = document->getDoctype();

    if (XMLString::compareString(checkDocType->getNodeName(),docType->getNodeName() ))
    {
        fprintf(stderr, "Warning!!! DOMDocument's 'getDocType method failed!\n" );
        OK = false;
    }

    if (XMLString::compareString(checkDocType->getNodeValue(), docType->getNodeValue()))
    {
        fprintf(stderr, "Warning!!! DOMDocument's 'getDocType method failed!\n" );
        OK = false;
    }

    /*
    if (! (checkDocType->getNodeName(), docType->getNodeName()) &&      // Compares node names for equality
          (checkDocType->getNodeValue() != 0 && docType->getNodeValue() != 0)   // Checks to make sure each node has a value node
        ?  checkDocType->getNodeValue(), docType->getNodeValue())       // If both have value nodes test those value nodes for equality
        : (checkDocType->getNodeValue() == 0 && docType->getNodeValue() == 0))) // If one node doesn't have a value node make sure both don't
    {
        fprintf(stderr, "Warning!!! DOMDocument's 'getDocType method failed!\n" );
        OK = false;
    }
    */

    DOMNode*  rootElement = document->getLastChild();

    bool check = (rootElement->getNodeValue() && document->getDocumentElement()->getNodeValue())   // Checks to make sure each node has a value node
        ?  !XMLString::compareString(rootElement->getNodeValue(), document->getDocumentElement()->getNodeValue())      // If both have value nodes test those value nodes for equality
        : (rootElement->getNodeValue() == 0 && document->getDocumentElement()->getNodeValue() == 0);    // If one node doesn't have a value node make sure both don't
    if (!(!XMLString::compareString(rootElement->getNodeName(), document->getDocumentElement()->getNodeName()) &&        // Compares node names for equality
         check))
    {
        fprintf(stderr, "Warning!!! DOMDocument's 'getDocumentElement' method failed!\n" );
        OK = false;
    }

    XMLString::transcode("*", tempStr, 3999);
    DOMNodeList* docElements = document->getElementsByTagName(tempStr);
    int docSize = docElements->getLength();
    int i;
    for (i = 0; i < docSize; i++)
    {
        DOMNode*  n = (DOMNode*) docElements->item(i);
        XMLString::transcode(elementNames[i], tempStr, 3999);
        if (XMLString::compareString(tempStr, n->getNodeName()))
        {
            fprintf(stderr, "Comparison of this document's elements failed at element number %d at line %i \n", i, __LINE__);
            OK = false;
            break;
        }
    }

    // What is this supposed to be doing?
    //
    //if (document->equals(document->getImplementation()))
    //{
    //  fprintf(stderr, "Warning!!! DOMDocument's 'getImplementation' method failed!\n" );
    //  OK = false;
    //}

    XMLString::transcode("NewElementTestsInsertBefore", tempStr, 3999);
    newElement = document->createElement(tempStr);
    //  doc->insertBefore(newElement,0);//!! Throws a HIERARCHY_REQUEST_ERR   *******
    //  doc->removeChild(docElements->item(9));//!! Throws a NOT_FOUND_ERR  ********

    docFragment = document->createDocumentFragment();
    //Tests removeChild and stores removed branch for tree reconstruction
    docFragment->appendChild(docElements->item(1)->removeChild(docElements->item(9)));
    docFragment2 = document->createDocumentFragment();
    //Tests removeChild and stores removed branch for tree reconstruction
    docFragment2->appendChild(docElements->item(1)->removeChild(docElements->item(2)));
    docSize = docElements->getLength();
    for (i = 0; i < docSize; i++)
    {
        DOMNode*  n = (DOMNode*) docElements->item(i);
        XMLString::transcode(newElementNames[i], tempStr, 3999);
        if (XMLString::compareString(tempStr, n->getNodeName()))
        {
            fprintf(stderr, "Comparison of new document's elements failed at element number %d at line %i \n", i, __LINE__);
            OK = false;
            break;
        }
    }
    docElements->item(1)->insertBefore(docFragment, 0); //Reattaches removed branch to restore tree to the original
                                                // AH Revist.  Note: insertBefore should be able to take
                                                //   a 0 as its second parameter.
    docElements->item(1)->insertBefore(docFragment2, docElements->item(2)); //Reattaches removed branch to restore tree to the original

    //  printf(docElements->item(2)->getNodeName());

    docSize = docElements->getLength();
    for (i = 0; i < docSize; i++)
    {
        DOMNode*  n = (DOMNode*) docElements->item(i);
        XMLString::transcode(elementNames[i], tempStr, 3999);
        if (XMLString::compareString(tempStr, n->getNodeName()))
        {
            fprintf(stderr, "Comparison of restored document's elements failed at element number %d at line %i \n", i, __LINE__);
            OK = false;
            break;
        }
    }

    DOMTest tests;


//  DOMDocument* z = tests.createDocument();
//  tests.docBuilder(z, "z");

//!! Throws WRONG_DOCUMENT_ERR **********
//  EXCEPTIONSTEST(z->appendChild(
    //  z->appendChild(d.createComment("Test doc d comment"));// Tries to append z document with document d comment
    //  d->getDocumentElement()->appendChild(z.createElement("newZdocElement"));// Tries to append d document with document z DOMElement
    //  d->getLastChild()->getLastChild()->insertBefore(z.createElement("newZdocElement"),d->getLastChild()->getLastChild()->getFirstChild());// Tries to insert into d document with document z DOMElement
    //  d->replaceChild(z.createElement("newZdocElement"),d->getLastChild()->getLastChild()->getFirstChild());  // Tries to replace in d document with document z DOMElement

    //  doc->setNodeValue("This shouldn't work");//!! Throws a NO_MODIFICATION_ALLOWED_ERR ********

    node = document;
    node2 = document->cloneNode(true);
    result = treeCompare(node, node2); // Deep clone test comparison of document cloneNode
    if (!result)
    {
        fprintf(stderr, "Warning!!! Deep clone of the document failed!\n");
        OK = false;
    }
    node2->release();

    // Deep clone test comparison is also in testNode

    // Test the user data
    // Test simple set and get
    DOMDocumentFragment* userTest = docFragment;
    DOMDocumentFragment*  userFirst = docFragment2;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (userFirst->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(document)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 207 );

    if (!OK)
        printf("\n*****The DOMDocument* method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DOMDocumentFragment* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 *
 *
 ********This really isn't needed, only exists to throw NO_MODIFICATION_ALLOWED_ERR ********
 */
bool DOMTest::testDocumentFragment(DOMDocument* document)
{
    bool OK = true;
// For debugging*****   printf("\n          testDocumentFragment's outputs:\n");
    DOMDocumentFragment* testDocFragment = document->createDocumentFragment();

    //  testDocFragment->setNodeValue("This is a document fragment!");//!! Throws a NO_MODIFICATION_ALLOWED_ERR ********

    // Test isSameNode
    DOMDocumentFragment* userTest = testDocFragment;
    DOMNode* mycloned = testDocFragment->cloneNode(true);
    DOMDocument* userFirst = document;

    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(userFirst)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 208 );

    if (!OK)
        printf("\n*****The DOMDocumentFragment* method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DOMDocumentType* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testDocumentType(DOMDocument* document)
{
    DOMTest test;
    DOMDocumentType* docType, *holdDocType;
    DOMNamedNodeMap* docNotationMap;
    DOMNode* node, *node2;
    bool OK = true;
// For debugging*****   printf("\n          testDocumentType's outputs:\n");
    XMLString::transcode("TestDocument", tempStr, 3999);
    DOMDocumentType* newDocumentType =  test.createDocumentType(document, tempStr);
    node = document->getFirstChild(); // node gets doc's docType node
    node2 = node->cloneNode(true);
    // Check nodes for equality, both their name and value or lack thereof
    if (!(!XMLString::compareString(node->getNodeName(), node2->getNodeName()) &&        // Compares node names for equality
          (node->getNodeValue() != 0 && node2->getNodeValue() != 0)     // Checks to make sure each node has a value node
        ? !XMLString::compareString(node->getNodeValue(), node2->getNodeValue())         // If both have value nodes test those value nodes for equality
        : (node->getNodeValue() == 0 && node2->getNodeValue() == 0)))   // If one node doesn't have a value node make sure both don't
    {
        fprintf(stderr, "'cloneNode' did not clone the DOMDocumentType* node correctly\n");
        OK = false;
    }
     // Deep clone test comparison is in testNode & testDocument

    DOMNode*   abc9 = document->getFirstChild();
    docType = (DOMDocumentType*) abc9;

    XMLString::transcode("ourNotationNode", tempStr, 3999);
    docNotationMap = docType->getNotations();
    if (XMLString::compareString(tempStr, docNotationMap->item(0)->getNodeName()))
    {
        fprintf(stderr, "Warning!!! DOMDocumentType's 'getNotations' failed!\n");
        OK = false;
    }
    //  doc->appendChild(newDocumentTypeImpl);//!! Throws a HIERARCHY_REQUEST_ERR    *******
    DOMNode*  abc10 = document->removeChild(document->getFirstChild()); //Tests removeChild and stores removed branch for tree reconstruction
    holdDocType = (DOMDocumentType*) abc10;
    document->insertBefore(newDocumentType, document->getDocumentElement());
    //** Other aspects of insertBefore are tested in docBuilder through appendChild*

    DOMNode* rem = document->removeChild(document->getFirstChild()); //Removes newDocumentType for tree restoral
    rem->release();
    document->insertBefore(holdDocType, document->getFirstChild()); //Reattaches removed branch to restore tree to the original

    // Test the user data
    // Test simple set and get
    DOMDocumentType* userTest = docType;
    DOMNamedNodeMap*  userFirst = docNotationMap;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (node2->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // document type cannot be imported, so no test import, rather test the exception
    document->setUserData(tempStr2, (void*) document, &userhandler);
    EXCEPTIONSTEST(document->importNode(userTest,true), DOMException::NOT_SUPPORTED_ERR, OK, 203 );

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(document)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(document)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 209 );

    if (!OK)
        printf("\n*****The DOMDocumentType* method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * @param document org.w3c.dom.DOMDocument
 */
bool DOMTest::testDOMerrors(DOMDocument* document) {
    bool OK = true;

    DOMTest tests;

    EXCEPTIONSTEST(document->appendChild(testElementNode), DOMException::HIERARCHY_REQUEST_ERR, OK, 201 );
    EXCEPTIONSTEST(testTextNode->appendChild(testTextNode), DOMException::HIERARCHY_REQUEST_ERR, OK, 202 );
    return OK;
};



/**
 * This method tests DOMImplementation methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testDOMImplementation(DOMDocument* document)
{

    DOMImplementation* implementation;
    bool result = false;
    bool OK = true;
// For debugging*****   printf("\n          testDOMImplementation's outputs:\n");
    implementation = document->getImplementation(); //Uses getDOMImplementation to obtain implementation

    XMLString::transcode("XML", tempStr, 3999);
    XMLString::transcode("1.0", tempStr2, 3999);
    result = implementation->hasFeature(tempStr, tempStr2);
    if(!result)
    {
        fprintf(stderr, "Warning!!! DOMImplementation's 'hasFeature' that should be 'true' failed!");
        OK = false;
    }

    XMLString::transcode("HTML", tempStr, 3999);
    XMLString::transcode("4.0", tempStr2, 3999);
    result = implementation->hasFeature(tempStr, tempStr2);
    if(result)
    {
        fprintf(stderr, "Warning!!! DOMImplementation's 'hasFeature' that should be 'false' failed!");
        OK = false;
    }


    if (!OK)
        fprintf(stderr, "\n*****The DOMImplementation method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DOMElement* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testElement(DOMDocument* document)
{
    DOMAttr* attributeNode, *newAttributeNode;
    DOMElement* element, *element2;
    DOMNode* node, *node2;

    const char* attributeCompare[] = {"AnotherFirstElementAttribute", "dFirstElement", "testAttribute"};
    const char* elementNames[] =  {"dFirstElement", "dTestBody", "dBodyLevel21","dBodyLevel31","dBodyLevel32",
                   "dBodyLevel22","dBodyLevel33","dBodyLevel34","dBodyLevel23","dBodyLevel24"};
    const char* textCompare[] = {"dBodyLevel31'sChildTextNode11",
                                "dBodyLevel31'sChildTextNode12",
                                "dBodyLevel31'sChildTextNode13"};

    DOMNamedNodeMap* nodeMap;
    bool OK = true;
    node = document->getDocumentElement(); // node gets doc's firstElement
    node2 = node->cloneNode(true);
    // Check nodes for equality, both their name and value or lack thereof
    if (!(!XMLString::compareString(node->getNodeName(), node2->getNodeName()) &&        // Compares node names for equality
          (node->getNodeValue() != 0 && node2->getNodeValue() != 0)     // Checks to make sure each node has a value node
        ? !XMLString::compareString(node->getNodeValue(), node2->getNodeValue())         // If both have value nodes test those value nodes for equality
        : (node->getNodeValue() == 0 && node2->getNodeValue() == 0)))   // If one node doesn't have a value node make sure both don't
    {
        fprintf(stderr, "'cloneNode' did not clone the DOMElement* node correctly.\n");
        OK = false;
    }
    // Deep clone test comparison is in testNode & testDocument

    element = document->getDocumentElement(); // element gets doc's firstElement

    XMLString::copyString(tempStr, document->getNodeValue());
    XMLString::transcode("'s test attribute", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    if (XMLString::compareString(XMLUni::fgZeroLenString, element->getAttribute(tempStr)))
    {
        fprintf(stderr, "Warning!!! DOMElement's 'getAttribute' failed!\n");
        OK = false;
    }

    XMLString::copyString(tempStr, document->getNodeValue());
    XMLString::transcode("FirstElement", tempStr2, 3999);
    XMLString::catString(tempStr, tempStr2);
    attributeNode = element->getAttributeNode(tempStr);
    if(! (attributeNode == 0))
    {
        fprintf(stderr, "Warning!!! DOMElement's 'getAttributeNode' failed! It should have returned '0' here!\n");
        OK = false;
    }


    XMLString::transcode("AnotherFirstElementAttribute", tempStr, 3999);
    newAttributeNode = document->createAttribute(tempStr);

    XMLString::transcode("A new attribute which helps test calls in DOMElement", tempStr, 3999);
    newAttributeNode->setValue(tempStr);
    // This test is incorrect.  It assumes that there is a defined ordering of the entries
    //  in a nodeMap, but there is no ordering required.
#ifdef TheFollowingCheckIsInvalid
    DOMNode* rem2 = element->setAttributeNode(newAttributeNode);
    if (rem2)
        rem2->release();
    nodeMap = element->getAttributes();
    int size = nodeMap->getLength();
    int k;
    for (k = 0; k < size; k++)
    {
        DOMNode*  n = (DOMNode) nodeMap->item(k);
        XMLString::transcode(attributeCompare[k], tempStr, 3999);
        if (XMLString::compareString(tempStr, n->getNodeName())))
        {
            fprintf(stderr, "Warning!!! Comparison of firstElement's attributes failed at line %i.\n", __LINE__);
            fprintf(stderr, "   This failure can be a result of DOMElement's 'setValue' and/or 'setAttributeNode' and/or 'getAttributes' failing.\n");
            OK = false;
            break;
        }
    //  printf("firstElement's attribute number " + k + " : " + n->getNodeName());
    }
#endif

    nodeMap = element->getAttributes();
    int size = nodeMap->getLength();
    if (size != 2)
    {
        fprintf(stderr, "DOMElement* Tests Failure 001\n");
        OK = false;
    };
    DOMNode* rem = element->setAttributeNode(newAttributeNode);
    if (rem)
        rem->release();
    size = nodeMap->getLength();
    if (size != 3)
    {
        fprintf(stderr, "DOMElement* Tests Failure 002\n");
        OK = false;
    };

    // Fetch the newly added attribute node back out of from the named node map,
    //  and check that we are returned the same node that we put in->
    XMLString::transcode("AnotherFirstElementAttribute", tempStr, 3999);
    DOMNode*  abc12 = nodeMap->getNamedItem(tempStr);
    DOMAttr* fetchedAttr = (DOMAttr*) abc12;
    if (fetchedAttr != newAttributeNode)
    {
        fprintf(stderr, "DOMElement* Tests Failure 003\n");
        OK = false;
    };

    // Fetch the newly added attribute back out directly from the element itself.
    XMLString::transcode("AnotherFirstElementAttribute", tempStr, 3999);
    fetchedAttr = element->getAttributeNode(tempStr);
    if (fetchedAttr != newAttributeNode)
    {
        fprintf(stderr, "DOMElement* Tests Failure 004\n");
        OK = false;
    };



    XMLString::transcode("*",tempStr, 3999);
    DOMNodeList* docElements = document->getElementsByTagName(tempStr);
    int docSize = docElements->getLength();
    int i;
    for (i = 0; i < docSize; i++)
    {
        DOMNode*  n = docElements->item(i);
        XMLString::transcode(elementNames[i], tempStr, 3999);
        if (XMLString::compareString(tempStr, n->getNodeName()))
        {
            fprintf(stderr, "Warning!!! Comparison of DOMElement's 'getElementsByTagName' "
                            "and/or 'item' failed at element number %d at line %i \n", i, __LINE__ );
            fprintf(stderr, "\n");
            OK = false;
            break;
        }
    //  printf("docElement's number " + i + " is: " + n->getNodeName());
    }
    XMLString::transcode("dBodyLevel21", tempStr, 3999);
    DOMNode*  abc15 = document->getElementsByTagName(tempStr)->item(0); // element gets DOMElement* test BodyLevel21
    element = (DOMElement*) abc15;

    XMLString::transcode("dBodyLevel31", tempStr, 3999);
    DOMNode*  abc16 = document->getElementsByTagName(tempStr)->item(0); // element2 gets DOMElement* test BodyLevel31
    element2 = (DOMElement*) abc16;
    DOMNodeList* text = ((DOMNode*  &) element2)->getChildNodes();
    int textSize = text->getLength();
    int j;
    static bool firstTime = true;
    if (firstTime)
    {
        firstTime = false;      // Temporary fix.  Subsequent tests alter the doc, causing
                                //   this test to fail on all but the first time through.
        for (j = 0; j < textSize; j++)
        {
            DOMNode*  n = text->item(j);
            XMLString::transcode(textCompare[j], tempStr, 3999);
            if (XMLString::compareString(tempStr, n->getNodeValue()))
            {
                fprintf(stderr, "Warning!!! Comparison of original text nodes via DOMNode*  'getChildNodes' & DOMNodeList 'item'\n"
                    "     failed at text node: #%d at line %i \n     ", j, __LINE__ );
                OK = false;
                break;
            }
            //  printf("DOMElement* testBodyLevel31's child text node " + j + " is: " + n->getNodeValue());
        }
    }

    element = document->getDocumentElement(); // element gets doc's firstElement
    element->normalize();        // Concatenates all adjacent text nodes in this element's subtree
    DOMNodeList* text2 = ((DOMNode*) element2)->getChildNodes();
    XMLString::transcode("dBodyLevel31'sChildTextNode11dBodyLevel31'sChildTextNode12dBodyLevel31'sChildTextNode13", tempStr, 3999);
    DOMNode*  n = text2->item(0);
    if (XMLString::compareString(tempStr, n->getNodeValue()))
    {
        fprintf(stderr, "Warning!!! Comparison of concatenated text nodes created by DOMElement's 'normalize' failed!\n");
        OK = false;
    }

    XMLString::transcode("FirstElementLastAttribute", tempStr, 3999);
    XMLString::transcode("More attribute stuff for firstElement!!", tempStr2, 3999);
    element->setAttribute(tempStr, tempStr2);

    XMLString::transcode("FirstElementLastAttribute", tempStr, 3999);
    element->removeAttribute(tempStr);
    rem = element->removeAttributeNode(newAttributeNode);
    if (rem)
        rem->release();

    //  doc->getLastChild()->setNodeValue("This shouldn't work");//!! Throws a NO_MODIFICATION_ALLOWED_ERR***

    // Test the user data
    // Test simple set and get
    DOMElement* userTest = element;
    DOMAttr*  userFirst = newAttributeNode;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (userFirst->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(userFirst)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test renameNode
    XMLString::transcode("http://nsa", tempStr4, 3999);
    XMLString::transcode("aa", tempStr5, 3999);
    XMLString::transcode("pnsa:aa", tempStr3, 3999);

    // create the element
    DOMElement* renameTestElement = document->createElement(tempStr5);
    DOMElement* renameTestElementNS = document->createElementNS(tempStr4, tempStr3);

    // create the parent
    DOMElement* renameTestParent = document->createElement(tempStr5);
    renameTestParent->appendChild(renameTestElement);
    renameTestParent->appendChild(renameTestElementNS);

    // set up the userdata
    renameTestElement->setUserData(tempStr5, (void*) document, &userhandler);
    renameTestElementNS->setUserData(tempStr4, (void*) document, 0);

    // append a text node as child
    DOMText* renameTestText = document->createTextNode(tempStr5);
    DOMText* renameTestTextNS = document->createTextNode(tempStr4);
    renameTestElement->appendChild(renameTestText);
    renameTestElementNS->appendChild(renameTestTextNS);

    XMLString::transcode("http://nsb", tempStr, 3999);
    XMLString::transcode("bb", tempStr2, 3999);
    XMLString::transcode("pnsb:bb", tempStr3, 3999);

    // set up some attributes
    DOMAttr* renameTestAttribute = document->createAttribute(tempStr5);
    DOMAttr* renameTestAttributeNS = document->createAttributeNS(tempStr4, tempStr3);
    renameTestElement->setAttributeNode(renameTestAttribute);
    renameTestElementNS->setAttributeNodeNS(renameTestAttributeNS);

    //Test compareTreePosition first before testing rename
    // renameTestParent
    //  |
    //  |_ renameTestElement (has renameTestAttribute)
    //  |          |
    //  |          |_ renameTestText
    //  |
    //  |_ renameTestElementNS (has renameTestAttributeNS)
    //  |          |
    //  |          |_ renameTestTextNS
    //
    COMPARETREEPOSITIONTEST(renameTestAttribute, renameTestAttributeNS, DOMNode::TREE_POSITION_FOLLOWING, __LINE__);
    COMPARETREEPOSITIONTEST(renameTestAttribute, renameTestElement, DOMNode::TREE_POSITION_PRECEDING, __LINE__);
    COMPARETREEPOSITIONTEST(renameTestAttribute, renameTestText, DOMNode::TREE_POSITION_FOLLOWING, __LINE__);
    COMPARETREEPOSITIONTEST(renameTestAttribute, renameTestTextNS, DOMNode::TREE_POSITION_FOLLOWING, __LINE__);
    COMPARETREEPOSITIONTEST(renameTestAttribute, renameTestParent, DOMNode::TREE_POSITION_PRECEDING, __LINE__);

    COMPARETREEPOSITIONTEST(renameTestAttributeNS, renameTestAttribute, DOMNode::TREE_POSITION_PRECEDING, __LINE__);
    COMPARETREEPOSITIONTEST(renameTestElement, renameTestAttributeNS, DOMNode::TREE_POSITION_FOLLOWING, __LINE__);
    COMPARETREEPOSITIONTEST(renameTestAttributeNS, renameTestText, DOMNode::TREE_POSITION_PRECEDING, __LINE__);
    COMPARETREEPOSITIONTEST(renameTestTextNS, renameTestAttributeNS, DOMNode::TREE_POSITION_PRECEDING, __LINE__);

    // start the rename tests
    // rename the NS Element
    DOMElement* renameTest = (DOMElement*) document->renameNode(renameTestElementNS, tempStr, tempStr3);
    // test the name
    if (XMLString::compareString(tempStr, renameTest->getNamespaceURI()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr2, renameTest->getLocalName()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr3, renameTest->getNodeName()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the child / nodevalue
    if (XMLString::compareString(tempStr4, renameTest->getFirstChild()->getNodeValue()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the attribute
    if (!renameTest->getAttributeNodeNS(tempStr4, tempStr2))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the owner element
    if (renameTestParent->getElementsByTagNameNS(tempStr, tempStr2)->getLength() != 1) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestParent->getElementsByTagNameNS(tempStr4, tempStr5)->getLength() != 0) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test user data
    void* renamedocument = renameTest->getUserData(tempStr4);
    if (document != renamedocument) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test isSame and isEqual
    if (!renameTestElementNS->isEqualNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (!renameTestElementNS->isSameNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }


    // rename the Element (null namespace)
    renameTest = (DOMElement*) document->renameNode(renameTestElement, 0, tempStr2);
    // test the name
    if (renameTest->getNamespaceURI())
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTest->getLocalName())
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr2, renameTest->getNodeName()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the child / nodevalue
    if (XMLString::compareString(tempStr5, renameTest->getFirstChild()->getNodeValue()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the attribute
    if (!renameTest->getAttributeNode(tempStr5))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the owner element
    if (renameTestParent->getElementsByTagName(tempStr2)->getLength() != 1) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestParent->getElementsByTagName(tempStr5)->getLength() != 0) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test user data
    renamedocument = renameTest->getUserData(tempStr5);
    if (document != renamedocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }
    // test isSame and isEqual
    if (!renameTestElement->isEqualNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (!renameTestElement->isSameNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }


    // rename the Element (with namespace)
    renameTest = (DOMElement*) document->renameNode(renameTestElement, tempStr, tempStr3);
    // test the name
    if (XMLString::compareString(tempStr, renameTest->getNamespaceURI()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr2, renameTest->getLocalName()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (XMLString::compareString(tempStr3, renameTest->getNodeName()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the child / nodevalue
    if (XMLString::compareString(tempStr5, renameTest->getFirstChild()->getNodeValue()))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestElement->getFirstChild())
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the attribute
    if (!renameTest->getAttributeNode(tempStr5))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestElement->getAttributeNode(tempStr5))
    {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test the owner element
    // the nodelist should be 2 items as we have to count the renameTestElementNS as well
    if (renameTestParent->getElementsByTagNameNS(tempStr, tempStr2)->getLength() != 2) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestParent->getElementsByTagNameNS(0, tempStr2)->getLength() != 0) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestParent->getElementsByTagName(tempStr2)->getLength() != 0) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test user data
    renamedocument = renameTest->getUserData(tempStr5);
    if (document != renamedocument) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    // test userdatahandler
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_RENAMED, tempStr5, document, renameTestElement, renameTest, __LINE__);
    // test isSame and isEqual
    // a new node is created here, so both isSame and isEqual are not compared
    if (renameTestElement->isEqualNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (renameTestElement->isSameNode(renameTest)) {
        fprintf(stderr, "renameNode failed in line %i\n", __LINE__);
        OK = false;
    }


    //setIdAttribute tests

    XMLString::transcode("http://nsa", tempStr4, 3999);
    XMLString::transcode("aa", tempStr5, 3999);

    DOMAttr *idAtt = document->createAttributeNS(tempStr4, tempStr5);

    //tests for node not being on testElementNode
    EXCEPTIONSTEST(testElementNode->setIdAttribute(tempStr4), DOMException::NOT_FOUND_ERR, OK,  1000);
    EXCEPTIONSTEST(testElementNode->setIdAttributeNS(tempStr4, tempStr5), DOMException::NOT_FOUND_ERR, OK,  1001);
    EXCEPTIONSTEST(testElementNode->setIdAttributeNode(idAtt), DOMException::NOT_FOUND_ERR, OK,  1002);

    //should test NO_MODIFICATION_ALLOWED_ERR but dont know how to without direct access to DOMAttrImpl.

    idAtt = document->createAttributeNS(tempStr4, tempStr5);
    idAtt->setValue(tempStr3);
    testElementNode->setAttributeNode(idAtt);
    testElementNode->setIdAttributeNode(idAtt);

    if(!idAtt->isId()) {
        fprintf(stderr, "setIdAttributeNode failed in line %i\n", __LINE__);
        OK = false;
    }

    DOMElement *idEle = document->getElementById(tempStr3);

    if(!idEle || !idEle->isSameNode(testElementNode)) {
        fprintf(stderr, "setIdAttributeNode failed in line %i\n", __LINE__);
        OK = false;
    }

    testElementNode->removeAttributeNode(idAtt);


    XMLString::transcode("someval", tempStr3, 3999);
    idAtt = document->createAttributeNS(tempStr4, tempStr5);
    idAtt->setValue(tempStr3);
    testElementNode->setAttributeNode(idAtt);
    testElementNode->setIdAttributeNS(tempStr4, tempStr5);

    if(!idAtt->isId()) {
        fprintf(stderr, "setIdAttributeNS failed in line %i\n", __LINE__);
        OK = false;
    }

    idEle = document->getElementById(tempStr3);

    if(!idEle || !idEle->isSameNode(testElementNode)) {
        fprintf(stderr, "setIdAttributeNS failed in line %i\n", __LINE__);
        OK = false;
    }

    testElementNode->removeAttributeNode(idAtt);
    idAtt->release();


    XMLString::transcode("somevalDif", tempStr3, 3999);
    idAtt = document->createAttribute(tempStr5);
    idAtt->setValue(tempStr3);
    testElementNode->setAttributeNode(idAtt);
    testElementNode->setIdAttribute(tempStr5);

    if(!idAtt->isId()) {
        fprintf(stderr, "setIdAttribute failed in line %i\n", __LINE__);
        OK = false;
    }

    idEle = document->getElementById(tempStr3);

    if(!idEle || !idEle->isSameNode(testElementNode)) {
        fprintf(stderr, "setIdAttribute failed in line %i\n", __LINE__);
        OK = false;
    }

    testElementNode->removeAttributeNode(idAtt);
    idAtt->release();

    if (!OK)
        printf("\n*****The DOMElement* method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DOMEntity* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testEntity(DOMDocument* document)
{
    DOMEntity* entity;
    DOMNode* node, *node2;
    bool OK = true;
// For debugging*****   printf("\n          testEntity's outputs:\n\n");
    XMLString::transcode("ourEntityNode", tempStr, 3999);
    DOMNode*  abc20 = document->getDoctype()->getEntities()->getNamedItem(tempStr);
    entity = (DOMEntity*) abc20;
    node = entity;
    node2 = entity->cloneNode(true);
    // Check nodes for equality, both their name and value or lack thereof
    if (!(!XMLString::compareString(node->getNodeName(), node2->getNodeName()) &&        // Compares node names for equality
          (node->getNodeValue() != 0 && node2->getNodeValue() != 0)     // Checks to make sure each node has a value node
        ? !XMLString::compareString(node->getNodeValue(), node2->getNodeValue())         // If both have value nodes test those value nodes for equality
        : (node->getNodeValue() == 0 && node2->getNodeValue() == 0)))   // If one node doesn't have a value node make sure both don't
    {
        fprintf(stderr, "'cloneNode' did not clone the DOMEntity* node correctly");
        OK = false;
    }
    // Deep clone test comparison is in testNode & testDocument

    // Test the user data
    // Test simple set and get
    DOMEntity* userTest = entity;
    DOMNode*  userFirst = node;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (userFirst->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(userFirst)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 210 );

    if (!OK)
        printf("\n*****The DOMEntity* method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};


/**
 * This method tests DOMEntityReference* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testEntityReference(DOMDocument* document)
{
    DOMEntityReference* entityReference;
    DOMNode* node, *node2;
    bool OK = true;
// For debugging*****   printf("\n          testEntityReference's outputs:\n");
    DOMNode*  abc30 = document->getLastChild()->getLastChild()->getLastChild()->getFirstChild();
    entityReference = (DOMEntityReference*) abc30;
    node = entityReference;
    node2 = node->cloneNode(true);
    // Check nodes for equality, both their name and value or lack thereof
    if (!(!XMLString::compareString(node->getNodeName(), node2->getNodeName()) &&        // Compares node names for equality
          (node->getNodeValue() != 0 && node2->getNodeValue() != 0)     // Checks to make sure each node has a value node
        ? !XMLString::compareString(node->getNodeValue(), node2->getNodeValue())         // If both have value nodes test those value nodes for equality
        : (node->getNodeValue() == 0 && node2->getNodeValue() == 0)))   // If one node doesn't have a value node make sure both don't
    {
        fprintf(stderr, "'cloneNode' did not clone the DOMEntityReference* node correctly\n");
        OK = false;
    }
    // Deep clone test comparison is in testNode & testDocument

    //  entityReference->setNodeValue("This shouldn't work");//!! Throws a NO_MODIFICATION_ALLOWED_ERR ********

    // Test the user data
    // Test simple set and get
    DOMEntityReference* userTest = entityReference;
    DOMNode*  userFirst = node2;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (node2->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(document)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 211 );

    if (!OK)
        printf("\n*****The DOMEntityReference* method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DOMNode*  methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 *
 *
 ********* This is only for a test of cloneNode "deep"*******
 ********* And for error tests*********
 */
bool DOMTest::testNode(DOMDocument* document)
{
    DOMNode* node, *node2;
    bool result;
    bool OK = true;
// For debugging*****   printf("\n          testNode's outputs:\n");
    node = document->getDocumentElement();
    node2 = node->cloneNode(true);
    result = treeCompare(node, node2); // Deep clone test of cloneNode
    if (result)
    {
        //printf("'cloneNode' successfully cloned this whole node tree (deep)!\n");
    }
    else
    {
        fprintf(stderr, "'cloneNode' did not successfully clone this whole node tree (deep)!\n");
        OK = false;
    }
    //!! The following gives a did not clone successfully message*********
    node = document->getDocumentElement();
    node2 = node->getFirstChild();
    result = treeCompare(node, node2);
    if (!result)
    {
        //fprintf(stderr, "'cloneNode' did not successfully clone this whole node tree (deep)!\n");
    }
    else
    {
        fprintf(stderr, "'cloneNode' was supposed to fail here, either it or 'treeCompare' failed!!!\n");
        OK = false;
    }
    // Deep clone test also in testDocument

    // Test the user data
    // Test simple set and get
    DOMNode* userTest = node;
    DOMNode*  userFirst = node2;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (userFirst->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(document)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!OK)
        printf("\n*****The DOMNode*  method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DOMNotation* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testNotation(DOMDocument* document)
{
    DOMNode* node, *node2;
    DOMNotation* notation;
    bool OK = true;
// For debugging*****   printf("\n          testNotation's outputs:\n");
    XMLString::transcode("ourNotationNode", tempStr, 3999);
	DOMNode*  abc40 = document->getDoctype()->getNotations()->getNamedItem(tempStr);
    notation = (DOMNotation*) abc40;
    node = notation;
    node2 = notation->cloneNode(true);//*****?
    // Check nodes for equality, both their name and value or lack thereof
    if (!(!XMLString::compareString(node->getNodeName(), node2->getNodeName()) &&        // Compares node names for equality
          (node->getNodeValue() != 0 && node2->getNodeValue() != 0)     // Checks to make sure each node has a value node
        ? !XMLString::compareString(node->getNodeValue(), node2->getNodeValue())         // If both have value nodes test those value nodes for equality
        : (node->getNodeValue() == 0 && node2->getNodeValue() == 0)))   // If one node doesn't have a value node make sure both don't
    {
        fprintf(stderr, "'cloneNode' did not clone the DOMNotation* node correctly");
        OK = false;
    }
    // Deep clone test comparison is in testNode & testDocument

    //  notation->setNodeValue("This shouldn't work");//!! Throws a NO_MODIFICATION_ALLOWED_ERR ********

    // Test the user data
    // Test simple set and get
    DOMNotation* userTest = notation;
    DOMNode*  userFirst = node2;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (userFirst->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(document)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 212 );

    if (!OK)
        printf("\n*****The DOMNotation* method calls listed above failed, all others worked correctly.*****\n");
    return OK;
};



/**
 * This method tests DOMProcessingInstruction* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testPI(DOMDocument* document)
{
    DOMProcessingInstruction* pI, *pI2;
    bool OK = true;
// For debugging*****   printf("\n          testPI's outputs:\n");
	DOMNode*   abc50 = document->getDocumentElement()->getFirstChild();// Get doc's DOMProcessingInstruction
    pI  = (DOMProcessingInstruction*) abc50;
	DOMNode*   abc51 = pI->cloneNode(true);//*****?
    pI2 = (DOMProcessingInstruction*) abc51;
    // Check nodes for equality, both their name and value or lack thereof
    if (!(!XMLString::compareString(pI->getNodeName(), pI2->getNodeName()) &&         // Compares node names for equality
         (pI->getNodeValue() != 0 && pI2->getNodeValue() != 0)  // Checks to make sure each node has a value node
        ? !XMLString::compareString(pI->getNodeValue(), pI2->getNodeValue())      // If both have value nodes test those value nodes for equality
        :(pI->getNodeValue() == 0 && pI2->getNodeValue() == 0)))// If one node doesn't have a value node make sure both don't
    {
        fprintf(stderr, "'cloneNode' did not clone the DOMEntity* node correctly\n");
        OK = false;
    }
    // Deep clone test comparison is in testNode & testDocument
    // compare = "This is [#document: 0]'s processing instruction";  // AH Revisit.  Where id
    //                  this ": 0]" stuff come from in the Java version??  I don' think that it is right.
    XMLString::transcode("This is #document's processing instruction", tempStr, 3999);
    if (XMLString::compareString(tempStr, pI->getData()))
    {
        fprintf(stderr, "Warning!!! PI's 'getData' failed!\n");
        OK = false;
    }

    XMLString::transcode("PI's reset data", tempStr, 3999);
    pI->setData(tempStr);
    if (XMLString::compareString(tempStr, pI->getData()))
    {
        fprintf(stderr, "Warning!!! PI's 'setData' failed!\n");
        OK = false;
    }
    XMLString::transcode("dTargetProcessorChannel", tempStr, 3999);
    if (XMLString::compareString(tempStr, pI->getTarget()))
    {
        fprintf(stderr, "Warning!!! PI's 'getTarget' failed!\n");
        OK = false;
    }


    // Restore original PI data.
    XMLString::transcode("This is #document's processing instruction", tempStr, 3999);
    pI->setData(tempStr);

    // Test the user data
    // Test simple set and get
    DOMProcessingInstruction* userTest = pI;
    DOMNode*  userFirst = abc51;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (abc51->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset(reset)
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(document)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 213 );

    if (!OK)
        printf("\n*****The PI method calls listed above failed, all others worked correctly.*****\n");

    return OK;
};



/**
 * This method tests DOMText* methods for the XML DOM implementation
 * @param document org.w3c.dom.DOMDocument
 *
 */
bool DOMTest::testText(DOMDocument* document)
{
    DOMNode* node, *node2;
    DOMText* text;
    bool OK = true;
// For debugging*****   printf("\n          testText's outputs:\n");
    XMLString::transcode("dBodyLevel31", tempStr, 3999);
    DOMNode*  abc70 = document->getDocumentElement()->getElementsByTagName(tempStr)->item(0);
    DOMElement* elem = (DOMElement*) abc70;
    node = elem->getFirstChild(); // charData gets textNode11
    text = (DOMText*) node;
    node2 = node->cloneNode(true);//*****?
    // Check nodes for equality, both their name and value or lack thereof
    if (!(!XMLString::compareString(node->getNodeName(), node2->getNodeName()) &&        // Compares node names for equality
          (node->getNodeValue() != 0 && node2->getNodeValue() != 0)     // Checks to make sure each node has a value node
        ? !XMLString::compareString(node->getNodeValue(), node2->getNodeValue())         // If both have value nodes test those value nodes for equality
        : (node->getNodeValue() == 0 && node2->getNodeValue() == 0)))   // If one node doesn't have a value node make sure both don't
    {
        fprintf(stderr, "'cloneNode' did not clone the DOMText* node correctly\n");
        OK = false;
    }
    // Deep clone test comparison is in testNode & testDocument

    text->splitText(25);
    // Three original text nodes were concatenated by 'normalize' in testElement
    XMLString::transcode("dBodyLevel31'sChildTextNo", tempStr, 3999);
    if (XMLString::compareString(tempStr, text->getNodeValue()))
        {
            fprintf(stderr, "First part of DOMText's split text failed!\n" );
            OK = false;
        }
    // Three original text nodes were concatenated by 'normalize' in testElement
    XMLString::transcode("de11dBodyLevel31'sChildTextNode12dBodyLevel31'sChildTextNode13", tempStr, 3999);
    if (XMLString::compareString(tempStr, text->getNextSibling()->getNodeValue()))
        {
            fprintf(stderr, "The second part of DOMText's split text failed!\n") ;
            OK = false;
        }

    // Re-normalize the text nodes under elem, so that this test can be rerun->
    elem->normalize();


//************************************************* ERROR TESTS
    DOMTest tests;
    //!! Throws INDEX_SIZE_ERR ********************
    //  text.splitText(-1);
    //  text.splitText(100);

    // Test the user data
    // Test simple set and get
    DOMText* userTest = text;
    DOMNode*  userFirst = node2;
    XMLCh* userSecond = tempStr2;
    XMLString::transcode("first", tempStr, 3999);

    XMLString::transcode("document", tempStr2, 3999);
    userTest->setUserData(tempStr2, (void*) document, 0);
    void* mydocument = userTest->getUserData(tempStr2);
    if (document != mydocument) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    userTest->setUserData(tempStr, (void*) userFirst, 0);
    void* myFirst = userTest->getUserData(tempStr);
    if (userFirst != myFirst) {
        fprintf(stderr, "'set/getUserData' in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test overwrite
    void* myFirst2 = userTest->setUserData(tempStr, (void*) userSecond, 0);
    void* mySecond = userTest->getUserData(tempStr);
    if (userSecond != mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst != myFirst2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userFirst == mySecond) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test null
    // test non-exist key
    XMLString::transcode("not-exist", tempStr3, 3999);
    if (userTest->getUserData(tempStr3)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // use a node that does not have user data set before
    if (userFirst->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test reset
    void* mySecond2 = userTest->setUserData(tempStr, (void*) 0, 0);
    void* myNull = userTest->getUserData(tempStr);
    if (userSecond != mySecond2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    void* mydocument2 = userTest->setUserData(tempStr2, (void*) 0, 0);
    void* myNull2 = userTest->getUserData(tempStr2);
    if (mydocument != mydocument2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (myNull2) {
        fprintf(stderr, "overwrite userdata with same key in line %i does not work\n", __LINE__);
        OK = false;
    }

    //the userTest user data table should be null now
    if (userTest->getUserData(tempStr)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }
    if (userTest->getUserData(tempStr2)) {
        fprintf(stderr, "get non-exist user data in line %i does not work\n", __LINE__);
        OK = false;
    }

    // Test DOMUserDataHandler
    // test clone
    userTest->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* mycloned = userTest->cloneNode(true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_CLONED, tempStr2, document, userTest, mycloned, __LINE__);

    // test import
    document->setUserData(tempStr2, (void*) document, &userhandler);
    DOMNode* myimport = document->importNode(userTest,true);
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_IMPORTED, tempStr2, document, userTest, myimport, __LINE__);

    // test delete
    myimport->setUserData(tempStr2, (void*) userTest, &userhandler);
    myimport->release();
    USERDATAHANDLERTEST(userhandler, DOMUserDataHandler::NODE_DELETED, tempStr2, userTest, 0, 0, __LINE__);

    // Test isSameNode
    if (!userTest->isSameNode(userTest)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isSameNode(userFirst)) {
        fprintf(stderr, "isSameNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test isEqualNode
    if (!userTest->isEqualNode(mycloned)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    if (!userTest->isEqualNode(userTest)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }
    if (userTest->isEqualNode(abc70)) {
        fprintf(stderr, "isEqualNode failed in line %i\n", __LINE__);
        OK = false;
    }

    // Test rename, should throw exception
    EXCEPTIONSTEST(document->renameNode(userTest, 0, tempStr2), DOMException::NOT_SUPPORTED_ERR, OK, 214 );

    if (!OK)
        printf("\n*****The DOMText* method calls listed above failed, all others worked correctly.*****\n");

    return OK;
};


/**
 * This method tests setting the DOM Level 3 baseURI attribute at
 * parse time on nodes from the document personal-schema.xml.xml
 *
 */

bool DOMTest::testBaseURI(XercesDOMParser* parser) {

    bool OK = true;

    try {
        // this one assumes executing in samples/data where personal-schema.xml resides
        // please modify if this is not correct
        parser->parse("personal-schema.xml");
    }
    catch (const OutOfMemoryException&)
    {
	    fprintf(stderr, "OutOfMemoryException.\n");        
	    return false;
    }
    catch (...) {
        fprintf(stderr, "parsing personal-schema.xml failed at line %i\n", __LINE__);
        return false;
    }

    // test only if there is no error
    if (!parser->getErrorCount()) {

        //Setup testing strings
        XMLCh *fileSchema = XMLString::transcode("file://");
        XMLCh *filePath = XMLString::transcode("samples/data/personal-schema.xml");

        //Test document baseURI
        DOMDocument *document = parser->getDocument();

        //The baseURI should contain `file://' and `samples/data/personal-schema.xml'
        const XMLCh *docBaseURI = document->getBaseURI();

        if(XMLString::patternMatch(docBaseURI, fileSchema) == -1) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n", __LINE__);
        }

        if(XMLString::patternMatch(docBaseURI, filePath) == -1) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n", __LINE__);
        }

        //Create relative paths from document baseURI

        XMLCh *docBaseURIRoot = new XMLCh [ XMLString::stringLen(docBaseURI) ];
        XMLString::copyNString(docBaseURIRoot, docBaseURI, XMLString::lastIndexOf(docBaseURI, chForwardSlash) + 1);

        XMLCh *base_foobar = new XMLCh [ XMLString::stringLen(docBaseURIRoot) + 8];
        XMLString::copyString(base_foobar, docBaseURIRoot);
        XMLCh *foobar = XMLString::transcode("foo/bar");
        XMLString::catString(base_foobar, foobar);

        XMLCh *base_foobarbar = new XMLCh [ XMLString::stringLen(docBaseURIRoot) + 12];
        XMLString::copyString(base_foobarbar, docBaseURIRoot);
        XMLCh *foobarbar = XMLString::transcode("foo/bar/bar");
        XMLString::catString(base_foobarbar, foobarbar);

        XMLCh *base_foocarbar = new XMLCh [ XMLString::stringLen(docBaseURIRoot) + 12];
        XMLString::copyString(base_foocarbar, docBaseURIRoot);
        XMLCh *foocarbar = XMLString::transcode("foo/car/bar");
        XMLString::catString(base_foocarbar, foocarbar);

        XMLCh *file_autobar = XMLString::transcode("file:///auto/bar");
        XMLCh *file_carfoo = XMLString::transcode("file:///car/foo/");
        XMLCh *file_carfoobarbar = XMLString::transcode("file:///car/foo/bar/bar");

        XMLCh *http_carcar = XMLString::transcode("http://www.example.com/car/car");
        XMLCh *http_barfoo = XMLString::transcode("http://www.example.com/bar/foo/");
        XMLCh *http_barfoofoobar = XMLString::transcode("http://www.example.com/bar/foo/foo/bar");

        //Processing instruction before Document Element (has document baseURI)

        DOMNode *node = document->getFirstChild();
        while(node->getNodeType() != DOMNode::PROCESSING_INSTRUCTION_NODE)
            node = node->getNextSibling();

        if(XMLString::compareString(node->getBaseURI(), docBaseURI) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }

        //Document Element baseURI (same as document)

        node = document->getDocumentElement();

        if(XMLString::compareString(node->getBaseURI(), docBaseURI) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI, failed at line %i\n", __LINE__);
        }

        // <level 1>

        node = node->getFirstChild();
        while(node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        DOMNode *level1 = node;

        // <one>

        node = node->getFirstChild();
        while(node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        if(XMLString::compareString(node->getBaseURI(), base_foobar) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }

        // <inner1>

        node = node->getFirstChild();
        while(node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        if(XMLString::compareString(node->getBaseURI(), base_foobarbar) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }

        // <inner2>

        node = node->getNextSibling();
        while(node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        if(XMLString::compareString(node->getBaseURI(), base_foocarbar) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }

        // <?proc-inst-2?>

        node = node->getNextSibling();
        while(node->getNodeType() != DOMNode::PROCESSING_INSTRUCTION_NODE)
            node = node->getNextSibling();

        if(XMLString::compareString(node->getBaseURI(), base_foobar) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }

        // <level2>

        node = level1->getNextSibling();
        while(node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        DOMNode *level2 = node;

        if(XMLString::compareString(node->getBaseURI(), file_autobar) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }

        // <two>

        node = node->getFirstChild();
        while(node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        if(XMLString::compareString(node->getBaseURI(), file_carfoo) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }

        // <inner1>

        node = node->getFirstChild();
        while(node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        if(XMLString::compareString(node->getBaseURI(), file_carfoobarbar) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }

        // <level3>

        node = level2->getNextSibling();
        while(node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        if(XMLString::compareString(node->getBaseURI(), http_carcar) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }

        // <three>

        node = node->getFirstChild();
        while(node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        if(XMLString::compareString(node->getBaseURI(), http_barfoo) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }

        // <inner1>

        node = node->getFirstChild();
        while(node->getNodeType() != DOMNode::ELEMENT_NODE)
            node = node->getNextSibling();

        if(XMLString::compareString(node->getBaseURI(), http_barfoofoobar) != 0) {
            OK = false;
            fprintf(stderr, "checking baseURI failed at line %i\n",  __LINE__);
        }
    }
    else {
        printf("baseURI test was not carried out\n");
    }

    return OK;
}


/**
 *
 * @param node org.w3c.dom.DOMNode
 * @param node2 org.w3c.dom.DOMNode
 *
 */
bool DOMTest::treeCompare(DOMNode* node, DOMNode* node2)
{
    bool answer = true;

    DOMNode*  kid, *kid2;         // Check the subtree for equality
    kid = node->getFirstChild();
    kid2 = node2->getFirstChild();
    if (kid && kid2)
    {
        answer = treeCompare(kid, kid2);
        if (!answer)
            return answer;
        else
            if (kid->getNextSibling() && kid2->getNextSibling())
            {
                while (kid->getNextSibling() && kid2->getNextSibling())
                {
                    answer = treeCompare(kid->getNextSibling(), kid2->getNextSibling());
                    if (!answer)
                        return answer;
                    else
                    {
                        kid = kid->getNextSibling();
                        kid2 = kid2->getNextSibling();
                    }
                }
            } else
                if (!(!kid->getNextSibling() && !kid2->getNextSibling()))
                {
                    return false;
                }
    } else
        if (kid != kid2)
        {
            // One or the other of (kid1, kid2) is 0, but not both.
            return false;
        }

    if (XMLString::compareString(node->getNodeName(), node2->getNodeName()))
        return false;
    if (node->getNodeValue()==0 && node2->getNodeValue()!=0)
        return false;
    if (node->getNodeValue()!=0 && node2->getNodeValue()==0)
        return false;
    if (XMLString::compareString(node->getNodeValue(), node2->getNodeValue()))
        return false;

    return answer;
};


