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
 * Revision 1.12  2004/09/08 13:57:03  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.11  2002/11/05 21:47:35  tng
 * Explicit code using namespace in application.
 *
 * Revision 1.10  2002/11/04 15:23:43  tng
 * C++ Namespace Support.
 *
 * Revision 1.9  2002/09/23 20:09:23  tng
 * DOM L3: Test baseURI with different parser's setting.
 *
 * Revision 1.8  2002/09/23 18:27:48  tng
 * DOM L3: Test baseURI.   Added by Gareth Reakes and Thomas Ford.
 *
 * Revision 1.7  2002/06/12 18:31:17  tng
 * DOM L3: test the DOMUserDataHandler and set/getUserData
 *
 * Revision 1.6  2002/05/21 18:50:16  tng
 * Test case update: modify to use the latest DOM interface
 *
 * Revision 1.3  2002/03/14 21:59:29  tng
 * Run methods test[NodeType] in the IDOMTest and other fixes.
 *
 * Revision 1.2  2002/02/01 22:44:24  peiyongz
 * sane_include
 *
 * Revision 1.1  2001/08/09 19:28:47  tng
 * Port test case DOMTest to  IDOMTest
 *
 */


/**
 * This class tests methods for XML DOM implementation
 *
 * DOMException errors are tested by calls to DOMExceptionsTest from: Main, docBuilder...
 *
 */

#include <xercesc/dom/DOM.hpp>

//  define null for compatibility with original Java source code.
#define null 0

XERCES_CPP_NAMESPACE_USE

XERCES_CPP_NAMESPACE_BEGIN
class XercesDOMParser;
XERCES_CPP_NAMESPACE_END


class DOMTest {
public:
	static DOMElement           *testElementNode;
	static DOMAttr              *testAttributeNode;
	static DOMText              *testTextNode;
	static DOMCDATASection      *testCDATASectionNode;
	static DOMEntityReference   *testEntityReferenceNode;
	static DOMEntity            *testEntityNode;
	static DOMProcessingInstruction *testProcessingInstructionNode;
	static DOMComment           *testCommentNode;
	static DOMDocument          *testDocumentNode;
	static DOMDocumentType      *testDocumentTypeNode;
	static DOMDocumentFragment  *testDocumentFragmentNode;
	static DOMNotation          *testNotationNode;


DOMTest();

DOMDocument* createDocument();
DOMDocumentType* createDocumentType(DOMDocument* doc, XMLCh* name);
DOMEntity* createEntity(DOMDocument* doc, XMLCh* name);
DOMNotation* createNotation(DOMDocument* doc, XMLCh* name);
bool docBuilder(DOMDocument* document, XMLCh* name);

void findTestNodes(DOMDocument* document);
void findTestNodes(DOMNode* node);


bool testAttr(DOMDocument* document);
bool testCDATASection(DOMDocument* document);
bool testCharacterData(DOMDocument* document);
bool testChildNodeList(DOMDocument* document);
bool testComment(DOMDocument* document);
bool testDeepNodeList(DOMDocument* document);

/**
 **** ALL DOMDocument create methods are run in docBuilder except createAttribute which is in testAttribute**
 */
bool testDocument(DOMDocument* document);


/**
 ********This really isn't needed, only exists to throw NO_MODIFICATION_ALLOWED_ERR ********
 */
bool testDocumentFragment(DOMDocument* document);

bool testDocumentType(DOMDocument* document);
bool testDOMerrors(DOMDocument* document);
bool testDOMImplementation(DOMDocument* document);
bool testElement(DOMDocument* document);
bool testEntity(DOMDocument* document);
bool testEntityReference(DOMDocument* document);


/**
 ********* This is only for a test of cloneNode "deep"*******
 ********* And for error tests*********
 */
bool testNode(DOMDocument* document);

bool testNotation(DOMDocument* document);
bool testPI(DOMDocument* document);
bool testText(DOMDocument* document);
bool treeCompare(DOMNode* node, DOMNode* node2);

bool testBaseURI(XercesDOMParser* parser);

};

class myUserDataHandler : public DOMUserDataHandler {
private:
    DOMOperationType currentType;
    XMLCh* currentKey;
    void* currentData;
    DOMNode* currentSrc;
    DOMNode* currentDst;

public:
    myUserDataHandler() :
      currentKey(0),
      currentData(0),
      currentSrc(0),
      currentDst(0) {};

    virtual void handle(DOMOperationType operation,
                const XMLCh* const key,
                void* data,
                const DOMNode* src,
                const DOMNode* dst)
    {
        currentType = operation;
        currentKey = (XMLCh*) key;
        currentData = data;
        currentSrc = (DOMNode*) src;
        currentDst = (DOMNode*) dst;
    };

    DOMOperationType getCurrentType() {
        return currentType;
    };
    XMLCh* getCurrentKey() const {
        return currentKey;
    };
    void* getCurrentData() const {
        return currentData;
    };
    DOMNode* getCurrentSrc() const {
        return currentSrc;
    };
    DOMNode* getCurrentDst() const {
        return currentDst;
    };

};


