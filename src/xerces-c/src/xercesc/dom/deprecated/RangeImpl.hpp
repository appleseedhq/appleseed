#ifndef RangeImpl_HEADER_GUARD_
#define RangeImpl_HEADER_GUARD_
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
 * $Id: RangeImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/deprecated/DOM.hpp> for the entire
//  DOM API, or DOM_*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//


#include "DOM_Node.hpp"
#include "RefCountedImpl.hpp"
#include "DOM_Range.hpp"
#include "DOM_Text.hpp"
#include "DOM_Document.hpp"
#include <xercesc/util/RefVectorOf.hpp>

XERCES_CPP_NAMESPACE_BEGIN


typedef RefVectorOf<RangeImpl> Ranges;

class DEPRECATED_DOM_EXPORT RangeImpl : public RefCountedImpl {
public:
    //c'tor
    RangeImpl(DOM_Document doc);
    RangeImpl(const RangeImpl& other);

    //d'tor
    ~RangeImpl();

    //referencing related functions
    virtual void                unreferenced();


    //getter functions
    DOM_Node    getStartContainer() const;
    unsigned    int getStartOffset() const;
    DOM_Node    getEndContainer() const;
    unsigned int getEndOffset() const;



    void        collapse(bool toStart);
    bool        getCollapsed() const;

    void        setStartBefore(const DOM_Node& node);
    void        setStartAfter(const DOM_Node& node);
    void        setEndBefore(const DOM_Node& node);
    void        setEndAfter(const DOM_Node& node);

    void        setStart(const DOM_Node& node, unsigned int offset);
    void        setEnd(const DOM_Node& node, unsigned int offset);

    void        selectNode(const DOM_Node& node);
    void        selectNodeContents(const DOM_Node& node);

    short       compareBoundaryPoints(DOM_Range::CompareHow how, RangeImpl* range) const;

    void        detach();

    void        deleteContents();

    RangeImpl*  cloneRange() const;
    DOMString   toString() const;

    DOM_Document getDocument();
    void        surroundContents(DOM_Node& node);
    DOM_DocumentFragment extractContents();
    DOM_DocumentFragment cloneContents() const;
    void        insertNode(DOM_Node& newNode);
    const DOM_Node    getCommonAncestorContainer() const;

    // functions to inform all existing valid ranges about a change
    void updateSplitInfo(TextImpl* oldNode, TextImpl* startNode, unsigned int offset);
    void updateRangeForInsertedNode(NodeImpl* node);
    void receiveReplacedText(NodeImpl* node);
    void updateRangeForDeletedText(DOM_Node& node, unsigned int offset, int count);
    void updateRangeForDeletedNode(NodeImpl* node);

private:
    enum TraversalType {
        EXTRACT_CONTENTS = 1,
        CLONE_CONTENTS   = 2,
        DELETE_CONTENTS  = 3
    };

    enum TraversePoint {
        BEFORE  = -1,
        START   = 0,
        AFTER   = 1
    };

    //setter functions
    void        setStartContainer(const DOM_Node& node);
    void        setStartOffset(unsigned int offset) ;
    void        setEndContainer(const DOM_Node& node);
    void        setEndOffset(unsigned int offset) ;

    //misc functions
    void        validateNode(const DOM_Node& node) const;
    bool        isValidAncestorType(const DOM_Node& node) const;
    bool        hasLegalRootContainer(const DOM_Node& node) const;
    bool        isLegalContainedNode(const DOM_Node& node ) const;
    void        checkIndex(const DOM_Node& node, unsigned int offset) const;
    static bool isAncestorOf(const DOM_Node& a, const DOM_Node& b);

    unsigned short indexOf(const DOM_Node& child, const DOM_Node& parent) const;

    const DOM_Node    commonAncestorOf(const DOM_Node& pointA, const DOM_Node& pointB) const;
    DOM_Node    nextNode(const DOM_Node& node, bool visitChildren) const;
    DOM_DocumentFragment traverseContents(TraversalType type);
    void        checkReadOnly(DOM_Node& start, DOM_Node& end,
                    unsigned int starOffset, unsigned int endOffset);
    void        recurseTreeAndCheck(DOM_Node& start, DOM_Node& end);
    DOM_Node    removeChild(DOM_Node& parent, DOM_Node& child);

    DOM_DocumentFragment traverseSameContainer( int how );
    DOM_DocumentFragment traverseCommonStartContainer( DOM_Node endAncestor, int how );
    DOM_DocumentFragment traverseCommonEndContainer( DOM_Node startAncestor, int how );
    DOM_DocumentFragment traverseCommonAncestors( DOM_Node startAncestor, DOM_Node endAncestor, int how );
    DOM_Node    traverseRightBoundary( DOM_Node root, int how );
    DOM_Node    traverseLeftBoundary( DOM_Node root, int how );
    DOM_Node    traverseNode( DOM_Node n, bool isFullySelected, bool isLeft, int how );
    DOM_Node    traverseFullySelected( DOM_Node n, int how );
    DOM_Node    traversePartiallySelected( DOM_Node n, int how );
    DOM_Node    traverseTextNode( DOM_Node n, bool isLeft, int how );
    DOM_Node    getSelectedNode( DOM_Node container, int offset );


    //private data
    DOM_Node        fStartContainer;
    unsigned int    fStartOffset;
    DOM_Node        fEndContainer;
    unsigned int    fEndOffset;
    bool            fCollapsed;
    DOM_Document    fDocument;
    bool            fDetached;

    DOM_Node        fRemoveChild;

};

XERCES_CPP_NAMESPACE_END

#endif
