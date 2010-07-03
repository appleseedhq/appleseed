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
 * $Id: TreeWalkerImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/deprecated/DOM.hpp> for the entire
//  DOM API, or DOM_*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//

#ifndef TreeWalkerImpl_HEADER_GUARD_
#define TreeWalkerImpl_HEADER_GUARD_

#include <xercesc/util/XMemory.hpp>
#include "DOM_TreeWalker.hpp"
#include "RefCountedImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN

class DEPRECATED_DOM_EXPORT TreeWalkerImpl : public RefCountedImpl {

	public:
    // Implementation Note: No state is kept except the data above
    // (fWhatToShow, fNodeFilter, fCurrentNode, fRoot) such that
    // setters could be created for these data values and the
    // implementation will still work.

    /** Public constructor */
    TreeWalkerImpl (
        DOM_Node root,
        unsigned long whatToShow,
        DOM_NodeFilter* nodeFilter,
        bool expandEntityRef);
    TreeWalkerImpl (const TreeWalkerImpl& twi);
    TreeWalkerImpl& operator= (const TreeWalkerImpl& twi);

    // Return the root DOM_Node.
    DOM_Node getRoot ();

    // Return the whatToShow value.
    unsigned long  getWhatToShow ();

    // Return the NodeFilter.
    DOM_NodeFilter* getFilter ();

	
    // Return the current DOM_Node.
    DOM_Node getCurrentNode ();

    // Return the current Node.
    void setCurrentNode (DOM_Node node);

    // Return the parent Node from the current node,
    //  after applying filter, whatToshow.
    //  If result is not null, set the current Node.
    DOM_Node parentNode ();

    // Return the first child Node from the current node,
    //  after applying filter, whatToshow.
    //  If result is not null, set the current Node.
    DOM_Node firstChild ();

    // Return the last child Node from the current node,
    //  after applying filter, whatToshow.
    //  If result is not null, set the current Node.
    DOM_Node lastChild ();

    // Return the previous sibling Node from the current node,
    //  after applying filter, whatToshow.
    //  If result is not null, set the current Node.
    DOM_Node previousSibling ();

    // Return the next sibling Node from the current node,
    //  after applying filter, whatToshow.
    //  If result is not null, set the current Node.

    DOM_Node nextSibling ();
    // Return the previous Node from the current node,
    //  after applying filter, whatToshow.
    //  If result is not null, set the current Node.
    DOM_Node previousNode ();

    // Return the next Node from the current node,
    //  after applying filter, whatToshow.
    //  If result is not null, set the current Node.
    DOM_Node nextNode ();

    void unreferenced ();

    // Get the expandEntity reference flag.
    bool getExpandEntityReferences();

protected:

    // Internal function.
    //  Return the parent Node, from the input node
    //  after applying filter, whatToshow.
    //  The current node is not consulted or set.
    DOM_Node getParentNode (DOM_Node node);

    // Internal function.
    //  Return the nextSibling Node, from the input node
    //  after applying filter, whatToshow.
    //  The current node is not consulted or set.
    DOM_Node getNextSibling (DOM_Node node);

    // Internal function.
    //  Return the previous sibling Node, from the input node
    //  after applying filter, whatToshow.
    //  The current node is not consulted or set.
    DOM_Node getPreviousSibling (DOM_Node node);

    // Internal function.
    //  Return the first child Node, from the input node
    //  after applying filter, whatToshow.
    //  The current node is not consulted or set.
    DOM_Node getFirstChild (DOM_Node node);

    // Internal function.
    //  Return the last child Node, from the input node
    //  after applying filter, whatToshow.
    //  The current node is not consulted or set.
    DOM_Node getLastChild (DOM_Node node);

    // The node is accepted if it passes the whatToShow and the filter.
    short acceptNode (DOM_Node node);

    		
private:
    // The whatToShow mask.
    unsigned long fWhatToShow;

    // The NodeFilter reference.
    DOM_NodeFilter* fNodeFilter;

    // The current Node.
    DOM_Node fCurrentNode;

    // The root Node.
    DOM_Node fRoot;

    // The expandEntity reference flag.
    bool fExpandEntityReferences;
};

XERCES_CPP_NAMESPACE_END

#endif
