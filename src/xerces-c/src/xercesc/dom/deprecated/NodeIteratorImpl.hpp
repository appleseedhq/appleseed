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
 * $Id: NodeIteratorImpl.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef NodeIteratorImpl_HEADER_GUARD_
#define NodeIteratorImpl_HEADER_GUARD_

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/deprecated/DOM.hpp> for the entire
//  DOM API, or DOM_*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//

#include "DOM_Node.hpp"
#include "DOM_NodeIterator.hpp"
#include "RefCountedImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


class DEPRECATED_DOM_EXPORT NodeIteratorImpl : public RefCountedImpl {
	protected:
		NodeIteratorImpl ();

	public:
		virtual ~NodeIteratorImpl ();
		NodeIteratorImpl (
            DOM_Node root,
            unsigned long whatToShow,
            DOM_NodeFilter* nodeFilter,
            bool expandEntityRef);

        NodeIteratorImpl ( const NodeIteratorImpl& toCopy);
		
        NodeIteratorImpl& operator= (const NodeIteratorImpl& other);
		
		DOM_Node getRoot ();
        unsigned long getWhatToShow ();
		DOM_NodeFilter* getFilter ();

		DOM_Node nextNode ();
		DOM_Node previousNode ();
		bool acceptNode (DOM_Node node);
		DOM_Node matchNodeOrParent (DOM_Node node);
		DOM_Node nextNode (DOM_Node node, bool visitChildren);
		DOM_Node previousNode (DOM_Node node);
		void removeNode (DOM_Node node);

		void unreferenced();

		void detach ();

        // Get the expandEntity reference flag.
        bool getExpandEntityReferences();


	private:
		//
		// Data
		//
		// The root.
		DOM_Node fRoot;

		// The whatToShow mask.
		unsigned long fWhatToShow;

		// The NodeFilter reference.
		DOM_NodeFilter* fNodeFilter;

        // The expandEntity reference flag.
        bool  fExpandEntityReferences;

		bool fDetached;

		//
		// Iterator state - current node and direction.
		//
		// Note: The current node and direction are sufficient to implement
		// the desired behaviour of the current pointer being _between_
		// two nodes. The fCurrentNode is actually the last node returned,
		// and the
		// direction is whether the pointer is in front or behind this node.
		// (usually akin to whether the node was returned via nextNode())
		// (eg fForward = true) or previousNode() (eg fForward = false).

		// The last Node returned.
		DOM_Node fCurrentNode;

		// The direction of the iterator on the fCurrentNode.
		//  <code>
		//  nextNode()  ==      fForward = true;<br>
		//  previousNode() ==   fForward = false;<br>
		//  </code>
		bool fForward;


};

XERCES_CPP_NAMESPACE_END

#endif
