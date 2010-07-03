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
 * $Id: ChildNode.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// This class only adds the ability to have siblings

#include "ChildNode.hpp"

XERCES_CPP_NAMESPACE_BEGIN


ChildNode::ChildNode(DocumentImpl *ownerDoc)
  : NodeImpl(ownerDoc)
{
    this->previousSibling  = null;
    this->nextSibling  = null;
}

// This only makes a shallow copy, cloneChildren must also be called for a
// deep clone
ChildNode::ChildNode(const ChildNode &other)
  : NodeImpl(other)
{
    // Need to break the association w/ original siblings and parent
    this->previousSibling = null;
    this->nextSibling = null;
    isFirstChild(false);
}

ChildNode::~ChildNode() {
}

NodeImpl * ChildNode::getNextSibling() {
    return nextSibling;
}

NodeImpl * ChildNode::getParentNode()
{
    // if we have an owner, ownerNode is our parent, otherwise it's
    // our ownerDocument and we don't have a parent
    return isOwned() ? ownerNode : null;
}

NodeImpl * ChildNode::getPreviousSibling() {
    // if we are the firstChild, previousSibling actually refers to our
    // parent's lastChild, but we hide that
    return isFirstChild() ? null : previousSibling;
}

XERCES_CPP_NAMESPACE_END

