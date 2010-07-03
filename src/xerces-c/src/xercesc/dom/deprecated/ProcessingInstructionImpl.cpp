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
 * $Id: ProcessingInstructionImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "ProcessingInstructionImpl.hpp"
#include "DocumentImpl.hpp"
#include "NodeImpl.hpp"
#include "DOM_DOMException.hpp"
#include "DOM_Node.hpp"

XERCES_CPP_NAMESPACE_BEGIN


ProcessingInstructionImpl::ProcessingInstructionImpl(DocumentImpl *ownerDoc,
                                                     const DOMString &targt,
                                                     const DOMString &dat)
    : ChildNode(ownerDoc)
{
    this->target = targt.clone();
    this->data = dat.clone();
};


ProcessingInstructionImpl::ProcessingInstructionImpl(
                                        const ProcessingInstructionImpl &other,
                                        bool /*deep*/)
    : ChildNode(other)
{
    target = other.target.clone();
    data = other.data.clone();
};


ProcessingInstructionImpl::~ProcessingInstructionImpl()
{
};


NodeImpl *ProcessingInstructionImpl::cloneNode(bool deep)
{
    return new ProcessingInstructionImpl(*this, deep);
};


DOMString ProcessingInstructionImpl::getNodeName()
{
    return target;
};


short ProcessingInstructionImpl::getNodeType() {
    return DOM_Node::PROCESSING_INSTRUCTION_NODE;
};


DOMString ProcessingInstructionImpl::getNodeValue()
{
    return data.clone();
};


void ProcessingInstructionImpl::setNodeValue(const DOMString &value)
{
    if (isReadOnly())
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                               null);
    data = value.clone();
};


DOMString ProcessingInstructionImpl::getData()
{
    return data.clone();
};


/** A PI's "target" states what processor channel the PI's data
should be directed to. It is defined differently in HTML and XML.

  In XML, a PI's "target" is the first (whitespace-delimited) token
  following the "<?" token that begins the PI.

    In HTML, target is always null.

      Note that getNodeName is aliased to getTarget.
*/
DOMString ProcessingInstructionImpl::getTarget()
{
    return target.clone();
};


/**
* Change the data content of this PI.
* Note that setNodeValue is aliased to setData
* @see getData().
* @throws DOMException(NO_MODIFICATION_ALLOWED_ERR) if node is read-only.
*/
void ProcessingInstructionImpl::setData(const DOMString &arg)
{
    if (isReadOnly())
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                               null);
    data = arg.clone();
};

XERCES_CPP_NAMESPACE_END

