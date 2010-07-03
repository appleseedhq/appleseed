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
 * $Id: CMAny.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/validators/common/CMStateSet.hpp>
#include <xercesc/validators/common/CMAny.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  CMUnaryOp: Constructors and Destructor
// ---------------------------------------------------------------------------
CMAny::CMAny( const ContentSpecNode::NodeTypes type
            , const unsigned int               URI
            , const unsigned int               position
            ,       MemoryManager* const       manager) :
       CMNode(type, manager)
     , fURI(URI)
     , fPosition(position)
{
    if ((type & 0x0f) != ContentSpecNode::Any
    &&  (type & 0x0f) != ContentSpecNode::Any_Other
    &&  (type & 0x0f) != ContentSpecNode::Any_NS)
    {
		ThrowXMLwithMemMgr1(RuntimeException,
		          XMLExcepts::CM_NotValidSpecTypeForNode,
				  "CMAny", manager);
    }

}

CMAny::~CMAny()
{
}

// ---------------------------------------------------------------------------
//  Getter methods
// ---------------------------------------------------------------------------
unsigned int CMAny::getURI() const
{
	return fURI;
}

unsigned int CMAny::getPosition() const
{
    return fPosition;
}

// ---------------------------------------------------------------------------
//  Setter methods
// ---------------------------------------------------------------------------
void CMAny::setPosition(const unsigned int newPosition)
{
    fPosition = newPosition;
}

// ---------------------------------------------------------------------------
//  Implementation of public CMNode virtual interface
// ---------------------------------------------------------------------------
bool CMAny::isNullable() const
{
    // Leaf nodes are never nullable unless its an epsilon node
    return (fPosition == -1);
}

// ---------------------------------------------------------------------------
//  Implementation of protected CMNode virtual interface
// ---------------------------------------------------------------------------
void CMAny::calcFirstPos(CMStateSet& toSet) const
{
    // If we are an epsilon node, then the first pos is an empty set
    if (fPosition == -1)
        toSet.zeroBits();
    else
    // Otherwise, its just the one bit of our position
        toSet.setBit(fPosition);

	return;
}

void CMAny::calcLastPos(CMStateSet& toSet) const
{
    // If we are an epsilon node, then the last pos is an empty set
    if (fPosition == -1)
        toSet.zeroBits();
    // Otherwise, its just the one bit of our position
    else
        toSet.setBit(fPosition);

	return;
}

XERCES_CPP_NAMESPACE_END
