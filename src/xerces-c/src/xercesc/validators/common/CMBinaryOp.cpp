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
 * $Id: CMBinaryOp.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/validators/common/ContentSpecNode.hpp>
#include <xercesc/validators/common/CMBinaryOp.hpp>
#include <xercesc/validators/common/CMStateSet.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  CMBinaryOp: Constructors
// ---------------------------------------------------------------------------
CMBinaryOp::CMBinaryOp( const ContentSpecNode::NodeTypes type
                        ,     CMNode* const              leftToAdopt
                        ,     CMNode* const              rightToAdopt
                        ,     MemoryManager* const       manager) :
    CMNode(type, manager)
    , fLeftChild(leftToAdopt)
    , fRightChild(rightToAdopt)
{
    // Insure that its one of the types we require
    if (((type & 0x0f) != ContentSpecNode::Choice)
    &&  ((type & 0x0f) != ContentSpecNode::Sequence))
    {
        ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::CM_BinOpHadUnaryType, manager);
    }
}

CMBinaryOp::~CMBinaryOp()
{
    delete fLeftChild;
    delete fRightChild;
}


// ---------------------------------------------------------------------------
//  CMBinaryOp: Getter methods
// ---------------------------------------------------------------------------
const CMNode* CMBinaryOp::getLeft() const
{
    return fLeftChild;
}

CMNode* CMBinaryOp::getLeft()
{
    return fLeftChild;
}

const CMNode* CMBinaryOp::getRight() const
{
    return fRightChild;
}

CMNode* CMBinaryOp::getRight()
{
    return fRightChild;
}


// ---------------------------------------------------------------------------
//  CMBinaryOp: Implementation of the public CMNode virtual interface
// ---------------------------------------------------------------------------
bool CMBinaryOp::isNullable() const
{
    //
    //  If its an alternation, then if either child is nullable then
    //  this node is nullable. If its a concatenation, then both of
    //  them have to be nullable.
    //
    if ((getType() & 0x0f) == ContentSpecNode::Choice)
        return (fLeftChild->isNullable() || fRightChild->isNullable());

    return (fLeftChild->isNullable() && fRightChild->isNullable());
}


// ---------------------------------------------------------------------------
//  CMBinaryOp: Implementation of the protected CMNode virtual interface
// ---------------------------------------------------------------------------
void CMBinaryOp::calcFirstPos(CMStateSet& toSet) const
{
    if ((getType() & 0x0f) == ContentSpecNode::Choice)
    {
        // Its the the union of the first positions of our children.
        toSet = fLeftChild->getFirstPos();
        toSet |= fRightChild->getFirstPos();
    }
    else if ((getType() & 0x0f) == ContentSpecNode::Sequence)
    {
        //
        //  If our left child is nullable, then its the union of our
        //  children's first positions. Else is our left child's first
        //  positions.
        //
        toSet = fLeftChild->getFirstPos();
        if (fLeftChild->isNullable())
            toSet |= fRightChild->getFirstPos();
    }
}

void CMBinaryOp::calcLastPos(CMStateSet& toSet) const
{
    if ((getType() & 0x0f) == ContentSpecNode::Choice)
    {
        // Its the the union of the first positions of our children.
        toSet = fLeftChild->getLastPos();
        toSet |= fRightChild->getLastPos();
    }
    else if ((getType() & 0x0f) == ContentSpecNode::Sequence)
    {
        //
        //  If our right child is nullable, then its the union of our
        //  children's last positions. Else is our right child's last
        //  positions.
        //
        toSet = fRightChild->getLastPos();
        if (fRightChild->isNullable())
            toSet |= fLeftChild->getLastPos();
    }
}

XERCES_CPP_NAMESPACE_END
