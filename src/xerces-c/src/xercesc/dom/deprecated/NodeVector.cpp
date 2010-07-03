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
 * $Id: NodeVector.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

//
//	file:   NodeVector.cpp
//			Implementation of class NodeVector.
//			(Use of STL vector, or equivalent, would have been nice,
//			but is not available.  'NodeImpl *' is the only type
//			kept in Vectors in this DOM implementation, so this is
//			a hardwired implementation for that type.
//

#include "NodeVector.hpp"
#include <xercesc/framework/MemoryManager.hpp>
#include <assert.h>

XERCES_CPP_NAMESPACE_BEGIN


NodeVector::NodeVector(MemoryManager* const manager)
: fMemoryManager(manager)
{
	init(10);
};

NodeVector::NodeVector(unsigned int size,
                       MemoryManager* const manager)
: fMemoryManager(manager)
{
	init(size);
};


void NodeVector::init(unsigned int size) {
	assert(size > 0);
	data = (NodeImpl**) fMemoryManager->allocate(size * sizeof(NodeImpl*));//new NodeImpl *[size];
	assert(data != 0);
	allocatedSize = size;
	nextFreeSlot = 0;
};


NodeVector::~NodeVector() {
	fMemoryManager->deallocate(data);//delete [] data;
};


void NodeVector::addElement(NodeImpl *elem) {
	checkSpace();
	data[nextFreeSlot] = elem;
	++nextFreeSlot;
};


void NodeVector::checkSpace() {
	if (nextFreeSlot == allocatedSize) {
                unsigned int grow = allocatedSize/2;
                if (grow < 50) grow = 50;
		unsigned int newAllocatedSize = allocatedSize + grow;
		NodeImpl **newData = (NodeImpl**) fMemoryManager->allocate
        (
            newAllocatedSize * sizeof(NodeImpl*)
        );//new NodeImpl *[newAllocatedSize];
		assert(newData != 0);
		for (unsigned int i=0; i<allocatedSize; i++) {
			newData[i] = data[i];
		};
		fMemoryManager->deallocate(data);//delete [] data;
		allocatedSize = newAllocatedSize;
		data = newData;
	};
};

	
NodeImpl *NodeVector::elementAt(unsigned int index) {
    if (index >= nextFreeSlot)
        return 0;
	return data[index];
};

NodeImpl *NodeVector::lastElement() {
	if (nextFreeSlot == 0)
		return 0;
	return data[nextFreeSlot-1];
};


void NodeVector::insertElementAt(NodeImpl *elem, unsigned int index) {
	unsigned int i;

	assert(index <= nextFreeSlot);

	checkSpace();
	for (i=nextFreeSlot; i>index; --i) {
		data[i] = data[i-1];
	}
	data[index] = elem;
	++nextFreeSlot;

};


void NodeVector::removeElementAt(unsigned int index) {
	assert(index < nextFreeSlot);
	for (unsigned int i=index; i<nextFreeSlot-1; ++i) {
		data[i] = data[i+1];
	}
	--nextFreeSlot;
};

void NodeVector::reset() {
	nextFreeSlot = 0;
};

void NodeVector::setElementAt(NodeImpl *elem, unsigned int index) {
	assert(index < nextFreeSlot);
	data[index] = elem;
};


unsigned int NodeVector::size() {
	return nextFreeSlot;
};
		

XERCES_CPP_NAMESPACE_END

