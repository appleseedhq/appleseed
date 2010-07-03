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
 * $Id: DOM_CharacterData.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_CharacterData.hpp"
#include "CharacterDataImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


DOM_CharacterData::DOM_CharacterData()
: DOM_Node(null)
{
};

DOM_CharacterData::DOM_CharacterData(CharacterDataImpl *impl) :
DOM_Node(impl)
{
};


DOM_CharacterData::DOM_CharacterData(const DOM_CharacterData & other) :
DOM_Node(other)
{
};


DOM_CharacterData::~DOM_CharacterData() {
};

DOM_CharacterData & DOM_CharacterData::operator = (const DOM_CharacterData &other)
{
    return (DOM_CharacterData &) DOM_Node::operator = (other);
};

DOM_CharacterData & DOM_CharacterData::operator = (const DOM_NullPtr *other)
{
    return (DOM_CharacterData &) DOM_Node::operator = (other);
};


DOMString DOM_CharacterData::getData() const
{
    return ((CharacterDataImpl *)fImpl)->getData().clone();
};


void DOM_CharacterData::setData(const DOMString &data){
    ((CharacterDataImpl *)fImpl)->setData(data);
};



unsigned int DOM_CharacterData::getLength() const
{
    return ((CharacterDataImpl *)fImpl)->getCharDataLength();
};



DOMString DOM_CharacterData::substringData(unsigned int offset, unsigned int count) const
{
    return ((CharacterDataImpl *)fImpl)->substringData(offset, count);
};



void DOM_CharacterData::appendData(const DOMString &arg)
{
    ((CharacterDataImpl *)fImpl)->appendData(arg);
};



void DOM_CharacterData::insertData(unsigned int offset, const DOMString &arg){
    ((CharacterDataImpl *)fImpl)->insertData(offset, arg);
};




void DOM_CharacterData::deleteData(unsigned int offset, unsigned int count)
{
    ((CharacterDataImpl *)fImpl)->deleteData(offset, count);
};


void DOM_CharacterData::replaceData(unsigned int offset, unsigned int count, const DOMString &arg)
{
    ((CharacterDataImpl *)fImpl)->replaceData(offset, count, arg);
};


XERCES_CPP_NAMESPACE_END

