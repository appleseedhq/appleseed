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
 * $Id: CharacterDataImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "CharacterDataImpl.hpp"
#include "DOM_DOMException.hpp"
#include "RangeImpl.hpp"
#include "DocumentImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


CharacterDataImpl::CharacterDataImpl(DocumentImpl *ownerDoc,
                                     const DOMString &dat)
    : ChildNode(ownerDoc)
{
    this->data = dat.clone();
};

CharacterDataImpl::CharacterDataImpl(const CharacterDataImpl &other, bool /*deep*/)
    : ChildNode(other)
{
    data = other.data.clone();
};


CharacterDataImpl::~CharacterDataImpl() {
};


DOMString CharacterDataImpl::getNodeValue()
{
    return data;
};


void CharacterDataImpl::setNodeValue(const DOMString &value)
{
    if (isReadOnly())
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                               null);
    data = value.clone();

    if (this->getOwnerDocument() != null) {
        typedef RefVectorOf<RangeImpl> RangeImpls;
        RangeImpls* ranges = this->getOwnerDocument()->getRanges();
        if (ranges != null) {
            unsigned int sz = ranges->size();
            if (sz != 0) {
                for (unsigned int i =0; i<sz; i++) {
                    ranges->elementAt(i)->receiveReplacedText( this);
                }
            }
        }
    }
};


void CharacterDataImpl::appendData(const DOMString &dat)
{
    if(isReadOnly())
        throw DOM_DOMException(
        DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);

    this->data.appendData(dat);
};


void CharacterDataImpl::deleteData(unsigned int offset, unsigned int count)
{
    if (isReadOnly())
        throw DOM_DOMException(
        DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);

    // Note: the C++ DOMString operation throws the correct DOMExceptions
    //       when parameter values are bad.
    //
    data.deleteData(offset, count);

    if (this->getOwnerDocument() != null) {
        typedef RefVectorOf<RangeImpl> RangeImpls;
        RangeImpls* ranges = this->getOwnerDocument()->getRanges();
        if (ranges != null) {
            unsigned int sz = ranges->size();
            if (sz != 0) {
                for (unsigned int i =0; i<sz; i++) {
                    DOM_Node dn = DOM_Node(this);
                    ranges->elementAt(i)->updateRangeForDeletedText( dn, offset, count);
                }
            }
        }
    }
};



DOMString &CharacterDataImpl::getData()
{
    return data;
};


//
//  getCharDataLength - return the length of the character data string.
//                      Note:  in the public DOM API, the name of this method
//                      is getLength(), but has been renamed here to avoid a
//                      conflict with NodeListImpl::getLength().  The conflict
//                      occurs because NodeListImpl is a base class of us.
//                      DOM_CharData::getLength() delegates to this method, so
//                      all of the names are correct from an external API
//                      point of view.
//
unsigned int CharacterDataImpl::getCharDataLength()
{
    return data.length();
};



void CharacterDataImpl::insertData(unsigned int offset, const DOMString &dat)
{

    if (isReadOnly())
        throw DOM_DOMException(
        DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);

    // Note: the C++ DOMString operation throws the correct DOMExceptions
    //       when parameter values are bad.
    //
    this->data.insertData(offset, dat);
}



void CharacterDataImpl::replaceData(unsigned int offset, unsigned int count,
                                    const DOMString &dat)
{
    if (isReadOnly())
        throw DOM_DOMException(
        DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR, null);
    deleteData(offset, count);
    insertData(offset, dat);
};




void CharacterDataImpl::setData(const DOMString &arg)
{
    if (isReadOnly())
        throw DOM_DOMException(DOM_DOMException::NO_MODIFICATION_ALLOWED_ERR,
                               null);
    data = arg.clone();
};





DOMString CharacterDataImpl::substringData(unsigned int offset,
                                           unsigned int count)
{

    // Note: the C++ DOMString operation throws the correct DOMExceptions
    //       when parameter values are bad.
    //
    return data.substringData(offset, count);
};

XERCES_CPP_NAMESPACE_END

