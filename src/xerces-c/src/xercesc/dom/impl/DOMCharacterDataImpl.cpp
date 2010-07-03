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
 * $Id: DOMCharacterDataImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOMCharacterDataImpl.hpp"
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/dom/DOMNode.hpp>
#include "DOMRangeImpl.hpp"
#include "DOMDocumentImpl.hpp"
#include "DOMCasts.hpp"
#include "DOMStringPool.hpp"
#include <xercesc/util/XMLUniDefs.hpp>

XERCES_CPP_NAMESPACE_BEGIN

DOMCharacterDataImpl::DOMCharacterDataImpl(DOMDocument *doc, const XMLCh *dat)
 : fDataBuf(0)
 , fDoc(0)
{
    fDoc = (DOMDocumentImpl*)doc;

    fDataBuf = fDoc->popBuffer();
    if (!fDataBuf)
        fDataBuf = new (fDoc) DOMBuffer(fDoc, dat);
    else
        fDataBuf->set(dat);

}


DOMCharacterDataImpl::DOMCharacterDataImpl(const DOMCharacterDataImpl &other)
 : fDataBuf(0)
 , fDoc(0)
{
    fDoc = (DOMDocumentImpl*)other.fDoc;

    fDataBuf = fDoc->popBuffer();
    if (!fDataBuf)
        fDataBuf = new (fDoc) DOMBuffer(fDoc, other.fDataBuf->getRawBuffer());
    else
        fDataBuf->set(other.fDataBuf->getRawBuffer());

}


DOMCharacterDataImpl::~DOMCharacterDataImpl() {
}


const XMLCh * DOMCharacterDataImpl::getNodeValue() const
{
    return fDataBuf->getRawBuffer();
}


void DOMCharacterDataImpl::setNodeValue(const DOMNode *node, const XMLCh *value)
{
    if (castToNodeImpl(node)->isReadOnly())
        throw DOMException(DOMException::NO_MODIFICATION_ALLOWED_ERR, 0, GetDOMCharacterDataImplMemoryManager);
    fDataBuf->set(value);

    if (node->getOwnerDocument() != 0) {
        Ranges* ranges = ((DOMDocumentImpl *)node->getOwnerDocument())->getRanges();
        if (ranges != 0) {
            XMLSize_t sz = ranges->size();
            if (sz != 0) {
                for (XMLSize_t i =0; i<sz; i++) {
                    ranges->elementAt(i)->receiveReplacedText((DOMNode*)node);
                }
            }
        }
    }
}


void DOMCharacterDataImpl::appendData(const DOMNode *node, const XMLCh *dat)
{
    if(castToNodeImpl(node)->isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0, GetDOMCharacterDataImplMemoryManager);

    fDataBuf->append(dat);
}


void DOMCharacterDataImpl::deleteData(const DOMNode *node, XMLSize_t offset, XMLSize_t count)
{
    if (castToNodeImpl(node)->isReadOnly())
        throw DOMException(DOMException::NO_MODIFICATION_ALLOWED_ERR, 0, GetDOMCharacterDataImplMemoryManager);

    // Note: the C++ XMLCh * operation throws the correct DOMExceptions
    //       when parameter values are bad.
    //

    XMLSize_t len = this->fDataBuf->getLen();
    if (offset > len)
        throw DOMException(DOMException::INDEX_SIZE_ERR, 0, GetDOMCharacterDataImplMemoryManager);



    // Cap the value of delLength to avoid trouble with overflows
    //  in the following length computations.
    if (count > len)
        count = len;

    // If the length of data to be deleted would extend off the end
    //   of the string, cut it back to stop at the end of string.
    if (offset + count >= len)
        count = len - offset;

    XMLSize_t newLen = len - count;

    XMLCh* newString;
    XMLCh temp[4000];
    if (newLen >= 3999)
        newString = (XMLCh*) XMLPlatformUtils::fgMemoryManager->allocate
        (
            (newLen+1) * sizeof(XMLCh)
        );//new XMLCh[newLen+1];
    else
        newString = temp;

    XMLString::copyNString(newString, fDataBuf->getRawBuffer(), offset);
    XMLString::copyString(newString+offset, fDataBuf->getRawBuffer()+offset+count);

    fDataBuf->set(newString);

    if (newLen >= 3999)
        XMLPlatformUtils::fgMemoryManager->deallocate(newString);//delete[] newString;

    // We don't delete the old string (doesn't work), or alter
    //   the old string (may be shared)
    //   It just hangs around, possibly orphaned.

    if (node->getOwnerDocument() != 0) {
        Ranges* ranges = ((DOMDocumentImpl *)node->getOwnerDocument())->getRanges();
        if (ranges != 0) {
            XMLSize_t sz = ranges->size();
            if (sz != 0) {
                for (XMLSize_t i =0; i<sz; i++) {
                    ranges->elementAt(i)->updateRangeForDeletedText( (DOMNode*)node, offset, count);
                }
            }
        }
    }
}



const XMLCh *DOMCharacterDataImpl::getData() const
{
    return fDataBuf->getRawBuffer();
}


//
//  getCharDataLength - return the length of the character data string.
//
XMLSize_t DOMCharacterDataImpl::getLength() const
{
    return fDataBuf->getLen();
}



void DOMCharacterDataImpl::insertData(const DOMNode *node, XMLSize_t offset, const XMLCh *dat)
{
    if (castToNodeImpl(node)->isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0, GetDOMCharacterDataImplMemoryManager);

    // Note: the C++ XMLCh * operation throws the correct DOMExceptions
    //       when parameter values are bad.
    //

    XMLSize_t len = fDataBuf->getLen();
    if (offset > len)
        throw DOMException(DOMException::INDEX_SIZE_ERR, 0, GetDOMCharacterDataImplMemoryManager);

    XMLSize_t datLen = XMLString::stringLen(dat);

    XMLSize_t newLen = len + datLen;

    XMLCh* newString;
    XMLCh temp[4000];
    if (newLen >= 3999)
        newString = (XMLCh*) XMLPlatformUtils::fgMemoryManager->allocate
        (
            (newLen + 1) * sizeof(XMLCh)
        );//new XMLCh[newLen+1];
    else
        newString = temp;

    XMLString::copyNString(newString, fDataBuf->getRawBuffer(), offset);
    XMLString::copyNString(newString+offset, dat, datLen);
    XMLString::copyString(newString+offset+datLen, fDataBuf->getRawBuffer()+offset);

    fDataBuf->set(newString);

    if (newLen >= 3999)
        XMLPlatformUtils::fgMemoryManager->deallocate(newString);//delete[] newString;

    if (node->getOwnerDocument() != 0) {
        Ranges* ranges = ((DOMDocumentImpl *)node->getOwnerDocument())->getRanges();
        if (ranges != 0) {
            XMLSize_t sz = ranges->size();
            if (sz != 0) {
                for (XMLSize_t i =0; i<sz; i++) {
                    ranges->elementAt(i)->updateRangeForInsertedText( (DOMNode*)node, offset, datLen);
                }
            }
        }
    }
}



void DOMCharacterDataImpl::replaceData(const DOMNode *node, XMLSize_t offset, XMLSize_t count,
                                    const XMLCh *dat)
{
    if (castToNodeImpl(node)->isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0, GetDOMCharacterDataImplMemoryManager);

    deleteData(node, offset, count);
    insertData(node, offset, dat);
}




void DOMCharacterDataImpl::setData(const DOMNode *node, const XMLCh *arg)
{
    setNodeValue(node, arg);
}





const XMLCh * DOMCharacterDataImpl::substringData(const DOMNode *node, XMLSize_t offset,
                                           XMLSize_t count) const
{

    // Note: the C++ XMLCh * operation throws the correct DOMExceptions
    //       when parameter values are bad.
    //


    XMLSize_t len = fDataBuf->getLen();

    if (offset > len)
        throw DOMException(DOMException::INDEX_SIZE_ERR, 0, GetDOMCharacterDataImplMemoryManager);


    XMLCh* newString;
    XMLCh temp[4000];
    if (len >= 3999)
        newString = (XMLCh*) ((DOMDocumentImpl *)node->getOwnerDocument())->getMemoryManager()->allocate
        (
            (len + 1) * sizeof(XMLCh)
        );//new XMLCh[len+1];
    else
        newString = temp;

    XMLString::copyNString(newString, fDataBuf->getRawBuffer()+offset, count);
    newString[count] = chNull;

    const XMLCh* retString = ((DOMDocumentImpl *)node->getOwnerDocument())->getPooledString(newString);

    if (len >= 3999)
        ((DOMDocumentImpl *)node->getOwnerDocument())->getMemoryManager()->deallocate(newString);//delete[] newString;

    return retString;

}


void DOMCharacterDataImpl::releaseBuffer() {
    fDoc->releaseBuffer(fDataBuf);
}

XERCES_CPP_NAMESPACE_END

