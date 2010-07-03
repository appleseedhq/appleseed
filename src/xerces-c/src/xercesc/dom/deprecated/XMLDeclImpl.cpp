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
 * $Id: XMLDeclImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "XMLDeclImpl.hpp"
#include <xercesc/util/XMLUni.hpp>
#include "DOM_Node.hpp"
#include "DStringPool.hpp"
#include "DocumentImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


static DOMString *gNam = 0;  // will be lazily initialized to "#xmldecl"
static XMLRegisterCleanup gNamCleanup;

XMLDeclImpl::XMLDeclImpl(DocumentImpl *ownerDoc)
    : ChildNode(ownerDoc),
    version(DOMString(XMLUni::fgVersion1_0)),
    encoding (DOMString(XMLUni::fgUTF8EncodingString)),
    standalone (DOMString(XMLUni::fgNoString))
{
}


//Introduced in DOM Level 2
XMLDeclImpl::XMLDeclImpl(DocumentImpl *ownerDoc, const DOMString &ver,
                         const DOMString &enc, const DOMString &isStd)
	: ChildNode(ownerDoc),
    version ( ver.clone() ),
    encoding ( enc.clone() ),
    standalone ( isStd.clone() )
{
}


XMLDeclImpl::XMLDeclImpl(const XMLDeclImpl &other, bool /*deep*/)
    : ChildNode(other)
{
    version     = other.version.clone();
    encoding    = other.encoding.clone();
    standalone  = other.standalone.clone();
}


XMLDeclImpl::~XMLDeclImpl()
{
}

NodeImpl * XMLDeclImpl::cloneNode(bool deep)
{
    return new (getOwnerDocument()->getMemoryManager()) XMLDeclImpl(*this, deep);
}

DOMString XMLDeclImpl::getNodeName()
{

    return DStringPool::getStaticString("#xmldecl"
                                       , &gNam
                                       , reinitXMLDeclImpl
                                       , gNamCleanup
                                       );
}

short XMLDeclImpl::getNodeType()
{
    return DOM_Node::XML_DECL_NODE;
}

DOMString XMLDeclImpl::getVersion() const
{
    return version;
}

DOMString XMLDeclImpl::getEncoding() const
{
    return encoding;
}

DOMString XMLDeclImpl::getStandalone() const
{
    return standalone;
}

void XMLDeclImpl::setVersion(const DOMString &data)
{
    version = data.clone();
}

void XMLDeclImpl::setEncoding(const DOMString &data)
{
    encoding = data.clone();
}

void XMLDeclImpl::setStandalone(const DOMString &data)
{
    standalone = data.clone();
}

// -----------------------------------------------------------------------
//  Notification that lazy data has been deleted
// -----------------------------------------------------------------------
void XMLDeclImpl::reinitXMLDeclImpl() {

    delete gNam;
    gNam = 0;

}

XERCES_CPP_NAMESPACE_END

