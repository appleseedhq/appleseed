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
 * $Id: XMLDOMUtil.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "stdafx.h"
#include "xml4com.h"
#include "XMLDOMElement.h"
#include "XMLDOMAttribute.h"
#include "XMLDOMText.h"
#include "XMLDOMCDATASection.h"
#include "XMLDOMEntityReference.h"
#include "XMLDOMEntity.h"
#include "XMLDOMProcessingInstruction.h"
#include "XMLDOMComment.h"
#include "XMLDOMDocument.h"
#include "XMLDOMDocumentType.h"
#include "XMLDOMDocumentFragment.h"
#include "XMLDOMNotation.h"
#include "XMLDOMUtil.h"
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLString.hpp>


const OLECHAR* g_DomNodeName[] =
{	
	OLESTR("invalid"),
	OLESTR("element"),
	OLESTR("attribute"),
	OLESTR("text"),
	OLESTR("cdatasection"),
	OLESTR("entityreference"),
	OLESTR("entity"),
	OLESTR("processinginstruction"),
	OLESTR("comment"),
	OLESTR("document"),
	OLESTR("documenttype"),
	OLESTR("documentfragment"),
	OLESTR("notation")
};

const int g_DomNodeNameSize = sizeof(g_DomNodeName) / sizeof(OLECHAR*);

template <class Base>
class CComObjectPool
{
public:
	CComObjectPool(unsigned long poolSize);

	virtual ~CComObjectPool();

	HRESULT WINAPI CreateInstance(Base** pp);

	HRESULT Deactivate(Base* obj);

private:
	Base** m_pool;
	unsigned long m_size;
	unsigned long m_hit;
	unsigned long m_attempt;
	HRESULT Activate(Base* obj);
};




template <class Base>
class CPooledComObject : public Base
{
public:
	typedef Base _BaseClass;
	CPooledComObject(void* = NULL)
	{
		_Module.Lock();
	}
	// Set refcount to 1 to protect destruction
	~CPooledComObject()
	{
		m_dwRef = 1L;
		FinalRelease();
#ifdef _ATL_DEBUG_INTERFACES
		_Module.DeleteNonAddRefThunk(_GetRawUnknown());
#endif
		_Module.Unlock();
	}
	//If InternalAddRef or InternalRelease is undefined then your class
	//doesn't derive from CComObjectRoot
	STDMETHOD_(ULONG, AddRef)() {return InternalAddRef();}
	STDMETHOD_(ULONG, Release)()
	{
		ULONG l = InternalRelease();
		if (l == 0) {
			if(SUCCEEDED(m_pool.Deactivate(this))) {
				FinalRelease();
			}
			else
				delete this;
		}
		return l;
	}
	//if _InternalQueryInterface is undefined then you forgot BEGIN_COM_MAP
	STDMETHOD(QueryInterface)(REFIID iid, void ** ppvObject)
	{return _InternalQueryInterface(iid, ppvObject);}
	template <class Q>
	HRESULT STDMETHODCALLTYPE QueryInterface(Q** pp)
	{
		return QueryInterface(__uuidof(Q), (void**)pp);
	}

	static HRESULT WINAPI CreateInstance(Base** pp) {
		return m_pool.CreateInstance(pp);
	}

private:
	static CComObjectPool<Base> m_pool;
};




template <class Base>
CComObjectPool<Base>::CComObjectPool(unsigned long poolSize) {
	m_pool = NULL;
	m_size = poolSize;
	m_pool = new Base*[m_size];
	for(unsigned long i = 0; i < m_size; i++)
		m_pool[i] = NULL;
	m_hit= 0;
	m_attempt = 0;
}

template <class Base>
CComObjectPool<Base>::~CComObjectPool() {
	for(unsigned long i = 0; i < m_size; i++) {
		if(m_pool[i]) delete m_pool[i];
	}
	delete [] m_pool;
}

template <class Base>
HRESULT CComObjectPool<Base>::Deactivate(Base* obj) {
	for(unsigned long i = 0; i < m_size; i++) {
		if(m_pool[i] == NULL) {
			m_pool[i] = obj;
			return S_OK;
		}
	}
	return E_FAIL;
}

template <class Base>
HRESULT CComObjectPool<Base>::Activate(Base* p)
{
	p->SetVoid(NULL);
	p->InternalFinalConstructAddRef();
	HRESULT hRes = p->FinalConstruct();
	p->InternalFinalConstructRelease();
	return hRes;
}


template <class Base>
HRESULT WINAPI CComObjectPool<Base>::CreateInstance(Base** pp) {
	ATLASSERT(pp != NULL);
	HRESULT hRes = E_OUTOFMEMORY;
	Base* p = NULL;

	m_attempt++;

	for(unsigned long i = 0; i < m_size; i++) {
		if(m_pool[i]) {
			p = m_pool[i];
			m_pool[i] = NULL;
			hRes = Activate(p);
			if (SUCCEEDED(hRes)) {
				m_hit++;
				break;
			}
			else {
				delete p;
				p = NULL;
			}
		}
	}

	if(FAILED(hRes)) {
		ATLTRY(p = new CPooledComObject<Base>())
		if (p != NULL) {
			hRes = Activate(p);
			if (hRes != S_OK) {
				delete p;
				p = NULL;
			}
		}
	}
	*pp = p;
	return hRes;
}


CComObjectPool<CXMLDOMElement> CPooledComObject<CXMLDOMElement>::m_pool(6);
typedef CPooledComObject<CXMLDOMElement> CPooledXMLDOMElementObj;

CComObjectPool<CXMLDOMAttribute> CPooledComObject<CXMLDOMAttribute>::m_pool(6);
typedef CPooledComObject<CXMLDOMAttribute> CPooledXMLDOMAttributeObj;

CComObjectPool<CXMLDOMText> CPooledComObject<CXMLDOMText>::m_pool(6);
typedef CPooledComObject<CXMLDOMText> CPooledXMLDOMTextObj;

CComObjectPool<CXMLDOMCDATASection> CPooledComObject<CXMLDOMCDATASection>::m_pool(6);
typedef CPooledComObject<CXMLDOMCDATASection> CPooledXMLDOMCDATASectionObj;

CComObjectPool<CXMLDOMEntityReference> CPooledComObject<CXMLDOMEntityReference>::m_pool(6);
typedef CPooledComObject<CXMLDOMEntityReference> CPooledXMLDOMEntityReferenceObj;

CComObjectPool<CXMLDOMEntity> CPooledComObject<CXMLDOMEntity>::m_pool(6);
typedef CPooledComObject<CXMLDOMEntity> CPooledXMLDOMEntityObj;

CComObjectPool<CXMLDOMProcessingInstruction> CPooledComObject<CXMLDOMProcessingInstruction>::m_pool(6);
typedef CPooledComObject<CXMLDOMProcessingInstruction> CPooledXMLDOMProcessingInstructionObj;

CComObjectPool<CXMLDOMComment> CPooledComObject<CXMLDOMComment>::m_pool(6);
typedef CPooledComObject<CXMLDOMComment> CPooledXMLDOMCommentObj;


HRESULT wrapNode(IXMLDOMDocument *pDoc, DOMNode* node, REFIID iid, LPVOID *pVal)
{
	HRESULT hr = S_OK;
	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	short type = node->getNodeType();

	// the way we are constructing the wrappers is kind of fishy but oh well...
	// the various IBM DOM wrapper classes don't ever add any members or have
	// any v-tables so what we are doing should be safe.  There isn't any other
	// way as far as I can tell to do this....

	switch(type)
	{
	case DOMNode::ELEMENT_NODE:
	{
		CXMLDOMElement *pObj = NULL;
		hr = CPooledXMLDOMElementObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->element = static_cast<DOMElement*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::ATTRIBUTE_NODE:
	{
		CXMLDOMAttribute *pObj = NULL;
		hr = CPooledXMLDOMAttributeObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->attr = static_cast<DOMAttr*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::TEXT_NODE:
	{
		CXMLDOMText *pObj = NULL;
		hr = CPooledXMLDOMTextObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->text = static_cast<DOMText*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::CDATA_SECTION_NODE:
	{
		CXMLDOMCDATASection *pObj = NULL;
		hr = CPooledXMLDOMCDATASectionObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->cdataSection = static_cast<DOMCDATASection*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::ENTITY_REFERENCE_NODE:
	{
		CXMLDOMEntityReference *pObj = NULL;
		hr = CPooledXMLDOMEntityReferenceObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->entityReference = static_cast<DOMEntityReference*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::ENTITY_NODE:
	{
		CXMLDOMEntity *pObj = NULL;
		hr = CPooledXMLDOMEntityObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);
		
		try
		{
			pObj->entity = static_cast<DOMEntity*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::PROCESSING_INSTRUCTION_NODE:
	{
		CXMLDOMProcessingInstruction *pObj = NULL;
		hr = CPooledXMLDOMProcessingInstructionObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->processingInstruction = static_cast<DOMProcessingInstruction*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::COMMENT_NODE:
	{
		CXMLDOMComment *pObj = NULL;
		hr = CPooledXMLDOMCommentObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->comment = static_cast<DOMComment*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::DOCUMENT_NODE:
	{
		CXMLDOMDocumentObj *pObj = NULL;
		hr = CXMLDOMDocumentObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->m_Document = static_cast<XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::DOCUMENT_TYPE_NODE:
	{
		CXMLDOMDocumentTypeObj *pObj = NULL;
		hr = CXMLDOMDocumentTypeObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->documentType = static_cast<DOMDocumentType*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::DOCUMENT_FRAGMENT_NODE:
	{
		CXMLDOMDocumentFragmentObj *pObj = NULL;
		hr = CXMLDOMDocumentFragmentObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->documentFragment = static_cast<DOMDocumentFragment*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}
	case DOMNode::NOTATION_NODE:
	{
		CXMLDOMNotationObj *pObj = NULL;
		hr = CXMLDOMNotationObj::CreateInstance(&pObj);
		if (S_OK != hr)
			return hr;
	
		pObj->AddRef();
		pObj->SetOwnerDoc(pDoc);

		try
		{
			pObj->notation = static_cast<DOMNotation*> (node);
		}
		catch(DOMException& ex)
		{
			pObj->Release();
			return MakeHRESULT(ex);
		}
		catch(...)
		{
			pObj->Release();
			return E_FAIL;
		}
	
		hr = pObj->QueryInterface(iid, pVal);
		if (S_OK != hr)
			*pVal = NULL;

		pObj->Release();
		break;
	}

	default:
		hr = E_NOTIMPL;
		break;
	}

	return hr;
}


class xmlstream {
public:
	xmlstream() {
		m_length = 0;
		m_alloc = 0;
		m_buffer = 0;
		m_next = 0;
	}

	~xmlstream() {
		delete [] m_buffer;
	}

	xmlstream& operator<<(const XMLCh* other) {
		//
		//   get length of string
		//
		unsigned long len = 0;
		for(const XMLCh* source = other; *source; source++,len++);

		//
		//    append to stream
		//
		append(other,len);
		return *this;
	}

	xmlstream& operator<<(const XMLCh other) {
		append(&other,1);
		return *this;
	}

	BSTR SysAllocString() {
		if(m_length > 0)
			return SysAllocStringLen(m_buffer,m_length);
		return 0;
	}

private:
	void append(const XMLCh* other,unsigned long length) {
		const XMLCh* source = NULL;

		if(m_length + length > m_alloc) {
			unsigned long chunk = 4096;
			if(length > chunk) chunk += length;
			XMLCh* newbuf = new XMLCh[m_alloc+ chunk];
			m_alloc += chunk;
			m_next = newbuf + m_length;

			//
			//    copy old content into new buffer
			//
			XMLCh* dest = newbuf;
			source = m_buffer;
			for(unsigned long i = 0; i < m_length; i++,dest++,source++) {
				*dest = *source;
			}
			delete [] m_buffer;
			m_buffer = newbuf;
		}

		source = other;
		for(unsigned long i = 0; i < length; i++,source++,m_next++) {
			*m_next = *source;
		}
		m_length += length;
	}

	unsigned long m_length;
	unsigned long m_alloc;
	XMLCh* m_buffer;
	XMLCh* m_next;
};




// ---------------------------------------------------------------------------
//  outputContent
//
//  Write document content from a string to a C++ ostream. Escape the
//  XML special characters (<, &, etc.) unless this is suppressed by the
//  command line option.
// ---------------------------------------------------------------------------
void outputContent(xmlstream& target, const XMLCh* toWrite)
{

    int            length = XMLString::stringLen(toWrite);

    int index;
    for (index = 0; index < length; index++)
    {
        switch (toWrite[index])
        {
        case chAmpersand :
            target << XMLStrL("&amp;");
            break;

        case chOpenAngle :
            target << XMLStrL("&lt;");
            break;

        case chCloseAngle:
            target << XMLStrL("&gt;");
            break;

        case chDoubleQuote :
            target << XMLStrL("&quot;");
            break;

        default:
            // If it is none of the special characters, print it as such
            target << toWrite[index];
            break;
        }
    }

    return;
}

xmlstream& operator<<(xmlstream& target, const DOMNode* toWrite)
{
    // Get the name and value out for convenience
    const XMLCh* nodeName = toWrite->getNodeName();
    const XMLCh* nodeValue = toWrite->getNodeValue();


	switch (toWrite->getNodeType())
    {
		case DOMNode::TEXT_NODE:
        {
            outputContent(target, nodeValue);
            break;
        }

        case DOMNode::PROCESSING_INSTRUCTION_NODE :
        {
            target  << XMLStrL("<?")
                    << nodeName
                    << XMLStrL(' ')
                    << nodeValue
                    << XMLStrL("?>");
            break;
        }

        case DOMNode::DOCUMENT_NODE :
        {
            //
            //  Bug here:  we need to find a way to get the encoding name
            //  for the default code page on the system where the program
            //  is running, and plug that in for the encoding name.
            //
            XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* document=(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument*)toWrite;
            target << XMLStrL("<?xml version=\"") << document->getVersion() << XMLStrL("\"");
            const XMLCh* str = document->getEncoding();
            if (str != 0)
                target << XMLStrL(" encoding=\"") << str << XMLStrL("\"");
            if(document->getStandalone())
                target << XMLStrL(" standalone=\"yes\"");
            target << XMLStrL("?>");

            DOMNode* child = toWrite->getFirstChild();
            while( child != 0)
            {
                target << child;
                child = child->getNextSibling();
            }
            break;
        }

        case DOMNode::ELEMENT_NODE :
        {
            // Output the element start tag.
            target << XMLStrL('<') << nodeName;

            // Output any attributes on this element
            DOMNamedNodeMap* attributes = toWrite->getAttributes();
            int attrCount = attributes->getLength();
            for (int i = 0; i < attrCount; i++)
            {
                DOMNode*  attribute = attributes->item(i);

                target  << XMLStrL(' ') << attribute->getNodeName()
                        << XMLStrL(" = \"");
                        //  Note that "<" must be escaped in attribute values.
                        outputContent(target, attribute->getNodeValue());
                        target << XMLStrL('"');
            }

            //
            //  Test for the presence of children, which includes both
            //  text content and nested elements.
            //
            DOMNode* child = toWrite->getFirstChild();
            if (child != 0)
            {
                // There are children. Close start-tag, and output children.
                target << XMLStrL(">");
                while( child != 0)
                {
                    target << child;
                    child = child->getNextSibling();
                }

                // Done with children.  Output the end tag.
                target << XMLStrL("</") << nodeName << XMLStrL(">");
            }
            else
            {
                //
                //  There were no children. Output the short form close of
                //  the element start tag, making it an empty-element tag.
                //
                target << XMLStrL("/>");
            }
            break;
        }

        case DOMNode::ENTITY_REFERENCE_NODE:
        {
            DOMNode* child;
            for (child = toWrite->getFirstChild(); child != 0; child = child->getNextSibling())
                target << child;
            break;
        }

        case DOMNode::CDATA_SECTION_NODE:
        {
            target << XMLStrL("<![CDATA[") << nodeValue << XMLStrL("]]>");
            break;
        }

        case DOMNode::COMMENT_NODE:
        {
            target << XMLStrL("<!--") << nodeValue << XMLStrL("-->");
            break;
        }

        case DOMNode::DOCUMENT_TYPE_NODE:
        {
			DOMDocumentType* doctype = (DOMDocumentType*)toWrite;;

			target << XMLStrL("<!DOCTYPE ") << nodeName ;
			const XMLCh* id = doctype->getPublicId();
			if (id != 0)
				target << XMLStrL(" PUBLIC \"") << id << XMLStrL("\"");
			id = doctype->getSystemId();
			if (id != 0)
				target << XMLStrL(" SYSTEM \"") << id << XMLStrL("\"");
			id = doctype->getInternalSubset();
			if (id !=0)
				target << XMLStrL(" [ ") << id  << XMLStrL("]");
			target  << XMLStrL(">");
            break;
        }
		case DOMNode::ENTITY_NODE:
        {
			DOMEntity* entity = (DOMEntity*)toWrite;;

			target << XMLStrL("<!ENTITY ") << nodeName;
			const XMLCh* id = entity->getPublicId();
			if (id != 0)
				target << XMLStrL("PUBLIC \"") << id << XMLStrL("\"");
			id = entity->getSystemId();
			if (id != 0)
				target << XMLStrL("SYSTEM \"") << id << XMLStrL("\"");
			id = entity->getNotationName();
			if (id != 0)
				target << XMLStrL("NDATA \"") << id << XMLStrL("\"");

            break;
        }
        default:
            target << XMLStrL("<!-- Unrecognized node type -->");
    }
	return target;
}

void GetXML(const DOMNode* node, _bstr_t &text)
{
	xmlstream stream;
	stream << node;
	text = _bstr_t(stream.SysAllocString(),false);
}

