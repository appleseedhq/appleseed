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
 * $Id: IXMLDOMNodeImpl.inl 568078 2007-08-21 11:43:25Z amassari $
 */


#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMText.hpp>
#include "XMLDOMNodeList.h"
#include "XMLDOMNamedNodeMap.h"
#include "XMLDOMUtil.h"
#include <xercesc/dom/DOMException.hpp>

XERCES_CPP_NAMESPACE_USE

template <class T, const IID* piid, class tihclass>
HRESULT STDMETHODCALLTYPE IXMLDOMNodeImpl<T,piid,tihclass>::InterfaceSupportsErrorInfo(REFIID riid)
{
	if(riid == *piid) return S_OK;
	return S_FALSE;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_nodeName(BSTR *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_nodeName\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	try
	{
		*pVal = SysAllocString(get_DOMNode()->getNodeName());
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_nodeValue(VARIANT *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_nodeValue\n"));

	if (NULL == pVal)
		return E_POINTER;

	::VariantInit(pVal);


	try
	{
        const XMLCh* val=get_DOMNode()->getNodeValue();
		if (val != 0)
		{
			V_VT(pVal)   = VT_BSTR;
			V_BSTR(pVal) = SysAllocString(val);
		}
		else
			V_VT(pVal) = VT_NULL;
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::put_nodeValue(VARIANT newVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::put_nodeValue\n"));

	try
	{
		if(V_VT(&newVal) == VT_BSTR)
		{
			get_DOMNode()->setNodeValue(V_BSTR(&newVal));
		}
		//
		//   coerce to BSTR or throw error
		//
		else
		{
			get_DOMNode()->setNodeValue((BSTR) (_bstr_t) newVal);
		}
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_nodeType(DOMNodeType *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_nodeType\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = get_DOMNodeType();

	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_parentNode(IXMLDOMNode **pVal)
{	
	ATLTRACE(_T("IXMLDOMNodeImpl::get_parentNode\n"));
	
	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	HRESULT hr = S_OK;

	try
	{
		DOMNode* n = get_DOMNode()->getParentNode();
		if(n!=NULL) {
			hr = wrapNode(m_pIXMLDOMDocument, n,IID_IXMLDOMNode,reinterpret_cast<LPVOID *> (pVal));
		}
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return hr;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_childNodes(IXMLDOMNodeList * *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_childNodes\n"));
	
	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	CXMLDOMNodeListObj *pObj = NULL;
	HRESULT hr = CXMLDOMNodeListObj::CreateInstance(&pObj);
	if (S_OK != hr)
		return hr;
	
	pObj->AddRef();
	pObj->SetOwnerDoc(m_pIXMLDOMDocument);

	try
	{
		pObj->m_container = get_DOMNode()->getChildNodes();
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
	
	hr = pObj->QueryInterface(IID_IXMLDOMNodeList, reinterpret_cast<LPVOID*> (pVal));
	if (S_OK != hr)
		*pVal = NULL;

	pObj->Release();
	return hr;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_firstChild(IXMLDOMNode **pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_firstChild\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	HRESULT hr = S_OK;

	try
	{
		DOMNode* n = get_DOMNode()->getFirstChild();
		//
		//   returns Nothing if no children
		//
		if(n!=NULL)
			hr = wrapNode(m_pIXMLDOMDocument,n,IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (pVal));
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return hr;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_lastChild(IXMLDOMNode **pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_lastChild\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	HRESULT hr = S_OK;

	try
	{
		DOMNode* n = get_DOMNode()->getLastChild();
		if(n!=NULL)
			hr = wrapNode(m_pIXMLDOMDocument,n,IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (pVal));
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return hr;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_previousSibling(IXMLDOMNode * *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_previousSibling\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	HRESULT hr = S_OK;

	try
	{
		DOMNode* n = get_DOMNode()->getPreviousSibling();
		if(n!=NULL)
			hr = wrapNode(m_pIXMLDOMDocument,n,IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (pVal));
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return hr;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_nextSibling(IXMLDOMNode * *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_nextSibling\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	HRESULT hr = S_OK;

	try
	{
		DOMNode* n = get_DOMNode()->getNextSibling();
		if(n!=NULL)
			hr = wrapNode(m_pIXMLDOMDocument,n,IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (pVal));
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return hr;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_attributes(IXMLDOMNamedNodeMap * *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_attributes\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	DOMNamedNodeMap* map=NULL;
	try
	{
		map = get_DOMNode()->getAttributes();
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	if ((map == 0) || (NODE_ELEMENT  != get_DOMNodeType()))
		//&&
		//NODE_ENTITY   != get_DOMNodeType() &&
		//NODE_NOTATION != get_DOMNodeType())
		return S_OK;
	
	CXMLDOMNamedNodeMapObj *pObj = NULL;
	HRESULT hr = CXMLDOMNamedNodeMapObj::CreateInstance(&pObj);
	if (S_OK != hr)
		return hr;
	
	pObj->AddRef();
	pObj->SetOwnerDoc(m_pIXMLDOMDocument);
	pObj->m_container = map;
	
	hr = pObj->QueryInterface(IID_IXMLDOMNamedNodeMap, reinterpret_cast<LPVOID*> (pVal));
	if (S_OK != hr)
		*pVal = NULL;

	pObj->Release();
	return hr;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::insertBefore(IXMLDOMNode *newChild, VARIANT refChild, IXMLDOMNode **outNewChild)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::insertBefore\n"));

	if (NULL == outNewChild)
		return E_POINTER;

	*outNewChild = NULL;

	if (NULL == newChild)
		return E_INVALIDARG;

	if (V_VT(&refChild) != VT_DISPATCH && V_VT(&refChild) != VT_NULL)
		return E_INVALIDARG;

	if (V_VT(&refChild) == VT_NULL)
		return appendChild(newChild,outNewChild);
	
	CComQIPtr<IIBMXMLDOMNodeIdentity,&IID_IIBMXMLDOMNodeIdentity> pNewChild(newChild);
	if (!pNewChild)
		return E_NOINTERFACE;

	long id = 0;
	HRESULT hr = pNewChild->get_NodeId(&id);
	if (S_OK != hr)
		return hr;

	DOMNode *pNewChildNode = reinterpret_cast<DOMNode*> (id);
	if (NULL == pNewChildNode)
		return E_INVALIDARG;

	CComQIPtr<IIBMXMLDOMNodeIdentity,&IID_IIBMXMLDOMNodeIdentity> pRefChild(V_DISPATCH(&refChild));
	if (!pRefChild)
		return E_NOINTERFACE;

	id = 0;
	hr = pRefChild->get_NodeId(&id);
	if (S_OK != hr)
		return hr;

	DOMNode *pRefChildNode = reinterpret_cast<DOMNode*> (id);
	if (NULL == pRefChildNode)
		return E_INVALIDARG;

	try
	{
		DOMNode* n = get_DOMNode()->insertBefore(pNewChildNode, pRefChildNode);
		hr = wrapNode(m_pIXMLDOMDocument,n,IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (outNewChild));
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return hr;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::replaceChild(IXMLDOMNode *newChild, IXMLDOMNode *oldChild, IXMLDOMNode * *outNewChild)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::replaceChild\n"));

	if (NULL == outNewChild)
		return E_POINTER;

	*outNewChild = NULL;

	if (NULL == oldChild)
		return E_INVALIDARG;

	if (NULL == newChild)
		return removeChild(oldChild,outNewChild);

		
	CComQIPtr<IIBMXMLDOMNodeIdentity,&IID_IIBMXMLDOMNodeIdentity> pNewChild(newChild);
	if (!pNewChild)
		return E_NOINTERFACE;

	long id = 0;
	HRESULT hr = pNewChild->get_NodeId(&id);
	if (S_OK != hr)
		return hr;

	DOMNode *pNewChildNode = reinterpret_cast<DOMNode*> (id);
	if (NULL == pNewChildNode)
		return E_INVALIDARG;

	CComQIPtr<IIBMXMLDOMNodeIdentity,&IID_IIBMXMLDOMNodeIdentity> pOldChild(oldChild);
	if (!pOldChild)
		return E_NOINTERFACE;

	id = 0;
	hr = pOldChild->get_NodeId(&id);
	if (S_OK != hr)
		return hr;

	DOMNode *pOldChildNode = reinterpret_cast<DOMNode*> (id);
	if (NULL == pOldChildNode)
		return E_INVALIDARG;

	try
	{
		DOMNode* n = get_DOMNode()->replaceChild(pNewChildNode, pOldChildNode);
		hr = wrapNode(m_pIXMLDOMDocument,n,IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (outNewChild));
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::removeChild(IXMLDOMNode *child, IXMLDOMNode * *oldChild)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::removeChild\n"));

	if (NULL == oldChild)
		return E_POINTER;

	*oldChild = NULL;

	if (NULL == child)
		return E_INVALIDARG;
		
	CComQIPtr<IIBMXMLDOMNodeIdentity,&IID_IIBMXMLDOMNodeIdentity> pChild(child);
	if (!pChild)
		return E_NOINTERFACE;

	long id = 0;
	HRESULT hr = pChild->get_NodeId(&id);
	if (S_OK != hr)
		return hr;

	DOMNode *pChildNode = reinterpret_cast<DOMNode*> (id);
	if (NULL == pChildNode)
		return E_INVALIDARG;

	try
	{
		DOMNode* n = get_DOMNode()->removeChild(pChildNode);
		if(n!=NULL)
			hr = wrapNode(m_pIXMLDOMDocument,n,IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (oldChild));
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::appendChild(IXMLDOMNode *newChild, IXMLDOMNode * *outNewChild)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::appendChild\n"));
	
	if (NULL == outNewChild)
		return E_POINTER;

	*outNewChild = NULL;

	if (NULL == newChild)
		return E_INVALIDARG;
		
	CComQIPtr<IIBMXMLDOMNodeIdentity,&IID_IIBMXMLDOMNodeIdentity> pNewChild(newChild);
	if (!pNewChild)
		return E_NOINTERFACE;

	long id = 0;
	HRESULT hr = pNewChild->get_NodeId(&id);
	if (S_OK != hr)
		return hr;

	DOMNode *pNewChildNode = reinterpret_cast<DOMNode*> (id);
	if (NULL == pNewChildNode)
		return E_INVALIDARG;

	try
	{
		DOMNode* n = get_DOMNode()->appendChild(pNewChildNode);
		hr = wrapNode(m_pIXMLDOMDocument,n,IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (outNewChild));
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::hasChildNodes(VARIANT_BOOL *hasChild)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::hasChildNodes\n"));

	if (NULL == hasChild)
		return E_POINTER;

	*hasChild = VARIANT_FALSE;

	try
	{
		*hasChild = (get_DOMNode()->hasChildNodes()) ? VARIANT_TRUE : VARIANT_FALSE;
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_ownerDocument(IXMLDOMDocument **pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_ownerDocument\n"));

	if (NULL == pVal)
		return E_POINTER;
	*pVal = NULL;

	if (get_DOMNodeType() != NODE_DOCUMENT)
	{
		*pVal = m_pIXMLDOMDocument;
		if (*pVal != NULL)
			(*pVal)->AddRef();
	}
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::cloneNode(VARIANT_BOOL deep, IXMLDOMNode **pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::cloneNode\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	HRESULT hr = S_OK;

	try
	{
		hr = wrapNode(m_pIXMLDOMDocument,get_DOMNode()->cloneNode((VARIANT_TRUE == deep) ? true : false),IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (pVal));
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return hr;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_nodeTypeString(BSTR *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_nodeTypeString\n"));

	USES_CONVERSION;

	if (NULL == pVal)
		return E_POINTER;

	*pVal = ::SysAllocString(g_DomNodeName[get_DOMNodeType()]);
	
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_text(BSTR *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_text\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = SysAllocString(get_DOMNode()->getTextContent());

	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::put_text(BSTR newVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::put_text\n"));

	DOMNodeType type = get_DOMNodeType();
	if (NODE_DOCUMENT_FRAGMENT == type ||
		NODE_DOCUMENT_TYPE	   == type ||
		NODE_ENTITY			   == type ||
		NODE_ENTITY_REFERENCE  == type ||
		NODE_NOTATION		   == type)
		return E_ACCESSDENIED;

	try
	{
		if(NODE_ELEMENT == type)
		{
			//
			//   remove all child elements
			//
			DOMNode* elem = get_DOMNode();
			DOMNode* child = elem->getLastChild();
			while(child!=NULL)
			{
				elem->removeChild(child);
				child = elem->getLastChild();
			}
			
			XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* doc = elem->getOwnerDocument();
			elem->appendChild(doc->createTextNode(newVal));
		}
		else
			get_DOMNode()->setNodeValue(newVal);
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}

	return S_OK;
}


template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_specified(VARIANT_BOOL *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_specified\n"));

	*pVal = VARIANT_TRUE;
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_definition(IXMLDOMNode * *pVal)
{	
	ATLTRACE(_T("IXMLDOMNodeImpl::get_definition\n"));

	return E_NOTIMPL;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_nodeTypedValue(VARIANT *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_nodeTypedValue\n"));

	return get_nodeValue(pVal);
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::put_nodeTypedValue(VARIANT newVal)
{	
	ATLTRACE(_T("IXMLDOMNodeImpl::put_nodeTypedValue\n"));

	return put_nodeValue(newVal);
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_dataType(VARIANT *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_dataType\n"));

	return E_NOTIMPL;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::put_dataType(BSTR dataTypeName)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::put_dataType\n"));

	return E_NOTIMPL;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_xml(BSTR *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_xml\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	_bstr_t text;
	
	try {
		GetXML(get_DOMNode(),text);
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}

	*pVal = text.copy();
	
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::transformNode(IXMLDOMNode *stylesheet, BSTR *xmlString)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::transformNode\n"));

	return E_NOTIMPL;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::selectNodes(BSTR queryString, IXMLDOMNodeList * *resultList)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::selectNodes\n"));

	return E_NOTIMPL;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::selectSingleNode(BSTR queryString, IXMLDOMNode * *resultNode)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::selectSingleNode\n"));

	return E_NOTIMPL;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_parsed(VARIANT_BOOL *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_parsed\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = VARIANT_TRUE;
	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_namespaceURI(BSTR *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_namespaceURI\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	try {
		*pVal = SysAllocString(get_DOMNode()->getNamespaceURI());
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}

	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_prefix(BSTR *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_prefix\n"));
	
	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	try {
		*pVal = SysAllocString(get_DOMNode()->getPrefix());
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}

	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::get_baseName(BSTR *pVal)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::get_baseName\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	try {
		*pVal = SysAllocString(get_DOMNode()->getLocalName());
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}

	return S_OK;
}

template <class T, const IID* piid, class tihclass>
STDMETHODIMP
IXMLDOMNodeImpl<T,piid,tihclass>::transformNodeToObject(IXMLDOMNode *stylesheet, VARIANT outputObject)
{
	ATLTRACE(_T("IXMLDOMNodeImpl::transformNodeToObject\n"));

	return E_NOTIMPL;
}

