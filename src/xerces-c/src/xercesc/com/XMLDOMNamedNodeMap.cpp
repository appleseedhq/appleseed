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
 * $Id: XMLDOMNamedNodeMap.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "stdafx.h"
#include "xml4com.h"
#include "XMLDOMNamedNodeMap.h"
#include "XMLDOMUtil.h"
#include "IXMLDOMNodeImpl.h"

typedef CComEnumOnSTL<IEnumVARIANT, &IID_IEnumVARIANT, VARIANT, _Copy<VARIANT>, NodeContainerImpl<DOMNamedNodeMap> >
		CComEnumUnknownOnNamedNodeContainer;

STDMETHODIMP CXMLDOMNamedNodeMap::getNamedItem(BSTR name, IXMLDOMNode  **pVal)
{
	ATLTRACE(_T("CXMLDOMNamedNodeMap::getNamedItem\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	HRESULT hr = S_OK;

	if (m_container == 0)
		return S_OK;

	try
	{
		DOMNode* n = m_container->getNamedItem(name);
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

STDMETHODIMP CXMLDOMNamedNodeMap::setNamedItem(IXMLDOMNode  *newItem, IXMLDOMNode  **pVal)
{
	ATLTRACE(_T("CXMLDOMNamedNodeMap::setNamedItem\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	if (m_container == 0)
		return S_OK;

	if (NULL == newItem)
		return E_INVALIDARG;

	CComQIPtr<IIBMXMLDOMNodeIdentity,&IID_IIBMXMLDOMNodeIdentity> pNewItem(newItem);
	if (!pNewItem)
		return E_NOINTERFACE;

	long id = 0;
	HRESULT hr = pNewItem->get_NodeId(&id);
	if (S_OK != hr)
		return hr;

	DOMNode *pNewItemNode = reinterpret_cast<DOMNode*> (id);
	if (NULL == pNewItemNode)
		return E_INVALIDARG;

	try
	{
		const XMLCh* name = pNewItemNode->getNodeName();
		//
		//  returns old node
		//
		DOMNode* n = m_container->setNamedItem(pNewItemNode);
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

STDMETHODIMP CXMLDOMNamedNodeMap::removeNamedItem(BSTR name, IXMLDOMNode  **pVal)
{
	ATLTRACE(_T("CXMLDOMNamedNodeMap::removeNamedItem\n"));
	
	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	HRESULT hr = S_OK;

	if (m_container == 0)
		return S_OK;

	try
	{
		DOMNode* n = m_container->removeNamedItem(name);
		if(n!=NULL)
			hr = wrapNode(m_pIXMLDOMDocument,n,IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (pVal));
	}
	//
	//   if we had a failure, return success anyway
	//
	catch(...)
	{
	}
	
	return hr;
}

STDMETHODIMP CXMLDOMNamedNodeMap::get_item(long index, IXMLDOMNode  **pVal)
{
	ATLTRACE(_T("CXMLDOMNamedNodeMap::get_item\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	HRESULT hr = S_OK;
	
	try
	{
		if (m_container == 0 || index < 0)
			return E_INVALIDARG;

		long length =  m_container->getLength();
		if (index < length)
			hr = wrapNode(m_pIXMLDOMDocument,m_container->item(index),IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (pVal));
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

STDMETHODIMP CXMLDOMNamedNodeMap::get_length(long  *pVal)
{
	ATLTRACE(_T("CXMLDOMNamedNodeMap::get_length\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = 0;

	if (m_container == 0)
		return S_OK;

	try
	{
		*pVal = m_container->getLength();
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

STDMETHODIMP CXMLDOMNamedNodeMap::getQualifiedItem(BSTR baseName, BSTR namespaceURI, IXMLDOMNode  **pVal)
{
	ATLTRACE(_T("CXMLDOMNamedNodeMap::getQualifiedItem\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	
	if (m_container == 0)
		return S_OK;
	
	HRESULT hr = S_OK;

	try
	{
		DOMNode* n = m_container->getNamedItemNS(namespaceURI,baseName);
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

	return S_OK;
}

STDMETHODIMP CXMLDOMNamedNodeMap::removeQualifiedItem(BSTR baseName, BSTR namespaceURI, IXMLDOMNode  **pVal)
{
	ATLTRACE(_T("CXMLDOMNamedNodeMap::removeQualifiedItem\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	
	if (m_container == 0)
		return S_OK;
	
	HRESULT hr = S_OK;

	try
	{
		DOMNode* n = m_container->removeNamedItemNS(namespaceURI,baseName);
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

	return S_OK;
}

STDMETHODIMP CXMLDOMNamedNodeMap::nextNode(IXMLDOMNode  **pVal)
{
	ATLTRACE(_T("CXMLDOMNamedNodeMap::nextNode\n"));
	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	if (m_container == 0)
		return S_OK;
	
	int length = m_container->getLength();
	if (0 == length)
		return S_OK;

	if (m_NextNodeIndex >= length)
		return S_OK;
	
	HRESULT hr = S_OK;

	try
	{
		DOMNode* n = m_container->item(m_NextNodeIndex);
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

	++m_NextNodeIndex;
	
	return hr;
}

STDMETHODIMP CXMLDOMNamedNodeMap::reset()
{
	ATLTRACE(_T("CXMLDOMNamedNodeMap::reset\n"));

	m_NextNodeIndex = 0;
	
	return S_OK;
}

STDMETHODIMP CXMLDOMNamedNodeMap::get__newEnum(IUnknown  **pVal)
{
	ATLTRACE(_T("CXMLDOMNamedNodeMap::get__newEnum\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	CComObject<CComEnumUnknownOnNamedNodeContainer> *pe = NULL;
	HRESULT hr = CComObject<CComEnumUnknownOnNamedNodeContainer>::CreateInstance(&pe);
	if (S_OK != hr)
		return hr;

	pe->AddRef();

	hr = pe->Init(GetUnknown(),*this);
	if (S_OK == hr)
		hr = pe->QueryInterface(pVal);

	pe->Release();

	return hr;
}

HRESULT CXMLDOMNamedNodeMap::InterfaceSupportsErrorInfo(REFIID riid)
{
	if(riid == IID_IXMLDOMNamedNodeMap) return S_OK;
	return S_FALSE;
}