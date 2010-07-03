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
 * $Id: XMLDOMNodeList.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "stdafx.h"
#include "xml4com.h"
#include "XMLDOMNodeList.h"
#include "XMLDOMUtil.h"
#include "IXMLDOMNodeImpl.h"

typedef CComEnumOnSTL<IEnumVARIANT, &IID_IEnumVARIANT, VARIANT, _Copy<VARIANT>,NodeContainerImpl<DOMNodeList> >
		CComEnumUnknownOnNodeContainer;

STDMETHODIMP CXMLDOMNodeList::get_item(long index, IXMLDOMNode  **pVal)
{
	ATLTRACE(_T("CXMLDOMNodeList::get_item\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	HRESULT hr = S_OK;

	try
	{
		if (m_container == 0 || index < 0)
			return E_INVALIDARG;

		long length = m_container->getLength();
		//
		//    if index is beyond end
		//       return a null object not an exception
		//
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

STDMETHODIMP CXMLDOMNodeList::get_length(long  *pVal)
{
	ATLTRACE(_T("CXMLDOMNodeList::get_length\n"));

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

STDMETHODIMP CXMLDOMNodeList::nextNode(IXMLDOMNode  **pVal)
{
	ATLTRACE(_T("CXMLDOMNodeList::nextNode\n"));

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
		hr = wrapNode(m_pIXMLDOMDocument,m_container->item(m_NextNodeIndex),IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (pVal));
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

STDMETHODIMP CXMLDOMNodeList::reset()
{
	ATLTRACE(_T("CXMLDOMNodeList::reset\n"));
	
	m_NextNodeIndex = 0;
	
	return S_OK;
}

STDMETHODIMP CXMLDOMNodeList::get__newEnum(IUnknown  **pVal)
{
	ATLTRACE(_T("CXMLDOMNodeList::get__newEnum\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	CComObject<CComEnumUnknownOnNodeContainer> *pe = NULL;
	HRESULT hr = CComObject<CComEnumUnknownOnNodeContainer>::CreateInstance(&pe);
	if (S_OK != hr)
		return hr;

	pe->AddRef();

	hr = pe->Init(GetUnknown(),*this);
	if (S_OK == hr)
		hr = pe->QueryInterface(pVal);

	pe->Release();

	return hr;
}


HRESULT CXMLDOMNodeList::InterfaceSupportsErrorInfo(REFIID riid)
{
	if(riid == IID_IXMLDOMNodeList) return S_OK;
	return S_FALSE;
}