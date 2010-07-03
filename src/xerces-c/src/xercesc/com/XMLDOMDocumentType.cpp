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
 * $Id: XMLDOMDocumentType.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "stdafx.h"
#include "xml4com.h"
#include "XMLDOMDocumentType.h"
#include "XMLDOMNamedNodeMap.h"

// IXMLDOMDocumentType methods
STDMETHODIMP CXMLDOMDocumentType::get_name(BSTR  *pVal)
{
	ATLTRACE(_T("CXMLDOMDocumentType::get_name\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	try
	{
		*pVal = SysAllocString(documentType->getName());
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

STDMETHODIMP CXMLDOMDocumentType::get_entities(IXMLDOMNamedNodeMap  **pVal)
{
	ATLTRACE(_T("CXMLDOMDocumentType::get_entities\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	CXMLDOMNamedNodeMapObj *pObj = NULL;
	HRESULT hr = CXMLDOMNamedNodeMapObj::CreateInstance(&pObj);
	if (S_OK != hr)
		return hr;
	
	pObj->AddRef();
	pObj->SetOwnerDoc(m_pIXMLDOMDocument);

	try
	{
		pObj->m_container = documentType->getEntities();
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
	
	hr = pObj->QueryInterface(IID_IXMLDOMNamedNodeMap, reinterpret_cast<LPVOID*> (pVal));
	if (S_OK != hr)
		*pVal = NULL;

	pObj->Release();
	return hr;
}

STDMETHODIMP CXMLDOMDocumentType::get_notations(IXMLDOMNamedNodeMap  **pVal)
{
	ATLTRACE(_T("CXMLDOMDocumentType::get_notations\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	CXMLDOMNamedNodeMapObj *pObj = NULL;
	HRESULT hr = CXMLDOMNamedNodeMapObj::CreateInstance(&pObj);
	if (S_OK != hr)
		return hr;
	
	pObj->AddRef();
	pObj->SetOwnerDoc(m_pIXMLDOMDocument);

	try
	{
		pObj->m_container = documentType->getNotations();
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
	
	hr = pObj->QueryInterface(IID_IXMLDOMNamedNodeMap, reinterpret_cast<LPVOID*> (pVal));
	if (S_OK != hr)
		*pVal = NULL;

	pObj->Release();
	return hr;

}