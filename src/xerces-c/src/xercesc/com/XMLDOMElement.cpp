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
 * $Id: XMLDOMElement.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "stdafx.h"
#include "xml4com.h"
#include "XMLDOMElement.h"
#include "XMLDOMAttribute.h"
#include "XMLDOMNodeList.h"

// IXMLDOMElement methods
STDMETHODIMP CXMLDOMElement::get_tagName(BSTR  *pVal)
{
	ATLTRACE(_T("CXMLDOMElement::get_tagName\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	try
	{
		*pVal = SysAllocString(element->getTagName());
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

STDMETHODIMP CXMLDOMElement::getAttribute(BSTR name, VARIANT  *pVal)
{
	ATLTRACE(_T("CXMLDOMElement::getAttribute\n"));

	if (NULL == pVal)
		return E_POINTER;

	::VariantInit(pVal);
	V_VT(pVal) = VT_EMPTY;

	try {
		V_VT(pVal)   = VT_BSTR;
		V_BSTR(pVal) = SysAllocString(element->getAttribute(name));
	}
	catch(DOMException& ex)
	{
		return MakeHRESULT(ex);
	}
	catch(...) {
		return E_FAIL;
	}
	
	return S_OK;
}

STDMETHODIMP CXMLDOMElement::setAttribute(BSTR name, VARIANT value)
{
	ATLTRACE(_T("CXMLDOMElement::setAttribute\n"));

	try
	{
		if (V_VT(&value) == VT_BSTR)
		{
			element->setAttribute(name, value.bstrVal);
		}
		else {
			element->setAttribute(name,(BSTR) (_bstr_t) value);
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

STDMETHODIMP CXMLDOMElement::removeAttribute(BSTR name)
{
	ATLTRACE(_T("CXMLDOMElement::removeAttribute\n"));

	try
	{
		element->removeAttribute(name);
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

STDMETHODIMP CXMLDOMElement::getAttributeNode(BSTR name, IXMLDOMAttribute  **attr)
{
	ATLTRACE(_T("CXMLDOMElement::getAttributeNode\n"));

	if (NULL == attr)
		return E_POINTER;

	*attr = NULL;
	DOMAttr* attrNode=element->getAttributeNode(name);
	if(attrNode==NULL)
		return S_OK;
	

	CXMLDOMAttributeObj *pObj = NULL;
	HRESULT hr = CXMLDOMAttributeObj::CreateInstance(&pObj);
	if (S_OK != hr)
		return hr;

	pObj->AddRef();
	pObj->SetOwnerDoc(m_pIXMLDOMDocument);

	try
	{
		pObj->attr = attrNode;
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
	
	hr = pObj->QueryInterface(IID_IXMLDOMAttribute, reinterpret_cast<LPVOID*> (attr));
	if (S_OK != hr)
		*attr = NULL;

	pObj->Release();
	return hr;
}

STDMETHODIMP CXMLDOMElement::setAttributeNode(IXMLDOMAttribute  *attr, IXMLDOMAttribute  **attributeNode)
{
	ATLTRACE(_T("CXMLDOMElement::setAttributeNode\n"));

	if (NULL == attr || NULL == attributeNode)
		return E_POINTER;

	*attributeNode = NULL;

	DOMAttr* newAttr = NULL;
	IIBMXMLDOMNodeIdentity* nodeID = NULL;
	HRESULT sc = attr->QueryInterface(IID_IIBMXMLDOMNodeIdentity,(void**) &nodeID);
	if(SUCCEEDED(sc)) {
		long id = 0;
		sc = nodeID->get_NodeId(&id);
		nodeID->Release();
		if(SUCCEEDED(sc)) {
			//
			//   any subsequent failure will be reported as an invalid arg
			//
			sc = E_INVALIDARG;
			try {
				DOMNode* newNode = (DOMNode*) id;
				if(newNode->getNodeType() == DOMNode::ATTRIBUTE_NODE) {
					newAttr = (DOMAttr*) newNode;
				}
			}
			catch(...) {
			}
		}
	}

	//
	//   if we couldn't extract an attribute out of the
	//       argument, then return with a failure code
	if(newAttr == NULL) return sc;

	sc = S_OK;
	try
	{
		DOMAttr* oldAttr = element->setAttributeNode(newAttr);
		if(oldAttr!=NULL) {
			CXMLDOMAttributeObj *pObj = NULL;
			sc = CXMLDOMAttributeObj::CreateInstance(&pObj);
			if (SUCCEEDED(sc)) {
				pObj->attr = oldAttr;
				pObj->AddRef();
				pObj->SetOwnerDoc(m_pIXMLDOMDocument);

				sc = pObj->QueryInterface(IID_IXMLDOMAttribute, reinterpret_cast<LPVOID*> (attributeNode));
				pObj->Release();
			}
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


	return sc;
}

STDMETHODIMP CXMLDOMElement::removeAttributeNode(IXMLDOMAttribute  *attr, IXMLDOMAttribute  * *attributeNode)
{
	ATLTRACE(_T("CXMLDOMElement::removeAttributeNode\n"));

	if (NULL == attr || NULL == attributeNode)
		return E_POINTER;

	*attributeNode = NULL;

	CXMLDOMAttributeObj *pObj = NULL;
	HRESULT hr = CXMLDOMAttributeObj::CreateInstance(&pObj);
	if (S_OK != hr)
		return hr;
	
	pObj->AddRef();
	pObj->SetOwnerDoc(m_pIXMLDOMDocument);

	try
	{
		long id = 0;
		IIBMXMLDOMNodeIdentity* nodeID = NULL;
		if(SUCCEEDED(attr->QueryInterface(IID_IIBMXMLDOMNodeIdentity,(void**) &nodeID))) {
			nodeID->get_NodeId(&id);
			nodeID->Release();
		}
		pObj->attr = element->removeAttributeNode((DOMAttr*) id);
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
	
	hr = pObj->QueryInterface(IID_IXMLDOMAttribute, reinterpret_cast<LPVOID*> (attributeNode));
	if (S_OK != hr)
		*attributeNode = NULL;

	pObj->Release();
	return hr;
}

STDMETHODIMP CXMLDOMElement::getElementsByTagName(BSTR tagName, IXMLDOMNodeList  **pVal)
{
	ATLTRACE(_T("CXMLDOMElement::getElementsByTagName\n"));

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
		pObj->m_container = element->getElementsByTagName(tagName);
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

STDMETHODIMP CXMLDOMElement::normalize(void)
{
	ATLTRACE(_T("CXMLDOMElement::normalize\n"));

	try
	{
		element->normalize();
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
