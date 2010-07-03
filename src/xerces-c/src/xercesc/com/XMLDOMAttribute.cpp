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
 * $Id: XMLDOMAttribute.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "stdafx.h"
#include "xml4com.h"
#include "XMLDOMAttribute.h"

// IXMLDOMAttribute methods
STDMETHODIMP CXMLDOMAttribute::get_name(BSTR  *pVal)
{
	ATLTRACE(_T("CXMLDOMAttribute::get_name\n"));

	if (NULL == pVal)
		return E_POINTER;

	try
	{
		*pVal = SysAllocString(attr->getName());
	}
	catch(DOMException& ex) {
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return S_OK;
}

STDMETHODIMP CXMLDOMAttribute::get_value(VARIANT  *pVal)
{
	ATLTRACE(_T("CXMLDOMAttribute::get_value\n"));

	if (NULL == pVal)
		return E_POINTER;

	::VariantInit(pVal);

	try
	{
		V_VT(pVal)   = VT_BSTR;
		V_BSTR(pVal) = SysAllocString(attr->getValue());
	}
	catch(DOMException& ex) {
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}


	return S_OK;
}

STDMETHODIMP CXMLDOMAttribute::put_value(VARIANT newVal)
{
	ATLTRACE(_T("CXMLDOMAttribute::put_value\n"));

	try
	{
		if(V_VT(&newVal) == VT_BSTR) {
			attr->setValue(V_BSTR(&newVal));
		}
		else {
			attr->setValue((BSTR) (_bstr_t) newVal);
		}

		attr->setValue(V_BSTR(&newVal));
	}
	catch(DOMException& ex) {
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return S_OK;
}

	//  IXMLDOMNode method
STDMETHODIMP CXMLDOMAttribute::get_specified(VARIANT_BOOL  *pVal)
{
	ATLTRACE(_T("CXMLDOMAttribute::get_specified\n"));

	try
	{
		*pVal = attr->getSpecified() ? VARIANT_TRUE : VARIANT_FALSE;
	}
	catch(DOMException& ex) {
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return S_OK;
}


STDMETHODIMP CXMLDOMAttribute::get_nodeValue(VARIANT  *pVal)
{
	ATLTRACE(_T("CXMLDOMAttribute::get_nodeValue\n"));

	if (NULL == pVal)
		return E_POINTER;

	::VariantInit(pVal);

	try
	{
		V_VT(pVal)   = VT_BSTR;
		V_BSTR(pVal) = SysAllocString(attr->getValue());
	}
	catch(DOMException& ex) {
		return MakeHRESULT(ex);
	}
	catch(...)
	{
		return E_FAIL;
	}


	return S_OK;
}


