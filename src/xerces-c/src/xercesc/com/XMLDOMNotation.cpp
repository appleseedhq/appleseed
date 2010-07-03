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
 * $Id: XMLDOMNotation.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "stdafx.h"
#include "xml4com.h"
#include "XMLDOMNotation.h"

STDMETHODIMP CXMLDOMNotation::get_publicId(VARIANT  *pVal)
{
	ATLTRACE(_T("CXMLDOMNotation::get_publicId\n"));

	if (NULL == pVal)
		return E_POINTER;

	::VariantInit(pVal);

	try
	{
        const XMLCh* val=notation->getPublicId();
		if(val == NULL)
		{
			V_VT(pVal) = VT_NULL;
		}
		else
		{
			V_VT(pVal) = VT_BSTR;
			V_BSTR(pVal) = SysAllocString(val);
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

STDMETHODIMP CXMLDOMNotation::get_systemId(VARIANT  *pVal)
{
	ATLTRACE(_T("CXMLDOMNotation::get_systemId\n"));

	if (NULL == pVal)
		return E_POINTER;

	::VariantInit(pVal);

	try
	{
        const XMLCh* val=notation->getSystemId();
		if(val == NULL)
		{
			V_VT(pVal) = VT_NULL;
		}
		else
		{
			V_VT(pVal) = VT_BSTR;
			V_BSTR(pVal) = SysAllocString(val);
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
