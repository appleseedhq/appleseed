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
 * $Id: XMLDOMXMLDecl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "stdafx.h"
#include "xml4com.h"
#include "XMLDOMXMLDecl.h"

// IXMLDOMProcessingInstruction methods
STDMETHODIMP CXMLDOMXMLDecl::get_target(BSTR  *pVal)
{
	ATLTRACE(_T("CXMLDOMXMLDecl::get_target\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	try
	{
		*pVal = SysAllocString(OLESTR("xml"));
	}
	catch(...)
	{
		return E_FAIL;
	}

	return S_OK;
}

STDMETHODIMP CXMLDOMXMLDecl::get_data(BSTR  *pVal)
{
	ATLTRACE(_T("CXMLDOMXMLDecl::get_data\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	try
	{
        *pVal = SysAllocString(xmlDecl->getTarget());
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

STDMETHODIMP CXMLDOMXMLDecl::put_data(BSTR newVal)
{
	ATLTRACE(_T("CXMLDOMXMLDecl::put_data\n"));

	return E_NOTIMPL;
}
