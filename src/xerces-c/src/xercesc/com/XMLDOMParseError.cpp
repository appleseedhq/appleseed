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
 * $Id: XMLDOMParseError.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "stdafx.h"
#include "xml4com.h"
#include "XMLDOMParseError.h"

HRESULT CXMLDOMParseError::FinalConstruct()
{
	m_CS.Init();
	return S_OK;
}

void CXMLDOMParseError::FinalRelease()	
{
	m_CS.Term(); 
}

void CXMLDOMParseError::SetData( long code,
								 const _bstr_t &url,
								 const _bstr_t &reason,
								 const _bstr_t &source,
								 long  lineNumber,
								 long  linePos,
								 long  filePos)
{
	m_CS.Lock(); 
	m_Code			= code;
	m_url			= url;
	m_Reason		= reason;
	m_Source		= source;
	m_LineNumber	= lineNumber;
	m_LinePos		= linePos;
	m_FilePos		= filePos;
	m_CS.Unlock(); 
}

void CXMLDOMParseError::Reset()
{
	m_CS.Lock(); 
	m_Code			= 0;
	m_url			= _T("");
	m_Reason		= _T("");
	m_Source		= _T("");
	m_LineNumber	= 0;
	m_LinePos		= 0;
	m_FilePos		= 0;
	m_CS.Unlock(); 
}

// IXMLDOMParseError methods
STDMETHODIMP CXMLDOMParseError::get_errorCode(long  *pVal)
{
	ATLTRACE(_T("CXMLDOMParseError::get_errorCode\n"));

	if (NULL == pVal)
		return E_POINTER;

	m_CS.Lock(); 
	*pVal = m_Code;
	m_CS.Unlock(); 

	return S_OK;
}

STDMETHODIMP CXMLDOMParseError::get_url(BSTR  *pVal)
{
	ATLTRACE(_T("CXMLDOMParseError::get_url\n"));

	if (NULL == pVal)
		return E_POINTER;

	m_CS.Lock(); 
	*pVal = m_url.copy();
	m_CS.Unlock(); 

	return S_OK;
}

STDMETHODIMP CXMLDOMParseError::get_reason(BSTR  *pVal)
{
	ATLTRACE(_T("CXMLDOMParseError::get_reason\n"));

	if (NULL == pVal)
		return E_POINTER;

	m_CS.Lock(); 
	*pVal = m_Reason.copy();
	m_CS.Unlock(); 

	return S_OK;
}

STDMETHODIMP CXMLDOMParseError::get_srcText(BSTR  *pVal)
{
	ATLTRACE(_T("CXMLDOMParseError::get_srcText\n"));

	if (NULL == pVal)
		return E_POINTER;

	m_CS.Lock(); 
	*pVal = m_Source.copy();
	m_CS.Unlock(); 
	
	return S_OK;
}

STDMETHODIMP CXMLDOMParseError::get_line(long  *pVal)
{
	ATLTRACE(_T("CXMLDOMParseError::get_line\n"));

	if (NULL == pVal)
		return E_POINTER;

	m_CS.Lock(); 
	*pVal = m_LineNumber;
	m_CS.Unlock(); 

	return S_OK;
}

STDMETHODIMP CXMLDOMParseError::get_linepos(long  *pVal)
{
	ATLTRACE(_T("CXMLDOMParseError::get_linepos\n"));

	if (NULL == pVal)
		return E_POINTER;

	m_CS.Lock(); 
	*pVal = m_LinePos;
	m_CS.Unlock(); 
	
	return S_OK;
}

STDMETHODIMP CXMLDOMParseError::get_filepos(long  *pVal)
{
	ATLTRACE(_T("CXMLDOMParseError::get_filepos\n"));

	if (NULL == pVal)
		return E_POINTER;

	m_CS.Lock(); 
	*pVal = m_FilePos;
	m_CS.Unlock(); 

	return S_OK;
}

