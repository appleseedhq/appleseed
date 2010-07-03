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
 * $Id: BindStatusCallback.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// CBindStatCallback.cpp : Implementation of BindStatusCallback
#include "stdafx.h"
#include "resource.h"
#include "BindStatusCallback.h"
#include "xml4com.h"
#include "XMLDOMDocument.h"

/////////////////////////////////////////////////////////////////////////////
// CBindStatCallback

STDMETHODIMP CBindStatCallback::OnStartBinding(DWORD dwReserved, IBinding *pBinding)
{
	ATLTRACE(_T("CBindStatCallback::OnStartBinding\n"));
	return S_OK;
}

STDMETHODIMP CBindStatCallback::GetPriority(LONG *pnPriority)
{
	ATLTRACE(_T("CBindStatCallback::GetPriority\n"));
	return E_NOTIMPL;
}

STDMETHODIMP CBindStatCallback::OnLowResource(DWORD reserved)
{
	ATLTRACE(_T("CBindStatCallback::OnLowResource\n"));
	return E_NOTIMPL;
}

STDMETHODIMP CBindStatCallback::OnProgress(ULONG ulProgress, ULONG ulProgressMax, ULONG ulStatusCode, LPCWSTR szStatusText)
{
	ATLTRACE(_T("CBindStatCallback::OnProgress %d %d\n"),ulProgress,ulProgressMax);
	if (m_pDoc->IsAbort())
		return E_ABORT; 

	_bstr_t text(szStatusText);
	switch (ulStatusCode) {
		case BINDSTATUS_FINDINGRESOURCE:
			text = _T("Finding resource ") + text;
			break;
		case BINDSTATUS_CONNECTING:
			text = _T("Connecting ") + text;
			break;
		case BINDSTATUS_REDIRECTING:
			text = _T("Redirecting ") + text;
			break;
		case BINDSTATUS_BEGINDOWNLOADDATA:
			text = _T("Begin to download data ") + text;
			break;
		case BINDSTATUS_DOWNLOADINGDATA:
			text = _T("Downloading data ") + text;
			break;
		case BINDSTATUS_ENDDOWNLOADDATA:
			text = _T("End of downloading data ") + text;
			break;
		case BINDSTATUS_BEGINDOWNLOADCOMPONENTS:
			text = _T("Downloading components ") + text;
			break;
	    case BINDSTATUS_INSTALLINGCOMPONENTS:
			text = _T("Installing components ") + text;
			break;
	    case BINDSTATUS_ENDDOWNLOADCOMPONENTS:
			text = _T("End of downloading components ") + text;
			break;
	    case BINDSTATUS_USINGCACHEDCOPY:
			text = _T("Using cached copy ") + text;
			break;
	    case BINDSTATUS_SENDINGREQUEST:
			text = _T("Sending request ") + text;
			break;
	    case BINDSTATUS_CLASSIDAVAILABLE:
			text = _T("Classid available ") + text;
			break;
	    case BINDSTATUS_MIMETYPEAVAILABLE:
			text = _T("Mime type available ") + text;
			break;
	    case BINDSTATUS_CACHEFILENAMEAVAILABLE:
			text = _T("Cache file name available ") + text;
			break;
	    case BINDSTATUS_BEGINSYNCOPERATION:
			text = _T("Begin sync operation ") + text;
			break;
	    case BINDSTATUS_ENDSYNCOPERATION:
			text = _T("End of sync operation ") + text;
			break;
	    case BINDSTATUS_BEGINUPLOADDATA:
			text = _T("Begin uploading data ") + text;
			break;
	    case BINDSTATUS_UPLOADINGDATA:
			text = _T("Uploading data ") + text;
			break;
	    case BINDSTATUS_PROTOCOLCLASSID:
			text = _T("Protocol classid ") + text;
			break;
		case BINDSTATUS_ENCODING:
			text = _T("Encoding ") + text;
			break;
	    case BINDSTATUS_CLASSINSTALLLOCATION:
			text = _T("Class intall location ") + text;
			break;
	    case BINDSTATUS_DECODING:
			text = _T("Decoding ") + text;
			break;
		default:
			break;
	}

	ATLTRACE(_T("CBindStatCallback::OnProgress %s\n"),text);
	
	return S_OK;
}

STDMETHODIMP CBindStatCallback::OnStopBinding(HRESULT hresult, LPCWSTR szError)
{
	ATLTRACE(_T("CBindStatCallback::OnStopBinding\n"));
	return S_OK;
}

STDMETHODIMP CBindStatCallback::GetBindInfo(DWORD *pgrfBINDF, BINDINFO *pbindInfo)
{
	ATLTRACE(_T("CBindStatCallback::GetBindInfo\n"));
	return E_NOTIMPL;
}

STDMETHODIMP CBindStatCallback::OnDataAvailable(DWORD grfBSCF, DWORD dwSize, FORMATETC *pformatetc, STGMEDIUM *pstgmed)
{
	ATLTRACE(_T("CBindStatCallback::OnDataAvailable\n"));
	return E_NOTIMPL;
}

STDMETHODIMP CBindStatCallback::OnObjectAvailable(REFIID riid, IUnknown *punk)
{
	ATLTRACE(_T("CBindStatCallback::OnObjectAvailable\n"));
	return E_NOTIMPL;
}
