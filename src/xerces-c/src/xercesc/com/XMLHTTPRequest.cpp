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
 * $Id: XMLHTTPRequest.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "stdafx.h"

#include "xml4com.h"
#include "XMLHttpRequest.h"
#include "XMLDOMDocument.h"

// I need to make sure the file is registered with long filenames
HRESULT WINAPI CXMLHttpRequest::UpdateRegistry(BOOL bRegister)
{
	USES_CONVERSION;

	TCHAR file[MAX_PATH];
	if (::GetModuleFileName(_Module.m_hInst, file, MAX_PATH)) {
		WIN32_FIND_DATA d;
		memset(&d,0,sizeof(WIN32_FIND_DATA));
		HANDLE h = FindFirstFile(file,&d);
		if (h != INVALID_HANDLE_VALUE) {
			TCHAR  *name = _tcsrchr(file,_T('\\'));
			TCHAR newFile[MAX_PATH] = _T("");
			_tcsncpy(newFile,file,name-file);
			_tcscat(newFile,_T("\\"));
			_tcscat(newFile,d.cFileName);
			FindClose(h);
			
			_ATL_REGMAP_ENTRY regmap[2] = {{NULL,NULL},{NULL,NULL}};
			regmap[0].szKey  = OLESTR("XMLMODULE");
			regmap[0].szData = T2OLE(newFile);
			return _Module.UpdateRegistryFromResource((UINT) IDR_XMLHTTPREQUEST, bRegister,regmap);
		}
	}
	return E_FAIL;
}

CXMLHttpRequest::CXMLHttpRequest()
	:m_pOnReadyStateChange	(NULL)
	,m_bAbort				(false)
	,m_hThread				(NULL)
	,m_lReadyState			(0)
	,m_bAsync				(false)
	,m_Method				(_T(""))
	,m_HostName				(_T(""))
	,m_Port					(INTERNET_DEFAULT_HTTP_PORT)
	,m_URLPath				(_T(""))
	,m_User					(_T(""))
	,m_Password				(_T(""))
	,m_dwStatus				(0)
	,m_StatusText			(_T(""))
	,m_ResponseHeaders		(_T(""))
	,m_HwndParent			(NULL)
	,m_pBody				(NULL)
	,m_lBodyLength			(0)	
	,m_pResponseBody		(NULL)
	,m_lResponseBodyLength	(0)	
	,m_Error				(_T(""))	
	,m_bSuccess				(true)
{
}

HRESULT CXMLHttpRequest::FinalConstruct()
{
	// create monitor window
	RECT rc;
    memset(&rc,0,sizeof(RECT));
	if (NULL == Create(NULL, rc, _T("XML HTTP Request Monitor Window"), 0))
		return E_FAIL;

	return S_OK;
}

void CXMLHttpRequest::FinalRelease()
{
	if (NULL != m_hThread) {
		m_bAbort = true;
		::WaitForSingleObject(m_hThread, INFINITE);
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	if (m_pOnReadyStateChange != NULL) {
		m_pOnReadyStateChange->Release();
		m_pOnReadyStateChange = NULL;
	}

	DestroyWindow();

	delete [] m_pBody;
	m_pBody = NULL;
	m_lBodyLength = 0;

	delete [] m_pResponseBody;
	m_pResponseBody = NULL;
	m_lResponseBodyLength = 0;
}

LRESULT CXMLHttpRequest::OnReadyStateChange(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	ATLTRACE(_T("CXMLHttpRequest::OnReadyStateChange\n"));

	bHandled = TRUE;

	m_lReadyState = wParam;
	if (NULL != m_pOnReadyStateChange) {
		CComVariant varResult;
		DISPPARAMS disp = { NULL, NULL, 0, 0 };
		m_pOnReadyStateChange->Invoke(DISPID_VALUE, IID_NULL, LOCALE_USER_DEFAULT, DISPATCH_METHOD, &disp, &varResult, NULL, NULL);
	}
		
	return 1L;
}


STDMETHODIMP CXMLHttpRequest::InterfaceSupportsErrorInfo(REFIID riid)
{
	if (IsEqualGUID(IID_IXMLHttpRequest,riid))
		return S_OK;
	return S_FALSE;
}

STDMETHODIMP CXMLHttpRequest::open(BSTR bstrMethod, BSTR bstrUrl,VARIANT varAsync,VARIANT bstrUser,VARIANT bstrPassword)
{
	ATLTRACE(_T("CXMLHttpRequest::open\n"));

	// do not open if there is a send active  
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_FAIL;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	if (V_VT(&varAsync) != VT_BOOL)
		return E_INVALIDARG;

	_bstr_t method = bstrMethod;
	if (0 == method.length())
		return E_INVALIDARG;

	_bstr_t url = bstrUrl;
	if (0 == url.length())
		return E_INVALIDARG;

	TCHAR hostName[INTERNET_MAX_PATH_LENGTH] = _T("");
	TCHAR strPathName[INTERNET_MAX_PATH_LENGTH] = _T("");
	URL_COMPONENTS urlComponents;
	memset(&urlComponents, 0, sizeof(URL_COMPONENTS)); 
	urlComponents.dwStructSize		= sizeof(URL_COMPONENTS);
	urlComponents.lpszHostName		= hostName;
	urlComponents.dwHostNameLength	= INTERNET_MAX_PATH_LENGTH;
	urlComponents.lpszUrlPath	  = strPathName;
	urlComponents.dwUrlPathLength = INTERNET_MAX_PATH_LENGTH;
		
	if (!InternetCrackUrl(url, url.length(), 0, &urlComponents)) 
		return E_INVALIDARG;

	m_Method	= method;
	m_HostName	= hostName;
	if (urlComponents.nPort != 0)
		m_Port		= urlComponents.nPort; 
	
	m_URLPath	= strPathName;
	m_bAsync	= (VARIANT_TRUE == V_BOOL(&varAsync)) ? true : false; 
	if (VT_BSTR == V_VT(&bstrUser))
		m_User	= V_BSTR(&bstrUser);
	else
		m_User	= _T("");

	if (VT_BSTR == V_VT(&bstrPassword) && m_User.length() > 0)
		m_Password = V_BSTR(&bstrPassword);
	else
		m_Password = _T(""); 

	return S_OK;
}

STDMETHODIMP CXMLHttpRequest::setRequestHeader(BSTR bstrHeader,  BSTR bstrValue)
{
	ATLTRACE(_T("CXMLHttpRequest::setRequestHeader\n"));

	if (NULL == bstrHeader || NULL == bstrValue)
		return E_INVALIDARG;

	// check if there is a send active  
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_PENDING;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	m_RequestHeaderMap.Remove(bstrHeader);
	m_RequestHeaderMap.Add(bstrHeader,bstrValue);	
	return S_OK;
}

STDMETHODIMP CXMLHttpRequest::getResponseHeader(BSTR bstrHeader, BSTR * pbstrValue)
{
	ATLTRACE(_T("CXMLHttpRequest::getResponseHeader\n"));

	if (NULL == pbstrValue)
		return E_POINTER;

	*pbstrValue = NULL;

	if (NULL == bstrHeader)
		return E_INVALIDARG;

	// check if there is a send active  
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_PENDING;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	if (0 == m_ResponseHeaders.length())
		return S_FALSE;

	_bstr_t value;
	_bstr_t header(bstrHeader);
	header += _T(": ");
	_tcslwr(header);
	TCHAR *pHeaders = new TCHAR[m_ResponseHeaders.length() + sizeof(TCHAR)];
	_tcscpy(pHeaders,m_ResponseHeaders);
	_tcslwr(pHeaders);
	TCHAR *pStart = _tcsstr(pHeaders,header);
	if (pStart) {
		pStart += header.length();
		TCHAR *pEnd = _tcsstr(pStart,_T("\r\n"));
		if (pEnd) {
			TCHAR *pHeader = new TCHAR[pEnd-pStart + sizeof(TCHAR)];
			_tcsncpy(pHeader,static_cast<LPCTSTR> (m_ResponseHeaders) + (pStart-pHeaders),pEnd-pStart);
			value = pHeader;
			delete [] pHeader;
		}
	}
	delete[] pHeaders;

	if (0 == value.length())
		return S_FALSE;

	*pbstrValue = value.copy();

	return S_OK;
}

STDMETHODIMP CXMLHttpRequest::getAllResponseHeaders(BSTR * pbstrHeaders)
{
	ATLTRACE(_T("CXMLHttpRequest::getAllResponseHeaders\n"));

	if (NULL == pbstrHeaders)
		return E_POINTER;

	*pbstrHeaders = NULL;

	if (NULL == pbstrHeaders)
		return E_INVALIDARG;
	
	// check if there is a send active  
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_PENDING;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	*pbstrHeaders = m_ResponseHeaders.copy();
	return S_OK;
}

STDMETHODIMP CXMLHttpRequest::send(VARIANT varBody)
{
	ATLTRACE(_T("CXMLHttpRequest::send\n"));

	if (V_VT(&varBody) != VT_BSTR					&& 
		V_VT(&varBody) != VT_DISPATCH				&&
		V_VT(&varBody) != (VT_ARRAY | VT_VARIANT)	&&
		V_VT(&varBody) != (VT_ARRAY | VT_UI1)		&&
		V_VT(&varBody) != VT_UNKNOWN)
		return E_INVALIDARG;

	// do not start another thread if there is another active  
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_PENDING;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	HRESULT hr = S_OK;
	m_bSuccess = true;
	m_bAbort = false;
	delete [] m_pBody;
	m_pBody = NULL;
	m_lBodyLength = 0;
	delete [] m_pResponseBody;
	m_pResponseBody = NULL;
	m_lResponseBodyLength = 0;
	m_dwStatus	 = 0;
	m_StatusText = _T("");
	m_ResponseHeaders = _T("");

	if (V_VT(&varBody) == VT_BSTR) {
		_bstr_t body = V_BSTR(&varBody);
		m_lBodyLength = body.length() + 1;
		m_pBody = new BYTE[m_lBodyLength];
		memset(m_pBody,0,m_lBodyLength);
		memcpy(m_pBody,static_cast<char*> (body),body.length());
	}
	else
	if (V_VT(&varBody) == VT_UNKNOWN) {
		CComQIPtr<IStream,&IID_IStream> pS(V_UNKNOWN(&varBody));
		if (!pS)
			return E_INVALIDARG;

		CComBSTR b;
		hr = b.ReadFromStream(pS);
		if (S_OK != hr)
			return hr;

		_bstr_t body = b;
		m_lBodyLength = body.length() + 1;
		m_pBody = new BYTE[m_lBodyLength];
		memset(m_pBody,0,m_lBodyLength);
		memcpy(m_pBody,static_cast<char*> (body),body.length());
	}
	else
	if (V_VT(&varBody) == VT_DISPATCH) {
		CComQIPtr<IXMLDOMDocument,&IID_IXMLDOMDocument> pDoc(V_DISPATCH(&varBody));
		if (!pDoc)
			return E_INVALIDARG;
		
		BSTR b = NULL;
		hr = pDoc->get_xml(&b);
		if (S_OK != hr)
			return hr;

		_bstr_t body = b;
		::SysFreeString(b);

		m_lBodyLength = body.length() + 1;
		m_pBody = new BYTE[m_lBodyLength];
		memset(m_pBody,0,m_lBodyLength);
		memcpy(m_pBody,static_cast<char*> (body),body.length());
	}
	else
	if (V_VT(&varBody) == (VT_ARRAY | VT_VARIANT)) {
		SAFEARRAY *pArray = reinterpret_cast<SAFEARRAY *> (varBody.byref);
		if (NULL == pArray)
			return E_INVALIDARG;

		long lLBoundVar = 0;
		long lUBoundVar = 0;
	
		UINT dims = ::SafeArrayGetDim(pArray);
		if (dims == 0)
			return E_INVALIDARG;
	
		hr = ::SafeArrayGetLBound(pArray, dims, &lLBoundVar);
		if (S_OK != hr)
			return hr;

		hr = ::SafeArrayGetUBound(pArray, dims, &lUBoundVar);
		if (S_OK != hr)
			return hr;

		if (lUBoundVar >= lLBoundVar) {
			VARIANT *pIndex = NULL; 
			hr = ::SafeArrayAccessData(pArray, reinterpret_cast<void **> (&pIndex));
			if (S_OK != hr)
				return hr;

			m_lBodyLength = lUBoundVar-lLBoundVar+1;
			m_pBody = new BYTE[m_lBodyLength];
			for (long i = 0; i <= lUBoundVar-lLBoundVar; ++i) {	
				VARIANT var = pIndex[i];
				if (V_VT(&var) != VT_UI1) {
					hr = E_INVALIDARG;
					break;
				}
				m_pBody[i] = V_UI1(&var); 
			}
					
			::SafeArrayUnaccessData(pArray);
			if (S_OK != hr) {
				delete [] m_pBody;
				m_pBody = NULL;
				m_lBodyLength = 0;
				return hr;
			}
		}
	}	
	else
	if (V_VT(&varBody) == (VT_ARRAY | VT_UI1)) {
		SAFEARRAY *pArray = reinterpret_cast<SAFEARRAY *> (varBody.byref);
		if (NULL == pArray)
			return E_INVALIDARG;

		long lLBoundVar = 0;
		long lUBoundVar = 0;
	
		UINT dims = ::SafeArrayGetDim(pArray);
		if (dims == 0)
			return E_INVALIDARG;
	
		hr = ::SafeArrayGetLBound(pArray, dims, &lLBoundVar);
		if (S_OK != hr)
			return hr;

		hr = ::SafeArrayGetUBound(pArray, dims, &lUBoundVar);
		if (S_OK != hr)
			return hr;

		if (lUBoundVar >= lLBoundVar) {
			BYTE *pIndex = NULL; 
			hr = ::SafeArrayAccessData(pArray, reinterpret_cast<void **> (&pIndex));
			if (S_OK != hr)
				return hr;

			m_lBodyLength = lUBoundVar-lLBoundVar+1;
			m_pBody = new BYTE[m_lBodyLength];
			for (long i = 0; i <= lUBoundVar-lLBoundVar; ++i)	
				m_pBody[i] = pIndex[i]; 
								
			::SafeArrayUnaccessData(pArray);
		}
	}	

	m_HwndParent = GetParentWindow();

	UINT nthreadID = 0;
	m_hThread = reinterpret_cast<HANDLE> (_beginthreadex(NULL,
												 0,
											     CXMLHttpRequest::SendThread,
												 (void *) this, 
												 0,
												 &nthreadID));
	if (NULL == m_hThread) 
		return E_FAIL;
	
	if (m_bAsync) 
		return S_OK;
	
	bool bWait = true;
	while (bWait) {
		DWORD dwEvt = MsgWaitForMultipleObjects(1,&m_hThread,FALSE,INFINITE,QS_ALLINPUT);
		switch(dwEvt) {
			case WAIT_OBJECT_0:
				bWait = false;
				break;
			case WAIT_OBJECT_0 + 1:
			{
				MSG msg;
				while(::PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE)) { 
					if (WM_CLOSE == msg.message || WM_QUIT == msg.message) {
						 bWait = false;
						 m_bAbort = true;
						 break;
					}
					else {
						PeekMessage(&msg, NULL, 0, 0, PM_REMOVE);
						TranslateMessage(&msg);  
						DispatchMessage(&msg);
					}
				}
				break;
			}
			default:
				m_bAbort = true;
				bWait = false;
				break;
		}
	}

	return S_OK;
}

UINT APIENTRY CXMLHttpRequest::SendThread(void *pParm)
{
	ATLTRACE(_T("CXMLHttpRequest::SendThread\n"));

	CXMLHttpRequest *pCtx = reinterpret_cast<CXMLHttpRequest *> (pParm);
	if (NULL == pCtx)
		return 0;

	HINTERNET hOpen    = NULL;
	HINTERNET hConnect = NULL; 
	HINTERNET hRequest = NULL; 

	hOpen = InternetOpen(_T("XMLHTTP/1.0"),INTERNET_OPEN_TYPE_PRECONFIG,
										NULL,NULL,0);
	if (NULL == hOpen) {
		DWORD res = GetLastError();
		pCtx->m_Error = CXMLHttpRequest::GetErrorMsg(res);
		pCtx->m_bSuccess = false;
	}

	if (!pCtx->m_bAbort && pCtx->m_bSuccess) {
		if (INTERNET_INVALID_STATUS_CALLBACK == InternetSetStatusCallback(hOpen,
				CXMLHttpRequest::InternetStatusCallback)) {
			pCtx->m_Error = _T("Invalid Internet Status Callback function.");
			pCtx->m_bSuccess = false;
		}
	}

	bool bPromptForAuthentication = true;
	if (!pCtx->m_bAbort && pCtx->m_bSuccess) {
		LPTSTR lpszUserName = NULL;
		LPTSTR lpszPassword = NULL;
		if (pCtx->m_User.length() > 0) {
			bPromptForAuthentication = false;
			lpszUserName = pCtx->m_User; 
			if (pCtx->m_Password.length() > 0) 
				lpszPassword = pCtx->m_Password;
		}    
		hConnect = InternetConnect(hOpen,pCtx->m_HostName,pCtx->m_Port,lpszUserName,lpszPassword,
										INTERNET_SERVICE_HTTP,0,reinterpret_cast<DWORD> (pCtx));
		if (NULL == hConnect) {
			DWORD res = GetLastError();
			pCtx->m_Error = CXMLHttpRequest::GetErrorMsg(res);
			pCtx->m_bSuccess = false;
		}
	}

	if (!pCtx->m_bAbort && pCtx->m_bSuccess) {
		DWORD dwFlags = (443 == pCtx->m_Port) ? INTERNET_FLAG_SECURE : 0;
		dwFlags |= INTERNET_FLAG_NO_CACHE_WRITE;
		hRequest = HttpOpenRequest(hConnect,pCtx->m_Method,
						pCtx->m_URLPath,NULL,NULL,NULL,
						dwFlags,reinterpret_cast<DWORD> (pCtx));
		if (NULL == hRequest) {
			DWORD res = GetLastError();
			pCtx->m_bSuccess = false;
			pCtx->m_Error = CXMLHttpRequest::GetErrorMsg(res);
		}
	}

	BOOL rc = TRUE;

	if (!pCtx->m_bAbort && pCtx->m_bSuccess && pCtx->m_RequestHeaderMap.GetSize() > 0) {
		_bstr_t requestHeaders;
		for (int i = 0 ; i < pCtx->m_RequestHeaderMap.GetSize(); ++i) {
			requestHeaders += pCtx->m_RequestHeaderMap.GetKeyAt(i);
			requestHeaders += _T(": ");
			requestHeaders += pCtx->m_RequestHeaderMap.GetValueAt(i);
			requestHeaders += _T("\r\n");
		}
		rc = HttpAddRequestHeaders(hRequest,requestHeaders,-1,HTTP_ADDREQ_FLAG_ADD | HTTP_ADDREQ_FLAG_REPLACE);
		if (!rc) {	
			DWORD res = GetLastError();
			pCtx->m_bSuccess = false;
			pCtx->m_Error = CXMLHttpRequest::GetErrorMsg(res);
		}
	}
	
	DWORD dwLen = 0;
	DWORD dwError = ERROR_SUCCESS; 
	do {
		if (!pCtx->m_bAbort && pCtx->m_bSuccess) {
			rc = HttpSendRequest(hRequest,NULL,0,pCtx->m_pBody,pCtx->m_lBodyLength);
			if (!rc) {	
				DWORD res = GetLastError();
				pCtx->m_bSuccess = false;
				pCtx->m_Error = CXMLHttpRequest::GetErrorMsg(res);
				break;
			}
		}
		if (!pCtx->m_bAbort && pCtx->m_bSuccess) {
			dwLen = sizeof(DWORD);
			rc = HttpQueryInfo(hRequest,
							   HTTP_QUERY_STATUS_CODE | HTTP_QUERY_FLAG_NUMBER,
							   &pCtx->m_dwStatus,&dwLen,NULL);
			if (!rc) {
				DWORD res = GetLastError();
				pCtx->m_bSuccess = false;
				pCtx->m_Error = CXMLHttpRequest::GetErrorMsg(res);
				break;
			}
		}
		if (!pCtx->m_bAbort && pCtx->m_bSuccess &&
			bPromptForAuthentication &&
			(HTTP_STATUS_PROXY_AUTH_REQ == pCtx->m_dwStatus ||
			 HTTP_STATUS_DENIED		    == pCtx->m_dwStatus)) 
   			dwError = InternetErrorDlg(pCtx->m_HwndParent,
									   hRequest,
									   ERROR_INTERNET_INCORRECT_PASSWORD,
									   FLAGS_ERROR_UI_FILTER_FOR_ERRORS	   |
									   FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS |
									   FLAGS_ERROR_UI_FLAGS_GENERATE_DATA,				
									   NULL);
		else
			break;
	
	} while (ERROR_INTERNET_FORCE_RETRY == dwError &&
			 !pCtx->m_bAbort && pCtx->m_bSuccess); 		
	
	if (!pCtx->m_bAbort && pCtx->m_bSuccess) {
		dwLen = 1024;
		TCHAR *pBuff = new TCHAR[dwLen];
		rc = HttpQueryInfo(hRequest,HTTP_QUERY_RAW_HEADERS_CRLF,pBuff,&dwLen,NULL);
		if (!rc) {
			DWORD res = GetLastError();
			if (ERROR_INSUFFICIENT_BUFFER == res) {
				delete [] pBuff;
				pBuff = new TCHAR[dwLen];
				rc = HttpQueryInfo(hRequest,HTTP_QUERY_RAW_HEADERS_CRLF,pBuff,&dwLen,NULL);
				if (!rc) {
					res = GetLastError();
					pCtx->m_bSuccess = false;
					pCtx->m_Error = CXMLHttpRequest::GetErrorMsg(res);
				}
			}
			else {
				pCtx->m_bSuccess = false;
				pCtx->m_Error = CXMLHttpRequest::GetErrorMsg(res);
			}
		}
		if (rc)
			 pCtx->m_ResponseHeaders = pBuff;

		delete [] pBuff;
	}

	if (!pCtx->m_bAbort && pCtx->m_bSuccess) {
		dwLen = 1024;
		TCHAR *pBuff = new TCHAR[dwLen];
		rc = HttpQueryInfo(hRequest,HTTP_QUERY_STATUS_TEXT,pBuff,&dwLen,NULL);
		if (!rc) {
			DWORD res = GetLastError();
			if (ERROR_INSUFFICIENT_BUFFER == res) {
				delete [] pBuff;
				pBuff = new TCHAR[dwLen];
				rc = HttpQueryInfo(hRequest,HTTP_QUERY_STATUS_TEXT,pBuff,&dwLen,NULL);
				if (!rc) 
					_tcscpy(pBuff,_T("Unknown"));
			}
			else 
				_tcscpy(pBuff,_T("Unknown"));
		}
		pCtx->m_StatusText = pBuff; 
		delete [] pBuff;
	
		if (HTTP_STATUS_OK != pCtx->m_dwStatus) {
			TCHAR errBuff[MAX_PATH] = _T("");
			wsprintf(errBuff,_T("HTTP Status Code: %d, Reason: "),pCtx->m_dwStatus);
			pCtx->m_Error = errBuff;
			pCtx->m_Error += pCtx->m_StatusText;
			pCtx->m_bSuccess = false;
		}
	}

	if (!pCtx->m_bAbort && pCtx->m_bSuccess) {
		PBYTE buffer[255];
		DWORD dwRead = 0;
		delete [] pCtx->m_pResponseBody;
		pCtx->m_pResponseBody = NULL;
		pCtx->m_lResponseBodyLength = 0;
		while (rc = InternetReadFile(hRequest,buffer,255,&dwRead)) {
			if (!rc || pCtx->m_bAbort || 0 == dwRead) 
				break;
					
			PBYTE tmp = new BYTE[pCtx->m_lResponseBodyLength + dwRead];
			if (pCtx->m_pResponseBody) {
				memcpy(tmp,pCtx->m_pResponseBody,pCtx->m_lResponseBodyLength);
				delete [] pCtx->m_pResponseBody;
			}

			memcpy(tmp+pCtx->m_lResponseBodyLength,buffer,dwRead);
			pCtx->m_pResponseBody = tmp;
			pCtx->m_lResponseBodyLength += dwRead;
		}
		if (!rc) {
			DWORD res = GetLastError();
			pCtx->m_Error = _T("Error reading response: ") + CXMLHttpRequest::GetErrorMsg(res);
			pCtx->m_bSuccess = false;
		}
	}

	if (hRequest != NULL)
		InternetCloseHandle(hRequest);
	
	if (hConnect != NULL) 
		InternetCloseHandle(hConnect);

	if (hOpen)
		InternetCloseHandle(hOpen);
	
	if (!pCtx->m_bAbort && pCtx->m_bAsync)
		::PostMessage(pCtx->m_hWnd,MSG_READY_STATE_CHANGE,4,0); 
  
	return 0;
}

HWND CXMLHttpRequest::GetParentWindow()
{
	HWND hWnd = GetDesktopWindow();
	
	CComPtr<IServiceProvider> pSP;
	HRESULT hr = GetSite(IID_IServiceProvider, reinterpret_cast<LPVOID *> (&pSP));
    if (S_OK != hr)
		return hWnd;

	CComPtr<IWebBrowser2> pWB;
    hr = pSP->QueryService(SID_SWebBrowserApp,IID_IWebBrowser2,
                                 reinterpret_cast<LPVOID *> (&pWB));
	if (S_OK != hr)
		return hWnd;

	SHANDLE_PTR lWnd = 0;
	hr = pWB->get_HWND(&lWnd);
	if (S_OK != hr)
		return hWnd;

	return reinterpret_cast<HWND> (lWnd);
}

void CALLBACK  CXMLHttpRequest::InternetStatusCallback(HINTERNET hInternet,
													DWORD_PTR dwContext,
													DWORD dwInternetStatus,
													LPVOID lpvStatusInformation,
													DWORD dwStatusInformationLength)
{				
	ATLTRACE(_T("CXMLHttpRequest::InternetStatusCallback - dwInternetStatus %d\n"),dwInternetStatus);
}	

STDMETHODIMP CXMLHttpRequest::abort()
{
	ATLTRACE(_T("CXMLHttpRequest::abort\n"));
	m_bAbort = true;
	return S_OK;
}	

STDMETHODIMP CXMLHttpRequest::get_status(long * plStatus)
{
	ATLTRACE(_T("CXMLHttpRequest::get_status\n"));

	if (NULL == plStatus)
		return E_POINTER;

	*plStatus = 0;

	// check if there is a send active  
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_PENDING;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	*plStatus = m_dwStatus;
	return S_OK;
}

STDMETHODIMP CXMLHttpRequest::get_statusText( BSTR * pbstrStatus)
{
	ATLTRACE(_T("CXMLHttpRequest::get_statusText\n"));
	
	if (NULL == pbstrStatus)
		return E_POINTER;

	*pbstrStatus = NULL;

	// check if there is a send active  
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_PENDING;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	*pbstrStatus = m_StatusText.copy();
	return S_OK;
}

STDMETHODIMP CXMLHttpRequest::get_responseXML(IDispatch **ppBody)
{
	ATLTRACE(_T("CXMLHttpRequest::get_responseXML\n"));

	if (NULL == ppBody)
		return E_POINTER;

	*ppBody = NULL;

	// check if there is a send active 
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_PENDING;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	BSTR text = NULL;
	HRESULT hr = get_responseText(&text);
	if (S_OK != hr || NULL == text)
		return hr;

	CXMLDOMDocumentObj *pObj = NULL;
	hr = CXMLDOMDocumentObj::CreateInstance(&pObj);
	if (S_OK == hr) {	
		pObj->AddRef();
		VARIANT_BOOL isSuccessful = VARIANT_FALSE;
		hr = pObj->loadXML(text, &isSuccessful);
		if (S_OK == hr && VARIANT_TRUE == isSuccessful) {
			*ppBody = pObj;
			(*ppBody)->AddRef();
		}
		pObj->Release();
	}

	::SysFreeString(text);

	return hr;
}

STDMETHODIMP CXMLHttpRequest::get_responseText(BSTR *pVal)
{
	ATLTRACE(_T("CXMLHttpRequest::get_responseText\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;

	// check if there is a send active 
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_PENDING;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	if (NULL == m_pResponseBody)
		return S_OK;

	TCHAR *psz = new TCHAR[m_lResponseBodyLength+1];
	ZeroMemory(psz,m_lResponseBodyLength+1);
	CopyMemory(psz,m_pResponseBody,m_lResponseBodyLength);
  	
	*pVal =  SysAllocStringByteLen(psz,m_lResponseBodyLength); 
	delete [] psz;
 
	return S_OK;
}

STDMETHODIMP CXMLHttpRequest::get_responseBody(VARIANT *pVal)
{
	ATLTRACE(_T("CXMLHttpRequest::get_responseBody\n"));

	if (NULL == pVal)
		return E_POINTER;

	::VariantInit(pVal);
	V_VT(pVal) = VT_NULL;

	// check if there is a send active 
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_PENDING;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}

	if (NULL == m_pResponseBody)
		return S_OK;

	return CXMLHttpRequest::InitializeVarFromByte(*pVal, m_pResponseBody,m_lResponseBodyLength);
}

STDMETHODIMP CXMLHttpRequest::get_responseStream(VARIANT *pVal)
{
	ATLTRACE(_T("CXMLHttpRequest::get_responseStream\n"));
	
	if (NULL == pVal)
		return E_POINTER;

	::VariantInit(pVal);
	V_VT(pVal) = VT_NULL;

	// check if there is a send active 
	if (NULL != m_hThread) {
		DWORD exitCode = 0;
		BOOL rc = ::GetExitCodeThread(m_hThread, &exitCode);
		if (!rc || STILL_ACTIVE == exitCode) 
			return E_PENDING;
		
		::CloseHandle(m_hThread);
		m_hThread = NULL;
	}
	
	if (NULL == m_pResponseBody)
		return S_OK;

	//Create an IStream from global memory
	CComPtr<IStream> pStm;
    HRESULT hr = CreateStreamOnHGlobal(NULL, TRUE, &pStm);
    if (S_OK != hr) 
		return hr;
	
	hr = pStm->Write(m_pResponseBody, m_lResponseBodyLength, NULL);
	if (S_OK != hr) 
		return hr;

	LARGE_INTEGER dlibMove;
	memset(&dlibMove,0,sizeof(LARGE_INTEGER));
	hr = pStm->Seek(dlibMove,STREAM_SEEK_SET,NULL);    
	if (S_OK != hr) 
		return hr;

	V_VT(pVal)		= VT_UNKNOWN;
	V_UNKNOWN(pVal) = pStm.Detach(); 

	return S_OK;
}

STDMETHODIMP CXMLHttpRequest::get_readyState(long *pVal)
{
	ATLTRACE(_T("CXMLHttpRequest::get_readyState\n"));
	
	if (NULL == pVal)
		return E_POINTER;

	*pVal = m_lReadyState;

	return S_OK;
}

STDMETHODIMP CXMLHttpRequest::put_onreadystatechange(IDispatch * pReadyStateSink)
{
	ATLTRACE(_T("CXMLHttpRequest::put_onreadystatechange\n"));

	if (m_pOnReadyStateChange != NULL) {
		m_pOnReadyStateChange->Release();
		m_pOnReadyStateChange = NULL;
	}
	m_pOnReadyStateChange = pReadyStateSink;
	if (m_pOnReadyStateChange != NULL) 
		m_pOnReadyStateChange->AddRef();

	return S_OK;
}

_bstr_t CXMLHttpRequest::GetErrorMsg(DWORD rc)
{
	_bstr_t msg(_T(""));
	TCHAR *lpBuffer = NULL;
	
	if (ERROR_INTERNET_EXTENDED_ERROR == rc) {
		DWORD dwError  = 0;
		DWORD dwLength = 0; 
		InternetGetLastResponseInfo (&dwError, NULL, &dwLength);
        if (dwLength > 0) {
			lpBuffer = (TCHAR *) LocalAlloc(LPTR,dwLength);
			if (!lpBuffer) {
				msg = _T("Unable to allocate memory to display Internet extended error: ");
                rc = GetLastError();
          	}
			else {
                if (!InternetGetLastResponseInfo(&dwError,lpBuffer,&dwLength)) {
				    msg = _T("Unable to get Internet extended error: ");
                    rc = GetLastError();
					LocalFree(lpBuffer);
                }
				else {
					int len = lstrlen(lpBuffer);
					for (int i=0; i < len; ++i) {
						if (_istcntrl(lpBuffer[i])) 
							lpBuffer[i] = _T(' ');
					}
					msg = lpBuffer;
					LocalFree(lpBuffer);
					return msg;
 				}
			}
		}
    }
	
	lpBuffer = NULL;
	HMODULE hModule = NULL; // default to system source
    
	if (rc >= INTERNET_ERROR_BASE) 
		 hModule = LoadLibraryEx(_T("wininet.dll"),NULL,LOAD_LIBRARY_AS_DATAFILE);
    
	::FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER	|
					FORMAT_MESSAGE_IGNORE_INSERTS	|
					FORMAT_MESSAGE_FROM_SYSTEM		|
					((hModule != NULL) ? FORMAT_MESSAGE_FROM_HMODULE : 0),
					hModule, 
					rc,
					MAKELANGID(LANG_NEUTRAL, SUBLANG_SYS_DEFAULT),
					(LPTSTR)&lpBuffer, 0, NULL);
			
	if (lpBuffer) {
		int len = lstrlen(lpBuffer);
		for (int i=0; i < len; ++i) {
			if (_istcntrl(lpBuffer[i])) 
				lpBuffer[i] = _T(' ');
		}
	}

	msg += lpBuffer;
	LocalFree(lpBuffer);

	if (hModule != NULL)
	    FreeLibrary(hModule);

	return msg;
}

HRESULT	CXMLHttpRequest::InitializeVarFromByte(VARIANT &varOut, const PBYTE pByte, long lSize)
{
	::VariantInit(&varOut);
	V_VT(&varOut) = VT_NULL;

	HRESULT hr = S_OK;
	SAFEARRAYBOUND rgsabound[1];
	rgsabound[0].cElements = lSize;
	rgsabound[0].lLbound   = 0;
	
	SAFEARRAY *psa = ::SafeArrayCreate( VT_VARIANT, 1, rgsabound);
	if (psa == NULL)
		return E_FAIL;
	
	if (pByte != NULL) {
		long ix[1];
		for (ULONG i = 0; i < rgsabound[0].cElements; ++i) {
			ix[0] = i;
			VARIANT var;
			::VariantInit(&var);
			V_VT(&var)   = VT_UI1;
			V_UI1(&var)	 = pByte[i];
	
			hr = ::SafeArrayPutElement(psa, ix, &var);
			if (S_OK != hr) {
				::SafeArrayDestroy(psa);
				break;		
			}
		}
	}

	if (S_OK == hr) {
		V_VT(&varOut)	 = VT_ARRAY | VT_VARIANT;
		V_ARRAY(&varOut) = psa;
	}
	
	return hr;
}