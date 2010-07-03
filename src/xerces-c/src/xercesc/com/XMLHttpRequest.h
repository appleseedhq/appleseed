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
 * $Id: XMLHttpRequest.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___xmlhttprequest_h___
#define ___xmlhttprequest_h___

#include <xercesc/dom/DOMDocument.hpp>
#include "IXMLDOMNodeImpl.h"

#include "resource.h"       // main symbols

class ATL_NO_VTABLE CXMLHttpRequest : 
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CXMLHttpRequest, &CLSID_XMLHTTPRequest>,
	public IObjectSafetyImpl<CXMLHttpRequest, INTERFACESAFE_FOR_UNTRUSTED_CALLER>,
	public IDispatchImpl<IXMLHttpRequest, &IID_IXMLHttpRequest, &LIBID_Xerces, XERCES_VERSION_MAJOR, INVK_CAT2_RAW_NUMERIC(XERCES_VERSION_MINOR,XERCES_VERSION_REVISION)>,
	public IObjectWithSiteImpl<CXMLHttpRequest>,
	public ISupportErrorInfo,
	public CWindowImpl<CXMLHttpRequest, CWindow, CWinTraits<0,0> >
{
public:
	CXMLHttpRequest();

	HRESULT FinalConstruct();
	void	FinalRelease();

	//DECLARE_REGISTRY_RESOURCEID(IDR_XMLHTTPREQUEST)
	static HRESULT WINAPI UpdateRegistry(BOOL bRegister);

DECLARE_NOT_AGGREGATABLE(CXMLHttpRequest)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXMLHttpRequest)
	COM_INTERFACE_ENTRY(IXMLHttpRequest)
	COM_INTERFACE_ENTRY(IDispatch)
	COM_INTERFACE_ENTRY(IObjectSafety)
	COM_INTERFACE_ENTRY(IObjectWithSite)
	COM_INTERFACE_ENTRY(ISupportErrorInfo)
END_COM_MAP()

	DECLARE_WND_CLASS(_T("XMLHttpRequestMonitor")) 

BEGIN_MSG_MAP(CMonitorWnd)
	MESSAGE_HANDLER(MSG_READY_STATE_CHANGE,	OnReadyStateChange)
END_MSG_MAP()

	LRESULT OnReadyStateChange(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);


	// ISupportsErrorInfo
	STDMETHOD(InterfaceSupportsErrorInfo)(REFIID riid);

	// IXMLHttpRequest methods
	STDMETHOD(open)(/*[in]*/ BSTR bstrMethod, /*[in]*/ BSTR bstrUrl, /*[in,optional]*/ VARIANT varAsync, /*[in,optional]*/ VARIANT bstrUser, /*[in,optional]*/ VARIANT bstrPassword);
	STDMETHOD(setRequestHeader)(/*[in]*/ BSTR bstrHeader, /*[in]*/ BSTR bstrValue);
	STDMETHOD(getResponseHeader)(/*[in]*/ BSTR bstrHeader, /*[out, retval]*/ BSTR * pbstrValue);
	STDMETHOD(getAllResponseHeaders)(/*[out, retval]*/ BSTR * pbstrHeaders);
	STDMETHOD(send)(/*[in, optional]*/ VARIANT varBody);
	STDMETHOD(abort)();
	STDMETHOD(get_status)(/*[out, retval]*/ long * plStatus);
	STDMETHOD(get_statusText)(/*[out, retval]*/ BSTR * pbstrStatus);
	STDMETHOD(get_responseXML)(/*[out, retval]*/ IDispatch ** ppBody);
	STDMETHOD(get_responseText)(/*[out, retval]*/ BSTR * pbstrBody);
	STDMETHOD(get_responseBody)(/*[out, retval]*/ VARIANT * pvarBody);
	STDMETHOD(get_responseStream)(/*[out, retval]*/ VARIANT * pvarBody);
	STDMETHOD(get_readyState)(/*[out, retval]*/ long * plState);
	STDMETHOD(put_onreadystatechange)(/*[in]*/ IDispatch * pReadyStateSink);

private:

	LPDISPATCH	  m_pOnReadyStateChange;
	bool		  m_bAbort; 	
	HANDLE		  m_hThread;		
	long		  m_lReadyState;
	bool		  m_bAsync;	
	_bstr_t		  m_Method;
	_bstr_t		  m_HostName;	
	INTERNET_PORT m_Port;
	_bstr_t		  m_URLPath;
	_bstr_t		  m_User;
	_bstr_t		  m_Password;
	DWORD		  m_dwStatus;
	_bstr_t		  m_StatusText;	
	_bstr_t		  m_ResponseHeaders;
	CSimpleMap<_bstr_t, _bstr_t>  m_RequestHeaderMap;
	HWND		  m_HwndParent;		
 
	PBYTE		  m_pBody;
	long		  m_lBodyLength;
	PBYTE		  m_pResponseBody;
	long		  m_lResponseBodyLength;
	_bstr_t	      m_Error;	
	bool	      m_bSuccess;

	HWND GetParentWindow();

	static _bstr_t GetErrorMsg(DWORD rc);
	static void CALLBACK InternetStatusCallback(HINTERNET hInternet,
												DWORD_PTR dwContext,
												DWORD dwInternetStatus,
												LPVOID lpvStatusInformation,
												DWORD dwStatusInformationLength);
	static UINT APIENTRY SendThread(void *pParm);
	static HRESULT InitializeVarFromByte(VARIANT &varOut, const PBYTE pByte, long lSize);
};

typedef CComObject<CXMLHttpRequest> CXMLHttpRequestObj;

#endif // ___xmlhttprequest_h___