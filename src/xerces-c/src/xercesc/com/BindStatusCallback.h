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
 * $Id: BindStatusCallback.h 568078 2007-08-21 11:43:25Z amassari $
 */

// BindStatusCallback.h : Declaration of the BindStatCallback

#ifndef ___bindstatuscallback_h___
#define ___bindstatuscallback_h___

class CXMLDOMDocument;

/////////////////////////////////////////////////////////////////////////////
// BindStatCallback
class ATL_NO_VTABLE CBindStatCallback : 
	public CComObjectRootEx<CComSingleThreadModel>,
	public IBindStatusCallback
{
public:
	CBindStatCallback()
		:m_pDoc(NULL)
	{
	}

DECLARE_NOT_AGGREGATABLE(CBindStatCallback)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CBindStatCallback)
	COM_INTERFACE_ENTRY(IBindStatusCallback)
END_COM_MAP()

// IBindStatusCallback
public:
	STDMETHOD(OnStartBinding)(DWORD dwReserved, IBinding *pBinding);
	STDMETHOD(GetPriority)(LONG *pnPriority);
	STDMETHOD(OnLowResource)(DWORD reserved);
	STDMETHOD(OnProgress)(ULONG ulProgress, ULONG ulProgressMax, ULONG ulStatusCode, LPCWSTR szStatusText);
	STDMETHOD(OnStopBinding)(HRESULT hresult, LPCWSTR szError);
	STDMETHOD(GetBindInfo)(DWORD *pgrfBINDF, BINDINFO *pbindInfo);
	STDMETHOD(OnDataAvailable)(DWORD grfBSCF, DWORD dwSize, FORMATETC *pformatetc, STGMEDIUM *pstgmed);
	STDMETHOD(OnObjectAvailable)(REFIID riid, IUnknown *punk);

	CXMLDOMDocument *m_pDoc;
};

typedef CComObject<CBindStatCallback> CBindStatCallbackObj;

#endif //___bindstatuscallback_h___
