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
 * $Id: XMLDOMNodeList.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___xmldomnodelist_h___
#define ___xmldomnodelist_h___

#include <xercesc/dom/DOMNodeList.hpp>
#include "NodeContainerImpl.h"

XERCES_CPP_NAMESPACE_USE

class ATL_NO_VTABLE CXMLDOMNodeList :
	public CComObjectRootEx<CComSingleThreadModel>,
	public IDispatchImpl<IXMLDOMNodeList, &IID_IXMLDOMNodeList, &LIBID_Xerces, XERCES_VERSION_MAJOR, INVK_CAT2_RAW_NUMERIC(XERCES_VERSION_MINOR,XERCES_VERSION_REVISION)>,
	public NodeContainerImpl<DOMNodeList>,
	public ISupportErrorInfo
{
public:
	CXMLDOMNodeList()
	{}

	void FinalRelease()
	{
		ReleaseOwnerDoc();
	}

DECLARE_NOT_AGGREGATABLE(CXMLDOMNodeList)
DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXMLDOMNodeList)
	COM_INTERFACE_ENTRY(IXMLDOMNodeList)
	COM_INTERFACE_ENTRY(IDispatch)
	COM_INTERFACE_ENTRY(ISupportErrorInfo)
END_COM_MAP()

	//   ISupportErrorInfo
	HRESULT STDMETHODCALLTYPE InterfaceSupportsErrorInfo(REFIID iid);
	

	// IXMLDOMNodeList
	STDMETHOD(get_item)(long index, IXMLDOMNode  **pVal);
	STDMETHOD(get_length)(long  *pVal);
	STDMETHOD(nextNode)(IXMLDOMNode  **nextItem);
	STDMETHOD(reset)();
	STDMETHOD(get__newEnum)(IUnknown  **pVal);
};
typedef CComObject<CXMLDOMNodeList> CXMLDOMNodeListObj;

#endif // ___xmldomnodelist_h___