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
 * $Id: IXMLDOMNodeImpl.h 569031 2007-08-23 15:05:28Z amassari $
 */

#ifndef ___ixmldomnodeimpl_h___
#define ___ixmldomnodeimpl_h___

#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/dom/DOMException.hpp>
XERCES_CPP_NAMESPACE_USE


//
//   This macro is defined in MSXML.H's compatible with IE5
//      and not defined in those from IE4.
//
//   To correct, install a IE5 or later version of the Microsoft Platform SDK
//      and add \Program Files\Microsoft Platform SDK\Include as the first entry
//      on the Directories tab on the dialog displayed after selecting Tools Options
//      from the Visual Studio IDE.
//
//   See http://xerces.apache.org/xerces-c/build.html#BuildCOM
#ifndef __IXMLDOMNode_INTERFACE_DEFINED__
#error "xerces-dom requires an MSXML.H compatible with IE5 or later.  See http://xerces.apache.org/xerces-c/build.html#BuildCOM for directions to correct this problem."
#endif


template <class T, const IID* piid, class tihclass = CComTypeInfoHolder>
class ATL_NO_VTABLE IXMLDOMNodeImpl:
	public IDispatchImpl<T,piid,&LIBID_Xerces, XERCES_VERSION_MAJOR, INVK_CAT2_RAW_NUMERIC(XERCES_VERSION_MINOR,XERCES_VERSION_REVISION),tihclass>,
	public IIBMXMLDOMNodeIdentity,
	public ISupportErrorInfo
{
public:

	IXMLDOMNodeImpl()
		:m_pIXMLDOMDocument(NULL)
	{}

	virtual DOMNode* get_DOMNode()		    = 0;
	virtual DOMNodeType get_DOMNodeType() const = 0;

	void	SetOwnerDoc(IXMLDOMDocument	*p)
	{
		m_pIXMLDOMDocument = p;
		if (m_pIXMLDOMDocument != NULL)
			m_pIXMLDOMDocument->AddRef();
	}

	// IIBMXMLDOMNodeIdentity
	STDMETHOD(get_NodeId)(long *pVal)
	{
		ATLTRACE(_T("IXMLDOMNodeImpl::get_NodeId\n"));

		if (NULL == pVal)
			return E_POINTER;

		*pVal = reinterpret_cast<long> (get_DOMNode());
		return S_OK;
	}

	//   ISupportErrorInfo
	HRESULT STDMETHODCALLTYPE InterfaceSupportsErrorInfo(REFIID iid);
	

	// IXMLDOMNode
    STDMETHOD(get_nodeName)(BSTR  *pVal);
	STDMETHOD(get_nodeValue)(VARIANT  *pVal);
	STDMETHOD(put_nodeValue)(VARIANT newVal);
	STDMETHOD(get_nodeType)(DOMNodeType  *pVal);
	STDMETHOD(get_parentNode)(IXMLDOMNode  * *pVal);
	STDMETHOD(get_childNodes)(IXMLDOMNodeList  * *pVal);
	STDMETHOD(get_firstChild)(IXMLDOMNode  * *pVal);
	STDMETHOD(get_lastChild)(IXMLDOMNode  * *pVal);
	STDMETHOD(get_previousSibling)(IXMLDOMNode  * *pVal);
	STDMETHOD(get_nextSibling)(IXMLDOMNode  * *pVal);
	STDMETHOD(get_attributes)(IXMLDOMNamedNodeMap  * *pVal);
	STDMETHOD(insertBefore)(IXMLDOMNode  *newChild, VARIANT refChild, IXMLDOMNode  * *outNewChild);
	STDMETHOD(replaceChild)(IXMLDOMNode  *newChild, IXMLDOMNode  *oldChild, IXMLDOMNode  * *outNewChild);
	STDMETHOD(removeChild)(IXMLDOMNode  *childNode, IXMLDOMNode  * *oldChild);
	STDMETHOD(appendChild)(IXMLDOMNode  *newChild, IXMLDOMNode  * *outNewChild);
	STDMETHOD(hasChildNodes)(VARIANT_BOOL  *hasChild);
	STDMETHOD(get_ownerDocument)(IXMLDOMDocument  * *pVal);
	STDMETHOD(cloneNode)(VARIANT_BOOL deep, IXMLDOMNode  * *cloneRoot);
	STDMETHOD(get_nodeTypeString)(BSTR  *pVal);
	STDMETHOD(get_text)(BSTR  *pVal);
	STDMETHOD(put_text)(BSTR newVal);
	STDMETHOD(get_specified)(VARIANT_BOOL  *pVal);
	STDMETHOD(get_definition)(IXMLDOMNode  * *pVal);
	STDMETHOD(get_nodeTypedValue)(VARIANT  *pVal);
	STDMETHOD(put_nodeTypedValue)(VARIANT newVal);
	STDMETHOD(get_dataType)(VARIANT  *pVal);
	STDMETHOD(put_dataType)(BSTR dataTypeName);
	STDMETHOD(get_xml)(BSTR  *pVal);
	STDMETHOD(transformNode)(IXMLDOMNode  *stylesheet, BSTR  *xmlString);
	STDMETHOD(selectNodes)(BSTR queryString, IXMLDOMNodeList  * *resultList);
	STDMETHOD(selectSingleNode)(BSTR queryString, IXMLDOMNode  * *resultNode);
	STDMETHOD(get_parsed)(VARIANT_BOOL  *pVal);
	STDMETHOD(get_namespaceURI)(BSTR  *pVal);
	STDMETHOD(get_prefix)(BSTR  *pVal);
	STDMETHOD(get_baseName)(BSTR  *pVal);
	STDMETHOD(transformNodeToObject)(IXMLDOMNode  *stylesheet, VARIANT outputObject);

protected:

	IXMLDOMDocument	*m_pIXMLDOMDocument;
	
	void	ReleaseOwnerDoc()
	{
		if (m_pIXMLDOMDocument != NULL) {
			m_pIXMLDOMDocument->Release();
			m_pIXMLDOMDocument = NULL;
		}
	}

};

HRESULT MakeHRESULT(DOMException& ex);

#include "IXMLDOMNodeImpl.inl"

#endif // ___ixmldomnodeimpl_h___
