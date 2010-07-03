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
 * $Id: XMLDOMDocument.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___xmldomdocument_h___
#define ___xmldomdocument_h___

#include <xercesc/dom/DOMDocument.hpp>
#include "IXMLDOMNodeImpl.h"

#include "resource.h"       // main symbols
#include "XMLDOMParseError.h"

#include <xercesc/sax/ErrorHandler.hpp>
#include "xml4comCP.h"

XERCES_CPP_NAMESPACE_USE

class XERCES_CPP_NAMESPACE_QUALIFIER SAXParseException;

class ATL_NO_VTABLE CXMLDOMDocument :
	public CComObjectRootEx<CComSingleThreadModel>,
	public CComCoClass<CXMLDOMDocument, &CLSID_DOMDocument>,
	public IObjectSafetyImpl<CXMLDOMDocument, INTERFACESAFE_FOR_UNTRUSTED_CALLER>,
	public IXMLDOMNodeImpl<IXMLDOMDocument, &IID_IXMLDOMDocument>,
	public IObjectWithSiteImpl<CXMLDOMDocument>,
	public CProxyXMLDOMDocumentEvents< CXMLDOMDocument >,
	public IConnectionPointContainerImpl<CXMLDOMDocument>,
	public IProvideClassInfo2Impl<&CLSID_DOMDocument, &DIID_XMLDOMDocumentEvents, &LIBID_Xerces, XERCES_VERSION_MAJOR, INVK_CAT2_RAW_NUMERIC(XERCES_VERSION_MINOR,XERCES_VERSION_REVISION)>,
	public CWindowImpl<CXMLDOMDocument, CWindow, CWinTraits<0,0> >,
	ErrorHandler
{
public:
	CXMLDOMDocument();

	HRESULT FinalConstruct();
	void	FinalRelease();

	virtual DOMNode*        get_DOMNode()		    { return m_Document;}
    virtual DOMNodeType     get_DOMNodeType() const { return NODE_DOCUMENT; }

	virtual void resetErrors() {};

	//DECLARE_REGISTRY_RESOURCEID(IDR_XMLDOCUMENT)
	static HRESULT WINAPI UpdateRegistry(BOOL bRegister);

DECLARE_NOT_AGGREGATABLE(CXMLDOMDocument)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXMLDOMDocument)
	COM_INTERFACE_ENTRY(IXMLDOMDocument)
	COM_INTERFACE_ENTRY(IXMLDOMNode)
	COM_INTERFACE_ENTRY(IDispatch)
	COM_INTERFACE_ENTRY(IIBMXMLDOMNodeIdentity)
	COM_INTERFACE_ENTRY(IObjectSafety)
	COM_INTERFACE_ENTRY(IObjectWithSite)
	COM_INTERFACE_ENTRY(ISupportErrorInfo)
	COM_INTERFACE_ENTRY_IMPL(IConnectionPointContainer)
	COM_INTERFACE_ENTRY(IProvideClassInfo)
	COM_INTERFACE_ENTRY(IProvideClassInfo2)
	COM_INTERFACE_ENTRY(ISupportErrorInfo)
END_COM_MAP()

BEGIN_CONNECTION_POINT_MAP(CXMLDOMDocument)
	CONNECTION_POINT_ENTRY(DIID_XMLDOMDocumentEvents)
END_CONNECTION_POINT_MAP()

	DECLARE_WND_CLASS(_T("XMLParseMonitor"))

BEGIN_MSG_MAP(CMonitorWnd)
	MESSAGE_HANDLER(MSG_READY_STATE_CHANGE,	OnReadyStateChange)
END_MSG_MAP()

	LRESULT OnReadyStateChange(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	// ISupportsErrorInfo
	STDMETHOD(InterfaceSupportsErrorInfo)(REFIID riid);

	// IXMLDOMDocument methods
	STDMETHOD(get_doctype)(IXMLDOMDocumentType  * *pVal);
	STDMETHOD(get_implementation)(IXMLDOMImplementation  * *pVal);
	STDMETHOD(get_documentElement)(IXMLDOMElement  * *pVal);
	STDMETHOD(putref_documentElement)(IXMLDOMElement  *newVal);
	STDMETHOD(createElement)(BSTR tagName, IXMLDOMElement  * *element);
	STDMETHOD(createDocumentFragment)(IXMLDOMDocumentFragment  * *docFrag);
	STDMETHOD(createTextNode)(BSTR data, IXMLDOMText  * *text);
	STDMETHOD(createComment)(BSTR data, IXMLDOMComment  * *comment);
	STDMETHOD(createCDATASection)(BSTR data, IXMLDOMCDATASection  * *cdata);
	STDMETHOD(createProcessingInstruction)(BSTR target, BSTR data, IXMLDOMProcessingInstruction  * *pVal);
	STDMETHOD(createAttribute)(BSTR name, IXMLDOMAttribute  * *attr);
	STDMETHOD(createEntityReference)(BSTR name, IXMLDOMEntityReference  * *entityRef);
	STDMETHOD(getElementsByTagName)(BSTR tagName, IXMLDOMNodeList  * *resultList);
	STDMETHOD(createNode)(VARIANT type, BSTR name, BSTR namespaceURI, IXMLDOMNode  * *node);
	STDMETHOD(nodeFromID)(BSTR idString, IXMLDOMNode  * *node);
	STDMETHOD(load)(VARIANT xmlSource, VARIANT_BOOL  *isSuccessful);
	STDMETHOD(get_readyState)(long  *pVal);
	STDMETHOD(get_parseError)(IXMLDOMParseError  * *pVal);
	STDMETHOD(get_url)(BSTR  *pVal);
	STDMETHOD(get_async)(VARIANT_BOOL  *pVal);
	STDMETHOD(put_async)(VARIANT_BOOL newVal);
	STDMETHOD(abort)(void);
	STDMETHOD(loadXML)(BSTR bstrXML, VARIANT_BOOL  *success);
	STDMETHOD(save)(VARIANT location);
	STDMETHOD(get_validateOnParse)(VARIANT_BOOL  *pVal);
	STDMETHOD(put_validateOnParse)(VARIANT_BOOL newVal);
	STDMETHOD(get_resolveExternals)(VARIANT_BOOL  *pVal);
	STDMETHOD(put_resolveExternals)(VARIANT_BOOL newVal);
	STDMETHOD(get_preserveWhiteSpace)(VARIANT_BOOL  *pVal);
	STDMETHOD(put_preserveWhiteSpace)(VARIANT_BOOL newVal);
	STDMETHOD(put_onreadystatechange)(VARIANT newVal);
	STDMETHOD(put_ondataavailable)(VARIANT newVal);
	STDMETHOD(put_ontransformnode)(VARIANT newVal);

	// Error handling
	virtual void warning(const SAXParseException& exc);
	virtual void error(const SAXParseException& exc);
    virtual void fatalError(const SAXParseException& exc);

	bool IsAbort() const { return m_bAbort; }

	XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument*		  m_Document;

private:

	bool				  m_bValidate;
	long				  m_lReadyState;
	_bstr_t			      m_url;
	CXMLDOMParseErrorObj *m_pParseError;
	bool			      m_bAsync;	
	bool				  m_bAbort;
	HANDLE				  m_hParseThread;
	LPDISPATCH			  m_pOnReadyStateChange;
	LPDISPATCH			  m_pOnDataAvailable;
	LPDISPATCH			  m_pOnTransformNode;

	// thread data
	_bstr_t				  m_FileName;
	_bstr_t				  m_xml;
	XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument*		  m_TmpDocument;
	bool			      m_bParseError;
	bool				  m_bThreadValidate;
	bool                  m_bPreserveWhiteSpace;
	
	HRESULT GetBaseURL(_bstr_t &baseURL);
	
	static UINT APIENTRY ParseThread(void *pParm);
};

typedef CComObject<CXMLDOMDocument> CXMLDOMDocumentObj;

#endif // ___xmldomdocument_h___