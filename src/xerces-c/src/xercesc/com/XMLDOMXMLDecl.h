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


#ifndef ___xmldomxmldecl_h___
#define ___xmldomxmldecl_h___

#include <xercesc/dom/DOMProcessingInstruction.hpp>
#include "IXMLDOMNodeImpl.h"

XERCES_CPP_NAMESPACE_USE

class ATL_NO_VTABLE CXMLDOMXMLDecl : 
	public CComObjectRootEx<CComSingleThreadModel>,
	public IXMLDOMNodeImpl<IXMLDOMProcessingInstruction, &IID_IXMLDOMProcessingInstruction>
{
public:
	CXMLDOMXMLDecl()
	{}
	
	void	FinalRelease()
	{
		ReleaseOwnerDoc();
	}

	virtual DOMNode* get_DOMNode()			 { return xmlDecl;} 
	virtual DOMNodeType get_DOMNodeType() const  { return NODE_PROCESSING_INSTRUCTION; }

DECLARE_NOT_AGGREGATABLE(CXMLDOMXMLDecl)
DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXMLDOMXMLDecl)
	COM_INTERFACE_ENTRY(IXMLDOMProcessingInstruction)
	COM_INTERFACE_ENTRY(IXMLDOMNode)
	COM_INTERFACE_ENTRY(IIBMXMLDOMNodeIdentity)
	COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

	// IXMLDOMProcessingInstruction methods
	STDMETHOD(get_target)(BSTR  *pVal);
	STDMETHOD(get_data)(BSTR  *pVal);
	STDMETHOD(put_data)(BSTR newVal);

	DOMProcessingInstruction* xmlDecl;
};

typedef CComObject<CXMLDOMXMLDecl> CXMLDOMXMLDeclObj;

#endif // ___xmldomprocessinginstruction_h___