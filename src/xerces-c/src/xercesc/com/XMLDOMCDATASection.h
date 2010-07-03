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
 * $Id: XMLDOMCDATASection.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___xmldomcdatasection_h___
#define ___xmldomcdatasection_h___

#include <xercesc/dom/DOMCDATASection.hpp>
#include "IXMLDOMTextImpl.h"
XERCES_CPP_NAMESPACE_USE

class ATL_NO_VTABLE CXMLDOMCDATASection :
	public CComObjectRootEx<CComSingleThreadModel>,
	public IXMLDOMTextImpl<IXMLDOMCDATASection, &IID_IXMLDOMCDATASection>
{
public:
	CXMLDOMCDATASection()
	{}

	void	FinalRelease()
	{
		ReleaseOwnerDoc();
	}

	virtual DOMText* get_DOMText()			 { return cdataSection;}
	virtual DOMNodeType get_DOMNodeType() const  { return NODE_CDATA_SECTION; }

DECLARE_NOT_AGGREGATABLE(CXMLDOMCDATASection)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXMLDOMCDATASection)
	COM_INTERFACE_ENTRY(IXMLDOMCDATASection)
	COM_INTERFACE_ENTRY(IXMLDOMText)
	COM_INTERFACE_ENTRY(IXMLDOMCharacterData)
	COM_INTERFACE_ENTRY(IXMLDOMNode)
	COM_INTERFACE_ENTRY(IIBMXMLDOMNodeIdentity)
	COM_INTERFACE_ENTRY(ISupportErrorInfo)
	COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

	DOMCDATASection* cdataSection;
};

typedef CComObject<CXMLDOMCDATASection> CXMLDOMCDATASectionObj;

#endif // ___xmldomcdatasection_h___