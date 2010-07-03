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
 * $Id: XMLDOMAttribute.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___xmldomattribute_h___
#define ___xmldomattribute_h___

#include <xercesc/dom/DOMAttr.hpp>
#include "IXMLDOMNodeImpl.h"
XERCES_CPP_NAMESPACE_USE

class ATL_NO_VTABLE CXMLDOMAttribute :
	public CComObjectRootEx<CComSingleThreadModel>,
	public IXMLDOMNodeImpl<IXMLDOMAttribute, &IID_IXMLDOMAttribute>
{
public:
	CXMLDOMAttribute()
	{}

	void	FinalRelease()
	{
		ReleaseOwnerDoc();
	}

	virtual DOMNode* get_DOMNode()			 { return attr;}
	virtual DOMNodeType get_DOMNodeType() const  { return NODE_ATTRIBUTE; }

DECLARE_NOT_AGGREGATABLE(CXMLDOMAttribute)
DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXMLDOMAttribute)
	COM_INTERFACE_ENTRY(IXMLDOMAttribute)
	COM_INTERFACE_ENTRY(IXMLDOMNode)
	COM_INTERFACE_ENTRY(IIBMXMLDOMNodeIdentity)
	COM_INTERFACE_ENTRY(ISupportErrorInfo)
	COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()
	
	//  IXMLDOMNode method
	STDMETHOD(get_specified)(VARIANT_BOOL  *pVal);


	// IXMLDOMAttribute methods
	STDMETHOD(get_name)(BSTR  *pVal);
	STDMETHOD(get_value)(VARIANT  *pVal);
	STDMETHOD(put_value)(VARIANT newVal);

	//
	//   override IXMLDOMNodeImpl to always return a string
	//      even when empty
	//
	STDMETHOD(get_nodeValue)(VARIANT* pVal);

	DOMAttr* attr;
};

typedef CComObject<CXMLDOMAttribute> CXMLDOMAttributeObj;

#endif // ___xmldomattribute_h___