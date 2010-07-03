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
 * $Id: XMLDOMEntity.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___xmldomentity_h___
#define ___xmldomentity_h___

#include <xercesc/dom/DOMEntity.hpp>
#include "IXMLDOMNodeImpl.h"

XERCES_CPP_NAMESPACE_USE

class ATL_NO_VTABLE CXMLDOMEntity : 
	public CComObjectRootEx<CComSingleThreadModel>,
	public IXMLDOMNodeImpl<IXMLDOMEntity, &IID_IXMLDOMEntity>
{
public:
	CXMLDOMEntity()
	{}

	void	FinalRelease()
	{
		ReleaseOwnerDoc();
	}

	virtual DOMNode* get_DOMNode()			 { return entity;} 
	virtual DOMNodeType get_DOMNodeType() const  { return NODE_ENTITY; }

DECLARE_NOT_AGGREGATABLE(CXMLDOMEntity)
DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXMLDOMEntity)
	COM_INTERFACE_ENTRY(IXMLDOMEntity)
	COM_INTERFACE_ENTRY(IXMLDOMNode)
	COM_INTERFACE_ENTRY(IIBMXMLDOMNodeIdentity)
	COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

	// IXMLDOMEntity methods
	STDMETHOD(get_publicId)(VARIANT  *pVal);
	STDMETHOD(get_systemId)(VARIANT  *pVal);
	STDMETHOD(get_notationName)(BSTR  *pVal);

	DOMEntity* entity;
};

typedef CComObject<CXMLDOMEntity> CXMLDOMEntityObj;

#endif // ___xmldomentity_h___