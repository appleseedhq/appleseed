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
 * $Id: XMLDOMProcessingInstruction.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___xmldomprocessinginstruction_h___
#define ___xmldomprocessinginstruction_h___

#include <xercesc/dom/DOMProcessingInstruction.hpp>
#include "IXMLDOMNodeImpl.h"

XERCES_CPP_NAMESPACE_USE

class ATL_NO_VTABLE CXMLDOMProcessingInstruction :
	public CComObjectRootEx<CComSingleThreadModel>,
	public IXMLDOMNodeImpl<IXMLDOMProcessingInstruction, &IID_IXMLDOMProcessingInstruction>
{
public:
	CXMLDOMProcessingInstruction()
	{}
	
	void	FinalRelease()
	{
		ReleaseOwnerDoc();
	}

	virtual DOMNode* get_DOMNode()			 { return processingInstruction;}
	virtual DOMNodeType get_DOMNodeType() const  { return NODE_PROCESSING_INSTRUCTION; }

DECLARE_NOT_AGGREGATABLE(CXMLDOMProcessingInstruction)
DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXMLDOMProcessingInstruction)
	COM_INTERFACE_ENTRY(IXMLDOMProcessingInstruction)
	COM_INTERFACE_ENTRY(IXMLDOMNode)
	COM_INTERFACE_ENTRY(IIBMXMLDOMNodeIdentity)
	COM_INTERFACE_ENTRY(IDispatch)
	COM_INTERFACE_ENTRY(ISupportErrorInfo)
END_COM_MAP()

	// IXMLDOMProcessingInstruction methods
	STDMETHOD(get_target)(BSTR  *pVal);
	STDMETHOD(get_data)(BSTR  *pVal);
	STDMETHOD(put_data)(BSTR newVal);

	DOMProcessingInstruction* processingInstruction;
};

typedef CComObject<CXMLDOMProcessingInstruction> CXMLDOMProcessingInstructionObj;

#endif // ___xmldomprocessinginstruction_h___