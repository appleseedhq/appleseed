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
 * $Id: IXMLDOMTextImpl.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___ixmldomtextimpl_h___
#define ___ixmldomtextimpl_h___

#include "XMLDOMUtil.h"
#include "IXMLDOMCharacterDataImpl.h"

template <class T, const IID* piid, class tihclass = CComTypeInfoHolder>
class ATL_NO_VTABLE IXMLDOMTextImpl: public IXMLDOMCharacterDataImpl<T,piid,tihclass>
{
public:

	virtual DOMText* get_DOMText() = 0; 
	virtual DOMCharacterData* get_DOMCharacterData() { return get_DOMText(); } 

	// IXMLDOMText

STDMETHOD(splitText)(long offset, IXMLDOMText  **rightHandTextNode)
{
	ATLTRACE(_T("IXMLDOMTextImpl::splitText\n"));

	if (NULL == rightHandTextNode)
		return E_POINTER;

	*rightHandTextNode = NULL;

	HRESULT hr = S_OK;

	try
	{
		hr = wrapNode(m_pIXMLDOMDocument,get_DOMText()->splitText(offset),IID_IXMLDOMText, reinterpret_cast<LPVOID *> (rightHandTextNode));
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return hr;
}

};

#endif // ___ixmldomtextimpl_h___