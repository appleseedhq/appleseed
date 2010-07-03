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
 * $Id: IXMLDOMCharacterDataImpl.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___ixmldomcharacterdataimpl_h___
#define ___ixmldomcharacterdataimpl_h___

#include "IXMLDOMNodeImpl.h"
XERCES_CPP_NAMESPACE_USE

template <class T, const IID* piid, class tihclass = CComTypeInfoHolder>
class ATL_NO_VTABLE IXMLDOMCharacterDataImpl: public IXMLDOMNodeImpl<T,piid,tihclass>
{
public:

	virtual DOMCharacterData* get_DOMCharacterData() = 0; 
	virtual DOMNode* get_DOMNode() { return get_DOMCharacterData(); } 

	// IXMLDOMCharacterData 

STDMETHOD(get_data)(BSTR  *pVal)
{
	ATLTRACE(_T("IXMLDOMCharacterDataImpl::get_data\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = NULL;
	
	try
	{
		*pVal = SysAllocString(get_DOMCharacterData()->getData());
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return S_OK;
}

STDMETHOD(put_data)(BSTR newVal)
{
	ATLTRACE(_T("IXMLDOMCharacterDataImpl::put_data\n"));

	try
	{
		get_DOMCharacterData()->setData(newVal);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return S_OK;
}

STDMETHOD(get_length)(long  *pVal)
{
	ATLTRACE(_T("IXMLDOMCharacterDataImpl::get_length\n"));

	if (NULL == pVal)
		return E_POINTER;

	*pVal = 0;

	try
	{
		*pVal = get_DOMCharacterData()->getLength();
	}
	catch(...)
	{
		return E_FAIL;
	}

	return S_OK;
}

STDMETHOD(substringData)(long offset, long count, BSTR  *data)
{
	ATLTRACE(_T("IXMLDOMCharacterDataImpl::substringData\n"));

	if (NULL == data)
		return E_POINTER;

	*data = NULL;

	try
	{
        *data = SysAllocString(get_DOMCharacterData()->substringData(offset, count));
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return S_OK;
}

STDMETHOD(appendData)(BSTR data)
{
	ATLTRACE(_T("IXMLDOMCharacterDataImpl::appendData\n"));

	try
	{
		get_DOMCharacterData()->appendData(data);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return S_OK;
}

STDMETHOD(insertData)(long offset, BSTR data)
{
	ATLTRACE(_T("IXMLDOMCharacterDataImpl::insertData\n"));

	try
	{
		get_DOMCharacterData()->insertData(offset, data);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return S_OK;
}

STDMETHOD(deleteData)(long offset, long count)
{
	ATLTRACE(_T("IXMLDOMCharacterDataImpl::deleteData\n"));

	try
	{
		get_DOMCharacterData()->deleteData(offset, count);
	}
	catch(...)
	{
		return E_FAIL;
	}
	

	return S_OK;
}

STDMETHOD(replaceData)(long offset, long count, BSTR data)
{
	ATLTRACE(_T("IXMLDOMCharacterDataImpl::replaceData\n"));

	try
	{
		get_DOMCharacterData()->replaceData(offset, count, data);
	}
	catch(...)
	{
		return E_FAIL;
	}
	
	return S_OK;
}

};

#endif // ___ixmldomcharacterdataimpl_h___