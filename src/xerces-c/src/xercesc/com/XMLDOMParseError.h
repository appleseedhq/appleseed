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
 * $Id: XMLDOMParseError.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___xmldomparseerror_h___
#define ___xmldomparseerror_h___

#include <xercesc/util/XercesDefs.hpp>
XERCES_CPP_NAMESPACE_USE

class ATL_NO_VTABLE CXMLDOMParseError : 
	public CComObjectRootEx<CComSingleThreadModel>,
	public IDispatchImpl<IXMLDOMParseError, &IID_IXMLDOMParseError, &LIBID_Xerces, XERCES_VERSION_MAJOR, INVK_CAT2_RAW_NUMERIC(XERCES_VERSION_MINOR,XERCES_VERSION_REVISION)>
{
public:
	CXMLDOMParseError()
		:m_Code		 (0)
		,m_url		 (_T(""))
		,m_Reason	 (_T(""))
		,m_Source	 (_T(""))
		,m_LineNumber(0)
		,m_LinePos	 (0)
		,m_FilePos	 (0)
	{}

	HRESULT FinalConstruct();
	void	FinalRelease();	

DECLARE_NOT_AGGREGATABLE(CXMLDOMParseError)

DECLARE_PROTECT_FINAL_CONSTRUCT()

BEGIN_COM_MAP(CXMLDOMParseError)
	COM_INTERFACE_ENTRY(IXMLDOMParseError)
	COM_INTERFACE_ENTRY(IDispatch)
END_COM_MAP()

	// IXMLDOMParseError methods
	STDMETHOD(get_errorCode)(/* [out][retval] */ long  *errorCode);
    STDMETHOD(get_url)(/* [out][retval] */ BSTR  *urlString);
    STDMETHOD(get_reason)(/* [out][retval] */ BSTR  *reasonString);
    STDMETHOD(get_srcText)(/* [out][retval] */ BSTR  *sourceString);
    STDMETHOD(get_line)(/* [out][retval] */ long  *lineNumber);
    STDMETHOD(get_linepos)(/* [out][retval] */ long  *linePosition);
    STDMETHOD(get_filepos)(/* [out][retval] */ long  *filePosition);

	void SetData(long code,
				 const _bstr_t &url,
				 const _bstr_t &reason,
				 const _bstr_t &source,
				 long  lineNumber,
				 long  linePos,
				 long  filePos);
	void Reset();

private:

	long	m_Code;
	_bstr_t m_url;
	_bstr_t m_Reason;
	_bstr_t m_Source;
	long	m_LineNumber;
	long	m_LinePos;
	long	m_FilePos;

	CComCriticalSection	m_CS;
};

typedef CComObject<CXMLDOMParseError> CXMLDOMParseErrorObj;

#endif // ___xmldomparseerror_h___