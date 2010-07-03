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
 * $Id: NodeContainerImpl.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___nodecontainerimpl_h___
#define ___nodecontainerimpl_h___

template <class T>
class NodeContainerImpl
{
public:

	class iterator 
	{
	public:
		iterator()
			:m_container(NULL)
			,m_NextNodeIndex(0)
			,m_pIXMLDOMDocument(NULL)
		{
			VariantInit(&m_NextVar);
			V_VT(&m_NextVar) = VT_NULL;
		}	

		iterator(const T* container,int idx,IXMLDOMDocument *p)
			:m_container(container)
			,m_NextNodeIndex(idx)
			,m_pIXMLDOMDocument(p)
		{
			VariantInit(&m_NextVar);
			V_VT(&m_NextVar) = VT_NULL;
			if (m_pIXMLDOMDocument != NULL)
				m_pIXMLDOMDocument->AddRef();
		}

		~iterator()
		{
			VariantClear(&m_NextVar);
			if (m_pIXMLDOMDocument != NULL)
				m_pIXMLDOMDocument->Release();
		}
		
		bool operator !=(const iterator& rhs)
		{
			return (m_NextNodeIndex != rhs.m_NextNodeIndex);
		}
		
		iterator& operator=(const iterator& rhs)
		{
			if (this != &rhs) {
				if (m_pIXMLDOMDocument != NULL) {
					m_pIXMLDOMDocument->Release() ;
					m_pIXMLDOMDocument = NULL ;
				}
				m_container = rhs.m_container ;
				m_NextNodeIndex = rhs.m_NextNodeIndex ;
				m_NextVar = rhs.m_NextVar ;
				m_pIXMLDOMDocument = rhs.m_pIXMLDOMDocument ;
				if (m_pIXMLDOMDocument != NULL) {
					m_pIXMLDOMDocument->AddRef() ;
				}
			}
			return *this ;
		}


		VARIANT& operator*()
		{
			if (m_container == 0)
				return m_NextVar;

			int length = m_container->getLength(); 
			if (m_NextNodeIndex >= length)
				return m_NextVar;
			
			CComPtr<IXMLDOMNode> pNode;
			HRESULT hr = wrapNode(m_pIXMLDOMDocument,m_container->item(m_NextNodeIndex),IID_IXMLDOMNode, reinterpret_cast<LPVOID *> (&pNode));
			if (S_OK == hr) {
				CComQIPtr<IDispatch,&IID_IDispatch> pDisp(pNode);
				if (pNode) {
					VariantClear(&m_NextVar);
					V_VT(&m_NextVar)	   = VT_DISPATCH;
					V_DISPATCH(&m_NextVar) = pDisp.Detach();
				}
			}

			return m_NextVar;
		}
		
		iterator operator++(int)
		{
			return iterator(m_container,m_NextNodeIndex++,m_pIXMLDOMDocument);		
		}
		
	private:

		const T*		 m_container;	
		int			     m_NextNodeIndex;
		IXMLDOMDocument	*m_pIXMLDOMDocument;
		VARIANT			 m_NextVar;
	};

	typedef iterator const_iterator;

	NodeContainerImpl()
		:m_NextNodeIndex(0)
		,m_pIXMLDOMDocument(NULL)
	{}

	iterator begin()
	{
		return iterator(m_container,0,m_pIXMLDOMDocument);
	}
	
	iterator end()
	{
		if (m_container == 0)
			return iterator(m_container,0,m_pIXMLDOMDocument);
		else	
			return iterator(m_container,m_container->getLength(),m_pIXMLDOMDocument);
	}
	
	void	SetOwnerDoc(IXMLDOMDocument	*p)
	{
		m_pIXMLDOMDocument = p;
		if (m_pIXMLDOMDocument != NULL)
			m_pIXMLDOMDocument->AddRef();
	}

	T*				 m_container;

protected:

	int				 m_NextNodeIndex;
	IXMLDOMDocument	*m_pIXMLDOMDocument;

	void	ReleaseOwnerDoc()
	{
		if (m_pIXMLDOMDocument != NULL) {
			m_pIXMLDOMDocument->Release();
			m_pIXMLDOMDocument = NULL;
		}
	}
};

#endif // ___nodecontainerimpl_h___
