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
 * $Id: XMLDOMUtil.h 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef ___wrapnode_h___
#define ___wrapnode_h___

#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/dom/DOMNode.hpp>
XERCES_CPP_NAMESPACE_USE

extern const OLECHAR* g_DomNodeName[];
extern const int	g_DomNodeNameSize;

void GetXML (const DOMNode* node, _bstr_t &text);
HRESULT wrapNode(IXMLDOMDocument *pDoc, DOMNode* node, REFIID iid, LPVOID *pVal);



#endif // ___wrapnode_h___