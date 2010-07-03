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
 * $Id: AttrMapImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "AttrMapImpl.hpp"
#include "NamedNodeMapImpl.hpp"
#include "NodeImpl.hpp"
#include "ElementImpl.hpp"
#include "DocumentImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


AttrMapImpl::AttrMapImpl(NodeImpl *ownerNod)
	: NamedNodeMapImpl(ownerNod)
{
	hasDefaults(false);
}

AttrMapImpl::AttrMapImpl(NodeImpl *ownerNod, NamedNodeMapImpl *defaults)
	: NamedNodeMapImpl(ownerNod)
{
	hasDefaults(false);
	if (defaults != null)
	{
		if (defaults->getLength() > 0)
		{
			hasDefaults(true);
			cloneContent(defaults);
		}
	}
}

AttrMapImpl::~AttrMapImpl()
{
}

AttrMapImpl *AttrMapImpl::cloneAttrMap(NodeImpl *ownerNode_p)
{
	AttrMapImpl *newmap = new (ownerNode_p->getDocument()->getMemoryManager()) AttrMapImpl(ownerNode_p);
	newmap->cloneContent(this);
	newmap->attrDefaults = this->attrDefaults;
	return newmap;
}

NodeImpl *AttrMapImpl::removeNamedItem(const DOMString &name)
{
	NodeImpl* removed = NamedNodeMapImpl::removeNamedItem(name);

	// Replace it if it had a default value
	// (DOM spec level 1 - Element Interface)
	if (hasDefaults() && (removed != null))
	{
		AttrMapImpl* defAttrs = ((ElementImpl*)ownerNode)->getDefaultAttributes();
		AttrImpl* attr = (AttrImpl*)(defAttrs->getNamedItem(name));
		if (attr != null)
		{
			AttrImpl* newAttr = (AttrImpl*)attr->cloneNode(true);
			setNamedItem(newAttr);
		}
	}

	return removed;
}

NodeImpl *AttrMapImpl::removeNamedItemNS(const DOMString &namespaceURI, const DOMString &localName)
{
	NodeImpl* removed = NamedNodeMapImpl::removeNamedItemNS(namespaceURI, localName);

	// Replace it if it had a default value
	// (DOM spec level 2 - Element Interface)
	if (hasDefaults() && (removed != null))
	{
		AttrMapImpl* defAttrs = ((ElementImpl*)ownerNode)->getDefaultAttributes();
		AttrImpl* attr = (AttrImpl*)(defAttrs->getNamedItemNS(namespaceURI, localName));
		if (attr != null)
		{
			AttrImpl* newAttr = (AttrImpl*)attr->cloneNode(true);
			setNamedItem(newAttr);
		}
	}

	return removed;
}

XERCES_CPP_NAMESPACE_END

