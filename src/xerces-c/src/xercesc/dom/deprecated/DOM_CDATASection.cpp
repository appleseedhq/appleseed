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
 * $Id: DOM_CDATASection.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include "DOM_CDATASection.hpp"
#include "DOM_Text.hpp"
#include "CDATASectionImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


DOM_CDATASection::DOM_CDATASection()
: DOM_Text(null)
{
};


DOM_CDATASection::DOM_CDATASection(const DOM_CDATASection & other)
: DOM_Text(other)
{
};


DOM_CDATASection::DOM_CDATASection(CDATASectionImpl *impl) :
        DOM_Text(impl)
{
};


DOM_CDATASection::~DOM_CDATASection()
{
};


DOM_CDATASection & DOM_CDATASection::operator = (const DOM_CDATASection & other)
{
     return (DOM_CDATASection &) DOM_Text::operator = (other);
};


DOM_CDATASection & DOM_CDATASection::operator = (const DOM_NullPtr *other)
{
     return (DOM_CDATASection &) DOM_Node::operator = (other);
};


XERCES_CPP_NAMESPACE_END

