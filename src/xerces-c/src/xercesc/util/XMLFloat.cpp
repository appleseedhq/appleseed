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
 * $Id: XMLFloat.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XMLFloat.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/NumberFormatException.hpp>
#include <xercesc/util/Janitor.hpp>

#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <float.h>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  ctor/dtor
// ---------------------------------------------------------------------------
XMLFloat::XMLFloat(const XMLCh* const strValue,
                   MemoryManager* const manager)
:XMLAbstractDoubleFloat(manager)
{
    init(strValue);
}

XMLFloat::~XMLFloat()
{
}

void XMLFloat::checkBoundary(char* const strValue)
{
    convert(strValue);

    if (fDataConverted == false)
    {
        /**
         *  float related checking
         */
        if (fValue < (-1) * FLT_MAX)
        {
            fType = NegINF;
            fDataConverted = true;
            fDataOverflowed = true;
        }
        else if (fValue > (-1)*FLT_MIN && fValue < 0)
        {
            fDataConverted = true;
            fValue = 0;
        }
        else if (fValue > 0 && fValue < FLT_MIN )
        {
            fDataConverted = true;
            fValue = 0;
        }
        else if  (fValue > FLT_MAX)
        {
            fType = PosINF;
            fDataConverted = true;
            fDataOverflowed = true;
        }
    }
}

/***
 * Support for Serialization/De-serialization
 ***/

IMPL_XSERIALIZABLE_TOCREATE(XMLFloat)

XMLFloat::XMLFloat(MemoryManager* const manager)
:XMLAbstractDoubleFloat(manager)
{
}

void XMLFloat::serialize(XSerializeEngine& serEng)
{
    XMLAbstractDoubleFloat::serialize(serEng);
}

XERCES_CPP_NAMESPACE_END
