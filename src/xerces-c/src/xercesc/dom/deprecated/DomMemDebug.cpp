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
 * $Id: DomMemDebug.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


#include "DomMemDebug.hpp"
#include "DOMString.hpp"
#include "NodeImpl.hpp"
#include "NamedNodeMapImpl.hpp"
#include <stdio.h>

XERCES_CPP_NAMESPACE_BEGIN


DomMemDebug::DomMemDebug()
{
    liveStringHandles   = DOMString::gLiveStringHandleCount;
    totalStringHandles  = DOMString::gTotalStringHandleCount;
    liveStringBuffers   = DOMString::gLiveStringDataCount;
    totalStringBuffers  = DOMString::gTotalStringDataCount;
    liveNodeImpls       = NodeImpl::gLiveNodeImpls;
    totalNodeImpls      = NodeImpl::gTotalNodeImpls;
    liveNamedNodeMaps   = NamedNodeMapImpl::gLiveNamedNodeMaps;
    totalNamedNodeMaps  = NamedNodeMapImpl::gTotalNamedNodeMaps;
};


DomMemDebug::~DomMemDebug()
{
};


bool DomMemDebug::operator == (const DomMemDebug &other)
{
    bool    r =
        liveStringHandles   ==  other.liveStringHandles  &&
        liveStringBuffers   ==  other.liveStringBuffers  &&
        liveNodeImpls       ==  other.liveNodeImpls      &&
        liveNamedNodeMaps   ==  other.liveNamedNodeMaps;
    return r;
};


bool DomMemDebug::operator != (const DomMemDebug &other)
{
    return ! operator == (other);
};


void DomMemDebug::operator = (const DomMemDebug &other)
{
    liveStringHandles  = other.liveStringHandles;
    totalStringHandles = other.totalStringHandles;
    liveStringBuffers  = other.liveStringBuffers;
    totalStringBuffers = other.totalStringBuffers;
    liveNodeImpls      = other.liveNodeImpls;
    totalNodeImpls     = other.totalNodeImpls;
    liveNamedNodeMaps  = other.liveNamedNodeMaps;
    totalNamedNodeMaps = other.totalNamedNodeMaps;
};

void DomMemDebug::print()
{
    printf("DOM reference counted memory alloction statistics:\n"
        "    live  string handles:   %d\n"
        "    total string handles:   %d\n"
        "    live  string buffers:   %d\n"
        "    total string buffers:   %d\n"
        "    live  nodeImpls:        %d\n"
        "    total nodeImpls:        %d\n"
        "    live  NamedNodeMaps:    %d\n"
        "    total NamedNodeMaps:    %d\n",
            liveStringHandles ,
            totalStringHandles,
            liveStringBuffers  ,
            totalStringBuffers ,
            liveNodeImpls      ,
            totalNodeImpls     ,
            liveNamedNodeMaps  ,
            totalNamedNodeMaps);
};


void DomMemDebug::printDifference(const DomMemDebug &other)
{
    int d;

    d = liveStringHandles - other.liveStringHandles;
    if (d != 0)
        printf("   %d StringHandles.", d);

    d = liveStringBuffers - other.liveStringBuffers;
    if (d != 0)
        printf("   %d StringBuffers.", d);

    d = liveNodeImpls - other.liveNodeImpls;
    if (d != 0)
        printf("   %d NodeImpls.", d);

    d = liveNamedNodeMaps - other.liveNamedNodeMaps;
    if (d != 0)
        printf("   %d NamedNodeMaps.", d);

    printf("\n");
};

XERCES_CPP_NAMESPACE_END

