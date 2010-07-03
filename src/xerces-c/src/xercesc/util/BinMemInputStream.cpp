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
 * $Id: BinMemInputStream.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/BinMemInputStream.hpp>
#include <xercesc/framework/MemoryManager.hpp>
#include <string.h>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  BinMemInputStream: Constructors and Destructor
// ---------------------------------------------------------------------------
BinMemInputStream::BinMemInputStream( const XMLByte* const       initData
                                    , const unsigned int         capacity
                                    , const BufOpts              bufOpt
                                    ,       MemoryManager* const manager) :
    fBuffer(0)
    , fBufOpt(bufOpt)
    , fCapacity(capacity)
    , fCurIndex(0)
    , fMemoryManager(manager)
{
    // According to the buffer option, do the right thing
    if (fBufOpt == BufOpt_Copy)
    {
        XMLByte* tmpBuf = (XMLByte*) fMemoryManager->allocate
        (
            fCapacity * sizeof(XMLByte)
        );//new XMLByte[fCapacity];
        memcpy(tmpBuf, initData, capacity);
        fBuffer = tmpBuf;
    }
     else
    {
        fBuffer = initData;
    }
}

BinMemInputStream::~BinMemInputStream()
{
    //
    //  If we adopted or copied the buffer, then clean it up. We have to
    //  cast off the const'ness in that case in order to delete it.
    //
    if ((fBufOpt == BufOpt_Adopt) || (fBufOpt == BufOpt_Copy))
        fMemoryManager->deallocate((XMLByte*)fBuffer);//delete [] (XMLByte*)fBuffer;
}


// ---------------------------------------------------------------------------
//  MemBinInputStream: Implementation of the input stream interface
// ---------------------------------------------------------------------------
unsigned int BinMemInputStream::readBytes(          XMLByte* const  toFill
                                            , const unsigned int    maxToRead)
{
    //
    //  Figure out how much we can really read. Its the smaller of the
    //  amount available and the amount asked for.
    //
    const unsigned int available = (fCapacity - fCurIndex);
    if (!available)
        return 0;

    const unsigned int actualToRead = available < maxToRead ?
                                      available : maxToRead;

    memcpy(toFill, &fBuffer[fCurIndex], actualToRead);
    fCurIndex += actualToRead;
    return actualToRead;
}

XERCES_CPP_NAMESPACE_END
