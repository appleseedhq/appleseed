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
 * $Id: BinURLInputStream.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

#if !defined(BINURLINPUTSTREAM_HPP)
#define BINURLINPUTSTREAM_HPP


#include <xercesc/util/XMLURL.hpp>
#include <xercesc/util/XMLExceptMsgs.hpp>
#include <xercesc/util/BinInputStream.hpp>

//
// Forward reference the libWWW constructs here, so as to avoid including
// any of the libWWW headers in this file. Just being careful in isolating
// the files that explicitly need to include the libWWW headers.
//

struct _HTAnchor;

XERCES_CPP_NAMESPACE_BEGIN

//
// This class implements the BinInputStream interface specified by the XML
// parser.
//

class XMLUTIL_EXPORT BinURLInputStream : public BinInputStream
{
public :
    BinURLInputStream(const XMLURL&  urlSource);
    ~BinURLInputStream();

    unsigned int curPos() const;
    unsigned int readBytes
    (
                XMLByte* const  toFill
        , const unsigned int    maxToRead
    );
    void reset();
    unsigned int bytesAvail() const;


private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    BinURLInputStream(const BinURLInputStream&);
    BinURLInputStream& operator=(const BinURLInputStream&); 

    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fAnchor
    //      This is the handle that LibWWW returns for the remote file that
    //      is being addressed.
    //  fBuffer
    //      This is the array in which the data is stored after reading it
    //      of the network. The maximum size of this array is decided in the
    //      constructor via a file specific #define.
    //  fBufferIndex
    //      Its the index into fBuffer and points to the next unprocessed
    //      character. When the parser asks for more data to be read of the
    //      stream, then fBuffer[fBufferIndex] is the first byte returned,
    //      unless fBufferIndex equals fBufferSize indicating that all
    //      data in the fBuffer has been processed.
    //  fBufferSize
    //      This represents the extent of valid data in the fBuffer array.
    //  fRemoteFileSize
    //      This stores the size in bytes of the remote file addressed by
    //      this URL input stream.
    //  fBytesProcessed
    //      Its a rolling count of the number of bytes processed off this
    //      input stream. Its only reset back to zero, if the stream is
    //      reset. The maximum value this can have is fRemoteFileSize.
    // -----------------------------------------------------------------------

    struct _HTAnchor*   fAnchor;
    XMLByte*            fBuffer;
    unsigned int        fBufferIndex;
    unsigned int        fBufferSize;
    int                 fRemoteFileSize;
    unsigned int        fBytesProcessed;
    MemoryManager*      fMemoryManager;
};

XERCES_CPP_NAMESPACE_END

#endif // BINURLINPUTSTREAM_HPP
