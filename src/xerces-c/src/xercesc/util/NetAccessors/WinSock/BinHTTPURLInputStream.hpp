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
 * $Id: BinHTTPURLInputStream.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

#if !defined(BINHTTPURLINPUTSTREAM_HPP)
#define BINHTTPURLINPUTSTREAM_HPP


#include <xercesc/util/XMLURL.hpp>
#include <xercesc/util/XMLExceptMsgs.hpp>
#include <xercesc/util/BinInputStream.hpp>
#include <xercesc/util/Mutexes.hpp>
#include <xercesc/util/XMLNetAccessor.hpp>

//
// This class implements the BinInputStream interface specified by the XML
// parser.
//
struct hostent;
struct sockaddr;

XERCES_CPP_NAMESPACE_BEGIN

class XMLUTIL_EXPORT BinHTTPURLInputStream : public BinInputStream
{
public :
    BinHTTPURLInputStream(const XMLURL&  urlSource, const XMLNetHTTPInfo* httpInfo=0);
    ~BinHTTPURLInputStream();

    unsigned int curPos() const;
    unsigned int readBytes
    (
                XMLByte* const  toFill
        , const unsigned int    maxToRead
    );

	static void Cleanup();


private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    BinHTTPURLInputStream(const BinHTTPURLInputStream&);
    BinHTTPURLInputStream& operator=(const BinHTTPURLInputStream&); 
    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fSocketHandle
    //      The socket representing the connection to the remote file.
    //      We deliberately did not define the type to be SOCKET, so as to
    //      avoid bringing in any Windows header into this file.
    //  fBytesProcessed
    //      Its a rolling count of the number of bytes processed off this
    //      input stream.
    //  fBuffer
    //      Holds the http header, plus the first part of the actual
    //      data.  Filled at the time the stream is opened, data goes
    //      out to user in response to readBytes().
    //  fBufferPos, fBufferEnd
    //      Pointers into fBuffer, showing start and end+1 of content
    //      that readBytes must return.
    // -----------------------------------------------------------------------
    MemoryManager*      fMemoryManager;
    unsigned int        fSocketHandle;
    unsigned int        fBytesProcessed;
    char                fBuffer[4000];
    char *              fBufferEnd;
    char *              fBufferPos;
    static bool         fInitialized;
    static XMLMutex*        fInitMutex;

	static void Initialize(MemoryManager* const manager  = XMLPlatformUtils::fgMemoryManager);

	inline static hostent* gethostbyname(const char* name);
	inline static unsigned long inet_addr(const char* cp);
	inline static hostent* gethostbyaddr(const char* addr,int len,int type);
	inline static unsigned short htons(unsigned short hostshort);
	inline static unsigned int socket(int af,int type,int protocol);
	inline static int connect(unsigned int s,const sockaddr* name,int namelen);
	inline static int send(unsigned int s,const char* buf,int len,int flags);
	inline static int recv(unsigned int s,char* buf,int len,int flags);
	inline static int shutdown(unsigned int s,int how);
	inline static int closesocket(unsigned int socket);

    friend class SocketJanitor;
};


inline unsigned int BinHTTPURLInputStream::curPos() const
{
    return fBytesProcessed;
}

XERCES_CPP_NAMESPACE_END

#endif // BINHTTPURLINPUTSTREAM_HPP
