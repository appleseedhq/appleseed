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
 * $Id: BinHTTPURLInputStream.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


#define INCL_WINSOCK_API_TYPEDEFS 1
#include <winsock2.h>
#include <windows.h>
#include <tchar.h>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLNetAccessor.hpp>
#include <xercesc/util/NetAccessors/WinSock/BinHTTPURLInputStream.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLExceptMsgs.hpp>
#include <xercesc/util/Janitor.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/Base64.hpp>

XERCES_CPP_NAMESPACE_BEGIN

class SocketJanitor
{
public:
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    SocketJanitor(SOCKET* toDelete) : fData(toDelete) {}
    ~SocketJanitor() { reset(); }

    SOCKET* get() const { return fData; }
    SOCKET* release() {	SOCKET* p = fData; fData = 0; return p; }

    void reset(SOCKET* p = 0) { if(fData) BinHTTPURLInputStream::closesocket(*fData); fData=p; }
    bool isDataNull() { return (fData == 0); }

private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    SocketJanitor();
    SocketJanitor(const SocketJanitor&);
    SocketJanitor& operator=(const SocketJanitor&);

    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fData
    //      This is the pointer to the socket that must be closed when 
    //      this object is destroyed.
    // -----------------------------------------------------------------------
    SOCKET*  fData;
};

static HMODULE gWinsockLib = NULL;
static LPFN_GETHOSTBYNAME gWSgethostbyname = NULL;
static LPFN_INET_ADDR gWSinet_addr = NULL;
static LPFN_GETHOSTBYADDR gWSgethostbyaddr = NULL;
static LPFN_HTONS gWShtons = NULL;
static LPFN_SOCKET gWSsocket = NULL;
static LPFN_CONNECT gWSconnect = NULL;
static LPFN_SEND gWSsend = NULL;
static LPFN_RECV gWSrecv = NULL;
static LPFN_SHUTDOWN gWSshutdown = NULL;
static LPFN_CLOSESOCKET gWSclosesocket = NULL;
static LPFN_WSACLEANUP gWSACleanup = NULL;

bool BinHTTPURLInputStream::fInitialized = false;
XMLMutex* BinHTTPURLInputStream::fInitMutex = 0;

void BinHTTPURLInputStream::Initialize(MemoryManager* const manager) {
    //
    // Initialize the WinSock library here.
    //
    WORD        wVersionRequested;
    WSADATA     wsaData;

	LPFN_WSASTARTUP startup = NULL;
	if(gWinsockLib == NULL) {
		gWinsockLib = LoadLibrary(_T("WSOCK32"));
		if(gWinsockLib == NULL) {
			ThrowXMLwithMemMgr(NetAccessorException, XMLExcepts::NetAcc_InitFailed, manager);
		}
		else {
			startup = (LPFN_WSASTARTUP) GetProcAddress(gWinsockLib,"WSAStartup");
			gWSACleanup = (LPFN_WSACLEANUP) GetProcAddress(gWinsockLib,"WSACleanup");
			gWSgethostbyname = (LPFN_GETHOSTBYNAME) GetProcAddress(gWinsockLib,"gethostbyname");
			gWSinet_addr = (LPFN_INET_ADDR) GetProcAddress(gWinsockLib,"inet_addr");
			gWSgethostbyaddr = (LPFN_GETHOSTBYADDR) GetProcAddress(gWinsockLib,"gethostbyaddr");
			gWShtons = (LPFN_HTONS) GetProcAddress(gWinsockLib,"htons");
			gWSsocket = (LPFN_SOCKET) GetProcAddress(gWinsockLib,"socket");
			gWSconnect = (LPFN_CONNECT) GetProcAddress(gWinsockLib,"connect");
			gWSsend = (LPFN_SEND) GetProcAddress(gWinsockLib,"send");
			gWSrecv = (LPFN_RECV) GetProcAddress(gWinsockLib,"recv");
			gWSshutdown = (LPFN_SHUTDOWN) GetProcAddress(gWinsockLib,"shutdown");
			gWSclosesocket = (LPFN_CLOSESOCKET) GetProcAddress(gWinsockLib,"closesocket");

			if(startup == NULL ||
				gWSACleanup == NULL ||
				gWSgethostbyname == NULL ||
				gWSinet_addr == NULL ||
				gWSgethostbyaddr == NULL ||
				gWShtons == NULL ||
				gWSsocket == NULL ||
				gWSconnect == NULL ||
				gWSsend == NULL ||
				gWSrecv == NULL ||
				gWSshutdown == NULL ||
				gWSclosesocket == NULL)
			{
				gWSACleanup = NULL;
				Cleanup();
				ThrowXMLwithMemMgr(NetAccessorException, XMLExcepts::NetAcc_InitFailed, manager);
			}
		}
	}
    wVersionRequested = MAKEWORD( 2, 2 );
    int err = (*startup)(wVersionRequested, &wsaData);
    if (err != 0)
    {
        // Call WSAGetLastError() to get the last error.
        ThrowXMLwithMemMgr(NetAccessorException, XMLExcepts::NetAcc_InitFailed, manager);
    }
    fInitialized = true;
}

void BinHTTPURLInputStream::Cleanup() {
	if(fInitialized)
	{
		if(gWSACleanup) (*gWSACleanup)();
		gWSACleanup = NULL;
		FreeLibrary(gWinsockLib);
		gWinsockLib = NULL;
		gWSgethostbyname = NULL;
		gWSinet_addr = NULL;
		gWSgethostbyaddr = NULL;
		gWShtons = NULL;
		gWSsocket = NULL;
		gWSconnect = NULL;
		gWSsend = NULL;
		gWSrecv = NULL;
		gWSshutdown = NULL;
		gWSclosesocket = NULL;

        fInitialized = false;
        delete fInitMutex;
        fInitMutex = 0;
	}
}


hostent* BinHTTPURLInputStream::gethostbyname(const char* name)
{
	return (*gWSgethostbyname)(name);
}

unsigned long BinHTTPURLInputStream::inet_addr(const char* cp)
{
	return (*gWSinet_addr)(cp);
}

hostent* BinHTTPURLInputStream::gethostbyaddr(const char* addr,int len,int type)
{
	return (*gWSgethostbyaddr)(addr,len,type);
}

unsigned short BinHTTPURLInputStream::htons(unsigned short hostshort)
{
	return (*gWShtons)(hostshort);
}

unsigned int BinHTTPURLInputStream::socket(int af,int type,int protocol)
{
	return (*gWSsocket)(af,type,protocol);
}

int BinHTTPURLInputStream::connect(unsigned int s,const sockaddr* name,int namelen)
{
	return (*gWSconnect)(s,name,namelen);
}

int BinHTTPURLInputStream::send(unsigned int s,const char* buf,int len,int flags)
{
	return (*gWSsend)(s,buf,len,flags);
}

int BinHTTPURLInputStream::recv(unsigned int s,char* buf,int len,int flags)
{
	return (*gWSrecv)(s,buf,len,flags);
}

int BinHTTPURLInputStream::shutdown(unsigned int s,int how)
{
	return (*gWSshutdown)(s,how);
}

int BinHTTPURLInputStream::closesocket(unsigned int socket)
{
	return (*gWSclosesocket)(socket);
}


BinHTTPURLInputStream::BinHTTPURLInputStream(const XMLURL& urlSource, const XMLNetHTTPInfo* httpInfo /*=0*/)
      : fSocketHandle(0)
      , fBytesProcessed(0)
{
    if(!fInitialized)
    {
        if (!fInitMutex)
        {
            XMLMutex* tmpMutex = new XMLMutex(XMLPlatformUtils::fgMemoryManager);
            if (XMLPlatformUtils::compareAndSwap((void**)&fInitMutex, tmpMutex, 0))
            {
                // Someone beat us to it, so let's clean up ours
                delete tmpMutex;
            }
         }
         XMLMutexLock lock(fInitMutex);
         if (!fInitialized)
         {
             Initialize(urlSource.getMemoryManager());
         }
    }

    fMemoryManager = urlSource.getMemoryManager();
    //
    // Pull all of the parts of the URL out of th urlSource object, and transcode them
    //   and transcode them back to ASCII.
    //
    const XMLCh*        hostName = urlSource.getHost();
    char*               hostNameAsCharStar = XMLString::transcode(hostName, urlSource.getMemoryManager());
    ArrayJanitor<char>  janBuf1(hostNameAsCharStar, urlSource.getMemoryManager());

    const XMLCh*        path = urlSource.getPath();
    char*               pathAsCharStar = XMLString::transcode(path, urlSource.getMemoryManager());
    ArrayJanitor<char>  janBuf2(pathAsCharStar, urlSource.getMemoryManager());

    const XMLCh*        fragment = urlSource.getFragment();
    char*               fragmentAsCharStar = 0;
    if (fragment)
        fragmentAsCharStar = XMLString::transcode(fragment, urlSource.getMemoryManager());
    ArrayJanitor<char>  janBuf3(fragmentAsCharStar, urlSource.getMemoryManager());

    const XMLCh*        query = urlSource.getQuery();
    char*               queryAsCharStar = 0;
    if (query)
        queryAsCharStar = XMLString::transcode(query, urlSource.getMemoryManager());
    ArrayJanitor<char>  janBuf4(queryAsCharStar, urlSource.getMemoryManager());		

    unsigned short      portNumber = (unsigned short) urlSource.getPortNum();

    //
    // Set up a socket.
    //
    struct hostent*     hostEntPtr = 0;
    struct sockaddr_in  sa;


    if ((hostEntPtr = gethostbyname(hostNameAsCharStar)) == NULL)
    {
        unsigned long  numAddress = inet_addr(hostNameAsCharStar);
        if (numAddress == INADDR_NONE)
        {
            // Call WSAGetLastError() to get the error number.
            ThrowXMLwithMemMgr1(NetAccessorException,
                     XMLExcepts::NetAcc_TargetResolution, hostName, fMemoryManager);
        }
        if ((hostEntPtr =
                gethostbyaddr((const char *) &numAddress,
                              sizeof(unsigned long), AF_INET)) == NULL)
        {
            // Call WSAGetLastError() to get the error number.
            ThrowXMLwithMemMgr1(NetAccessorException,
                     XMLExcepts::NetAcc_TargetResolution, hostName, fMemoryManager);
        }
    }

    memcpy((void *) &sa.sin_addr,
           (const void *) hostEntPtr->h_addr, hostEntPtr->h_length);
    sa.sin_family = hostEntPtr->h_addrtype;
    sa.sin_port = htons(portNumber);

    SOCKET s = socket(hostEntPtr->h_addrtype, SOCK_STREAM, 0);
    if (s == INVALID_SOCKET)
    {
        // Call WSAGetLastError() to get the error number.
        ThrowXMLwithMemMgr1(NetAccessorException,
                 XMLExcepts::NetAcc_CreateSocket, urlSource.getURLText(), fMemoryManager);
    }
    SocketJanitor janSock(&s);

    if (connect(s, (struct sockaddr *) &sa, sizeof(sa)) == SOCKET_ERROR)
    {
        // Call WSAGetLastError() to get the error number.
        ThrowXMLwithMemMgr1(NetAccessorException,
                 XMLExcepts::NetAcc_ConnSocket, urlSource.getURLText(), fMemoryManager);
    }


    // Set a flag so we know that the headers have not been read yet.
    bool fHeaderRead = false;

    // The port is open and ready to go.
    // Build up the http GET command to send to the server.
    // To do:  We should really support http 1.1.  This implementation
    //         is weak.

    memset(fBuffer, 0, sizeof(fBuffer));

    if(httpInfo==0)
        strcpy(fBuffer, "GET ");
    else {
        switch(httpInfo->fHTTPMethod) {
        case XMLNetHTTPInfo::GET:   strcpy(fBuffer, "GET "); break;
        case XMLNetHTTPInfo::PUT:   strcpy(fBuffer, "PUT "); break;
        case XMLNetHTTPInfo::POST:  strcpy(fBuffer, "POST "); break;
        }
    }
    strcat(fBuffer, pathAsCharStar);

    if (queryAsCharStar != 0)
    {
        // Tack on a ? before the fragment
        strcat(fBuffer,"?");
        strcat(fBuffer, queryAsCharStar);
    }

    if (fragmentAsCharStar != 0)
    {
        strcat(fBuffer, fragmentAsCharStar);
    }
    strcat(fBuffer, " HTTP/1.0\r\n");


    strcat(fBuffer, "Host: ");
    strcat(fBuffer, hostNameAsCharStar);
    if (portNumber != 80)
    {
        strcat(fBuffer, ":");
        int i = strlen(fBuffer);
        _itoa(portNumber, fBuffer+i, 10);
    }
    strcat(fBuffer, "\r\n");

    const XMLCh* username = urlSource.getUser();
    const XMLCh* password = urlSource.getPassword();
    if (username && password)
    {
        XMLBuffer userPass(256, fMemoryManager);
        userPass.append(username);
        userPass.append(chColon);
        userPass.append(password);
        char* userPassAsCharStar = XMLString::transcode(userPass.getRawBuffer(), fMemoryManager);
        ArrayJanitor<char>  janBuf(userPassAsCharStar, fMemoryManager);

        unsigned int len;
        XMLByte* encodedData = Base64::encode((XMLByte *)userPassAsCharStar, strlen(userPassAsCharStar), &len, fMemoryManager);
        ArrayJanitor<XMLByte>  janBuf2(encodedData, fMemoryManager);
 
        if (encodedData)
        {
            // HTTP doesn't want the 0x0A separating the data in chunks of 76 chars per line
            XMLByte* authData = (XMLByte*)fMemoryManager->allocate((len+1)*sizeof(XMLByte));
            ArrayJanitor<XMLByte>  janBuf(authData, fMemoryManager);
            XMLByte* cursor=authData;
            for(unsigned int i=0;i<len;i++)
                if(encodedData[i]!=chLF)
                    *cursor++=encodedData[i];
            *cursor++=0;
            strcat(fBuffer, "Authorization: Basic ");
            strcat(fBuffer, (char*)authData);
            strcat(fBuffer, "\r\n");
        }
    }

    if(httpInfo!=0 && httpInfo->fHeaders!=0)
        strncat(fBuffer,httpInfo->fHeaders,httpInfo->fHeadersLen);

    strcat(fBuffer, "\r\n");

    // Send the http request
    int lent = strlen(fBuffer);
    int  aLent = 0;
    if ((aLent = send(s, fBuffer, lent, 0)) != lent)
    {
        // Call WSAGetLastError() to get the error number.
        ThrowXMLwithMemMgr1(NetAccessorException,
                 XMLExcepts::NetAcc_WriteSocket, urlSource.getURLText(), fMemoryManager);
    }

    if(httpInfo!=0 && httpInfo->fPayload!=0) {
        int  aLent = 0;
        if ((aLent = send(s, httpInfo->fPayload, httpInfo->fPayloadLen, 0)) != httpInfo->fPayloadLen)
        {
            // Call WSAGetLastError() to get the error number.
            ThrowXMLwithMemMgr1(NetAccessorException,
                     XMLExcepts::NetAcc_WriteSocket, urlSource.getURLText(), fMemoryManager);
        }
    }

    //
    // get the response, check the http header for errors from the server.
    //
    memset(fBuffer, 0, sizeof(fBuffer));
    aLent = recv(s, fBuffer, sizeof(fBuffer)-1, 0);
    if (aLent == SOCKET_ERROR || aLent == 0)
    {
        // Call WSAGetLastError() to get the error number.
        ThrowXMLwithMemMgr1(NetAccessorException, XMLExcepts::NetAcc_ReadSocket, urlSource.getURLText(), fMemoryManager);
    }

    fBufferEnd = fBuffer+aLent;
    *fBufferEnd = 0;

    do {
        // Find the break between the returned http header and any data.
        //  (Delimited by a blank line)
        // Hang on to any data for use by the first read from this BinHTTPURLInputStream.
        //
        fBufferPos = strstr(fBuffer, "\r\n\r\n");
        if (fBufferPos != 0)
        {
            fBufferPos += 4;
            *(fBufferPos-2) = 0;
            fHeaderRead = true;
        }
        else
        {
            fBufferPos = strstr(fBuffer, "\n\n");
            if (fBufferPos != 0)
            {
                fBufferPos += 2;
                *(fBufferPos-1) = 0;
                fHeaderRead = true;
            }
            else
            {
                //
                // Header is not yet read, do another recv() to get more data...
                aLent = recv(s, fBufferEnd, (sizeof(fBuffer) - 1) - (fBufferEnd - fBuffer), 0);
                if (aLent == SOCKET_ERROR || aLent == 0)
                {
                    // Call WSAGetLastError() to get the error number.
                    ThrowXMLwithMemMgr1(NetAccessorException, XMLExcepts::NetAcc_ReadSocket, urlSource.getURLText(), fMemoryManager);
                }
                fBufferEnd = fBufferEnd + aLent;
                *fBufferEnd = 0;
            }
        }
    } while(fHeaderRead == false);

    // Make sure the header includes an HTTP 200 OK response.
    //
    char *p = strstr(fBuffer, "HTTP");
    if (p == 0)
    {
        ThrowXMLwithMemMgr1(NetAccessorException, XMLExcepts::NetAcc_ReadSocket, urlSource.getURLText(), fMemoryManager);
    }

    p = strchr(p, ' ');
    if (p == 0)
    {
        ThrowXMLwithMemMgr1(NetAccessorException, XMLExcepts::NetAcc_ReadSocket, urlSource.getURLText(), fMemoryManager);
    }

    int httpResponse = atoi(p);
    if (httpResponse != 200)
    {
        // Most likely a 404 Not Found error.
        //   Should recognize and handle the forwarding responses.
        //
        ThrowXMLwithMemMgr1(NetAccessorException, XMLExcepts::File_CouldNotOpenFile, urlSource.getURLText(), fMemoryManager);
    }

    fSocketHandle = (unsigned int) *janSock.release();
}



BinHTTPURLInputStream::~BinHTTPURLInputStream()
{
    shutdown(fSocketHandle, SD_BOTH);
    closesocket(fSocketHandle);
}


//
//  readBytes
//
unsigned int BinHTTPURLInputStream::readBytes(XMLByte* const    toFill
                                    , const unsigned int    maxToRead)
{
    unsigned int len = fBufferEnd - fBufferPos;
    if (len > 0)
    {
        // If there's any data left over in the buffer into which we first
        //   read from the server (to get the http header), return that.
        if (len > maxToRead)
            len = maxToRead;
        memcpy(toFill, fBufferPos, len);
        fBufferPos += len;
    }
    else
    {
        // There was no data in the local buffer.
        // Read some from the socket, straight into our caller's buffer.
        //
        len = recv((SOCKET) fSocketHandle, (char *) toFill, maxToRead, 0);
        if (len == SOCKET_ERROR)
        {
            // Call WSAGetLastError() to get the error number.
            ThrowXMLwithMemMgr(NetAccessorException, XMLExcepts::NetAcc_ReadSocket, fMemoryManager);
        }
    }

    fBytesProcessed += len;
    return len;
}


XERCES_CPP_NAMESPACE_END
