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
 * $Id: UnixHTTPURLInputStream.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#if !defined(XML_BEOS)
  #include <netinet/in.h>
  #include <arpa/inet.h>
#endif
#include <netdb.h>
#include <errno.h>

#include <xercesc/util/XMLNetAccessor.hpp>
#include <xercesc/util/NetAccessors/Socket/UnixHTTPURLInputStream.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLExceptMsgs.hpp>
#include <xercesc/util/Janitor.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/TransService.hpp>
#include <xercesc/util/TranscodingException.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/Base64.hpp>

XERCES_CPP_NAMESPACE_BEGIN

class SocketJanitor
{
public:
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    SocketJanitor(int* toDelete) : fData(toDelete) {}
    ~SocketJanitor() { reset(); }

    int* get() const { return fData; }
    int* release() { int* p = fData; fData = 0; return p; }

    void reset(int* p = 0) { if(fData) close(*fData); fData=p; }
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
    int*  fData;
};

UnixHTTPURLInputStream::UnixHTTPURLInputStream(const XMLURL& urlSource, const XMLNetHTTPInfo* httpInfo/*=0*/)
      : fSocket(0)
      , fBytesProcessed(0)
      , fMemoryManager(urlSource.getMemoryManager())
{

    //
    //  Constants in ASCII to send/check in the HTTP request/response
    //

    const char GET[] =
    {
        chLatin_G, chLatin_E, chLatin_T, chSpace, chNull
    };

    const char PUT[] =
    {
        chLatin_P, chLatin_U, chLatin_T, chSpace, chNull
    };

    const char POST[] =
    {
        chLatin_P, chLatin_O, chLatin_S, chLatin_T, chSpace, chNull
    };

    const char HTTP[] =
    {
        chLatin_H, chLatin_T, chLatin_T, chLatin_P, chNull
    };

    const char HTTP10[] =
    {
        chSpace, chLatin_H, chLatin_T, chLatin_T, chLatin_P, chForwardSlash, chDigit_1, chPeriod, chDigit_0, chCR, chLF, chNull
    };

    const char CRLF[] =
    {
        chCR, chLF, chNull
    };

    const char CRLF2X[] =
    {
        chCR, chLF, chCR, chLF, chNull
    };

    const char LF2X[] =
    {
        chLF, chLF, chNull
    };

    const char HOST[] =
    {
        chLatin_H, chLatin_o, chLatin_s, chLatin_t, chColon, chSpace, chNull
    };

    const char COLON[] =
    {
        chColon, chNull
    };

    const char AUTHORIZATION[] =
    {
        chLatin_A, chLatin_u, chLatin_t, chLatin_h, chLatin_o, chLatin_r, chLatin_i, chLatin_z, chLatin_a, chLatin_t, 
        chLatin_i, chLatin_o, chLatin_n, chColon, chSpace, chLatin_B, chLatin_a, chLatin_s, chLatin_i, chLatin_c, chSpace, chNull
    };

    const char resp200 [] =
    {
        chSpace, chDigit_2, chDigit_0, chDigit_0, chSpace, chNull
    };

    unsigned int charsEaten;
    unsigned int transSize;
    XMLTransService::Codes failReason;
    const unsigned int blockSize = 2048;
    const unsigned int bufSize = 5;
    static XMLCh portBuffer[bufSize+1];

    //
    // Pull all of the parts of the URL out of the urlSource object
    //

    const XMLCh*        hostName = urlSource.getHost();
    const XMLCh*        path = urlSource.getPath();
    const XMLCh*        fragment = urlSource.getFragment();
    const XMLCh*        query = urlSource.getQuery();                        

    //
    //  Convert the hostName to the platform's code page for gethostbyname and
    //  inet_addr functions.
    //

    char*               hostNameAsCharStar = XMLString::transcode(hostName, fMemoryManager);
    ArrayJanitor<char>  janBuf1(hostNameAsCharStar, fMemoryManager);

    //
    //  Convert all the parts of the urlSource object to ASCII so they can be
    //  sent to the remote host in that format
    //

    transSize = XMLString::stringLen(hostName)+1;
    char*               hostNameAsASCII = (char*) fMemoryManager->allocate
    (
        (transSize+1) * sizeof(char)
    );//new char[transSize+1];
    ArrayJanitor<char>  janBuf2(hostNameAsASCII, fMemoryManager);

    XMLTranscoder* trans = XMLPlatformUtils::fgTransService->makeNewTranscoderFor("ISO8859-1", failReason, blockSize, fMemoryManager);
    trans->transcodeTo(hostName, transSize, (unsigned char *) hostNameAsASCII, transSize, charsEaten, XMLTranscoder::UnRep_Throw);

    char*               pathAsASCII = 0;
    ArrayJanitor<char>  janBuf3(pathAsASCII, fMemoryManager);
    if (path)
    {
        transSize = XMLString::stringLen(path)+1;
        pathAsASCII = (char*) fMemoryManager->allocate
        (
            (transSize+1) * sizeof(char)
        );//new char[transSize+1];
        janBuf3.reset(pathAsASCII, fMemoryManager);
        trans->transcodeTo(path, transSize, (unsigned char *) pathAsASCII, transSize, charsEaten, XMLTranscoder::UnRep_Throw);
    }

    char*               fragmentAsASCII = 0;
    ArrayJanitor<char>  janBuf4(fragmentAsASCII, fMemoryManager);
    if (fragment)
    {
        transSize = XMLString::stringLen(fragment)+1;
        fragmentAsASCII = (char*) fMemoryManager->allocate
        (
            (transSize+1) * sizeof(char)
        );//new char[transSize+1];
        janBuf4.reset(fragmentAsASCII, fMemoryManager);
        trans->transcodeTo(fragment, transSize, (unsigned char *) fragmentAsASCII, transSize, charsEaten, XMLTranscoder::UnRep_Throw);
    }

    char*               queryAsASCII = 0;
    ArrayJanitor<char>  janBuf5(queryAsASCII, fMemoryManager);
    if (query)
    {
        transSize = XMLString::stringLen(query)+1;
        queryAsASCII = (char*) fMemoryManager->allocate
        (
            (transSize+1) * sizeof(char)
        );//new char[transSize+1];
        janBuf5.reset(queryAsASCII, fMemoryManager);
        trans->transcodeTo(query, transSize, (unsigned char *) queryAsASCII, transSize, charsEaten, XMLTranscoder::UnRep_Throw);
    }

    unsigned short      portNumber = (unsigned short) urlSource.getPortNum();

    //
    //  Convert port number integer to unicode so we can transcode it to ASCII
    //

    XMLString::binToText((unsigned int) portNumber, portBuffer, bufSize, 10, fMemoryManager);
    transSize = XMLString::stringLen(portBuffer)+1;
    char*               portAsASCII = (char*) fMemoryManager->allocate
    (
        (transSize+1) * sizeof(char)
    );//new char[transSize+1];
    ArrayJanitor<char>  janBuf6(portAsASCII, fMemoryManager);
    trans->transcodeTo(portBuffer, transSize, (unsigned char *) portAsASCII, transSize, charsEaten, XMLTranscoder::UnRep_Throw);

    delete trans;

    //
    // Set up a socket.
    //
    struct hostent*     hostEntPtr = 0;
    struct sockaddr_in  sa;

    // Use the hostName in the local code page ....
    if ((hostEntPtr = gethostbyname(hostNameAsCharStar)) == NULL)
    {
        unsigned long  numAddress = inet_addr(hostNameAsCharStar);
        if (numAddress < 0)
        {
            ThrowXMLwithMemMgr1(NetAccessorException,
                     XMLExcepts::NetAcc_TargetResolution, hostName, fMemoryManager);
        }
        if ((hostEntPtr =
                gethostbyaddr((char *) &numAddress,
                              sizeof(unsigned long), AF_INET)) == NULL)
        {
            ThrowXMLwithMemMgr1(NetAccessorException,
                     XMLExcepts::NetAcc_TargetResolution, hostName, fMemoryManager);
        }
    }

    memset(&sa, '\0', sizeof(sockaddr_in));  // iSeries fix ??
    memcpy((void *) &sa.sin_addr,
           (const void *) hostEntPtr->h_addr, hostEntPtr->h_length);
    sa.sin_family = hostEntPtr->h_addrtype;
    sa.sin_port = htons(portNumber);

    int s = socket(hostEntPtr->h_addrtype, SOCK_STREAM, 0);
    if (s < 0)
    {
        ThrowXMLwithMemMgr1(NetAccessorException,
                 XMLExcepts::NetAcc_CreateSocket, urlSource.getURLText(), fMemoryManager);
    }
    SocketJanitor janSock(&s);

    if (connect(s, (struct sockaddr *) &sa, sizeof(sa)) < 0)
    {
        ThrowXMLwithMemMgr1(NetAccessorException,
                 XMLExcepts::NetAcc_ConnSocket, urlSource.getURLText(), fMemoryManager);
    }

    // The port is open and ready to go.
    // Build up the http GET command to send to the server.
    // To do:  We should really support http 1.1.  This implementation
    //         is weak.
    if(httpInfo==0)
      strcpy(fBuffer, GET);
    else
      switch(httpInfo->fHTTPMethod)
      {
        case XMLNetHTTPInfo::GET:   strcpy(fBuffer, GET); break;
        case XMLNetHTTPInfo::PUT:   strcpy(fBuffer, PUT); break;
        case XMLNetHTTPInfo::POST:  strcpy(fBuffer, POST); break;
      }
    if (pathAsASCII != 0)
    {
         strcat(fBuffer, pathAsASCII);
    }

    if (queryAsASCII != 0)
    {
        size_t n = strlen(fBuffer);
        fBuffer[n] = chQuestion;
        fBuffer[n+1] = chNull;
        strcat(fBuffer, queryAsASCII);
    }

    if (fragmentAsASCII != 0)
    {
        strcat(fBuffer, fragmentAsASCII);
    }
    strcat(fBuffer, HTTP10);

    strcat(fBuffer, HOST);
    strcat(fBuffer, hostNameAsASCII);
    if (portNumber != 80)
    {
        strcat(fBuffer,COLON);
        strcat(fBuffer,portAsASCII);
    }
    strcat(fBuffer, CRLF);

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
            strcat(fBuffer, AUTHORIZATION);
            strcat(fBuffer, (char*)authData);
            strcat(fBuffer, CRLF);
        }
    }

    if(httpInfo!=0 && httpInfo->fHeaders!=0)
        strncat(fBuffer,httpInfo->fHeaders,httpInfo->fHeadersLen);

    strcat(fBuffer, CRLF);

    // Send the http request
    int lent = strlen(fBuffer);
    int  aLent = 0;

    if ((aLent = write(s, (void *) fBuffer, lent)) != lent)
    {
        ThrowXMLwithMemMgr1(NetAccessorException,
                 XMLExcepts::NetAcc_WriteSocket, urlSource.getURLText(), fMemoryManager);
    }

    if(httpInfo!=0 && httpInfo->fPayload!=0) {
        int  aLent = 0;
        if ((aLent = write(s, (void *) httpInfo->fPayload, httpInfo->fPayloadLen)) != httpInfo->fPayloadLen)
        {
            ThrowXMLwithMemMgr1(NetAccessorException,
                     XMLExcepts::NetAcc_WriteSocket, urlSource.getURLText(), fMemoryManager);
        }
    }

    //
    // get the response, check the http header for errors from the server.
    //
    aLent = read(s, (void *)fBuffer, sizeof(fBuffer)-1);
    if (aLent <= 0)
    {
        ThrowXMLwithMemMgr1(NetAccessorException, XMLExcepts::NetAcc_ReadSocket, urlSource.getURLText(), fMemoryManager);
    }

    fBufferEnd = fBuffer+aLent;
    *fBufferEnd = 0;

    // Find the break between the returned http header and any data.
    //  (Delimited by a blank line)
    // Hang on to any data for use by the first read from this BinHTTPURLInputStream.
    //
    fBufferPos = strstr(fBuffer, CRLF2X);
    if (fBufferPos != 0)
    {
        fBufferPos += 4;
        *(fBufferPos-2) = 0;
    }
    else
    {
        fBufferPos = strstr(fBuffer, LF2X);
        if (fBufferPos != 0)
        {
            fBufferPos += 2;
            *(fBufferPos-1) = 0;
        }
        else
            fBufferPos = fBufferEnd;
    }

    // Make sure the header includes an HTTP 200 OK response.
    //
    char *p = strstr(fBuffer, HTTP);
    if (p == 0)
    {
        ThrowXMLwithMemMgr1(NetAccessorException, XMLExcepts::NetAcc_ReadSocket, urlSource.getURLText(), fMemoryManager);
    }

    p = strchr(p, chSpace);
    if (p == 0)
    {
        ThrowXMLwithMemMgr1(NetAccessorException, XMLExcepts::NetAcc_ReadSocket, urlSource.getURLText(), fMemoryManager);
    }
  
    if (memcmp(p, resp200, strlen(resp200)))
    {
        // Most likely a 404 Not Found error.
        //   Should recognize and handle the forwarding responses.
        //
        ThrowXMLwithMemMgr1(NetAccessorException, XMLExcepts::File_CouldNotOpenFile, urlSource.getURLText(), fMemoryManager);
    }

    fSocket = *janSock.release();

}


UnixHTTPURLInputStream::~UnixHTTPURLInputStream()
{
    shutdown(fSocket, 2);
    close(fSocket);
}


unsigned int UnixHTTPURLInputStream::readBytes(XMLByte* const    toFill
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
        len = read(fSocket, (void *) toFill, maxToRead);
        if (len == -1)
        {
            ThrowXMLwithMemMgr(NetAccessorException, XMLExcepts::NetAcc_ReadSocket, fMemoryManager);
        }
    }

    fBytesProcessed += len;
    return len;
}

XERCES_CPP_NAMESPACE_END

