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
 * $Id: URLAccessBinInputStream.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


#include <xercesc/util/XMLNetAccessor.hpp>
#include <xercesc/util/NetAccessors/MacOSURLAccess/URLAccessBinInputStream.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLExceptMsgs.hpp>
#include <xercesc/util/Janitor.hpp>

#include <cstdlib>
#include <cstring>

XERCES_CPP_NAMESPACE_BEGIN

URLAccessBinInputStream::URLAccessBinInputStream(const XMLURL& urlSource)
      : mBytesProcessed(0),
        mURLReference(NULL),
        mBuffer(NULL),
        mBufPos(NULL),
        mBufAvailable(0)
{
	OSStatus status = noErr;
	
	//	Get the full URL from the source
    char*               url = XMLString::transcode(urlSource.getURLText(), urlSource.getMemoryManager());
    ArrayJanitor<char>  janBuf(url, urlSource.getMemoryManager());

	//	Create a URL reference from the URL
	status = URLNewReference(url, &mURLReference);
	
	//	Begin the transfer
	if (status == noErr)
		status = URLOpen(
					mURLReference,
					NULL,	// FSSpec* (not reading to file)
					0, 		// URLOpenFlags
					NULL,	// URLNotifyUPP
					0,		// URLEventMask
					0);		// userContext
	
	//	If we failed, we throw
	switch (status)
	{
		case noErr:
			break;
			
		case kURLInvalidURLError:
        	ThrowXML(MalformedURLException, XMLExcepts::URL_MalformedURL);
			break;
		case kURLUnsupportedSchemeError:
        	ThrowXML(MalformedURLException, XMLExcepts::URL_UnsupportedProto);
			break;
		
		default:
        	ThrowXML1(NetAccessorException, XMLExcepts::NetAcc_ConnSocket, urlSource.getURLText());
        	break;
	}
}



URLAccessBinInputStream::~URLAccessBinInputStream()
{
	OSStatus status = noErr;
	
	//	Release any buffer we've still got a hold of
	if (status == noErr && mBuffer)
	{
		status = URLReleaseBuffer(mURLReference, mBuffer);
		mBuffer = NULL;
	}

	//	Get the current state
	URLState state = 0;
	if (status == noErr)
		status = URLGetCurrentState(mURLReference, &state);
	
	//	Abort if we're not completed
	if (status == noErr && state != kURLNullState && state != kURLCompletedState)
		status = URLAbort(mURLReference);
		
	//	Loop til we reach a terminal state.
	//	This may be unneeded by URLAccess, but the docs are
	//	not very good.
	while (
			status == noErr
		 && state != kURLNullState
		 && state != kURLErrorOccurredState
		 && state != kURLCompletedState
		  )
	{
		status = URLIdle();
		if (status == noErr)
			status = URLGetCurrentState(mURLReference, &state);
	}
		
	//	Dispose the reference
	status = URLDisposeReference(mURLReference);
}


//
//	Call URLAccess to fullfill the read request. Since it
//	passes us back buffers full of data, our object maintains
//	a partial buffer across calls.
//
unsigned int
URLAccessBinInputStream::readBytes(XMLByte* const    toFill
                                    , const unsigned int    maxToRead)
{
	OSStatus status = noErr;

	XMLByte* writePos			= toFill;
	std::size_t bytesDesired	= maxToRead;
	URLState state				= kURLNullState;
	
	while (												// while...
			status == noErr								// there's been no error
		 && bytesDesired > 0							// more data is wanted
		 && (status = URLGetCurrentState(mURLReference, &state)) == noErr	// we can get the state
		 && (state != kURLErrorOccurredState)			// no error has occurred in the transaction
		 && (mBuffer || state != kURLCompletedState)	// we have data still buffered or the request isn't complete
		 && (mBuffer || bytesDesired == maxToRead)		// we have data still buffered or we've supplied absolutely none
		  )
	{
		//	Give time to URLAccess
		status = URLIdle();
		
		//	If we've got buffered data, use it
		if (status == noErr && mBuffer)
		{
			//	Supply as much as we can from the buffer
			std::size_t n = mBufAvailable;
			if (n > bytesDesired)
				n = bytesDesired;
				
			//	If we've got data, copy it over and update our pointers
			if (n > 0)
			{
				std::memcpy(writePos, mBufPos, n);
				
				writePos		+= n;
				bytesDesired	-= n;
				
				mBufPos			+= n;
				mBufAvailable	-= n;
				
				mBytesProcessed	+= n;
			}
			
			//	If we exhausted the buffer, release it
			if (mBufAvailable == 0)
			{
				status = URLReleaseBuffer(mURLReference, mBuffer);
				mBuffer = NULL;
			}
		}
		
		//	If the buffer is exhausted, get a new one
		if (status == noErr && !mBuffer)
		{
			status = URLGetBuffer(mURLReference, &mBuffer, &mBufAvailable);
			if (status == noErr)
				mBufPos = reinterpret_cast<char*>(mBuffer);
		}
	}
	
	//	Throw on any error
	if (status != noErr || state == kURLErrorOccurredState)
	    ThrowXML(NetAccessorException, XMLExcepts::NetAcc_ReadSocket);
	
	//	Return number of bytes delivered
	return maxToRead - bytesDesired;
}

XERCES_CPP_NAMESPACE_END
