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
 * $Id: MacPosixFile.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/Janitor.hpp>

#include <limits.h>
#include <stdio.h>

#include <xercesc/util/Platforms/MacOS/MacPosixFile.hpp>


XERCES_CPP_NAMESPACE_BEGIN

//----------------------------------------------------------------------------
// XMLMacPosixFile methods
//----------------------------------------------------------------------------

XMLMacPosixFile::XMLMacPosixFile()
  : mFile(NULL)
{
}


XMLMacPosixFile::~XMLMacPosixFile()
{
	if (mFile)
		close();
}


unsigned int
XMLMacPosixFile::currPos()
{
    if (!mFile)
		ThrowXML(XMLPlatformUtilsException, XMLExcepts::CPtr_PointerIsZero);
		 
    long curPos = ftell(mFile);
	
    if (curPos == -1)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetSize);

    return (unsigned int)curPos;
}


void
XMLMacPosixFile::close()
{
    if (!mFile)
		ThrowXML(XMLPlatformUtilsException, XMLExcepts::CPtr_PointerIsZero);
		
    if (fclose(mFile))
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotCloseFile);
		
	mFile = NULL;
}


unsigned int
XMLMacPosixFile::size()
{
    if (mFile == NULL)
		ThrowXML(XMLPlatformUtilsException, XMLExcepts::CPtr_PointerIsZero);
		
    // Get the current position
    long curPos = ftell(mFile);
    if (curPos == -1)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetCurPos);

    // Seek to the end and save that value for return
    if (fseek(mFile, 0, SEEK_END))
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotSeekToEnd);

    long retVal = ftell(mFile);
    if (retVal == -1)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotSeekToEnd);

    // And put the pointer back
    if (fseek(mFile, curPos, SEEK_SET))
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotSeekToPos);

    return (unsigned int)retVal;
}


bool
XMLMacPosixFile::open(const XMLCh* const path, bool toWrite)
{
    if (!path)
		ThrowXML(XMLPlatformUtilsException, XMLExcepts::CPtr_PointerIsZero);

	//	Transcode the unicode path to UTF8, which is what the Mac posix routines want
	char tmpPath[kMaxMacStaticPathChars];
	std::size_t len = TranscodeUniCharsToUTF8(path, tmpPath, XMLString::stringLen(path), kMaxMacStaticPathChars-1);
	tmpPath[len] = 0;
	
	//	Call through to the char version to do the work
	return open(tmpPath, toWrite);
}


bool
XMLMacPosixFile::open(const char* const path, bool toWrite)
{
    if (!path)
		ThrowXML(XMLPlatformUtilsException, XMLExcepts::CPtr_PointerIsZero);

	const char* perms = (toWrite) ? "w" : "r";
	mFile = fopen(path, perms);
	
	return (mFile != NULL);
}


unsigned int
XMLMacPosixFile::read(const unsigned int byteCount, XMLByte* const buffer)
{
    if (!mFile || !buffer)
		ThrowXML(XMLPlatformUtilsException, XMLExcepts::CPtr_PointerIsZero);
		
    size_t bytesRead = 0;
	if (byteCount > 0)
	{
    	bytesRead = fread((void*)buffer, 1, byteCount, mFile);

		if (ferror(mFile))
			ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotReadFromFile);
	}
	
    return (unsigned int)bytesRead;
}


void
XMLMacPosixFile::write(long byteCount, const XMLByte* buffer)
{
    if (!mFile || !buffer)
		ThrowXML(XMLPlatformUtilsException, XMLExcepts::CPtr_PointerIsZero);

    while (byteCount > 0)
    {
        size_t bytesWritten = fwrite(buffer, sizeof(XMLByte), byteCount, mFile);

        if (ferror(mFile))
			ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotWriteToFile);

		buffer		+= bytesWritten;
		byteCount	-= bytesWritten;
    }
}


void
XMLMacPosixFile::reset()
{
    if (!mFile)
		ThrowXML(XMLPlatformUtilsException, XMLExcepts::CPtr_PointerIsZero);
		
    // Seek to the start of the file
    if (fseek(mFile, 0, SEEK_SET))
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotResetFile);
}


XERCES_CPP_NAMESPACE_END
