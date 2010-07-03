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
 * $Id: MacCarbonFile.cpp 568078 2007-08-21 11:43:25Z amassari $
 */
 
#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/Janitor.hpp>

#include <xercesc/util/Platforms/MacOS/MacCarbonFile.hpp>


#if defined(__APPLE__)
    //	Include from Frameworks Headers under ProjectBuilder
    #include <Carbon/Carbon.h>
#else
    //	Classic include styles
    #include <Files.h>
    #include <TextUtils.h>
#endif


XERCES_CPP_NAMESPACE_BEGIN

//----------------------------------------------------------------------------
// XMLMacCarbonFile methods
//----------------------------------------------------------------------------

unsigned int
XMLMacCarbonFile::currPos()
{
    OSErr err = noErr;
    unsigned int pos = 0;

    if (!mFileValid)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetCurPos);

    if (gHasHFSPlusAPIs)
    {
        SInt64 bigPos = 0;
        err = FSGetForkPosition(mFileRefNum, &bigPos);
        if (err == noErr)
            pos = bigPos;
    }
    else
    {
        long longPos;
        err = GetFPos(mFileRefNum, &longPos);
        if (err == noErr)
            pos = longPos;
    }

    if (err != noErr)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetCurPos);

    return pos;
}


void
XMLMacCarbonFile::close()
{
    OSErr err = noErr;
    if (!mFileValid)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotCloseFile);

    if (gHasHFSPlusAPIs)
        err = FSCloseFork(mFileRefNum);
    else
        err = FSClose(mFileRefNum);

    mFileValid = false;

    if (err != noErr)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotCloseFile);
}


unsigned int
XMLMacCarbonFile::size()
{
    OSErr err = noErr;
    unsigned int len = 0;

    if (!mFileValid)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetSize);

    if (gHasHFSPlusAPIs)
    {
        SInt64 bigLen = 0;
        err = FSGetForkSize(mFileRefNum, &bigLen);
        if (err == noErr)
            len = bigLen;
    }
    else
    {
        long longLen;
        err = GetEOF(mFileRefNum, &longLen);
        if (err == noErr)
            len = longLen;
    }

    if (err != noErr)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetSize);

    return len;
}


bool
XMLMacCarbonFile::openWithPermission(const XMLCh* const fileName, int macPermission)
{
    OSErr err = noErr;

    if (mFileValid)
        ThrowXML1(XMLPlatformUtilsException, XMLExcepts::File_CouldNotOpenFile, fileName);

    if (gHasHFSPlusAPIs)
    {
        FSRef ref;
        if (!XMLParsePathToFSRef(fileName, ref))
            err = fnfErr;

        HFSUniStr255 forkName;
        if (err == noErr)
            err = FSGetDataForkName(&forkName);

        if (err == noErr)
            err = FSOpenFork(&ref, forkName.length, forkName.unicode, macPermission, &mFileRefNum);
    }
    else
    {
        FSSpec spec;
        if (!XMLParsePathToFSSpec(fileName, spec))
            err = fnfErr;

        if (err == noErr)
            err = FSpOpenDF(&spec, macPermission, &mFileRefNum);
    }

    if (err != noErr)
        ThrowXML1(XMLPlatformUtilsException, XMLExcepts::File_CouldNotOpenFile, fileName);

    mFileValid = true;
	return mFileValid;
}


void
XMLMacCarbonFile::create(const XMLCh* const filePath)
{
    OSErr err = noErr;

    //	Split path into directory and filename components
    int posSlash = XMLString::lastIndexOf(filePath, '/', XMLString::stringLen(filePath) - 1);
    int posName = (posSlash == -1) ? 0 : posSlash+1;

    const XMLCh* namePtr = filePath + posName;
    int nameLen = XMLString::stringLen(namePtr);

    //	Make a temporary string of the directory
    ArrayJanitor<XMLCh> dirPath(new XMLCh[namePtr - filePath + 1]);
    XMLString::subString(dirPath.get(), filePath, 0, posName);

    //	Create the file as appropriate for API set
    if (gHasHFSPlusAPIs)
    {
    	//	HFS+
        FSRef ref;

        //	If we find an existing file, delete it
        if (XMLParsePathToFSRef(filePath, ref))
            FSDeleteObject(&ref);

        //	Get a ref to the parent directory
        if (!XMLParsePathToFSRef(dirPath.get(), ref))
            err = fnfErr;

        //	Create a new file using the unicode name
        if (err == noErr)
        {
            UniChar uniName[256];
            err = FSCreateFileUnicode(
                    &ref,
                    nameLen, CopyXMLChsToUniChars(namePtr, uniName, nameLen, sizeof(uniName)),
                    0, NULL, NULL, NULL);
        }
    }
    else
    {
    	//	HFS
        FSSpec spec;

        //	If we find an existing file, delete it
        if (XMLParsePathToFSSpec(filePath, spec))
            FSpDelete(&spec);

        //	Get a spec to the parent directory
        if (!XMLParsePathToFSSpec(dirPath.get(), spec))
            err = fnfErr;

        //	Check that the new name is not too long for HFS
        if (err == noErr && nameLen > 31)
            err = errFSNameTooLong;

        if (err == noErr)
        {
            //	Transcode the unicode name to native encoding
            ArrayJanitor<const char> nativeName(XMLString::transcode(namePtr));

            // Make a partial pathname from our current spec (parent directory) to the new file
            unsigned char name[31 * 2 + 1 * 2 + 1];
            unsigned char* partial = &name[1];

            *partial++ = ':';      			 // Partial leads with :
            const unsigned char* specName = spec.name;	// Copy in spec name
            for (int specCnt = *specName++; specCnt > 0; --specCnt)
                *partial++ = *specName++;

            *partial++ = ':';      			 // Path component separator
            char c;
            for (const char* p = nativeName.get(); (c = *p++) != 0; ) // Copy in new element
                *partial++ = (c == ':') ? '/' : c;		// Convert : to /

            name[0] = partial - &name[1];   // Set the pascal string name length

            //	Update the spec: this will probably return fnfErr
            //					 (since we just deleted any existing file)
            err = FSMakeFSSpec(spec.vRefNum, spec.parID, name, &spec);

            //	Create the file from the spec
            err = FSpCreate(&spec, '??\??', 'TEXT', smSystemScript);
        }
    }

    //	Fail if we didn't create the file
    if (err != noErr)
    {
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotReadFromFile);
        //ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotWriteToFile);
    }
}


bool
XMLMacCarbonFile::open(const XMLCh* const path, bool toWrite)
{
	bool success = false;
	
	if (toWrite)
	{
		create(path);
		success = openWithPermission(path, fsRdWrPerm);
	}
	else
	{
		success = openWithPermission(path, fsRdPerm);
	}
	
	return success;
}


bool
XMLMacCarbonFile::open(const char* fileName, bool toWrite)
{
	//	Transcode the input filename from UTF8 into UTF16
	UniChar uniBuf[kMaxMacStaticPathChars];
	std::size_t pathLen = TranscodeUTF8ToUniChars(fileName, uniBuf, kMaxMacStaticPathChars-1);
	uniBuf[pathLen++] = 0;
	
	//	Call through to the unicode open routine
	return open(uniBuf, toWrite);
}


unsigned int
XMLMacCarbonFile::read(const unsigned int toRead, XMLByte* const toFill)
{
    unsigned int bytesRead = 0;
    OSErr err = noErr;

    if (!mFileValid)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotReadFromFile);

    if (gHasHFSPlusAPIs)
    {
        ByteCount actualCount;
        err = FSReadFork(mFileRefNum, fsFromMark, 0, toRead, toFill, &actualCount);
        bytesRead = actualCount;
    }
    else
    {
        long byteCount = toRead;
        err = FSRead(mFileRefNum, &byteCount, toFill);
        bytesRead = byteCount;
    }

    if (err != noErr && err != eofErr)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotReadFromFile);

    return bytesRead;
}


void
XMLMacCarbonFile::write(const long byteCount, const XMLByte* const buffer)
{
    long bytesWritten = 0;
    OSErr err = noErr;

    if (byteCount <= 0 || buffer == NULL)
        return;

    if (!mFileValid)
    {
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotWriteToFile);
    }

    if (gHasHFSPlusAPIs)
    {
        ByteCount actualCount;
        err = FSWriteFork(mFileRefNum, fsFromMark, 0, byteCount, buffer, &actualCount);
        bytesWritten = actualCount;
    }
    else
    {
        long count = byteCount;
        err = FSWrite(mFileRefNum, &count, buffer);
        bytesWritten = count;
    }

    if ((err != noErr && err != eofErr) || (bytesWritten != byteCount))
    {
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotWriteToFile);
    }
}


void
XMLMacCarbonFile::reset()
{
    OSErr err = noErr;

    if (!mFileValid)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotResetFile);

    if (gHasHFSPlusAPIs)
        err = FSSetForkPosition(mFileRefNum, fsFromStart, 0);
    else
        err = SetFPos(mFileRefNum, fsFromStart, 0);

    if (err != noErr)
        ThrowXML(XMLPlatformUtilsException, XMLExcepts::File_CouldNotResetFile);
}


XMLMacCarbonFile::~XMLMacCarbonFile()
{
    if (mFileValid)
        close();
}

XERCES_CPP_NAMESPACE_END
