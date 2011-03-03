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
 * $Id: ParserTest.cpp 470088 2006-11-01 20:35:12Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include    <xercesc/util/PlatformUtils.hpp>
#include    <xercesc/util/XMLString.hpp>
#include    <xercesc/util/XMLURL.hpp>
#include    <xercesc/internal/XMLScanner.hpp>
#include    <xercesc/validators/DTD/DTDValidator.hpp>
#include    "ParserTest.hpp"


// ---------------------------------------------------------------------------
//  Program entry point
// ---------------------------------------------------------------------------
int main(int argC, char** argV)
{
    // Init the XML platform
    try
    {
        XMLPlatformUtils::Initialize();
    }

    catch(const XMLException& toCatch)
    {
        XERCES_STD_QUALIFIER cout << "Error during platform init! Message:\n"
             << StrX(toCatch.getMessage()) << XERCES_STD_QUALIFIER endl;
        return 1;
    }

    //
    //  Create our test parser object. This object implements the internal
    //  event APIs and is plugged into the scanner.
    //
    TestParser parserTest;

    // Figure out the parameters
    bool doValidation = false;
    bool doNamespaces = false;
    bool keepGoing = false;
    XMLCh*  urlPath = 0;
    for (int index = 1; index < argC; index++)
    {
        if (!XMLString::compareIString(argV[index], "/Debug"))
            parserTest.setOutputType(OutputType_Debug);
        else if (!XMLString::compareIString(argV[index], "/Validate"))
            doValidation = true;
        else if (!XMLString::compareIString(argV[index], "/Namespaces"))
        {
            doNamespaces = true;
            parserTest.setDoNamespaces(true);
        }
        else if (!XMLString::compareIString(argV[index], "/XML"))
            parserTest.setOutputType(OutputType_XML);
        else if (!XMLString::compareIString(argV[index], "/IntDTD"))
            parserTest.setShowIntDTD(true);
        else if (!XMLString::compareIString(argV[index], "/ShowWarnings"))
            parserTest.setShowWarnings(true);
        else if (!XMLString::compareIString(argV[index], "/ShowErrLoc"))
            parserTest.setShowErrLoc(true);
        else if (!XMLString::compareIString(argV[index], "/JCCanon"))
            parserTest.setOutputType(OutputType_JCCanon);
        else if (!XMLString::compareIString(argV[index], "/SunCanon"))
            parserTest.setOutputType(OutputType_SunCanon);
        else if (!XMLString::compareIString(argV[index], "/KeepGoing"))
            keepGoing = true;
        else if (!XMLString::compareNIString(argV[index], "/URL=", 5))
            urlPath = XMLString::transcode(&argV[index][5]);
        else
            XERCES_STD_QUALIFIER cout << "Unknown parameter: " << argV[index] << XERCES_STD_QUALIFIER endl;
    }

    // We have to have a URL to work on
    if (!urlPath)
    {
        XERCES_STD_QUALIFIER cout << "A URL must be provided, /URL=xxxx" << XERCES_STD_QUALIFIER endl;
        return 1;
    }

    //
    //  Create a validator of the correct type so that we can install it
    //  on the scanner.
    //
    //  <TBD> Later, when Schema validators exist, we'll have a parameter
    //  to select one or the other
    //
    XMLValidator* validator = 0;
    DTDValidator* dtdVal = new DTDValidator(&parserTest);
    validator = dtdVal;

    // And now create the scanner and give it all the handlers
    XMLScanner scanner
    (
        &parserTest
        , &parserTest
        , 0
        , &parserTest
        , validator
    );

    // Set the scanner flags that we were told to
    scanner.setDoValidation(doValidation);
    scanner.setDoNamespaces(doNamespaces);
    scanner.setExitOnFirstFatal(!keepGoing);

    // Tell the parser about the scanner
    parserTest.setScanner(&scanner);

    try
    {
        scanner.scanDocument(urlPath);
    }

    catch(const XMLException& toCatch)
    {
        XERCES_STD_QUALIFIER cout << "Exception during scan:\n    "
             << StrX(toCatch.getMessage())
             << XERCES_STD_QUALIFIER endl;
    }

    // And call the termination method
    XMLPlatformUtils::Terminate();

    return 0;
}



// ---------------------------------------------------------------------------
//  StrX: Private helper methods
// ---------------------------------------------------------------------------
void StrX::transcode(const XMLCh* const toTranscode, const unsigned int len)
{
    // Short circuit if its a null pointer
    if (!toTranscode || (!toTranscode[0]))
    {
        fLocalForm = new char[1];
        fLocalForm[0] = 0;
        return;
	}

    // See if our XMLCh and wchar_t as the same on this platform
    const bool isSameSize = (sizeof(XMLCh) == sizeof(wchar_t));

    //
    //  Get the actual number of chars. If the passed len is zero, its null
    //  terminated. Else we have to use the len.
    //
    wchar_t realLen = (wchar_t)len;
    if (!realLen)
    {
        //
        //  We cannot just assume we can use wcslen() because we don't know
        //  if our XMLCh is the same as wchar_t on this platform.
        //
        const XMLCh* tmpPtr = toTranscode;
        while (*(tmpPtr++))
            realLen++;
    }

    //
    //  If either the passed length was non-zero or our char sizes are not
    //  same, we have to use a temp buffer. Since this is common in these
    //  samples, we just do it anyway.
    //
    wchar_t* tmpSource = new wchar_t[realLen + 1];
    if (isSameSize)
    {
        memcpy(tmpSource, toTranscode, realLen * sizeof(wchar_t));
    }
     else
    {
        for (unsigned int index = 0; index < realLen; index++)
            tmpSource[index] = (wchar_t)toTranscode[index];
    }
    tmpSource[realLen] = 0;

    // See now many chars we need to transcode this guy
    const unsigned int targetLen = ::wcstombs(0, tmpSource, 0);

    // Allocate out storage member
    fLocalForm = new char[targetLen + 1];

    //
    //  And transcode our temp source buffer to the local buffer. Cap it
    //  off since the converter won't do it (because the null is beyond
    //  where the target will fill up.)
    //
    ::wcstombs(fLocalForm, tmpSource, targetLen);
    fLocalForm[targetLen] = 0;

    // Don't forget to delete our temp buffer
    delete [] tmpSource;
}
