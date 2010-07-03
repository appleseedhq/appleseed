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
 * $Log$
 * Revision 1.11  2004/09/08 13:57:05  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.10  2003/05/30 13:08:27  gareth
 * move over to macros for std:: and iostream/iostream.h issues.
 *
 * Revision 1.9  2002/02/01 22:46:28  peiyongz
 * sane_include
 *
 * Revision 1.8  2000/03/23 01:02:40  roddey
 * Updates to the XMLURL class to correct a lot of parsing problems
 * and to add support for the port number. Updated the URL tests
 * to test some of this new stuff.
 *
 * Revision 1.7  2000/03/02 19:55:49  roddey
 * This checkin includes many changes done while waiting for the
 * 1.1.0 code to be finished. I can't list them all here, but a list is
 * available elsewhere.
 *
 * Revision 1.6  2000/02/17 18:51:10  roddey
 * Added a new test for pruning leading whitespace
 *
 * Revision 1.5  2000/02/06 07:48:39  rahulj
 * Year 2K copyright swat.
 *
 * Revision 1.4  2000/01/19 00:59:06  roddey
 * Get rid of dependence on old utils output streams.
 *
 * Revision 1.3  2000/01/12 00:17:48  roddey
 * Removed validator tests temporarily, since they have changed and the tests need
 * to be rewritten. Added new tests for the new URL class.
 *
 * Revision 1.2  1999/12/07 23:11:01  roddey
 * Add in some new tests for transcoders and update the URL tests
 * a bit.
 *
 * Revision 1.1.1.1  1999/11/09 01:02:05  twl
 * Initial checkin
 *
 * Revision 1.2  1999/11/08 20:42:28  rahul
 * Swat for adding in Product name and CVS comment log variable.
 *
 */

// ---------------------------------------------------------------------------
//  XML4C2 includes
// ---------------------------------------------------------------------------
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/util/XMLURL.hpp>
#include "CoreTests.hpp"



// ---------------------------------------------------------------------------
//  Local testing functions
// ---------------------------------------------------------------------------

//
//  This test just sets up a list of variations on URLs and parses them. The
//  URL list includes the expected text for each of the parts that should be
//  parsed out of the URL.
//
struct BasicTestEntry
{
    const XMLCh*        orgURL;
    const XMLCh*        fullText;
    XMLURL::Protocols   protocol;
    unsigned int        portNum;
    const XMLCh*        fragment;
    const XMLCh*        host;
    const XMLCh*        path;
    const XMLCh*        password;
    const XMLCh*        query;
    const XMLCh*        user;
};

static bool checkAField(const   XMLCh* const test
                        , const XMLCh* const expected
                        , const XMLCh* const fieldName)
{
    if (!test && !expected)
        return true;

    if (!test && expected)
    {
        XERCES_STD_QUALIFIER wcout << L"Expected value for the " << fieldName
                   << " field was not present" << XERCES_STD_QUALIFIER endl;
        return false;
    }
     else if (test && !expected)
    {
        XERCES_STD_QUALIFIER wcout << L"The value '" << test << L"' for the " << fieldName
                   << L" was not expected" << XERCES_STD_QUALIFIER endl;
        return false;
    }
     else if (XMLString::compareString(test, expected))
    {
        XERCES_STD_QUALIFIER wcout  << L"Expected: " << expected << L", but got: " << test
                    << XERCES_STD_QUALIFIER endl;
        return false;
    }
    return true;
}

static bool checkBasicResult(const  XMLURL&         testURL
                            , const BasicTestEntry& testInfo)
{
    //
    //  Check each part to insure that its what its supposed to be. Since
    //  any of them can be a null pointer, we have a little helper function
    //  that spits out the actual testing code for each one.
    //
    if (!checkAField(testURL.getURLText(), testInfo.fullText, L"Full Text"))
        return false;

    if (!checkAField(testURL.getFragment(), testInfo.fragment, L"Fragment"))
        return false;

    if (!checkAField(testURL.getHost(), testInfo.host, L"Host"))
        return false;

    if (testURL.getPortNum() != testInfo.portNum)
    {
        XERCES_STD_QUALIFIER wcout << L"Expected port number: " << testInfo.portNum
                   << L" but got: " << testURL.getPortNum() << XERCES_STD_QUALIFIER endl;
        return false;
    }

    if (!checkAField(testURL.getPath(), testInfo.path, L"Path"))
        return false;

    if (!checkAField(testURL.getPassword(), testInfo.password, L"Password"))
        return false;

    if (!checkAField(testURL.getQuery(), testInfo.query, L"Query"))
        return false;

    if (!checkAField(testURL.getUser(), testInfo.user, L"User"))
        return false;

    return true;
}

static bool basicURLTest()
{
    static BasicTestEntry testList[] =
    {
        {
            L"file://user:password@host/path1/path2/file.txt?query#fragment"
            , L"file://user:password@host/path1/path2/file.txt?query#fragment"
            , XMLURL::File
            , 0
            , L"fragment"
            , L"host"
            , L"/path1/path2/file.txt"
            , L"password"
            , L"query"
            , L"user"
        }
      , {
            L"file:///path2/file.txt?query#fragment"
            , L"file:///path2/file.txt?query#fragment"
            , XMLURL::File
            , 0
            , L"fragment"
            , 0
            , L"/path2/file.txt"
            , 0
            , L"query"
            , 0
        }
      , {
            L"#fragment"
            , L"#fragment"
            , XMLURL::Unknown
            , 0
            , L"fragment"
            , 0
            , 0
            , 0
            , 0
            , 0
        }
      , {
            L"file://user@host/path1/path2/file.txt#fragment"
            , L"file://user@host/path1/path2/file.txt#fragment"
            , XMLURL::File
            , 0
            , L"fragment"
            , L"host"
            , L"/path1/path2/file.txt"
            , 0
            , 0
            , L"user"
        }
      , {
            L"     file://user@host/path1/path2/file.txt#fragment"
            , L"file://user@host/path1/path2/file.txt#fragment"
            , XMLURL::File
            , 0
            , L"fragment"
            , L"host"
            , L"/path1/path2/file.txt"
            , 0
            , 0
            , L"user"
        }
      , {
            L"http://host:90/path1/path2/file.txt"
            , L"http://host:90/path1/path2/file.txt"
            , XMLURL::HTTP
            , 90
            , 0
            , L"host"
            , L"/path1/path2/file.txt"
            , 0
            , 0
            , 0
        }
      , {
            L"http://host/path1/path2/file.txt"
            , L"http://host/path1/path2/file.txt"
            , XMLURL::HTTP
            , 80
            , 0
            , L"host"
            , L"/path1/path2/file.txt"
            , 0
            , 0
            , 0
        }
      , {
            L"ftp://"
            , L"ftp://"
            , XMLURL::FTP
            , 21
            , 0
            , 0
            , 0
            , 0
            , 0
            , 0
        }
      , {
            L"ftp://user@"
            , L"ftp://user@"
            , XMLURL::FTP
            , 21
            , 0
            , 0
            , 0
            , 0
            , 0
            , L"user"
        }
    };
    const unsigned int testCount = sizeof(testList) / sizeof(testList[0]);

    bool retVal = true;

    //
    //  Do a run where we construct the URL over and over for each
    //  test.
    //
    unsigned int index;
    for (index = 0; index < testCount; index++)
    {
        // Force full destruction each time
        {
            XMLURL testURL(testList[index].orgURL);

            // Call the comparison function
            if (!checkBasicResult(testURL, testList[index]))
                retVal = false;
        }
    }

    //
    //  Do a run where we use a single URL object and just reset it over
    //  and over again.
    //
    XMLURL testURL;
    for (index = 0; index < testCount; index++)
    {
        testURL.setURL(testList[index].orgURL);

        // Call the comparison function
        if (!checkBasicResult(testURL, testList[index]))
            retVal = false;
    }

    return retVal;
}


//
//  This test makes sure that parsing one URL relative to another works
//  correctly. The tests used here come from one of the internet RFCs on
//  generic URI syntax. A single base URL is created, then a number of
//  relative URLs are parsed against it and the results compared to the
//  expected result.
//
static bool relativeURLTest()
{
    static struct TestEntry
    {
        const XMLCh*    relative;
        const XMLCh*    result;
    } testList[] =
    {
        { L"g"      , L"http://a/b/c/g" }
      , { L"./g"    , L"http://a/b/c/g" }
      , { L"g/"     , L"http://a/b/c/g/" }
      , { L"/g"     , L"http://a/g" }
      , { L"?y"     , L"http://a/b/c/?y" }
      , { L"g?y"    , L"http://a/b/c/g?y" }
      , { L"#s"     , L"http://a/b/c/d;p#s" }
      , { L"g#s"    , L"http://a/b/c/g#s" }
      , { L"g?y#s"  , L"http://a/b/c/g?y#s" }
      , { L";x"     , L"http://a/b/c/;x" }
      , { L"g;x"    , L"http://a/b/c/g;x" }
      , { L"g;x?y#s", L"http://a/b/c/g;x?y#s" }
      , { L"."      , L"http://a/b/c/" }
      , { L"./"     , L"http://a/b/c/" }
      , { L".."     , L"http://a/b/" }
      , { L"../"    , L"http://a/b/" }
      , { L"../g"   , L"http://a/b/g" }
      , { L"../.."  , L"http://a/" }
      , { L"../../" , L"http://a/" }
      , { L"../../g", L"http://a/g" }
    };
    const unsigned int testCount = sizeof(testList) / sizeof(testList[0]);

    // This is the base URL against which the tests are run
    XMLURL baseURL(L"http://a/b/c/d;p?q");

    bool retVal = true;
    for (unsigned int index = 0; index < testCount; index++)
    {
        XMLURL testURL(baseURL, testList[index].relative);

        if (XMLString::compareString(testURL.getURLText(), testList[index].result))
        {
            XERCES_STD_QUALIFIER wcout  << L"Expected URL: " << testList[index].result
                        << L" but got: " << testURL.getURLText() << XERCES_STD_QUALIFIER endl;
            retVal = false;
        }
    }
    return retVal;
};


// ---------------------------------------------------------------------------
//  Test entry point
// ---------------------------------------------------------------------------
bool testURL()
{
    XERCES_STD_QUALIFIER wcout  << L"----------------------------------\n"
                << L"Testing URL class \n"
                << L"----------------------------------"
                << XERCES_STD_QUALIFIER endl;

    bool retVal = true;
    try
    {
        // Call other local methods to do specific tests
        XERCES_STD_QUALIFIER wcout << L"Testing basic URL parsing" << XERCES_STD_QUALIFIER endl;
        if (!basicURLTest())
        {
            XERCES_STD_QUALIFIER wcout << L"Basic URL parsing tests failed" << XERCES_STD_QUALIFIER endl;
            retVal = false;
        }
         else
        {
            XERCES_STD_QUALIFIER wcout << L"Basic URL parsing tests passed" << XERCES_STD_QUALIFIER endl;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;

        XERCES_STD_QUALIFIER wcout << L"Testing relative URL parsing" << XERCES_STD_QUALIFIER endl;
        if (!relativeURLTest())
        {
            XERCES_STD_QUALIFIER wcout << L"Relative URL parsing tests failed" << XERCES_STD_QUALIFIER endl;
            retVal = false;
        }
         else
        {
            XERCES_STD_QUALIFIER wcout << L"Relative URL parsing tests passed" << XERCES_STD_QUALIFIER endl;
        }
        XERCES_STD_QUALIFIER wcout << XERCES_STD_QUALIFIER endl;
    }

    catch(const XMLException& toCatch)
    {
        XERCES_STD_QUALIFIER wcout  << L"  ERROR: Unexpected exception!\n   Msg: "
                    << toCatch.getMessage() << XERCES_STD_QUALIFIER endl;
        return false;
    }
    return retVal;
}
