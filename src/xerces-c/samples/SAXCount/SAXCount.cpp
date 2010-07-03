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
* Revision 1.30  2004/09/08 13:55:33  peiyongz
* Apache License Version 2.0
*
* Revision 1.29  2004/09/02 14:59:29  cargilld
* Add OutOfMemoryException block to samples.
*
* Revision 1.28  2003/05/30 09:36:36  gareth
* Use new macros for iostream.h and std:: issues.
*
* Revision 1.27  2002/12/10 13:34:42  tng
* Samples minor update in usage information.
*
* Revision 1.26  2002/11/08 16:19:05  peiyongz
* no message
*
* Revision 1.25  2002/11/07 18:31:04  peiyongz
* command line option for "locale"
*
* Revision 1.24  2002/11/04 14:09:16  tng
* [Bug 14201] use of ios::nocreate breaks build.
*
* Revision 1.23  2002/11/01 22:05:44  tng
* Samples/Test update: Issue error if the list file failed to open.
*
* Revision 1.22  2002/09/27 19:25:10  tng
* Samples Fix: wrong length in memset
*
* Revision 1.21  2002/07/17 18:58:36  tng
* samples update: for testing special encoding purpose.
*
* Revision 1.20  2001/11/13 13:22:35  tng
* SAXCount fix: restore previous feature to accept multiple input files.
*
* Revision 1.19  2001/10/29 17:02:57  tng
* Fix typo in samples.
*
* Revision 1.18  2001/10/25 15:18:33  tng
* delete the parser before XMLPlatformUtils::Terminate.
*
* Revision 1.17  2001/10/19 19:02:43  tng
* [Bug 3909] return non-zero an exit code when error was encounted.
* And other modification for consistent help display and return code across samples.
*
* Revision 1.16  2001/08/15 12:41:04  tng
* Initialize the fURI array to zeros, in case, some compilers like AIX xlC_r doesn't reset the memory.
*
* Revision 1.15  2001/08/08 12:12:32  tng
* Print the file name only if doList is on.
*
* Revision 1.14  2001/08/03 15:08:17  tng
* close the list file.
*
* Revision 1.13  2001/08/02 17:10:29  tng
* Allow DOMCount/SAXCount/IDOMCount/SAX2Count to take a file that has a list of xml file as input.
*
* Revision 1.12  2001/08/01 19:11:01  tng
* Add full schema constraint checking flag to the samples and the parser.
*
* Revision 1.11  2001/05/11 13:24:57  tng
* Copyright update.
*
* Revision 1.10  2001/05/03 16:00:12  tng
* Schema: samples update with schema
*
* Revision 1.9  2000/10/19 23:52:41  andyh
* SAXCount: Allow multiple files on command line
*
* Revision 1.8  2000/06/16 20:25:38  rahulj
* Add the -v=always option to force validation checking. Need this
* option for running the conformance tests.
*
* Revision 1.7  2000/05/31 18:39:59  rahulj
* 'Auto' validation is the default processing mode.
*
* Revision 1.6  2000/05/09 00:22:29  andyh
* Memory Cleanup.  XMLPlatformUtils::Terminate() deletes all lazily
* allocated memory; memory leak checking tools will no longer report
* that leaks exist.  (DOM GetElementsByTagID temporarily removed
* as part of this.)
*
* Revision 1.5  2000/03/03 01:29:31  roddey
* Added a scanReset()/parseReset() method to the scanner and
* parsers, to allow for reset after early exit from a progressive parse.
* Added calls to new Terminate() call to all of the samples. Improved
* documentation in SAX and DOM parsers.
*
* Revision 1.4  2000/03/02 19:53:47  roddey
* This checkin includes many changes done while waiting for the
* 1.1.0 code to be finished. I can't list them all here, but a list is
* available elsewhere.
*
* Revision 1.3  2000/02/11 02:39:10  abagchi
* Removed StrX::transcode
*
* Revision 1.2  2000/02/06 07:47:23  rahulj
* Year 2K copyright swat.
*
* Revision 1.1.1.1  1999/11/09 01:09:30  twl
* Initial checkin
*
* Revision 1.7  1999/11/08 20:43:40  rahul
* Swat for adding in Product name and CVS comment log variable.
*
*/


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include "SAXCount.hpp"
#if defined(XERCES_NEW_IOSTREAMS)
#include <fstream>
#else
#include <fstream.h>
#endif
#include <xercesc/util/OutOfMemoryException.hpp>

// ---------------------------------------------------------------------------
//  Local helper methods
// ---------------------------------------------------------------------------
void usage()
{
    XERCES_STD_QUALIFIER cout << "\nUsage:\n"
            "    SAXCount [options] <XML file | List file>\n\n"
            "This program invokes the SAX Parser, and then prints the\n"
            "number of elements, attributes, spaces and characters found\n"
            "in each XML file, using SAX API.\n\n"
            "Options:\n"
            "    -l          Indicate the input file is a List File that has a list of xml files.\n"
            "                Default to off (Input file is an XML file).\n"
            "    -v=xxx      Validation scheme [always | never | auto*].\n"
            "    -n          Enable namespace processing. Defaults to off.\n"
            "    -s          Enable schema processing. Defaults to off.\n"
            "    -f          Enable full schema constraint checking. Defaults to off.\n"
            "    -locale=ll_CC specify the locale, default: en_US.\n"
		    "    -?          Show this help.\n\n"
            "  * = Default if not provided explicitly.\n"
         << XERCES_STD_QUALIFIER endl;
}


// ---------------------------------------------------------------------------
//  Program entry point
// ---------------------------------------------------------------------------
int main(int argC, char* argV[])
{

    // Check command line and extract arguments.
    if (argC < 2)
    {
        usage();
        return 1;
    }

    const char*              xmlFile = 0;
    SAXParser::ValSchemes    valScheme = SAXParser::Val_Auto;
    bool                     doNamespaces       = false;
    bool                     doSchema           = false;
    bool                     schemaFullChecking = false;
    bool                     doList = false;
    bool                     errorOccurred = false;
    bool                     recognizeNEL = false;
    char                     localeStr[64];
    memset(localeStr, 0, sizeof localeStr);

    int argInd;
    for (argInd = 1; argInd < argC; argInd++)
    {
        // Break out on first parm not starting with a dash
        if (argV[argInd][0] != '-')
            break;

        // Watch for special case help request
        if (!strcmp(argV[argInd], "-?"))
        {
            usage();
            return 2;
        }
         else if (!strncmp(argV[argInd], "-v=", 3)
              ||  !strncmp(argV[argInd], "-V=", 3))
        {
            const char* const parm = &argV[argInd][3];

            if (!strcmp(parm, "never"))
                valScheme = SAXParser::Val_Never;
            else if (!strcmp(parm, "auto"))
                valScheme = SAXParser::Val_Auto;
            else if (!strcmp(parm, "always"))
                valScheme = SAXParser::Val_Always;
            else
            {
                XERCES_STD_QUALIFIER cerr << "Unknown -v= value: " << parm << XERCES_STD_QUALIFIER endl;
                return 2;
            }
        }
         else if (!strcmp(argV[argInd], "-n")
              ||  !strcmp(argV[argInd], "-N"))
        {
            doNamespaces = true;
        }
         else if (!strcmp(argV[argInd], "-s")
              ||  !strcmp(argV[argInd], "-S"))
        {
            doSchema = true;
        }
         else if (!strcmp(argV[argInd], "-f")
              ||  !strcmp(argV[argInd], "-F"))
        {
            schemaFullChecking = true;
        }
         else if (!strcmp(argV[argInd], "-l")
              ||  !strcmp(argV[argInd], "-L"))
        {
            doList = true;
        }
         else if (!strcmp(argV[argInd], "-special:nel"))
        {
            // turning this on will lead to non-standard compliance behaviour
            // it will recognize the unicode character 0x85 as new line character
            // instead of regular character as specified in XML 1.0
            // do not turn this on unless really necessary
             recognizeNEL = true;
        }
         else if (!strncmp(argV[argInd], "-locale=", 8))
        {
             // Get out the end of line
             strcpy(localeStr, &(argV[argInd][8]));
        }			
        else
        {
            XERCES_STD_QUALIFIER cerr << "Unknown option '" << argV[argInd]
                << "', ignoring it\n" << XERCES_STD_QUALIFIER endl;
        }
    }

    //
    //  There should at least one parameter left, and that
    //  should be the file name(s).
    //
    if (argInd == argC)
    {
        usage();
        return 1;
    }

    // Initialize the XML4C2 system
    try
    {
        if (strlen(localeStr))
        {
            XMLPlatformUtils::Initialize(localeStr);
        }
        else
        {
            XMLPlatformUtils::Initialize();
        }

        if (recognizeNEL)
        {
            XMLPlatformUtils::recognizeNEL(recognizeNEL);
        }
    }

    catch (const XMLException& toCatch)
    {
        XERCES_STD_QUALIFIER cerr << "Error during initialization! Message:\n"
            << StrX(toCatch.getMessage()) << XERCES_STD_QUALIFIER endl;
        return 1;
    }

    //
    //  Create a SAX parser object. Then, according to what we were told on
    //  the command line, set it to validate or not.
    //
    SAXParser* parser = new SAXParser;

    parser->setValidationScheme(valScheme);
    parser->setDoNamespaces(doNamespaces);
    parser->setDoSchema(doSchema);
    parser->setValidationSchemaFullChecking(schemaFullChecking);

    //
    //  Create our SAX handler object and install it on the parser, as the
    //  document and error handler.
    //
    SAXCountHandlers handler;
    parser->setDocumentHandler(&handler);
    parser->setErrorHandler(&handler);


    //
    //  Get the starting time and kick off the parse of the indicated
    //  file. Catch any exceptions that might propogate out of it.
    //
    unsigned long duration;

    XERCES_STD_QUALIFIER ifstream fin;

    // the input is a list file
    if (doList)
        fin.open(argV[argInd]);

    if (fin.fail()) {
        XERCES_STD_QUALIFIER cerr <<"Cannot open the list file: " << argV[argInd] << XERCES_STD_QUALIFIER endl;
        return 2;
    }

    while (true)
    {
        char fURI[1000];
        //initialize the array to zeros
        memset(fURI,0,sizeof(fURI));

        if (doList) {
            if (! fin.eof() ) {
                fin.getline (fURI, sizeof(fURI));
                if (!*fURI)
                    continue;
                else {
                    xmlFile = fURI;
                    XERCES_STD_QUALIFIER cerr << "==Parsing== " << xmlFile << XERCES_STD_QUALIFIER endl;
                }
            }
            else
                break;
        }
        else {
            if (argInd < argC)
            {
                 xmlFile = argV[argInd];
                 argInd++;
            }
            else
                break;
        }

        //reset error count first
        handler.resetErrors();

        try
        {
            const unsigned long startMillis = XMLPlatformUtils::getCurrentMillis();
            parser->parse(xmlFile);
            const unsigned long endMillis = XMLPlatformUtils::getCurrentMillis();
            duration = endMillis - startMillis;
        }
        catch (const OutOfMemoryException&)
        {
            XERCES_STD_QUALIFIER cerr << "OutOfMemoryException" << XERCES_STD_QUALIFIER endl;
            errorOccurred = true;
            continue;
        }
        catch (const XMLException& e)
        {
            XERCES_STD_QUALIFIER cerr << "\nError during parsing: '" << xmlFile << "'\n"
                << "Exception message is:  \n"
                << StrX(e.getMessage()) << "\n" << XERCES_STD_QUALIFIER endl;
            errorOccurred = true;
            continue;
        }

        catch (...)
        {
            XERCES_STD_QUALIFIER cerr << "\nUnexpected exception during parsing: '" << xmlFile << "'\n";
            errorOccurred = true;
            continue;
        }


        // Print out the stats that we collected and time taken
        if (!handler.getSawErrors())
        {
            XERCES_STD_QUALIFIER cout << xmlFile << ": " << duration << " ms ("
                << handler.getElementCount() << " elems, "
                << handler.getAttrCount() << " attrs, "
                << handler.getSpaceCount() << " spaces, "
                << handler.getCharacterCount() << " chars)" << XERCES_STD_QUALIFIER endl;
        }
        else
            errorOccurred = true;
    }

    if (doList)
        fin.close();

    //
    //  Delete the parser itself.  Must be done prior to calling Terminate, below.
    //
    delete parser;

    // And call the termination method
    XMLPlatformUtils::Terminate();

    if (errorOccurred)
        return 4;
    else
        return 0;

}

