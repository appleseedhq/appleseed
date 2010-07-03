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
 * Revision 1.17  2004/09/08 13:55:34  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.16  2004/09/02 14:59:30  cargilld
 * Add OutOfMemoryException block to samples.
 *
 * Revision 1.15  2003/05/30 09:36:36  gareth
 * Use new macros for iostream.h and std:: issues.
 *
 * Revision 1.14  2003/05/16 15:32:12  knoaman
 * Change scope of 'src' so that it's deallocated properly.
 *
 * Revision 1.13  2002/02/01 22:41:37  peiyongz
 * sane_include
 *
 * Revision 1.12  2001/10/25 15:18:33  tng
 * delete the parser before XMLPlatformUtils::Terminate.
 *
 * Revision 1.11  2001/10/19 19:02:43  tng
 * [Bug 3909] return non-zero an exit code when error was encounted.
 * And other modification for consistent help display and return code across samples.
 *
 * Revision 1.10  2001/08/01 19:11:01  tng
 * Add full schema constraint checking flag to the samples and the parser.
 *
 * Revision 1.9  2001/05/11 13:24:59  tng
 * Copyright update.
 *
 * Revision 1.8  2001/05/03 16:00:32  tng
 * Schema: samples update with schema
 *
 * Revision 1.7  2001/02/22 20:59:57  tng
 * [Bug 678] StdInParse doesn't output filename or duration
 *
 * Revision 1.6  2000/06/20 02:23:10  rahulj
 * Help message added by Joe Polastre.
 *
 * Revision 1.5  2000/03/02 19:53:50  roddey
 * This checkin includes many changes done while waiting for the
 * 1.1.0 code to be finished. I can't list them all here, but a list is
 * available elsewhere.
 *
 * Revision 1.4  2000/02/11 02:40:58  abagchi
 * Removed StrX::transcode
 *
 * Revision 1.3  2000/02/06 07:47:25  rahulj
 * Year 2K copyright swat.
 *
 * Revision 1.2  2000/01/12 00:27:01  roddey
 * Updates to work with the new URL and input source scheme.
 *
 * Revision 1.1.1.1  1999/11/09 01:09:27  twl
 * Initial checkin
 *
 * Revision 1.5  1999/11/08 20:43:43  rahul
 * Swat for adding in Product name and CVS comment log variable.
 *
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/framework/StdInInputSource.hpp>
#include <xercesc/parsers/SAXParser.hpp>
#include "StdInParse.hpp"
#include <xercesc/util/OutOfMemoryException.hpp>

// ---------------------------------------------------------------------------
//  Local data
//
//  doNamespaces
//      Indicates whether namespace processing should be enabled or not.
//      The default is no, but -n overrides that.
//
//  doSchema
//      Indicates whether schema processing should be enabled or not.
//      The default is no, but -s overrides that.
//
//  schemaFullChecking
//      Indicates whether full schema constraint checking should be enabled or not.
//      The default is no, but -s overrides that.
//
//  valScheme
//      Indicates what validation scheme to use. It defaults to 'auto', but
//      can be set via the -v= command.
// ---------------------------------------------------------------------------
static bool     doNamespaces       = false;
static bool     doSchema           = false;
static bool     schemaFullChecking = false;
static SAXParser::ValSchemes    valScheme       = SAXParser::Val_Auto;



// ---------------------------------------------------------------------------
//  Local helper methods
// ---------------------------------------------------------------------------
void usage()
{
    XERCES_STD_QUALIFIER cout << "\nUsage:\n"
            "    StdInParse [options] < <XML file>\n\n"
            "This program demonstrates streaming XML data from standard\n"
            "input.  It then uses the SAX Parser, and prints the\n"
            "number of elements, attributes, spaces and characters found\n"
            "in the input, using SAX API.\n\n"
            "Options:\n"
            "    -v=xxx      Validation scheme [always | never | auto*].\n"
            "    -n          Enable namespace processing. Defaults to off.\n"
            "    -s          Enable schema processing. Defaults to off.\n"
            "    -f          Enable full schema constraint checking. Defaults to off.\n"
		      "    -?          Show this help.\n\n"
            "  * = Default if not provided explicitly.\n"
         << XERCES_STD_QUALIFIER endl;
}


// ---------------------------------------------------------------------------
//  Program entry point
// ---------------------------------------------------------------------------
int main(int argC, char* argV[])
{
    // Initialize the XML4C system
    try
    {
         XMLPlatformUtils::Initialize();
    }

    catch (const XMLException& toCatch)
    {
         XERCES_STD_QUALIFIER cerr << "Error during initialization! Message:\n"
              << StrX(toCatch.getMessage()) << XERCES_STD_QUALIFIER endl;
         return 1;
    }

    int parmInd;
    for (parmInd = 1; parmInd < argC; parmInd++)
    {
        // Break out on first parm not starting with a dash
        if (argV[parmInd][0] != '-')
            break;

        // Watch for special case help request
        if (!strcmp(argV[parmInd], "-?"))
        {
            usage();
            XMLPlatformUtils::Terminate();
            return 2;
        }
         else if (!strncmp(argV[parmInd], "-v=", 3)
              ||  !strncmp(argV[parmInd], "-V=", 3))
        {
            const char* const parm = &argV[parmInd][3];

            if (!strcmp(parm, "never"))
                valScheme = SAXParser::Val_Never;
            else if (!strcmp(parm, "auto"))
                valScheme = SAXParser::Val_Auto;
            else if (!strcmp(parm, "always"))
                valScheme = SAXParser::Val_Always;
            else
            {
                XERCES_STD_QUALIFIER cerr << "Unknown -v= value: " << parm << XERCES_STD_QUALIFIER endl;
                XMLPlatformUtils::Terminate();
                return 2;
            }
        }
         else if (!strcmp(argV[parmInd], "-n")
              ||  !strcmp(argV[parmInd], "-N"))
        {
            doNamespaces = true;
        }
         else if (!strcmp(argV[parmInd], "-s")
              ||  !strcmp(argV[parmInd], "-S"))
        {
            doSchema = true;
        }
         else if (!strcmp(argV[parmInd], "-f")
              ||  !strcmp(argV[parmInd], "-F"))
        {
            schemaFullChecking = true;
        }
         else
        {
            XERCES_STD_QUALIFIER cerr << "Unknown option '" << argV[parmInd]
                 << "', ignoring it\n" << XERCES_STD_QUALIFIER endl;
        }
    }

    //
    //  Create a SAX parser object. Then, according to what we were told on
    //  the command line, set the options.
    //
    SAXParser* parser = new SAXParser;
    parser->setValidationScheme(valScheme);
    parser->setDoNamespaces(doNamespaces);
    parser->setDoSchema(doSchema);
    parser->setValidationSchemaFullChecking(schemaFullChecking);


    //
    //  Create our SAX handler object and install it on the parser, as the
    //  document and error handler. We are responsible for cleaning them
    //  up, but since its just stack based here, there's nothing special
    //  to do.
    //
    StdInParseHandlers handler;
    parser->setDocumentHandler(&handler);
    parser->setErrorHandler(&handler);

    unsigned long duration;
    int errorCount = 0;
    // create a faux scope so that 'src' destructor is called before
    // XMLPlatformUtils::Terminate
    {
        //
        //  Kick off the parse and catch any exceptions. Create a standard
        //  input input source and tell the parser to parse from that.
        //
        StdInInputSource src;
        try
        {
            const unsigned long startMillis = XMLPlatformUtils::getCurrentMillis();
            parser->parse(src);
            const unsigned long endMillis = XMLPlatformUtils::getCurrentMillis();
            duration = endMillis - startMillis;
            errorCount = parser->getErrorCount();
        }
        catch (const OutOfMemoryException&)
        {
            XERCES_STD_QUALIFIER cerr << "OutOfMemoryException" << XERCES_STD_QUALIFIER endl;
            errorCount = 2;
            return 4;
        }
        catch (const XMLException& e)
        {
            XERCES_STD_QUALIFIER cerr << "\nError during parsing: \n"
                 << StrX(e.getMessage())
                 << "\n" << XERCES_STD_QUALIFIER endl;
            errorCount = 1;
            return 4;
        }

        // Print out the stats that we collected and time taken
        if (!errorCount) {
            XERCES_STD_QUALIFIER cout << StrX(src.getSystemId()) << ": " << duration << " ms ("
                 << handler.getElementCount() << " elems, "
                 << handler.getAttrCount() << " attrs, "
                 << handler.getSpaceCount() << " spaces, "
                 << handler.getCharacterCount() << " chars)" << XERCES_STD_QUALIFIER endl;
        }
    }

    //
    //  Delete the parser itself.  Must be done prior to calling Terminate, below.
    //
    delete parser;

    XMLPlatformUtils::Terminate();

    if (errorCount > 0)
        return 4;
    else
        return 0;
}

