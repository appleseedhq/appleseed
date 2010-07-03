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
 * Revision 1.18  2004/09/08 13:55:32  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.17  2004/09/02 14:59:28  cargilld
 * Add OutOfMemoryException block to samples.
 *
 * Revision 1.16  2003/09/12 18:14:19  neilg
 * enable MemParse to work on OS400; thanks to Jay Hansen.
 *
 * Revision 1.15  2003/08/07 21:21:38  neilg
 * fix segmentation faults that may arise when the parser throws exceptions during document parsing.  In general, XMLPlatformUtils::Terminate() should not be called from within a catch statement.
 *
 * Revision 1.14  2003/05/30 09:36:35  gareth
 * Use new macros for iostream.h and std:: issues.
 *
 * Revision 1.13  2002/02/01 22:37:14  peiyongz
 * sane_include
 *
 * Revision 1.12  2001/10/25 15:18:33  tng
 * delete the parser before XMLPlatformUtils::Terminate.
 *
 * Revision 1.11  2001/10/19 18:56:08  tng
 * Pulled the hardcoded "encoding" out of the document itself and made it a #define
 * to make it easier to support other encodings.  Patch from David McCreedy.
 * And other modification for consistent help display and return code across samples.
 *
 * Revision 1.10  2001/08/01 19:11:01  tng
 * Add full schema constraint checking flag to the samples and the parser.
 *
 * Revision 1.9  2001/05/11 13:24:55  tng
 * Copyright update.
 *
 * Revision 1.8  2001/05/03 15:59:40  tng
 * Schema: samples update with schema
 *
 * Revision 1.7  2000/09/11 18:43:48  aruna1
 * OS390 related updates
 *
 * Revision 1.6  2000/03/02 19:53:42  roddey
 * This checkin includes many changes done while waiting for the
 * 1.1.0 code to be finished. I can't list them all here, but a list is
 * available elsewhere.
 *
 * Revision 1.5  2000/02/11 02:37:01  abagchi
 * Removed StrX::transcode
 *
 * Revision 1.4  2000/02/06 07:47:19  rahulj
 * Year 2K copyright swat.
 *
 * Revision 1.3  2000/01/12 00:27:00  roddey
 * Updates to work with the new URL and input source scheme.
 *
 * Revision 1.2  1999/11/20 01:09:55  rahulj
 * Fixed usage message.
 *
 * Revision 1.1.1.1  1999/11/09 01:09:49  twl
 * Initial checkin
 *
 * Revision 1.7  1999/11/08 20:43:36  rahul
 * Swat for adding in Product name and CVS comment log variable.
 *
 */


/**
 * This sample program illustrates how one can use a memory buffer as the
 * input to parser. The memory buffer contains raw bytes representing XML
 * statements.
 *
 * Look at the API documentation for 'MemBufInputSource' for more information
 * on parameters to the constructor.
 *
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include "MemParse.hpp"
#include <xercesc/util/OutOfMemoryException.hpp>

// ---------------------------------------------------------------------------
//  Local const data
//
//  gXMLInMemBuf
//      Defines the memory buffer contents here which parsed by the XML
//      parser. This is the cheap way to do it, instead of reading it from
//      somewhere. For this demo, its fine.
//
//      NOTE: If your encoding is not ascii you will need to change
//            the MEMPARSE_ENCODING #define
//
//  gMemBufId
//      A simple name to give as the system id for the memory buffer. This
//      just for indentification purposes in case of errors. Its not a real
//      system id (and the parser knows that.)
// ---------------------------------------------------------------------------

#ifndef MEMPARSE_ENCODING
   #if defined(OS390)
      #define MEMPARSE_ENCODING "ibm-1047-s390"
   #elif defined(OS400)
      #define MEMPARSE_ENCODING "ibm037"
   #else
      #define MEMPARSE_ENCODING "ascii"
   #endif
#endif /* ifndef MEMPARSE_ENCODING */

static const char*  gXMLInMemBuf =
"\
<?xml version='1.0' encoding='" MEMPARSE_ENCODING "'?>\n\
<!DOCTYPE company [\n\
<!ELEMENT company     (product,category,developedAt)>\n\
<!ELEMENT product     (#PCDATA)>\n\
<!ELEMENT category    (#PCDATA)>\n\
<!ATTLIST category idea CDATA #IMPLIED>\n\
<!ELEMENT developedAt (#PCDATA)>\n\
]>\n\n\
<company>\n\
    <product>XML4C</product>\n\
    <category idea='great'>XML Parsing Tools</category>\n\
    <developedAt>\n\
      IBM Center for Java Technology, Silicon Valley, Cupertino, CA\n\
    </developedAt>\n\
</company>\
";

static const char*  gMemBufId = "prodInfo";



// ---------------------------------------------------------------------------
//  Local helper methods
// ---------------------------------------------------------------------------
void usage()
{
    XERCES_STD_QUALIFIER cout << "\nUsage:\n"
            "    MemParse [options]\n\n"
            "This program uses the SAX Parser to parse a memory buffer\n"
            "containing XML statements, and reports the number of\n"
            "elements and attributes found.\n\n"
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
    // Initialize the XML4C2 system
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

    SAXParser::ValSchemes    valScheme = SAXParser::Val_Auto;
    bool doNamespaces       = false;
    bool doSchema           = false;
    bool schemaFullChecking = false;

    int argInd;
    for (argInd = 1; argInd < argC; argInd++)
    {
        // Break out on first parm not starting with a dash
        if (argV[argInd][0] != '-')
        {
            usage();
            XMLPlatformUtils::Terminate();
            return 1;
        }

        // Watch for special case help request
        if (!strcmp(argV[argInd], "-?"))
        {
            usage();
            XMLPlatformUtils::Terminate();
            return 1;
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
         else
        {
            XERCES_STD_QUALIFIER cerr << "Unknown option '" << argV[argInd]
                 << "', ignoring it\n" << XERCES_STD_QUALIFIER endl;
        }
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
    //  document and error handlers.
    //
    MemParseHandlers handler;
    parser->setDocumentHandler(&handler);
    parser->setErrorHandler(&handler);

    //
    //  Create MemBufferInputSource from the buffer containing the XML
    //  statements.
    //
    //  NOTE: We are using strlen() here, since we know that the chars in
    //  our hard coded buffer are single byte chars!!! The parameter wants
    //  the number of BYTES, not chars, so when you create a memory buffer
    //  give it the byte size (which just happens to be the same here.)
    //
    MemBufInputSource* memBufIS = new MemBufInputSource
    (
        (const XMLByte*)gXMLInMemBuf
        , strlen(gXMLInMemBuf)
        , gMemBufId
        , false
    );

    //
    //  Get the starting time and kick off the parse of the indicated
    //  file. Catch any exceptions that might propogate out of it.
    //
    unsigned long duration;
    int errorCount = 0;
    int errorCode = 0;
    try
    {
        const unsigned long startMillis = XMLPlatformUtils::getCurrentMillis();
        parser->parse(*memBufIS);
        const unsigned long endMillis = XMLPlatformUtils::getCurrentMillis();
        duration = endMillis - startMillis;
        errorCount = parser->getErrorCount();
    }
    catch (const OutOfMemoryException&)
    {
        XERCES_STD_QUALIFIER cerr << "OutOfMemoryException" << XERCES_STD_QUALIFIER endl;
        errorCode = 5;
    }
    catch (const XMLException& e)
    {
        XERCES_STD_QUALIFIER cerr << "\nError during parsing memory stream:\n"
             << "Exception message is:  \n"
             << StrX(e.getMessage()) << "\n" << XERCES_STD_QUALIFIER endl;
        errorCode = 4;
    }
    if(errorCode) {
        XMLPlatformUtils::Terminate();
        return errorCode;
    }

    // Print out the stats that we collected and time taken.
    if (!errorCount) {
        XERCES_STD_QUALIFIER cout << "\nFinished parsing the memory buffer containing the following "
             << "XML statements:\n\n"
             << gXMLInMemBuf
             << "\n\n\n"
             << "Parsing took " << duration << " ms ("
             << handler.getElementCount() << " elements, "
             << handler.getAttrCount() << " attributes, "
             << handler.getSpaceCount() << " spaces, "
             << handler.getCharacterCount() << " characters).\n" << XERCES_STD_QUALIFIER endl;
    }

    //
    //  Delete the parser itself.  Must be done prior to calling Terminate, below.
    //
    delete parser;

    delete memBufIS;

    // And call the termination method
    XMLPlatformUtils::Terminate();

    if (errorCount > 0)
        return 4;
    else
        return 0;
}

