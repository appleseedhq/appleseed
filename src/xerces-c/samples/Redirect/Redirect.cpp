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


/**
 *
 * This simplistic sample illustrates how an XML application can use
 * the SAX entityResolver handler to provide customized handling for
 * external entities.
 *
 * It registers an entity resolver with the parser. When ever the parser
 * comes across an external entity, like a reference to an external DTD
 * file, it calls the 'resolveEntity()' callback. This callback in this
 * sample checks to see if the external entity to be resolved is the file
 * 'personal.dtd'.
 *
 * If it is then, it redirects the input stream to the file 'redirect.dtd',
 * which is then read instead of 'personal.dtd'.
 *
 * If the external entity to be resolved was not the file 'personal.dtd', the
 * callback returns NULL indicating that do the default behaviour which is
 * to read the contents of 'personal.dtd'.
 *
 * $Log$
 * Revision 1.12  2005/04/05 15:16:36  cargilld
 * Update Redirect sample to use newer entity resolver callback.
 *
 * Revision 1.11  2004/09/08 13:55:33  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.10  2004/09/02 14:59:29  cargilld
 * Add OutOfMemoryException block to samples.
 *
 * Revision 1.9  2003/08/07 21:21:38  neilg
 * fix segmentation faults that may arise when the parser throws exceptions during document parsing.  In general, XMLPlatformUtils::Terminate() should not be called from within a catch statement.
 *
 * Revision 1.8  2003/05/30 09:36:35  gareth
 * Use new macros for iostream.h and std:: issues.
 *
 * Revision 1.7  2002/02/01 22:38:26  peiyongz
 * sane_include
 *
 * Revision 1.6  2001/10/25 15:18:33  tng
 * delete the parser before XMLPlatformUtils::Terminate.
 *
 * Revision 1.5  2001/10/19 19:02:42  tng
 * [Bug 3909] return non-zero an exit code when error was encounted.
 * And other modification for consistent help display and return code across samples.
 *
 * Revision 1.4  2000/05/31 18:53:15  rahulj
 * Removed extraneous command line arguments.
 *
 * Revision 1.3  2000/02/11 02:38:28  abagchi
 * Removed StrX::transcode
 *
 * Revision 1.2  2000/02/06 07:47:21  rahulj
 * Year 2K copyright swat.
 *
 * Revision 1.1.1.1  1999/11/09 01:09:37  twl
 * Initial checkin
 *
 * Revision 1.6  1999/11/08 20:43:39  rahul
 * Swat for adding in Product name and CVS comment log variable.
 *
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/parsers/SAXParser.hpp>
#include "Redirect.hpp"
#include <xercesc/util/OutOfMemoryException.hpp>

// ---------------------------------------------------------------------------
//  Local helper methods
// ---------------------------------------------------------------------------
void usage()
{
    XERCES_STD_QUALIFIER cout << "\nUsage:\n"
            "    Redirect <XML file>\n\n"
            "This program installs an entity resolver, traps the call to\n"
            "the external DTD file and redirects it to another application\n"
            "specific file which contains the actual dtd.\n\n"
            "The program then counts and reports the number of elements and\n"
            "attributes in the given XML file.\n"
         << XERCES_STD_QUALIFIER endl;
}


// ---------------------------------------------------------------------------
//  Program entry point
// ---------------------------------------------------------------------------
int main(int argc, char* args[])
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

    // We only have one parameter, which is the file to process
    // We only have one required parameter, which is the file to process
    if ((argc != 2) || (*(args[1]) == '-'))
    {
        usage();
        XMLPlatformUtils::Terminate();
        return 1;
    }

    const char*              xmlFile = args[1];

    //
    //  Create a SAX parser object. Then, according to what we were told on
    //  the command line, set it to validate or not.
    //
    SAXParser* parser = new SAXParser;

    //
    //  Create our SAX handler object and install it on the parser, as the
    //  document, entity and error handlers.
    //
    RedirectHandlers handler;
    parser->setDocumentHandler(&handler);
    parser->setErrorHandler(&handler);
    //Use the new XML Entity Resolver
    //parser->setEntityResolver(&handler);
    parser->setXMLEntityResolver(&handler);    

    //
    //  Get the starting time and kick off the parse of the indicated file.
    //  Catch any exceptions that might propogate out of it.
    //
    unsigned long duration;
    int errorCount = 0;
    int errorCode = 0;
    try
    {
        const unsigned long startMillis = XMLPlatformUtils::getCurrentMillis();
        parser->parse(xmlFile);
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
        XERCES_STD_QUALIFIER cerr << "\nError during parsing: '" << xmlFile << "'\n"
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
        XERCES_STD_QUALIFIER cout << xmlFile << ": " << duration << " ms ("
             << handler.getElementCount() << " elems, "
             << handler.getAttrCount() << " attrs, "
             << handler.getSpaceCount() << " spaces, "
             << handler.getCharacterCount() << " chars)" << XERCES_STD_QUALIFIER endl;
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

