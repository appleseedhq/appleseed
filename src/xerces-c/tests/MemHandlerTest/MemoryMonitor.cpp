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
 * Revision 1.7  2004/09/08 13:57:05  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.6  2004/09/02 15:11:42  cargilld
 * Add OutOfMemoryException block to tests.
 *
 * Revision 1.5  2004/02/15 19:43:04  amassari
 * Removed cause for warnings in VC 7.1
 *
 * Revision 1.4  2003/06/03 21:42:42  neilg
 * update name of test in usage info
 *
 * Revision 1.3  2003/06/03 14:00:34  neilg
 * attempt to make VC6 happy.
 *
 * Revision 1.2  2003/06/03 13:50:44  neilg
 * make Forte compiler happy
 *
 * Revision 1.1  2003/06/02 22:22:26  neilg
 * new test for the pluggable memory management mechanism.  This tests all 4 common Xerces parsers (SAX, SAX2, DOMBuilder and DeprecatedDOM) and ensures all allocated memory is dallocated, and that the manager which created the memory is called to deallocate it.
 *
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include "MemoryMonitor.hpp"
#if defined(XERCES_NEW_IOSTREAMS)
#include <iostream>
#include <fstream>
#else
#include <iostream.h>
#include <fstream.h>
#endif
#include <assert.h>
#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>

void* MemoryMonitor::allocate(size_t size)
{
    void *key = ::operator new(size);
    fHashTable->put(key, (unsigned int)size);
    return key;
}

void MemoryMonitor::deallocate(void* p)
{
    // if fHashTable doesn't contain p, then this memory manager
    // didn't allocate that memory--a segfault waiting to happen...
    assert(p == 0 || fHashTable->get(p) != 0);
    if (p != 0)
        fHashTable->removeKey(p);
    ::operator delete(p);
}

unsigned int MemoryMonitor::getTotalMemory() 
{
    unsigned int total = 0;
    ValueHashTableOfEnumerator<unsigned int> *memEnum = 
            new ValueHashTableOfEnumerator<unsigned int>(fHashTable);
    while(memEnum->hasMoreElements()) {
        total += memEnum->nextElement();
    }
    delete memEnum;
    return total;
}

static void usage()
{
    XERCES_STD_QUALIFIER cout << "\nUsage:\n"
            "    MemHandlerTest [options] <XML file | List file>\n\n"
            "This program invokes the XercesDOMParser, DOMBuilder, SAXParser ,\n"
            "and the SAX2XMLReader, and ensures that MemoryManagers set on these\n"
            "domBuilders are called to delete just as many bytes as they allocate.\n"
            "This is done for each XML file, and each file is processed\n"
            "as many times as indicated.\n"
            "Options:\n"
            "    -l          Indicate the input file is a List File that has a list of xml files.\n"
            "                Default to off (Input file is an XML file).\n"
            "    -v=xxx      Validation scheme [always | never | auto*].\n"
            "    -n          Enable namespace processing. Defaults to off.\n"
            "    -s          Enable schema processing. Defaults to off.\n"
            "    -f          Enable full schema constraint checking. Defaults to off.\n"
            "    -r=n        Run file through domBuilders n times.\n" 
		    "    -?          Show this help.\n\n"
            "  * = Default if not provided explicitly.\n"
         << XERCES_STD_QUALIFIER endl;
}

class DOMBuilderHandler : public DOMErrorHandler 
{
public:
    DOMBuilderHandler() {};
    ~DOMBuilderHandler() {};
    bool handleError(const DOMError &error) 
    {
        char *message = 0;
        XERCES_STD_QUALIFIER cerr << "Error occurred in DOMBuilder!  Message:  " << 
            (message = XMLString::transcode(error.getMessage())) << " of severity " << error.getSeverity() << "." << XERCES_STD_QUALIFIER endl;
        XMLString::release(&message);
        return true;
    }
};

class SAXErrorHandler : public ErrorHandler 
{
public:
    SAXErrorHandler() {};
    ~SAXErrorHandler() {};
    void warning(const SAXParseException &exc )
    {
        char *message = 0;
        XERCES_STD_QUALIFIER cerr << "SAX warning received!  Text:  " <<
            (message = XMLString::transcode(exc.getMessage())) << "." << XERCES_STD_QUALIFIER endl;
        XMLString::release(&message);
    }
    void error(const SAXParseException &exc )
    {
        char *message = 0;
        XERCES_STD_QUALIFIER cerr << "SAX error received!  Text:  " <<
            (message = XMLString::transcode(exc.getMessage())) << "." << XERCES_STD_QUALIFIER endl;
        XMLString::release(&message);
    }
    void fatalError(const SAXParseException &exc )
    {
        char *message = 0;
        XERCES_STD_QUALIFIER cerr << "SAX fatalError received!  Text:  " <<
            (message = XMLString::transcode(exc.getMessage())) << "." << XERCES_STD_QUALIFIER endl;
        XMLString::release(&message);
    }

    // no state so no body
    void resetErrors() {};
};

/**
 * This utility takes similar parameters as DOMCount,
 * with similar meanings.  The only difference is that it runs
 * the file(s) in question through a DOMParser, a DOMBuilder, a SAXParser and
 * a SAX2XMLReader, setting options as appropriate.  It does this
 * sequentially, n times per file with a single domBuilder
 * object, and reports what it finds in terms of memory
 * allocations/deallocations.
 */

int main (int argC,  char *argV[]) 
{

    MemoryMonitor *staticMemMonitor = new MemoryMonitor();

    // Initialize the XML4C system
    try
    {
        XMLPlatformUtils::Initialize(XMLUni::fgXercescDefaultLocale, 0, 0, staticMemMonitor);
    }
    catch (const XMLException& toCatch)
    {
         char *msg = XMLString::transcode(toCatch.getMessage());
         XERCES_STD_QUALIFIER cerr << "Error during initialization! :\n"
              << msg << XERCES_STD_QUALIFIER endl;
         XMLString::release(&msg);
         return 1;
    }

    // Check command line and extract arguments.
    if (argC < 2)
    {
        usage();
        return 1;
    }

    const char*                xmlFile = 0;
    AbstractDOMParser::ValSchemes domBuilderValScheme = AbstractDOMParser::Val_Auto;
    bool                       doNamespaces       = false;
    bool                       doSchema           = false;
    bool                       schemaFullChecking = false;
    bool                       doList = false;
    bool                       errorOccurred = false;
    int                        numReps =1;

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
                domBuilderValScheme = AbstractDOMParser::Val_Never;
            else if (!strcmp(parm, "auto"))
                domBuilderValScheme = AbstractDOMParser::Val_Auto;
            else if (!strcmp(parm, "always"))
                domBuilderValScheme = AbstractDOMParser::Val_Always;
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
         else if (!strncmp(argV[argInd], "-r=", 3)
              ||  !strncmp(argV[argInd], "-R=", 3))
        {
            const char* const numStr = &argV[argInd][3];
            XMLCh* numXStr = XMLString::transcode(numStr);
            numReps = XMLString::parseInt(numXStr);
            XMLString::release(&numXStr);
        }
         else
        {
            XERCES_STD_QUALIFIER cerr << "Unknown option '" << argV[argInd]
                 << "', ignoring it\n" << XERCES_STD_QUALIFIER endl;
        }
    }

    //
    //  There should be only one and only one parameter left, and that
    //  should be the file name.
    //
    if (argInd != argC - 1)
    {
        usage();
        return 1;
    }

    // Instantiate the DOM domBuilder with its memory manager.
    MemoryMonitor *domBuilderMemMonitor = new MemoryMonitor();
    static const XMLCh gLS[] = { chLatin_L, chLatin_S, chNull };
    DOMImplementation *impl = DOMImplementationRegistry::getDOMImplementation(gLS);
    DOMBuilder        *domBuilder = ((DOMImplementationLS*)impl)->createDOMBuilder(DOMImplementationLS::MODE_SYNCHRONOUS, 0, domBuilderMemMonitor);
    DOMBuilderHandler domBuilderHandler;
    domBuilder->setErrorHandler(&domBuilderHandler);

    // Instantiate the SAX2 domBuilder with its memory manager.
    MemoryMonitor *sax2MemMonitor = new MemoryMonitor();
    SAX2XMLReader *sax2parser = XMLReaderFactory::createXMLReader(sax2MemMonitor);
    SAXErrorHandler saxErrorHandler;
    sax2parser->setErrorHandler(&saxErrorHandler);

    // Instantiate the deprecated DOM parser with its memory manager.
    MemoryMonitor *depDOMMemMonitor = new MemoryMonitor();
    DOMParser *depDOMParser = new (depDOMMemMonitor)DOMParser(0, depDOMMemMonitor);
    depDOMParser->setErrorHandler(&saxErrorHandler);

    // Instantiate the SAX 1 parser with its memory manager.
    MemoryMonitor *sax1MemMonitor = new MemoryMonitor();
    SAXParser *saxParser = new (sax1MemMonitor) SAXParser(0, sax1MemMonitor);
    saxParser->setErrorHandler(&saxErrorHandler);

    // set features 
    domBuilder->setFeature(XMLUni::fgDOMNamespaces, doNamespaces);
    sax2parser->setFeature(XMLUni::fgSAX2CoreNameSpaces, doNamespaces);
    depDOMParser->setDoNamespaces(doNamespaces);
    saxParser->setDoNamespaces(doNamespaces);

    domBuilder->setFeature(XMLUni::fgXercesSchema, doSchema);
    sax2parser->setFeature(XMLUni::fgXercesSchema, doSchema);
    depDOMParser->setDoSchema(doSchema);
    saxParser->setDoSchema(doSchema);

    domBuilder->setFeature(XMLUni::fgXercesSchemaFullChecking, schemaFullChecking);
    sax2parser->setFeature(XMLUni::fgXercesSchemaFullChecking, schemaFullChecking);
    depDOMParser->setValidationSchemaFullChecking(schemaFullChecking);
    saxParser->setValidationSchemaFullChecking(schemaFullChecking);

    if (domBuilderValScheme == AbstractDOMParser::Val_Auto)
    {
        domBuilder->setFeature(XMLUni::fgDOMValidateIfSchema, true);
        sax2parser->setFeature(XMLUni::fgSAX2CoreValidation, true);
        sax2parser->setFeature(XMLUni::fgXercesDynamic, true);
        depDOMParser->setValidationScheme(DOMParser::Val_Auto);
        saxParser->setValidationScheme(SAXParser::Val_Auto);
    }
    else if (domBuilderValScheme == AbstractDOMParser::Val_Never)
    {
        domBuilder->setFeature(XMLUni::fgDOMValidation, false);
        sax2parser->setFeature(XMLUni::fgSAX2CoreValidation, false);
        depDOMParser->setValidationScheme(DOMParser::Val_Never);
        saxParser->setValidationScheme(SAXParser::Val_Never);
    }
    else if (domBuilderValScheme == AbstractDOMParser::Val_Always)
    {
        domBuilder->setFeature(XMLUni::fgDOMValidation, true);
        sax2parser->setFeature(XMLUni::fgSAX2CoreValidation, true);
        sax2parser->setFeature(XMLUni::fgXercesDynamic, false);
        depDOMParser->setValidationScheme(DOMParser::Val_Always);
        saxParser->setValidationScheme(SAXParser::Val_Always);
    }

    // enable datatype normalization - default is off
    domBuilder->setFeature(XMLUni::fgDOMDatatypeNormalization, true);

    XERCES_STD_QUALIFIER ifstream fin;
    bool more = true;

    // the input is a list file
    if (doList)
        fin.open(argV[argInd]);

    if (fin.fail()) {
        XERCES_STD_QUALIFIER cerr <<"Cannot open the list file: " << argV[argInd] << XERCES_STD_QUALIFIER endl;
        return 2;
    }

    while (more)
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
            xmlFile = argV[argInd];
            more = false;
        }

        // parse numReps times (in case we need it for some reason)
        for (int i=0; i<numReps; i++)
        {

            XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument *doc = 0;

            try
            {
                // reset document pool
                domBuilder->resetDocumentPool();

                doc = domBuilder->parseURI(xmlFile);
                sax2parser->parse(xmlFile);
                depDOMParser->parse(xmlFile);
                saxParser->parse(xmlFile);
            }
            catch (const OutOfMemoryException&)
            {
                XERCES_STD_QUALIFIER cerr << "OutOfMemoryException during parsing: '" << xmlFile << "'\n" << XERCES_STD_QUALIFIER endl;;
                continue;
            }
            catch (const XMLException& toCatch)
            {
                char *msg = XMLString::transcode(toCatch.getMessage()); 
                XERCES_STD_QUALIFIER cerr << "\nError during parsing: '" << xmlFile << "'\n"
                    << "Exception message is:  \n"
                    << msg << "\n" << XERCES_STD_QUALIFIER endl;
                XMLString::release(&msg);
                continue;
            }
            catch (const DOMException& toCatch)
            {
                const unsigned int maxChars = 2047;
                XMLCh errText[maxChars + 1];

                XERCES_STD_QUALIFIER cerr << "\nDOM Error during parsing: '" << xmlFile << "'\n"
                    << "DOMException code is:  " << toCatch.code << XERCES_STD_QUALIFIER endl;

                if (DOMImplementation::loadDOMExceptionMsg(toCatch.code, errText, maxChars))
                {
                    char * msg = XMLString::transcode(errText); 
                    XERCES_STD_QUALIFIER cerr << "Message is: " << msg << XERCES_STD_QUALIFIER endl;

                    continue;
                }
            }
            catch (...)
            {
                XERCES_STD_QUALIFIER cerr << "\nUnexpected exception during parsing: '" << xmlFile << "'\n";
                continue;
            }

        }
    }

    //
    //  Delete the domBuilder itself.  Must be done prior to calling Terminate, below.
    //
    domBuilder->release();
    delete sax2parser;
    delete depDOMParser;
    delete saxParser;

    XERCES_STD_QUALIFIER cout << "At destruction, domBuilderMemMonitor has " << domBuilderMemMonitor->getTotalMemory() << " bytes." << XERCES_STD_QUALIFIER endl;
    XERCES_STD_QUALIFIER cout << "At destruction, sax2MemMonitor has " << sax2MemMonitor->getTotalMemory() << " bytes." << XERCES_STD_QUALIFIER endl;
    XERCES_STD_QUALIFIER cout << "At destruction, depDOMMemMonitor has " << depDOMMemMonitor->getTotalMemory() << " bytes." << XERCES_STD_QUALIFIER endl;
    XERCES_STD_QUALIFIER cout << "At destruction, sax1MemMonitor has " << sax1MemMonitor->getTotalMemory() << " bytes." << XERCES_STD_QUALIFIER endl;
    delete domBuilderMemMonitor;
    delete sax2MemMonitor;
    delete depDOMMemMonitor;
    delete sax1MemMonitor;

    XMLPlatformUtils::Terminate();
    XERCES_STD_QUALIFIER cout << "At destruction, staticMemMonitor has " << staticMemMonitor->getTotalMemory() << " bytes." << XERCES_STD_QUALIFIER endl;
    delete staticMemMonitor;
    return 0;
}

