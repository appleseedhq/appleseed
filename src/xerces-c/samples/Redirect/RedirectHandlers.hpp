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
 * Revision 1.9  2005/04/05 15:16:36  cargilld
 * Update Redirect sample to use newer entity resolver callback.
 *
 * Revision 1.8  2004/09/08 13:55:33  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.7  2004/02/15 19:43:15  amassari
 * Removed cause for warnings in VC 7.1
 *
 * Revision 1.6  2002/11/05 21:46:19  tng
 * Explicit code using namespace in application.
 *
 * Revision 1.5  2002/11/04 15:23:03  tng
 * C++ Namespace Support.
 *
 * Revision 1.4  2002/02/01 22:38:26  peiyongz
 * sane_include
 *
 * Revision 1.3  2000/03/02 19:53:46  roddey
 * This checkin includes many changes done while waiting for the
 * 1.1.0 code to be finished. I can't list them all here, but a list is
 * available elsewhere.
 *
 * Revision 1.2  2000/02/06 07:47:22  rahulj
 * Year 2K copyright swat.
 *
 * Revision 1.1.1.1  1999/11/09 01:09:38  twl
 * Initial checkin
 *
 * Revision 1.6  1999/11/08 20:43:39  rahul
 * Swat for adding in Product name and CVS comment log variable.
 *
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/XMLEntityResolver.hpp>

XERCES_CPP_NAMESPACE_USE

XERCES_CPP_NAMESPACE_BEGIN
class AttributeList;
XERCES_CPP_NAMESPACE_END


class RedirectHandlers : public HandlerBase, public XMLEntityResolver
{
public:
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
    RedirectHandlers();
    ~RedirectHandlers();


    // -----------------------------------------------------------------------
    //  Getter methods
    // -----------------------------------------------------------------------
    unsigned int getElementCount()
    {
        return fElementCount;
    }

    unsigned int getAttrCount()
    {
        return fAttrCount;
    }

    unsigned int getCharacterCount()
    {
        return fCharacterCount;
    }

    unsigned int getSpaceCount()
    {
        return fSpaceCount;
    }


    // -----------------------------------------------------------------------
    //  Handlers for the SAX DocumentHandler interface
    // -----------------------------------------------------------------------
    void startElement(const XMLCh* const name, AttributeList& attributes);
    void characters(const XMLCh* const chars, const unsigned int length);
    void ignorableWhitespace(const XMLCh* const chars, const unsigned int length);
    void resetDocument();


    // -----------------------------------------------------------------------
    //  Handlers for the SAX ErrorHandler interface
    // -----------------------------------------------------------------------
    void warning(const SAXParseException& exc);
    void error(const SAXParseException& exc);
    void fatalError(const SAXParseException& exc);


#if 0
    // This is the old resolveEntity interface
    // -----------------------------------------------------------------------
    //  Handlers for the SAX EntityResolver interface
    // -----------------------------------------------------------------------
    InputSource* resolveEntity
    (
        const   XMLCh* const    publicId
        , const XMLCh* const    systemId
    );
#endif
    // -----------------------------------------------------------------------
    //  Handlers for the XMLEntityResolver interface
    // -----------------------------------------------------------------------
    InputSource* resolveEntity
    (
        XMLResourceIdentifier* resourceIdentifier
    );

private:
    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fAttrCount
    //  fCharacterCount
    //  fElementCount
    //  fSpaceCount
    //      These are just counters that are run upwards based on the input
    //      from the document handlers.
    // -----------------------------------------------------------------------
    unsigned int    fAttrCount;
    unsigned int    fCharacterCount;
    unsigned int    fElementCount;
    unsigned int    fSpaceCount;
};
