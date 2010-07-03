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
 * Revision 1.9  2004/09/08 13:57:05  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.8  2003/05/30 13:08:26  gareth
 * move over to macros for std:: and iostream/iostream.h issues.
 *
 * Revision 1.7  2003/02/05 18:55:21  tng
 * [Bug 11915] Utility for freeing memory.
 *
 * Revision 1.6  2000/03/02 19:55:46  roddey
 * This checkin includes many changes done while waiting for the
 * 1.1.0 code to be finished. I can't list them all here, but a list is
 * available elsewhere.
 *
 * Revision 1.5  2000/02/06 07:48:37  rahulj
 * Year 2K copyright swat.
 *
 * Revision 1.4  2000/01/24 20:38:56  roddey
 * Fixed some small bugs introduced in the recent move away from the util/* streams.
 *
 * Revision 1.3  2000/01/21 23:58:06  roddey
 * Initial move away from util streams was bad. Wide char APIs didn't allow enough
 * control to do canonical output, so changed to use std short char APIs.
 *
 * Revision 1.1  1999/11/09 01:02:17  twl
 * Initial revision
 *
 * Revision 1.3  1999/11/08 20:42:25  rahul
 * Swat for adding in Product name and CVS comment log variable.
 *
 */

// ---------------------------------------------------------------------------
//  Some types used by this program
// ---------------------------------------------------------------------------
enum OutputTypes
{
    OutputType_None
    , OutputType_Debug
    , OutputType_JCCanon
    , OutputType_SunCanon
    , OutputType_XML
};



// ---------------------------------------------------------------------------
//  Includes that everyone uses inside this program
// ---------------------------------------------------------------------------
#include "ParserTest_Parser.hpp"
#include <stdlib.h>
#include <xercesc/util/PlatformUtils.hpp>

#if defined(XERCES_NEW_IOSTREAMS)
#include <iostream>
#else
#include <iostream.h>
#endif


// ---------------------------------------------------------------------------
//  This is a simple class that lets us do easy (though not terribly efficient)
//  trancoding of XMLCh data to local code page for display.
// ---------------------------------------------------------------------------
class StrX
{
public :
    // -----------------------------------------------------------------------
    //  Constructors and Destructor
    // -----------------------------------------------------------------------
	StrX(const XMLCh* const toTranscode, const unsigned int len = 0) :

        fLocalForm(0)
    {
        // Call the private transcoding method
        transcode(toTranscode, len);
    }

    ~StrX()
    {
        XMLString::release(&fLocalForm);
    }


    // -----------------------------------------------------------------------
    //  Getter methods
    // -----------------------------------------------------------------------
    const char* localForm() const
    {
        return fLocalForm;
    }


private :
    // -----------------------------------------------------------------------
    //  Private helper methods
    // -----------------------------------------------------------------------
	void transcode (const XMLCh* const toTranscode, const unsigned int len);


    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fLocalForm
    //      This is the local code page form of the string.
    // -----------------------------------------------------------------------
    char*   fLocalForm;
};

inline XERCES_STD_QUALIFIER ostream& operator<<(XERCES_STD_QUALIFIER ostream& target, const StrX& toDump)
{
    target << toDump.localForm();
    return target;
}
