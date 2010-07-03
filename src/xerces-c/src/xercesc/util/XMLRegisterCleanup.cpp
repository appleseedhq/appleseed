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
 * $Id: XMLRegisterCleanup.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XMLRegisterCleanup.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// This is a mutex for exclusive use by this class
extern XMLMutex* gXMLCleanupListMutex;

// This is the head of a list of XMLRegisterCleanup objects that
// is used during XMLPlatformUtils::Terminate() to find objects to
// clean up
extern XMLRegisterCleanup* gXMLCleanupList;

void XMLRegisterCleanup::doCleanup()
{
    // When performing cleanup, we only do this once, but we can
    // cope if somehow we have been called twice.
    if (m_cleanupFn)
        m_cleanupFn();

    // We need to remove "this" from the list
    // irregardless of the cleanup Function
    unregisterCleanup();
}

// This function is called during initialisation of static data to
// register a function to be called on XMLPlatformUtils::Terminate.
// It gives an object that uses static data an opportunity to reset
// such data.
void XMLRegisterCleanup::registerCleanup(XMLCleanupFn cleanupFn)
{
    // Store the cleanup function
    m_cleanupFn = cleanupFn;
		
    // Add this object to the list head, if it is not already
    // present - which it shouldn't be.
    // This is done under a mutex to ensure thread safety.
    gXMLCleanupListMutex->lock();

    if (!m_nextCleanup && !m_prevCleanup) 
    {
        m_nextCleanup = gXMLCleanupList;
        gXMLCleanupList = this;

        if (m_nextCleanup)
            m_nextCleanup->m_prevCleanup = this;
    }

    gXMLCleanupListMutex->unlock();

}

// This function can be called either from XMLPlatformUtils::Terminate
// to state that the cleanup has been performed and should not be
// performed again, or from code that you have written that determines
// that cleanup is no longer necessary.
void XMLRegisterCleanup::unregisterCleanup()
{
    gXMLCleanupListMutex->lock();

    //
    // To protect against some compiler's (eg hp11) optimization
    // to change "this" as they update gXMLCleanupList
    //
    // refer to
    // void XMLPlatformUtils::Terminate()
    //       ...
    //       while (gXMLCleanupList)
    //            gXMLCleanupList->doCleanup();
    //

    XMLRegisterCleanup *tmpThis = (XMLRegisterCleanup*) this;

    // Unlink this object from the cleanup list
    if (m_nextCleanup) 
        m_nextCleanup->m_prevCleanup = m_prevCleanup;
		
    if (!m_prevCleanup) 
        gXMLCleanupList = m_nextCleanup;
    else 
        m_prevCleanup->m_nextCleanup = m_nextCleanup;

    gXMLCleanupListMutex->unlock();
		
    // Reset the object to the default state
    tmpThis->resetCleanup();

}

// The default constructor sets a state that ensures that this object
// will do nothing
XMLRegisterCleanup::XMLRegisterCleanup()
{
	resetCleanup();
}

// This function reinitialises the object to the default state
void XMLRegisterCleanup::resetCleanup() 
{
	m_nextCleanup = 0;
	m_prevCleanup = 0;
	m_cleanupFn = 0;
}

XERCES_CPP_NAMESPACE_END
