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
 * $Id: IdentityConstraintHandler.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include "IdentityConstraintHandler.hpp"

#include <xercesc/validators/schema/SchemaElementDecl.hpp>

#include <xercesc/validators/schema/identity/FieldActivator.hpp>
#include <xercesc/validators/schema/identity/ValueStore.hpp>
#include <xercesc/validators/schema/identity/IC_Selector.hpp>

#include <xercesc/util/OutOfMemoryException.hpp>

XERCES_CPP_NAMESPACE_BEGIN

typedef JanitorMemFunCall<IdentityConstraintHandler>    CleanupType;

// ---------------------------------------------------------------------------
//  IdentityConstraintHandler: Constructors and Destructor
// ---------------------------------------------------------------------------
IdentityConstraintHandler::IdentityConstraintHandler(XMLScanner*    const scanner
                     , MemoryManager* const manager)
: fScanner(scanner)
, fMemoryManager(manager)
, fMatcherStack(0)
, fValueStoreCache(0)
, fFieldActivator(0)
{
    CleanupType cleanup(this, &IdentityConstraintHandler::cleanUp);

    try {

        fMatcherStack    = new (fMemoryManager) XPathMatcherStack(fMemoryManager);
        fValueStoreCache = new (fMemoryManager) ValueStoreCache(fMemoryManager);
        fFieldActivator  = new (fMemoryManager) FieldActivator(fValueStoreCache, fMatcherStack, fMemoryManager);

        fValueStoreCache->setScanner(scanner);
    }
    catch(const OutOfMemoryException&)
    {
        cleanup.release();

        throw;
    }

    cleanup.release();
}

IdentityConstraintHandler::~IdentityConstraintHandler()
{
    cleanUp();
}

// ---------------------------------------------------------------------------
//  IdentityConstraintHandler:  methods
// ---------------------------------------------------------------------------
void IdentityConstraintHandler::deactivateContext(      SchemaElementDecl* const elem
                                                , const XMLCh*             const content)
{

    int oldCount = fMatcherStack->getMatcherCount();

    if (oldCount || elem->getIdentityConstraintCount()) 
    {

        for (int i = oldCount - 1; i >= 0; i--) 
        {
            XPathMatcher* matcher = fMatcherStack->getMatcherAt(i);
            matcher->endElement(*(elem), content);
        }

        if (fMatcherStack->size() > 0) 
        {
            fMatcherStack->popContext();
        }

        // handle everything *but* keyref's.
        int newCount = fMatcherStack->getMatcherCount();

        for (int j = oldCount - 1; j >= newCount; j--) 
        {
            XPathMatcher* matcher = fMatcherStack->getMatcherAt(j);
            IdentityConstraint* ic = matcher->getIdentityConstraint();

            if (ic  && (ic->getType() != IdentityConstraint::KEYREF))
                fValueStoreCache->transplant(ic, matcher->getInitialDepth());
        }

        // now handle keyref's...
        for (int k = oldCount - 1; k >= newCount; k--) 
        {
            XPathMatcher* matcher = fMatcherStack->getMatcherAt(k);
            IdentityConstraint* ic = matcher->getIdentityConstraint();

            if (ic && (ic->getType() == IdentityConstraint::KEYREF)) 
            {
                ValueStore* values = fValueStoreCache->getValueStoreFor(ic, matcher->getInitialDepth());

                if (values) { // nothing to do if nothing matched!
                    values->endDcocumentFragment(fValueStoreCache);
                }
            }
        }

        fValueStoreCache->endElement();

    }
}

void IdentityConstraintHandler::activateIdentityConstraint
                     (      
                             SchemaElementDecl* const     elem
                     ,       int                          elemDepth
                     , const unsigned int                 uriId
                     , const XMLCh*                 const elemPrefix
                     , const RefVectorOf<XMLAttr>&        attrList
                     , const unsigned int                 attrCount
                      )
{

    unsigned int count = elem->getIdentityConstraintCount();

    if (count || fMatcherStack->getMatcherCount()) 
    {

        fValueStoreCache->startElement();
        fMatcherStack->pushContext();
        fValueStoreCache->initValueStoresFor( elem, elemDepth);

        for (unsigned int i = 0; i < count; i++) 
        {
            activateSelectorFor(elem->getIdentityConstraintAt(i), elemDepth);
        }

        // call all active identity constraints
        count = fMatcherStack->getMatcherCount();

        for (unsigned int j = 0; j < count; j++) 
        {
            XPathMatcher* matcher = fMatcherStack->getMatcherAt(j);
            matcher->startElement(*elem, uriId, elemPrefix, attrList, attrCount);
        }
    }
}

void IdentityConstraintHandler::activateSelectorFor(      IdentityConstraint* const ic
                                   , const int                       initialDepth) 
{

    IC_Selector* selector = ic->getSelector();

    if (!selector)
        return;

    XPathMatcher* matcher = selector->createMatcher(fFieldActivator, initialDepth, fMemoryManager);

    fMatcherStack->addMatcher(matcher);
    matcher->startDocumentFragment();
}

// ---------------------------------------------------------------------------
//  IdentityConstraintHandler:  Getter methods
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//  IdentityConstraintHandler: cleanUp methods
// ---------------------------------------------------------------------------
void IdentityConstraintHandler::cleanUp() 
{
    if (fMatcherStack)
        delete fMatcherStack;

    if (fValueStoreCache)
        delete fValueStoreCache;

    if (fFieldActivator)
        delete fFieldActivator;

}

void IdentityConstraintHandler::reset()
{
    fValueStoreCache->startDocument();
    fMatcherStack->clear();
}

XERCES_CPP_NAMESPACE_END

/**
  * End of file IdentityConstraintHandler.cpp
  */

