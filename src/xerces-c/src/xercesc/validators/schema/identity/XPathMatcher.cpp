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
 * $Id: XPathMatcher.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/validators/schema/identity/XPathMatcher.hpp>
#include <xercesc/validators/schema/identity/XercesXPath.hpp>
#include <xercesc/validators/schema/SchemaElementDecl.hpp>
#include <xercesc/validators/schema/SchemaAttDef.hpp>
#include <xercesc/validators/schema/SchemaSymbols.hpp>
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>

XERCES_CPP_NAMESPACE_BEGIN

typedef JanitorMemFunCall<XPathMatcher>     CleanupType;

// ---------------------------------------------------------------------------
//  XPathMatcher: Constructors and Destructor
// ---------------------------------------------------------------------------
XPathMatcher::XPathMatcher( XercesXPath* const xpath
                          , MemoryManager* const manager)
    : fLocationPathSize(0)
    , fMatched(0)
    , fNoMatchDepth(0)
    , fCurrentStep(0)
    , fStepIndexes(0)
    , fLocationPaths(0)
    , fIdentityConstraint(0)
    , fMemoryManager(manager)
{
    CleanupType cleanup(this, &XPathMatcher::cleanUp);

    try {
        init(xpath);
    }
    catch(const OutOfMemoryException&)
    {
        cleanup.release();

        throw;
    }

    cleanup.release();
}


XPathMatcher::XPathMatcher(XercesXPath* const xpath,
                           IdentityConstraint* const ic,
						   MemoryManager* const manager)
    : fLocationPathSize(0)
    , fMatched(0)
    , fNoMatchDepth(0)
    , fCurrentStep(0)
    , fStepIndexes(0)
    , fLocationPaths(0)
    , fIdentityConstraint(ic)
    , fMemoryManager(manager)
{
    CleanupType cleanup(this, &XPathMatcher::cleanUp);

    try {
        init(xpath);
    }
    catch(const OutOfMemoryException&)
    {
        cleanup.release();

        throw;
    }

    cleanup.release();
}


XPathMatcher::~XPathMatcher()
{
    cleanUp();
}

// ---------------------------------------------------------------------------
//  XPathMatcher: Helper methods
// ---------------------------------------------------------------------------
void XPathMatcher::init(XercesXPath* const xpath) {

    if (xpath) {

        fLocationPaths = xpath->getLocationPaths();
        fLocationPathSize = (fLocationPaths ? fLocationPaths->size() : 0);

        if (fLocationPathSize) {

            fStepIndexes = new (fMemoryManager) RefVectorOf<ValueStackOf<int> >(fLocationPathSize, true, fMemoryManager);
            fCurrentStep = (int*) fMemoryManager->allocate
            (
                fLocationPathSize * sizeof(int)
            );//new int[fLocationPathSize];
            fNoMatchDepth = (int*) fMemoryManager->allocate
            (
                fLocationPathSize * sizeof(int)
            );//new int[fLocationPathSize];
            fMatched = (int*) fMemoryManager->allocate
            (
                fLocationPathSize * sizeof(int)
            );//new int[fLocationPathSize];

            for(unsigned int i=0; i < fLocationPathSize; i++) {
                fStepIndexes->addElement(new (fMemoryManager) ValueStackOf<int>(8, fMemoryManager));
            }
        }
    }
}


// ---------------------------------------------------------------------------
//  XPathMatcher: XMLDocumentHandler methods
// ---------------------------------------------------------------------------
void XPathMatcher::startDocumentFragment() {

    for(unsigned int i = 0; i < fLocationPathSize; i++) {

        fStepIndexes->elementAt(i)->removeAllElements();
        fCurrentStep[i] = 0;
        fNoMatchDepth[i] = 0;
        fMatched[i] = 0;
    }
}

void XPathMatcher::startElement(const XMLElementDecl& elemDecl,
                                const unsigned int urlId,
                                const XMLCh* const elemPrefix,
								const RefVectorOf<XMLAttr>& attrList,
                                const unsigned int attrCount) {

    for (int i = 0; i < (int) fLocationPathSize; i++) {

        // push context
        int startStep = fCurrentStep[i];
        fStepIndexes->elementAt(i)->push(startStep);

        // try next xpath, if not matching
        if ((fMatched[i] & XP_MATCHED_D) == XP_MATCHED || fNoMatchDepth[i] > 0) {
            fNoMatchDepth[i]++;
            continue;
        }

        if((fMatched[i] & XP_MATCHED_D) == XP_MATCHED_D) {
            fMatched[i] = XP_MATCHED_DP;
        }

        // consume self::node() steps
        XercesLocationPath* locPath = fLocationPaths->elementAt(i);
        int stepSize = locPath->getStepSize();

        while (fCurrentStep[i] < stepSize &&
               locPath->getStep(fCurrentStep[i])->getAxisType() == XercesStep::SELF) {
            fCurrentStep[i]++;
        }

        if (fCurrentStep[i] == stepSize) {

            fMatched[i] = XP_MATCHED;
            continue;
        }

        // now if the current step is a descendant step, we let the next
        // step do its thing; if it fails, we reset ourselves
        // to look at this step for next time we're called.
        // so first consume all descendants:
        int descendantStep = fCurrentStep[i];

        while (fCurrentStep[i] < stepSize &&
               locPath->getStep(fCurrentStep[i])->getAxisType() == XercesStep::DESCENDANT) {
            fCurrentStep[i]++;
        }

        bool sawDescendant = fCurrentStep[i] > descendantStep;
        if (fCurrentStep[i] == stepSize) {

            fNoMatchDepth[i]++;
            continue;
        }

        // match child::... step, if haven't consumed any self::node()
        if ((fCurrentStep[i] == startStep || fCurrentStep[i] > descendantStep) &&
            locPath->getStep(fCurrentStep[i])->getAxisType() == XercesStep::CHILD) {

            XercesStep* step = locPath->getStep(fCurrentStep[i]);
            XercesNodeTest* nodeTest = step->getNodeTest();

            if (nodeTest->getType() == XercesNodeTest::QNAME) {

                QName elemQName(elemPrefix, elemDecl.getElementName()->getLocalPart(), urlId, fMemoryManager);

//                if (!(*(nodeTest->getName()) == *(elemDecl.getElementName()))) {
                if (!(*(nodeTest->getName()) == elemQName)) {

                    if(fCurrentStep[i] > descendantStep) {
                        fCurrentStep[i] = descendantStep;
                        continue;
                    }

                    fNoMatchDepth[i]++;
                    continue;
                }
            }

            fCurrentStep[i]++;
        }

        if (fCurrentStep[i] == stepSize) {

            if (sawDescendant) {

                fCurrentStep[i] = descendantStep;
                fMatched[i] = XP_MATCHED_D;
            }
            else {
                fMatched[i] = XP_MATCHED;
            }

            continue;
        }

        // match attribute::... step
        if (fCurrentStep[i] < stepSize &&
            locPath->getStep(fCurrentStep[i])->getAxisType() == XercesStep::ATTRIBUTE) {

            if (attrCount) {

                XercesNodeTest* nodeTest = locPath->getStep(fCurrentStep[i])->getNodeTest();

                for (unsigned int attrIndex = 0; attrIndex < attrCount; attrIndex++) {

                    const XMLAttr* curDef = attrList.elementAt(attrIndex);

                    if (nodeTest->getType() != XercesNodeTest::QNAME ||
                        (*(nodeTest->getName()) == *(curDef->getAttName()))) {

                        fCurrentStep[i]++;

                        if (fCurrentStep[i] == stepSize) {

                            fMatched[i] = XP_MATCHED_A;
                            int j=0;

                            for(; j<i && ((fMatched[j] & XP_MATCHED) != XP_MATCHED); j++) ;

                            if(j == i) {

                                SchemaAttDef* attDef = ((SchemaElementDecl&) elemDecl).getAttDef(curDef->getName(), curDef->getURIId());
                                DatatypeValidator* dv = (attDef) ? attDef->getDatatypeValidator() : 0;
                                matched(curDef->getValue(), dv, false);
                            }
                        }
                        break;
                    }
                }
            }

            if ((fMatched[i] & XP_MATCHED) != XP_MATCHED) {

                if(fCurrentStep[i] > descendantStep) {

                    fCurrentStep[i] = descendantStep;
                    continue;
                }

                fNoMatchDepth[i]++;
            }
        }
    }
}

void XPathMatcher::endElement(const XMLElementDecl& elemDecl,
                              const XMLCh* const elemContent) {

    for(int i = 0; i < (int) fLocationPathSize; i++) {

        // go back a step
        fCurrentStep[i] = fStepIndexes->elementAt(i)->pop();

        // don't do anything, if not matching
        if (fNoMatchDepth[i] > 0) {
            fNoMatchDepth[i]--;
        }
        // signal match, if appropriate
        else {

            int j=0;
            for(; j<i && ((fMatched[j] & XP_MATCHED) != XP_MATCHED); j++) ;

            if (j < i || (fMatched[j] == 0)
                || ((fMatched[j] & XP_MATCHED_A) == XP_MATCHED_A))
				continue;

            DatatypeValidator* dv = ((SchemaElementDecl*) &elemDecl)->getDatatypeValidator();
            bool isNillable = (((SchemaElementDecl *) &elemDecl)->getMiscFlags() & SchemaSymbols::XSD_NILLABLE) != 0;

            matched(elemContent, dv, isNillable);
            fMatched[i] = 0;
        }
    }
}


// ---------------------------------------------------------------------------
//  XPathMatcher: Match methods
// ---------------------------------------------------------------------------
int XPathMatcher::isMatched() {

    // xpath has been matched if any one of the members of the union have matched.
    for (int i=0; i < (int) fLocationPathSize; i++) {
        if (((fMatched[i] & XP_MATCHED) == XP_MATCHED)
            && ((fMatched[i] & XP_MATCHED_DP) != XP_MATCHED_DP))
            return fMatched[i];
    }

    return 0;
}

void XPathMatcher::matched(const XMLCh* const,
                           DatatypeValidator* const,
                           const bool) {
    return;
}


// ---------------------------------------------------------------------------
//  XPathMatcher: Match methods
// ---------------------------------------------------------------------------
int XPathMatcher::getInitialDepth() const
{
    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // to make some compilers happy
}

XERCES_CPP_NAMESPACE_END

/**
  * End of file XPathMatcher.cpp
  */

