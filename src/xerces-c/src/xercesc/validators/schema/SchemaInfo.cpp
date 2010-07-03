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
 * $Id: SchemaInfo.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/validators/schema/SchemaInfo.hpp>
#include <xercesc/validators/schema/XUtil.hpp>
#include <xercesc/validators/schema/SchemaSymbols.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/internal/ValidationContextImpl.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  SchemaInfo: Constructors and Destructor
// ---------------------------------------------------------------------------
SchemaInfo::SchemaInfo(const unsigned short elemAttrDefaultQualified,
                       const int blockDefault,
                       const int finalDefault,
                       const int targetNSURI,
                       const int scopeCount,
                       const unsigned int namespaceScopeLevel,
                       XMLCh* const schemaURL,
                       const XMLCh* const targetNSURIString,
                       const DOMElement* const root,
                       MemoryManager* const manager)
    : fAdoptInclude(false)
    , fProcessed(false)
    , fElemAttrDefaultQualified(elemAttrDefaultQualified)
    , fBlockDefault(blockDefault)
    , fFinalDefault(finalDefault)
    , fTargetNSURI(targetNSURI)
    , fScopeCount(scopeCount)
    , fNamespaceScopeLevel(namespaceScopeLevel)
    , fCurrentSchemaURL(schemaURL)
    , fTargetNSURIString(targetNSURIString)
    , fSchemaRootElement(root)
    , fIncludeInfoList(0)
    , fImportedInfoList(0)
    , fImportingInfoList(0)
    , fFailedRedefineList(0)
    , fImportedNSList(0)
    , fRecursingAnonTypes(0)
    , fRecursingTypeNames(0)
    , fNonXSAttList(0)
    , fValidationContext(0)
    , fMemoryManager(manager)
{
    fImportingInfoList = new (fMemoryManager) RefVectorOf<SchemaInfo>(4, false, fMemoryManager);

	memset(
         fTopLevelComponents,
         0,
         sizeof(fTopLevelComponents[0]) * C_Count);    
    memset(
         fLastTopLevelComponent,
         0,
         sizeof(fLastTopLevelComponent[0]) * C_Count);    
    
    fNonXSAttList = new (fMemoryManager) ValueVectorOf<DOMNode*>(2, fMemoryManager);
    fValidationContext = new (fMemoryManager) ValidationContextImpl(fMemoryManager);
}


SchemaInfo::~SchemaInfo()
{
    fMemoryManager->deallocate(fCurrentSchemaURL);//delete [] fCurrentSchemaURL;
    delete fImportedInfoList;

    if (fAdoptInclude)
        delete fIncludeInfoList;

    delete fImportingInfoList;  
    delete fImportedNSList;
    delete fFailedRedefineList;
    delete fRecursingAnonTypes;
    delete fRecursingTypeNames;   

    for (unsigned int i = 0; i < C_Count; i++) {
        delete fTopLevelComponents[i];
    }

    delete fNonXSAttList;  
    delete fValidationContext;    
}

// ---------------------------------------------------------------------------
//  SchemaInfo:
// ---------------------------------------------------------------------------
DOMElement*
SchemaInfo::getTopLevelComponent(const unsigned short compCategory,
                                 const XMLCh* const compName,
                                 const XMLCh* const name,
                                 SchemaInfo** enclosingSchema) {

    SchemaInfo* currentInfo = this;
    DOMElement* child = getTopLevelComponent(compCategory, compName, name);

    if (child == 0) {

        unsigned int listSize = (fIncludeInfoList) ? fIncludeInfoList->size() : 0;

        for (unsigned int i=0; i < listSize; i++) {

            currentInfo = fIncludeInfoList->elementAt(i);

            if (currentInfo == this)
                continue;

            child = currentInfo->getTopLevelComponent(compCategory, compName, name);

            if (child != 0) {

                *enclosingSchema = currentInfo;
                break;
            }
        }
    }

    return child;
}


DOMElement*
SchemaInfo::getTopLevelComponent(const unsigned short compCategory,
                                 const XMLCh* const compName,
                                 const XMLCh* const name) {

    if (compCategory >= C_Count)
        return 0;

    DOMElement* child = XUtil::getFirstChildElement(fSchemaRootElement);

    if (!child)
        return 0;

    RefHashTableOf<DOMElement>* compList = fTopLevelComponents[compCategory];

    if (fTopLevelComponents[compCategory] == 0) {

        compList= new (fMemoryManager) RefHashTableOf<DOMElement>(17, false, fMemoryManager);
        fTopLevelComponents[compCategory] = compList;
    }
    else {
        DOMElement* cachedChild = compList->get(name);
        if(cachedChild)
            return cachedChild;

        child = fLastTopLevelComponent[compCategory];
    }

    DOMElement* redefParent = (DOMElement*) child->getParentNode();

    // Parent is not "redefine"
    if (!XMLString::equals(redefParent->getLocalName(),SchemaSymbols::fgELT_REDEFINE))
        redefParent = 0;

    while (child != 0) {

        fLastTopLevelComponent[compCategory]=child;
        if (XMLString::equals(child->getLocalName(), compName)) {

            const XMLCh* cName=child->getAttribute(SchemaSymbols::fgATT_NAME);
            compList->put((void*)cName, child);

            if (XMLString::equals(cName, name))
                return child;
        }
        else if (XMLString::equals(child->getLocalName(),SchemaSymbols::fgELT_REDEFINE)
                 && (!fFailedRedefineList || !fFailedRedefineList->containsElement(child))) { // if redefine

            DOMElement* redefineChild = XUtil::getFirstChildElement(child);

            while (redefineChild != 0) {

                fLastTopLevelComponent[compCategory]=redefineChild;
                if ((!fFailedRedefineList || !fFailedRedefineList->containsElement(redefineChild))
                    && XMLString::equals(redefineChild->getLocalName(), compName)) {

                    const XMLCh* rName=redefineChild->getAttribute(SchemaSymbols::fgATT_NAME);
                    compList->put((void*)rName, redefineChild);

                    if (XMLString::equals(rName, name))
                        return redefineChild;
                }

                redefineChild = XUtil::getNextSiblingElement(redefineChild);
            }
        }

        child = XUtil::getNextSiblingElement(child);

        if (child == 0 && redefParent) {

            child = XUtil::getNextSiblingElement(redefParent);
            redefParent = 0;
        }
    }

    return child;
}

void SchemaInfo::updateImportingInfo(SchemaInfo* const importingInfo) {

    if (!fImportingInfoList->containsElement(importingInfo)) {
        fImportingInfoList->addElement(importingInfo);
    }

    unsigned int listSize = importingInfo->fImportingInfoList->size();

    for (unsigned int i=0; i < listSize; i++) {

        SchemaInfo* tmpInfo = importingInfo->fImportingInfoList->elementAt(i);

        if (tmpInfo != this && !fImportingInfoList->containsElement(tmpInfo)) {
            fImportingInfoList->addElement(tmpInfo);
        }
    }
}

XERCES_CPP_NAMESPACE_END

/**
  * End of file SchemaInfo.cpp
  */


