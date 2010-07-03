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
 * $Id: ComplexTypeInfo.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/framework/XMLBuffer.hpp>
#include <xercesc/validators/schema/ComplexTypeInfo.hpp>
#include <xercesc/validators/schema/SchemaAttDefList.hpp>
#include <xercesc/validators/common/AllContentModel.hpp>
#include <xercesc/validators/common/ContentSpecNode.hpp>
#include <xercesc/validators/common/DFAContentModel.hpp>
#include <xercesc/validators/common/MixedContentModel.hpp>
#include <xercesc/validators/common/SimpleContentModel.hpp>
#include <xercesc/validators/schema/XSDLocator.hpp>
#include <xercesc/internal/XTemplateSerializer.hpp>
#include <xercesc/util/XMLRegisterCleanup.hpp>
#include <xercesc/util/XMLInitializer.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local static data
// ---------------------------------------------------------------------------
static bool               sAnyTypeMutexRegistered = false;
static XMLMutex*          sAnyTypeMutex = 0;
static XMLRegisterCleanup anyTypeCleanup;


// ---------------------------------------------------------------------------
//  ComplexTypeInfo: Static member data
// ---------------------------------------------------------------------------
ComplexTypeInfo* ComplexTypeInfo::fAnyType = 0;


// ---------------------------------------------------------------------------
//  ComplexTypeInfo: Static meber methods
// ---------------------------------------------------------------------------
void ComplexTypeInfo::reinitAnyType() {

    delete fAnyType;
    fAnyType = 0;

    // delete local static data
    delete sAnyTypeMutex;
    sAnyTypeMutex = 0;
    sAnyTypeMutexRegistered = false;
}

void XMLInitializer::initializeAnyType()
{
    ComplexTypeInfo::getAnyType(1);
}

ComplexTypeInfo* ComplexTypeInfo::getAnyType(unsigned int emptyNSId)
{
    if (!sAnyTypeMutexRegistered)
    {
        if (!sAnyTypeMutex)
        {
            XMLMutexLock lock(XMLPlatformUtils::fgAtomicMutex);
            if (!sAnyTypeMutex)
                sAnyTypeMutex = new XMLMutex(XMLPlatformUtils::fgMemoryManager);
        }

        // Use a faux scope to synchronize while we do this
        {
            XMLMutexLock lock(sAnyTypeMutex);

            // If we got here first, then register it and set the registered flag
            if (!sAnyTypeMutexRegistered)
            {
                // create type name
                XMLCh typeName[128];
                unsigned int nsLen = XMLString::stringLen(
                    SchemaSymbols::fgURI_SCHEMAFORSCHEMA);

			    XMLString::copyString(
                    typeName, SchemaSymbols::fgURI_SCHEMAFORSCHEMA);
                typeName[nsLen] = chComma;
                XMLString::copyString(
                    typeName + nsLen + 1, SchemaSymbols::fgATTVAL_ANYTYPE);

                // Create and initialize 'anyType'
                fAnyType = new ComplexTypeInfo();

                ContentSpecNode* term = new ContentSpecNode
                (
                    new QName
                    (
                        XMLUni::fgZeroLenString
                        , XMLUni::fgZeroLenString
                        , emptyNSId
                    )
                    , false
                );
                term->setType(ContentSpecNode::Any_Lax);
                term->setMinOccurs(0);
                term->setMaxOccurs(SchemaSymbols::XSD_UNBOUNDED);

                ContentSpecNode* particle = new ContentSpecNode
                (
                    ContentSpecNode::ModelGroupSequence
                    , term
                    , 0
                );

                SchemaAttDef* attWildCard = new SchemaAttDef
                (
                    XMLUni::fgZeroLenString
                    , XMLUni::fgZeroLenString
                    , emptyNSId
                    , XMLAttDef::Any_Any
                    , XMLAttDef::ProcessContents_Lax
                );

                fAnyType->setTypeName(typeName);
                fAnyType->setBaseComplexTypeInfo(fAnyType);
                fAnyType->setDerivedBy(SchemaSymbols::XSD_RESTRICTION);
                fAnyType->setContentType(SchemaElementDecl::Mixed_Complex);
                fAnyType->setContentSpec(particle);
                fAnyType->setAttWildCard(attWildCard);

                // register cleanup method
                anyTypeCleanup.registerCleanup(ComplexTypeInfo::reinitAnyType);
                sAnyTypeMutexRegistered = true;
            }
        }
    }

    return fAnyType;
}


// ---------------------------------------------------------------------------
//  ComplexTypeInfo: Constructors and Destructor
// ---------------------------------------------------------------------------
ComplexTypeInfo::ComplexTypeInfo(MemoryManager* const manager)
    : fAnonymous(false)
    , fAbstract(false)
    , fAdoptContentSpec(true)
    , fAttWithTypeId(false)
    , fPreprocessed(false)
    , fDerivedBy(0)
    , fBlockSet(0)
    , fFinalSet(0)
    , fScopeDefined(Grammar::TOP_LEVEL_SCOPE)
    , fContentType(SchemaElementDecl::Empty)
    , fElementId(XMLElementDecl::fgInvalidElemId)
    , fUniqueURI(0)
    , fContentSpecOrgURISize(16)
    , fTypeName(0)
    , fTypeLocalName(0)
    , fTypeUri(0)
    , fBaseDatatypeValidator(0)
    , fDatatypeValidator(0)
    , fBaseComplexTypeInfo(0)
    , fContentSpec(0)
    , fAttWildCard(0)    
    , fAttList(0)
    , fElements(0)    
    , fAttDefs(0)
    , fContentModel(0)
    , fFormattedModel(0)
    , fContentSpecOrgURI(0)
    , fLocator(0)
    , fMemoryManager(manager)
{
    fAttDefs = new (fMemoryManager) RefHash2KeysTableOf<SchemaAttDef>(29, true, fMemoryManager);
    fAttList = new (fMemoryManager) SchemaAttDefList(fAttDefs,fMemoryManager);
}


ComplexTypeInfo::~ComplexTypeInfo()
{
    fMemoryManager->deallocate(fTypeName); //delete [] fTypeName;
    fMemoryManager->deallocate(fTypeLocalName); //delete [] fTypeLocalName;
    fMemoryManager->deallocate(fTypeUri); //delete [] fTypeUri;

    if (fAdoptContentSpec) {
        delete fContentSpec;
    }

    delete fAttWildCard;
    delete fAttDefs;
    delete fAttList;
    delete fElements;    
    delete fLocator;

    delete fContentModel;
    fMemoryManager->deallocate(fFormattedModel); //delete [] fFormattedModel;
    fMemoryManager->deallocate(fContentSpecOrgURI); //delete [] fContentSpecOrgURI;

}

// ---------------------------------------------------------------------------
//  ComplexTypeInfo: Setter methods
// ---------------------------------------------------------------------------
void ComplexTypeInfo::addAttDef(SchemaAttDef* const toAdd) {

    // Tell this guy the element id of its parent (us)
    toAdd->setElemId(getElementId());

    fAttDefs->put((void*)(toAdd->getAttName()->getLocalPart()),
                          toAdd->getAttName()->getURI(), toAdd);
    // update and/or create fAttList
    fAttList->addAttDef(toAdd);
}

void ComplexTypeInfo::setContentSpec(ContentSpecNode* const toAdopt) {

    if (fContentSpec && fAdoptContentSpec) {
        delete fContentSpec;
    }

    fContentSpec = toAdopt;

    //reset Content Model
    setContentModel(0);
}

void ComplexTypeInfo::setLocator(XSDLocator* const aLocator) {

    if (fLocator)
        delete fLocator;

    fLocator = aLocator;
}

// ---------------------------------------------------------------------------
//  ComplexTypeInfo: Getter methods
// ---------------------------------------------------------------------------
XMLAttDefList& ComplexTypeInfo::getAttDefList() const
{
    // NOTE: if users plan on using nextElement() to access attributes
    //       they need to call Reset() explicitly (i.e attList.Reset()).
    //       It's better to get the attribute count and use an index to
    //       access attributes (especially if same grammar is used in
    //       multiple threads).
    return *fAttList;
}

const XMLCh*
ComplexTypeInfo::getFormattedContentModel() const
{
    //
    //  If its not already built, then call the protected virtual method
    //  to allow the derived class to build it (since only it knows.)
    //  Otherwise, just return the previously formatted methods.
    //
    //  Since we are faulting this in, within a const getter, we have to
    //  cast off the const-ness.
    //
    if (!fFormattedModel)
        ((ComplexTypeInfo*)this)->fFormattedModel = formatContentModel();

    return fFormattedModel;
}

// ---------------------------------------------------------------------------
//  ComplexTypeInfo: Helper methods
// ---------------------------------------------------------------------------
XMLAttDef* ComplexTypeInfo::findAttr(const XMLCh* const
                                     , const unsigned int uriId
                                     , const XMLCh* const baseName
                                     , const XMLCh* const prefix
                                     , const XMLElementDecl::LookupOpts   options
                                     , bool&              wasAdded) const
{
    SchemaAttDef* retVal = fAttDefs->get(baseName, uriId);

    // Fault it in if not found and ask to add it
    if (!retVal && (options == XMLElementDecl::AddIfNotFound))
    {
        // And add a default attribute for this name
        retVal = new (fMemoryManager) SchemaAttDef
        (
            prefix
            , baseName
            , uriId
            , XMLAttDef::CData
            , XMLAttDef::Implied
            , fMemoryManager
        );
        retVal->setElemId(getElementId());
        fAttDefs->put((void*)retVal->getAttName()->getLocalPart(), uriId, retVal);

        // update fAttList
        fAttList->addAttDef(retVal);
        wasAdded = true;
    }
    else
    {
        wasAdded = false;
    }
    return retVal;
}

bool ComplexTypeInfo::resetDefs() {

    //  Ok, run through them and clear the 'provided' flag on each of them.
    //  This lets the scanner use them to track which has been provided and
    //  which have not.
    RefHash2KeysTableOfEnumerator<SchemaAttDef> enumDefs(fAttDefs, false, fMemoryManager);
    while (enumDefs.hasMoreElements())
        enumDefs.nextElement().setProvided(false);

    return true;
}


void ComplexTypeInfo::checkUniqueParticleAttribution (SchemaGrammar*    const pGrammar,
                                                      GrammarResolver*  const pGrammarResolver,
                                                      XMLStringPool*    const pStringPool,
                                                      XMLValidator*     const pValidator)
{
    if (fContentSpec && !fContentModel)
    {
        fContentModel = makeContentModel(true);
        if (fContentModel) {
            fContentModel->checkUniqueParticleAttribution(pGrammar, pGrammarResolver, pStringPool, pValidator, fContentSpecOrgURI, fTypeLocalName);
        }
    }
}

// ---------------------------------------------------------------------------
//  ComplexTypeInfo: Private Helper methods
// ---------------------------------------------------------------------------
void ComplexTypeInfo::faultInAttDefList() const
{
    // Use a hash modulus of 29 and tell it owns its elements
    ((ComplexTypeInfo*)this)->fAttDefs =
        new (fMemoryManager) RefHash2KeysTableOf<SchemaAttDef>(29, true, fMemoryManager);
}

XMLCh* ComplexTypeInfo::formatContentModel() const
{
    XMLCh* newValue = 0;
    if (fContentType == SchemaElementDecl::Any)
    {
        newValue = XMLString::replicate(XMLUni::fgAnyString, fMemoryManager);
    }
    else if (fContentType == SchemaElementDecl::Empty)
    {
        newValue = XMLString::replicate(XMLUni::fgEmptyString, fMemoryManager);
    }
    else
    {
        //
        //  Use a temp XML buffer to format into. Content models could be
        //  pretty long, but very few will be longer than one K. The buffer
        //  will expand to handle the more pathological ones.
        //
        const ContentSpecNode* specNode = fContentSpec;

        if (specNode) {
            XMLBuffer bufFmt(1023, fMemoryManager);

            specNode->formatSpec(bufFmt);
            newValue = XMLString::replicate
            (
                bufFmt.getRawBuffer()
                , fMemoryManager
            );
        }
    }
    return newValue;
}

XMLContentModel* ComplexTypeInfo::makeContentModel(const bool checkUPA)
{
    ContentSpecNode* aSpecNode = new (fMemoryManager) ContentSpecNode(*fContentSpec);
    XMLContentModel* retModel = 0;

    if (checkUPA) {
        fContentSpecOrgURI = (unsigned int*) fMemoryManager->allocate
        (
            fContentSpecOrgURISize * sizeof(unsigned int)
        ); //new unsigned int[fContentSpecOrgURISize];
    }

    aSpecNode = convertContentSpecTree(aSpecNode, checkUPA);
    retModel = buildContentModel(aSpecNode);

    delete aSpecNode;
    return retModel;
}

XMLContentModel* ComplexTypeInfo::buildContentModel(ContentSpecNode* const aSpecNode)
{
    XMLContentModel* cmRet = 0;
    if (fContentType == SchemaElementDecl::Simple) {
       // just return nothing
    }
    else if (fContentType == SchemaElementDecl::Mixed_Simple)
    {
        //
        //  Just create a mixel content model object. This type of
        //  content model is optimized for mixed content validation.
        //
        cmRet = new (fMemoryManager) MixedContentModel(false, aSpecNode, false, fMemoryManager);
    }
    else if (fContentType == SchemaElementDecl::Mixed_Complex) {

            cmRet = createChildModel(aSpecNode, true);
    }
    else if (fContentType == SchemaElementDecl::Children)
    {
        //
        //  This method will create an optimal model for the complexity
        //  of the element's defined model. If its simple, it will create
        //  a SimpleContentModel object. If its a simple list, it will
        //  create a SimpleListContentModel object. If its complex, it
        //  will create a DFAContentModel object.
        //
         cmRet = createChildModel(aSpecNode, false);
    }
     else
    {
        ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::CM_MustBeMixedOrChildren, fMemoryManager);
    }

    return cmRet;
}

// ---------------------------------------------------------------------------
//  SchemaElementDecl: Private helper methods
// ---------------------------------------------------------------------------
XMLContentModel* ComplexTypeInfo::createChildModel(ContentSpecNode* specNode, const bool isMixed)
{
    if(!specNode)
        ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::CM_UnknownCMSpecType, fMemoryManager);

    ContentSpecNode::NodeTypes specType = specNode->getType();
    //
    //  Do a sanity check that the node is does not have a PCDATA id. Since,
    //  if it was, it should have already gotten taken by the Mixed model.
    //
    if (specNode->getElement()) {
        if (specNode->getElement()->getURI() == XMLElementDecl::fgPCDataElemId)
            ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::CM_NoPCDATAHere, fMemoryManager);
    }

    //
    //  According to the type of node, we will create the correct type of
    //  content model.
    //
    if (((specType & 0x0f) == ContentSpecNode::Any) ||
       ((specType & 0x0f) == ContentSpecNode::Any_Other) ||
       ((specType & 0x0f) == ContentSpecNode::Any_NS)) {
       // let fall through to build a DFAContentModel
    }
    else if (isMixed)
    {
        if (specType == ContentSpecNode::All) {
            // All the nodes under an ALL must be additional ALL nodes and
            // ELEMENTs (or ELEMENTs under ZERO_OR_ONE nodes.)
            // We collapse the ELEMENTs into a single vector.
            return new (fMemoryManager) AllContentModel(specNode, true, fMemoryManager);
        }
        else if (specType == ContentSpecNode::ZeroOrOne) {
            // An ALL node can appear under a ZERO_OR_ONE node.
            if (specNode->getFirst()->getType() == ContentSpecNode::All) {
                return new (fMemoryManager) AllContentModel(specNode->getFirst(), true, fMemoryManager);
            }
        }

        // otherwise, let fall through to build a DFAContentModel
    }
     else if (specType == ContentSpecNode::Leaf)
    {
        // Create a simple content model
        return new (fMemoryManager) SimpleContentModel
        (
            false
            , specNode->getElement()
            , 0
            , ContentSpecNode::Leaf
            , fMemoryManager
        );
    }
     else if (((specType & 0x0f) == ContentSpecNode::Choice)
          ||  ((specType & 0x0f) == ContentSpecNode::Sequence))
    {
        //
        //  Lets see if both of the children are leafs. If so, then it has to
        //  be a simple content model
        //
        if ((specNode->getFirst()->getType() == ContentSpecNode::Leaf)
        &&  (specNode->getSecond())
        &&  (specNode->getSecond()->getType() == ContentSpecNode::Leaf))
        {
            return new (fMemoryManager) SimpleContentModel
            (
                false
                , specNode->getFirst()->getElement()
                , specNode->getSecond()->getElement()
                , specType
                , fMemoryManager
            );
        }
    }
     else if ((specType == ContentSpecNode::OneOrMore)
          ||  (specType == ContentSpecNode::ZeroOrMore)
          ||  (specType == ContentSpecNode::ZeroOrOne))
    {
        //
        //  Its a repetition, so see if its one child is a leaf. If so its a
        //  repetition of a single element, so we can do a simple content
        //  model for that.
        //
        if (specNode->getFirst()->getType() == ContentSpecNode::Leaf)
        {
            return new (fMemoryManager) SimpleContentModel
            (
                false
                , specNode->getFirst()->getElement()
                , 0
                , specType
                , fMemoryManager
            );
        }
        else if (specNode->getFirst()->getType() == ContentSpecNode::All)
            return new (fMemoryManager) AllContentModel(specNode->getFirst(), false, fMemoryManager);

    }
    else if (specType == ContentSpecNode::All)
        return new (fMemoryManager) AllContentModel(specNode, false, fMemoryManager);

    else
    {
        ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::CM_UnknownCMSpecType, fMemoryManager);
    }

    // Its not any simple type of content, so create a DFA based content model
    return new (fMemoryManager) DFAContentModel(false, specNode, isMixed, fMemoryManager);
}

ContentSpecNode*
ComplexTypeInfo::convertContentSpecTree(ContentSpecNode* const curNode,
                                        const bool checkUPA) {

    if (!curNode)
        return 0;

    const ContentSpecNode::NodeTypes curType = curNode->getType();

    // When checking Unique Particle Attribution, rename leaf elements
    if (checkUPA) {
        if (curNode->getElement()) {
            if (fUniqueURI == fContentSpecOrgURISize) {
                resizeContentSpecOrgURI();
            }

            fContentSpecOrgURI[fUniqueURI] = curNode->getElement()->getURI();
            curNode->getElement()->setURI(fUniqueURI);
            fUniqueURI++;
        }
    }

    // Get the spec type of the passed node
    int minOccurs = curNode->getMinOccurs();
    int maxOccurs = curNode->getMaxOccurs();
    ContentSpecNode* retNode = curNode;

    if ((curType & 0x0f) == ContentSpecNode::Any
        || (curType & 0x0f) == ContentSpecNode::Any_Other
        || (curType & 0x0f) == ContentSpecNode::Any_NS
        || curType == ContentSpecNode::Leaf)
    {
        retNode =  expandContentModel(curNode, minOccurs, maxOccurs);
    }
    else if (((curType & 0x0f) == ContentSpecNode::Choice)
        ||   (curType == ContentSpecNode::All)
        ||   ((curType & 0x0f) == ContentSpecNode::Sequence))
    {
        ContentSpecNode* childNode = curNode->getFirst();
        ContentSpecNode* leftNode = convertContentSpecTree(childNode, checkUPA);
        ContentSpecNode* rightNode = curNode->getSecond();

        if (!rightNode) {

            retNode = expandContentModel(leftNode, minOccurs, maxOccurs);
            curNode->setAdoptFirst(false);
            delete curNode;
            return retNode;
        }

        if (leftNode != childNode) {

            curNode->setAdoptFirst(false);
            curNode->setFirst(leftNode);
            curNode->setAdoptFirst(true);
        }

        childNode = rightNode;
        rightNode =  convertContentSpecTree(childNode, checkUPA);

        if (rightNode != childNode) {

            curNode->setAdoptSecond(false);
            curNode->setSecond(rightNode);
            curNode->setAdoptSecond(true);
        }

        retNode =  expandContentModel(curNode, minOccurs, maxOccurs);
    }

    return retNode;
}

ContentSpecNode* ComplexTypeInfo::expandContentModel(ContentSpecNode* const specNode,
                                                     const int minOccurs,
                                                     const int maxOccurs)
{
    if (!specNode) {
        return 0;
    }

    ContentSpecNode* saveNode = specNode;
    ContentSpecNode* retNode = specNode;

    if (minOccurs == 1 && maxOccurs == 1) {
    }
    else if (minOccurs == 0 && maxOccurs == 1) {

        retNode = new (fMemoryManager) ContentSpecNode
        (
            ContentSpecNode::ZeroOrOne
            , retNode
            , 0
            , true
            , true
            , fMemoryManager
        );
    }
    else if (minOccurs == 0 && maxOccurs == -1) {
        retNode = new (fMemoryManager) ContentSpecNode
        (
            ContentSpecNode::ZeroOrMore
            , retNode
            , 0
            , true
            , true
            , fMemoryManager
        );
    }
    else if (minOccurs == 1 && maxOccurs == -1) {
        retNode = new (fMemoryManager) ContentSpecNode
        (
            ContentSpecNode::OneOrMore
            , retNode
            , 0
            , true
            , true
            , fMemoryManager
        );
    }
    else if (maxOccurs == -1) {

        retNode = new (fMemoryManager) ContentSpecNode
        (
            ContentSpecNode::OneOrMore
            , retNode
            , 0
            , true
            , true
            , fMemoryManager
        );

        for (int i=0; i < (int)(minOccurs-1); i++) {
            retNode = new (fMemoryManager) ContentSpecNode
            (
                ContentSpecNode::Sequence
                , saveNode
                , retNode
                , false
                , true
                , fMemoryManager
            );
        }
    }
    else {

        if (minOccurs == 0) {

            ContentSpecNode* optional = new (fMemoryManager) ContentSpecNode
            (
                ContentSpecNode::ZeroOrOne
                , saveNode
                , 0
                , true
                , true
                , fMemoryManager
            );

            retNode = optional;

            for (int i=0; i < (int)(maxOccurs-minOccurs-1); i++) {
                retNode = new (fMemoryManager) ContentSpecNode
                (
                    ContentSpecNode::Sequence
                    , retNode
                    , optional
                    , true
                    , false
                    , fMemoryManager
                );
            }
        }
        else {

            if (minOccurs > 1) {

                retNode = new (fMemoryManager) ContentSpecNode
                (
                    ContentSpecNode::Sequence
                    , retNode
                    , saveNode
                    , true
                    , false
                    , fMemoryManager
                );

                for (int i=1; i < (int)(minOccurs-1); i++) {
                    retNode = new (fMemoryManager) ContentSpecNode
                    (
                        ContentSpecNode::Sequence
                        , retNode
                        , saveNode
                        , true
                        , false
                        , fMemoryManager
                    );
                }
            }

            int counter = maxOccurs-minOccurs;

            if (counter > 0) {

                ContentSpecNode* optional = new (fMemoryManager) ContentSpecNode
                (
                    ContentSpecNode::ZeroOrOne
                    , saveNode
                    , 0
                    , false
                    , true
                    , fMemoryManager
                );

                retNode = new (fMemoryManager) ContentSpecNode
                (
                    ContentSpecNode::Sequence
                    , retNode
                    , optional
                    , true
                    , true
                    , fMemoryManager
                );

                for (int j=1; j < counter; j++) {

                    retNode = new (fMemoryManager) ContentSpecNode
                    (
                        ContentSpecNode::Sequence
					    , retNode
                        , optional
                        , true
                        , false
                        , fMemoryManager
                    );
                }
            }
        }
    }

    return retNode;
}

void ComplexTypeInfo::resizeContentSpecOrgURI() {

    unsigned int newSize = fContentSpecOrgURISize * 2;
    unsigned int* newContentSpecOrgURI = (unsigned int*) fMemoryManager->allocate
    (
        newSize * sizeof(unsigned int)
    ); //new unsigned int[newSize];

    // Copy the existing values
    unsigned int index = 0;
    for (; index < fContentSpecOrgURISize; index++)
        newContentSpecOrgURI[index] = fContentSpecOrgURI[index];

    for (; index < newSize; index++)
        newContentSpecOrgURI[index] = 0;

    // Delete the old array and udpate our members
    fMemoryManager->deallocate(fContentSpecOrgURI); //delete [] fContentSpecOrgURI;
    fContentSpecOrgURI = newContentSpecOrgURI;
    fContentSpecOrgURISize = newSize;
}

/***
 * Support for Serialization/De-serialization
 ***/

IMPL_XSERIALIZABLE_TOCREATE(ComplexTypeInfo)

void ComplexTypeInfo::serialize(XSerializeEngine& serEng)
{
   
    if (serEng.isStoring())
    {    
        serEng<<fAnonymous;
        serEng<<fAbstract;
        serEng<<fAdoptContentSpec;
        serEng<<fAttWithTypeId;
        serEng<<fPreprocessed;
        serEng<<fDerivedBy;
        serEng<<fBlockSet;
        serEng<<fFinalSet;
        serEng<<fScopeDefined;
        serEng<<fContentType;

        serEng<<fElementId;

        serEng.writeString(fTypeName);
        serEng.writeString(fTypeLocalName);
        serEng.writeString(fTypeUri);

        DatatypeValidator::storeDV(serEng, fBaseDatatypeValidator);
        DatatypeValidator::storeDV(serEng, fDatatypeValidator);

        serEng<<fBaseComplexTypeInfo;
        serEng<<fContentSpec;
        serEng<<fAttWildCard;
        serEng<<fAttList;

        /***
         * 
         * Serialize RefVectorOf<SchemaElementDecl>*    fElements;
         * Serialize RefHash2KeysTableOf<SchemaAttDef>* fAttDefs;
         ***/
        XTemplateSerializer::storeObject(fElements, serEng);
        XTemplateSerializer::storeObject(fAttDefs, serEng);

         /***
          *   Don't serialize 
          *
          *   fContentModel;
          *   fFormattedModel;
          *   fLocator;          
          * 
          *   fContentSpecOrgURI:     start of the array
          *   fContentSpecOrgURISize: size of the array
          *   fUniqueURI:             the current last element in the array
          ***/
    }
    else
    {
        serEng>>fAnonymous;
        serEng>>fAbstract;
        serEng>>fAdoptContentSpec;
        serEng>>fAttWithTypeId;
        serEng>>fPreprocessed;
        serEng>>fDerivedBy;
        serEng>>fBlockSet;
        serEng>>fFinalSet;
        serEng>>fScopeDefined;
        serEng>>fContentType;

        serEng>>fElementId;

        serEng.readString(fTypeName);
        serEng.readString(fTypeLocalName);
        serEng.readString(fTypeUri);

        fBaseDatatypeValidator = DatatypeValidator::loadDV(serEng);
        fDatatypeValidator     = DatatypeValidator::loadDV(serEng);

        serEng>>fBaseComplexTypeInfo;
        serEng>>fContentSpec;
        serEng>>fAttWildCard;
        delete fAttList; // will recreate it next...
        serEng>>fAttList;

        /***
         * 
         * Deserialize RefVectorOf<SchemaElementDecl>*    fElements;
         * Deserialize RefHash2KeysTableOf<SchemaAttDef>* fAttDefs;
         ***/
        XTemplateSerializer::loadObject(&fElements, 8, false, serEng);
        delete fAttDefs; // will recreate it next...
        XTemplateSerializer::loadObject(&fAttDefs, 29, true, serEng);

         /***
          *   Don't deserialize 
          *
          *   fFormattedModel;
          *   fLocator;          
          * 
          *   fContentSpecOrgURI:     start of the array
          *   fContentSpecOrgURISize: size of the array
          *   fUniqueURI:             the current last element in the array
          ***/

         fFormattedModel = 0;
         fLocator = 0;         
         fContentSpecOrgURI = 0;
         fContentSpecOrgURISize = 0;
         fUniqueURI = 0;

         // Create the content model by calling getContentModel().  This
         // will ensure the grammar can be used concurrently by multiple
         // parsers.
         // Don't bother to do check unique particle attribution, since
         // this will already have been done when the grammar was first
         // created (if full schema checking was enabled).
         getContentModel(false);
    }
}


XERCES_CPP_NAMESPACE_END

/**
  * End of file ComplexTypeInfo.cpp
  */


