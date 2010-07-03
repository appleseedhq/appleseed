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
 * $Id: SimpleContentModel.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/framework/XMLValidator.hpp>
#include <xercesc/validators/common/SimpleContentModel.hpp>
#include <xercesc/validators/schema/SubstitutionGroupComparator.hpp>
#include <xercesc/validators/schema/XercesElementWildcard.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  SimpleContentModel: Implementation of the ContentModel virtual interface
// ---------------------------------------------------------------------------
//
//  This method is called to validate our content. For this one, its just a
//  pretty simple 'bull your way through it' test according to what kind of
//  operation it is for.
//
int
SimpleContentModel::validateContent(QName** const       children
                                  , const unsigned int  childCount
                                  , const unsigned int) const
{
    //
    //  According to the type of operation, we do the correct type of
    //  content check.
    //
    unsigned int index;
    switch(fOp & 0x0f)
    {
        case ContentSpecNode::Leaf :
            //
            //  There can only be one child and it has to be of the
            //  element type we stored.
            //
            if (!childCount)
                return 0;

            // If the 0th child is not the right kind, report an error at 0
            if (fDTD) {
                if (!XMLString::equals(children[0]->getRawName(), fFirstChild->getRawName())) {
                    return 0;
                }
            }
            else {
                if ((children[0]->getURI() != fFirstChild->getURI()) ||
                    !XMLString::equals(children[0]->getLocalPart(), fFirstChild->getLocalPart())) {
                    return 0;
                }
            }

            if (childCount > 1)
                return 1;
            break;

        case ContentSpecNode::ZeroOrOne :
            //
            //  If the child count is greater than one, then obviously
            //  bad. Otherwise, if its one, then the one child must be
            //  of the type we stored.
            //
            if (childCount == 1) {
                if (fDTD) {
                    if (!XMLString::equals(children[0]->getRawName(), fFirstChild->getRawName())) {
                            return 0;
                    }
                }
                else {
                    if ((children[0]->getURI() != fFirstChild->getURI()) ||
                        (!XMLString::equals(children[0]->getLocalPart(), fFirstChild->getLocalPart()))) {
                        return 0;
                    }
                }
            }


            if (childCount > 1)
                return 1;
            break;

        case ContentSpecNode::ZeroOrMore :
            //
            //  If the child count is zero, that's fine. If its more than
            //  zero, then make sure that all children are of the element
            //  type that we stored.
            //
            if (childCount > 0)
            {
                if (fDTD) {
                    for (index = 0; index < childCount; index++) {
                        if (!XMLString::equals(children[index]->getRawName(), fFirstChild->getRawName())) {
                            return index;
                        }
                    }
                }
                else {
                    for (index = 0; index < childCount; index++) {
                        if ((children[index]->getURI() != fFirstChild->getURI()) ||
                            !XMLString::equals(children[index]->getLocalPart(), fFirstChild->getLocalPart())) {
                            return index;
                        }
                    }
                }
            }
            break;

        case ContentSpecNode::OneOrMore :
            //
            //  If the child count is zero, that's an error. If its more
            //  than zero, then make sure that all children are of the
            //  element type that we stored.
            //
            if (childCount == 0)
                return 0;

            if (fDTD) {
                for (index = 0; index < childCount; index++) {
                    if (!XMLString::equals(children[index]->getRawName(), fFirstChild->getRawName())) {
                        return index;
                    }
                }
            }
            else {
                for (index = 0; index < childCount; index++) {
                    if ((children[index]->getURI() != fFirstChild->getURI()) ||
                        !XMLString::equals(children[index]->getLocalPart(), fFirstChild->getLocalPart())) {
                        return index;
                    }
                }
            }
            break;

        case ContentSpecNode::Choice :
            //
            //  There can only be one child, and it must be one of the
            //  two types we stored.
            //
            if (!childCount)
                return 0;

            if (fDTD) {
                if (!XMLString::equals(children[0]->getRawName(), fFirstChild->getRawName()) &&
                    !XMLString::equals(children[0]->getRawName(), fSecondChild->getRawName())) {
                    return 0;
                }
            }
            else {
                if (((children[0]->getURI() != fFirstChild->getURI()) ||
                     !XMLString::equals(children[0]->getLocalPart(), fFirstChild->getLocalPart())) &&
                    ((children[0]->getURI() != fSecondChild->getURI()) ||
                     !XMLString::equals(children[0]->getLocalPart(), fSecondChild->getLocalPart()))) {
                    return 0;
                }
            }

            if (childCount > 1)
                return 1;
            break;

        case ContentSpecNode::Sequence :
            //
            //  There must be two children and they must be the two values
            //  we stored, in the stored order. So first check the obvious
            //  problem of an empty content, which would never be valid
            //  in this content mode.
            //
            if (!childCount)
                return 0;

            if (childCount == 2) {
                if (fDTD) {
                    if (!XMLString::equals(children[0]->getRawName(), fFirstChild->getRawName())) {
                        return 0;
                    }
                    if (!XMLString::equals(children[1]->getRawName(), fSecondChild->getRawName())) {
                        return 1;
                    }
                }
                else {
                    if ((children[0]->getURI() != fFirstChild->getURI()) ||
                        !XMLString::equals(children[0]->getLocalPart(), fFirstChild->getLocalPart())) {
                        return 0;
                    }

                    if ((children[1]->getURI() != fSecondChild->getURI()) ||
                        !XMLString::equals(children[1]->getLocalPart(), fSecondChild->getLocalPart())) {
                        return 1;
                    }
                }
            }
            else {
                if (childCount > 2) {
                    return 2;
                }

                return childCount;
            }
            break;

        default :
            ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::CM_UnknownCMSpecType, fMemoryManager);
            break;
    }
    return -1;
}

int SimpleContentModel::validateContentSpecial(QName** const          children
                                            , const unsigned int      childCount
                                            , const unsigned int
                                            , GrammarResolver*  const pGrammarResolver
                                            , XMLStringPool*    const pStringPool) const
{

    SubstitutionGroupComparator comparator(pGrammarResolver, pStringPool);

    //
    //  According to the type of operation, we do the correct type of
    //  content check.
    //
    unsigned int index;
    switch(fOp & 0x0f)
    {
        case ContentSpecNode::Leaf :
            //
            //  There can only be one child and it has to be of the
            //  element type we stored.
            //
            if (!childCount)
                return 0;

            if ((children[0]->getURI() != fFirstChild->getURI()) ||
                !XMLString::equals(children[0]->getLocalPart(), fFirstChild->getLocalPart()))
            {
                if (!comparator.isEquivalentTo(children[0], fFirstChild))
                   return 0;
            }

            if (childCount > 1)
                return 1;
            break;

        case ContentSpecNode::ZeroOrOne :
            //
            //  If the child count is greater than one, then obviously
            //  bad. Otherwise, if its one, then the one child must be
            //  of the type we stored.
            //
            if ((childCount == 1) &&
               ((children[0]->getURI() != fFirstChild->getURI()) ||
                !XMLString::equals(children[0]->getLocalPart(), fFirstChild->getLocalPart())))
            {
                if(!comparator.isEquivalentTo(children[0], fFirstChild))
                    return 0;
            }

            if (childCount > 1)
                return 1;
            break;

        case ContentSpecNode::ZeroOrMore :
            //
            //  If the child count is zero, that's fine. If its more than
            //  zero, then make sure that all children are of the element
            //  type that we stored.
            //
            if (childCount > 0)
            {
                for (index = 0; index < childCount; index++)
                {
                    if ((children[index]->getURI() != fFirstChild->getURI()) ||
                        !XMLString::equals(children[index]->getLocalPart(), fFirstChild->getLocalPart()))
                    {
    				    if (!comparator.isEquivalentTo(children[index], fFirstChild))
                            return index;
                    }
                }
            }
            break;

        case ContentSpecNode::OneOrMore :
            //
            //  If the child count is zero, that's an error. If its more
            //  than zero, then make sure that all children are of the
            //  element type that we stored.
            //
            if (childCount == 0)
                return 0;

            for (index = 0; index < childCount; index++)
            {
                if ((children[index]->getURI() != fFirstChild->getURI()) ||
                    !XMLString::equals(children[index]->getLocalPart(), fFirstChild->getLocalPart()))
                {
    			    if (!comparator.isEquivalentTo(children[index], fFirstChild))
                        return index;
                }
            }
            break;

        case ContentSpecNode::Choice :
            //
            //  There can only be one child, and it must be one of the
            //  two types we stored.
            //
            if (!childCount)
                return 0;

            if (((children[0]->getURI() != fFirstChild->getURI()) ||
                 !XMLString::equals(children[0]->getLocalPart(), fFirstChild->getLocalPart())) &&
                ((children[0]->getURI() != fSecondChild->getURI()) ||
                 !XMLString::equals(children[0]->getLocalPart(), fSecondChild->getLocalPart())))
            {

                 if (!comparator.isEquivalentTo(children[0], fFirstChild) &&
                     !comparator.isEquivalentTo(children[0], fSecondChild) )
                     return 0;
            }

            if (childCount > 1)
                return 1;
            break;

        case ContentSpecNode::Sequence :
            //
            //  There must be two children and they must be the two values
            //  we stored, in the stored order. So first check the obvious
            //  problem of an empty content, which would never be valid
            //  in this content mode.
            //
            if (!childCount)
                return 0;

            if (childCount == 2)
            {
                if ((children[0]->getURI() != fFirstChild->getURI()) ||
                    !XMLString::equals(children[0]->getLocalPart(), fFirstChild->getLocalPart()))
                {
                    if(!comparator.isEquivalentTo(children[0], fFirstChild))
                        return 0;
                }

                if ((children[1]->getURI() != fSecondChild->getURI()) ||
                    !XMLString::equals(children[1]->getLocalPart(), fSecondChild->getLocalPart()))
                {
                    if (!comparator.isEquivalentTo(children[1], fSecondChild))
                        return 1;
                }
            }
            else
            {
                if (childCount > 2)
                {
                    return 2;
                }

                return childCount;
            }
            break;

        default :
            ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::CM_UnknownCMSpecType, fMemoryManager);
            break;
    }
    return -1;
}

ContentLeafNameTypeVector* SimpleContentModel::getContentLeafNameTypeVector() const
{
    return 0;
}

void SimpleContentModel::checkUniqueParticleAttribution
    (
        SchemaGrammar*    const pGrammar
      , GrammarResolver*  const pGrammarResolver
      , XMLStringPool*    const pStringPool
      , XMLValidator*     const pValidator
      , unsigned int*     const pContentSpecOrgURI
      , const XMLCh*            pComplexTypeName /*= 0*/
    )
{
    // rename back
    unsigned int orgURIIndex = 0;

    orgURIIndex = fFirstChild->getURI();
    if ((orgURIIndex != XMLContentModel::gEOCFakeId) &&
        (orgURIIndex != XMLElementDecl::fgInvalidElemId) &&
        (orgURIIndex != XMLElementDecl::fgPCDataElemId))
        fFirstChild->setURI(pContentSpecOrgURI[orgURIIndex]);

    orgURIIndex = fSecondChild->getURI();
    if ((orgURIIndex != XMLContentModel::gEOCFakeId) &&
        (orgURIIndex != XMLElementDecl::fgInvalidElemId) &&
        (orgURIIndex != XMLElementDecl::fgPCDataElemId))
        fSecondChild->setURI(pContentSpecOrgURI[orgURIIndex]);

    // only possible violation is when it's a choice
    if ((fOp & 0x0f) == ContentSpecNode::Choice) {

        SubstitutionGroupComparator comparator(pGrammarResolver, pStringPool);

        if (XercesElementWildcard::conflict(pGrammar,
                                            ContentSpecNode::Leaf,
                                            fFirstChild,
                                            ContentSpecNode::Leaf,
                                            fSecondChild,
                                            &comparator))

            pValidator->emitError(XMLValid::UniqueParticleAttributionFail,
                                  pComplexTypeName,
                                  fFirstChild->getRawName(),
                                  fSecondChild->getRawName());
    }
}

XERCES_CPP_NAMESPACE_END

