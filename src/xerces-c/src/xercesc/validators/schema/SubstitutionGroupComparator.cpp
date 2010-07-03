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
 * $Log: SubstitutionGroupComparator.cpp,v $
 * Revision 1.10  2004/09/08 13:56:57  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.9  2003/12/17 00:18:40  cargilld
 * Update to memory management so that the static memory manager (one used to call Initialize) is only for static data.
 *
 * Revision 1.8  2003/07/31 17:14:27  peiyongz
 * Grammar embed grammar description
 *
 * Revision 1.7  2003/06/25 22:38:18  peiyongz
 * to use new GrammarResolver::getGrammar()
 *
 * Revision 1.6  2003/01/13 20:16:51  knoaman
 * [Bug 16024] SchemaSymbols.hpp conflicts C++ Builder 6 dir.h
 *
 * Revision 1.5  2002/11/04 14:49:42  tng
 * C++ Namespace Support.
 *
 * Revision 1.4  2002/09/24 20:12:48  tng
 * Performance: use XMLString::equals instead of XMLString::compareString
 *
 * Revision 1.3  2002/07/12 15:17:48  knoaman
 * For a given global element, store info about a substitution group element
 * as a SchemaElementDecl and not as a string.
 *
 * Revision 1.2  2002/02/25 21:18:18  tng
 * Schema Fix: Ensure no invalid uri index for UPA checking.
 *
 * Revision 1.1.1.1  2002/02/01 22:22:47  peiyongz
 * sane_include
 *
 * Revision 1.11  2001/11/28 16:46:03  tng
 * Schema fix: Initialize the temporary string as null terminated.
 *
 * Revision 1.10  2001/11/21 14:30:13  knoaman
 * Fix for UPA checking.
 *
 * Revision 1.9  2001/11/07 21:50:28  tng
 * Fix comment log that lead to error.
 *
 * Revision 1.8  2001/11/07 21:12:15  tng
 * Performance: Create QName in ContentSpecNode only if it is a leaf/Any/PCDataNode.
 *
 * Revision 1.7  2001/10/04 15:08:56  knoaman
 * Add support for circular import.
 *
 * Revision 1.6  2001/08/21 15:57:51  tng
 * Schema: Add isAllowedByWildcard.  Help from James Murphy.
 *
 * Revision 1.5  2001/05/29 19:47:22  knoaman
 * Fix bug -  memory was not allocated before call to XMLString::subString
 *
 * Revision 1.4  2001/05/28 20:55:42  tng
 * Schema: Null pointer checking in SubsitutionGropuComparator
 *
 * Revision 1.3  2001/05/11 13:27:37  tng
 * Copyright update.
 *
 * Revision 1.2  2001/05/04 14:50:28  tng
 * Fixed the cvs symbols.
 *
 *
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/framework/XMLGrammarPool.hpp>
#include <xercesc/framework/XMLSchemaDescription.hpp>
#include <xercesc/validators/schema/SubstitutionGroupComparator.hpp>
#include <xercesc/validators/common/Grammar.hpp>
#include <xercesc/validators/schema/SchemaGrammar.hpp>
#include <xercesc/validators/schema/ComplexTypeInfo.hpp>
#include <xercesc/validators/schema/SchemaSymbols.hpp>

XERCES_CPP_NAMESPACE_BEGIN

bool SubstitutionGroupComparator::isEquivalentTo(QName* const anElement
                                               , QName* const exemplar)
{
    if (!anElement && !exemplar)
        return true;

    if ((!anElement && exemplar) || (anElement && !exemplar))
        return false;


    if (XMLString::equals(anElement->getLocalPart(), exemplar->getLocalPart()) &&
        (anElement->getURI() == exemplar->getURI()))
        return true; // they're the same!

    if (!fGrammarResolver || !fStringPool )
    {
        ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::SubGrpComparator_NGR, anElement->getMemoryManager());
    }

    unsigned int uriId = anElement->getURI();
    if (uriId == XMLContentModel::gEOCFakeId ||
        uriId == XMLContentModel::gEpsilonFakeId ||
        uriId == XMLElementDecl::fgPCDataElemId ||
        uriId == XMLElementDecl::fgInvalidElemId)
        return false;

    const XMLCh* uri = fStringPool->getValueForId(uriId);
    const XMLCh* localpart = anElement->getLocalPart();

    // In addition to simply trying to find a chain between anElement and exemplar,
    // we need to make sure that no steps in the chain are blocked.
    // That is, at every step, we need to make sure that the element
    // being substituted for will permit being substituted
    // for, and whether the type of the element will permit derivations in
    // instance documents of this sort.

    if (!uri)
        return false;

    SchemaGrammar *sGrammar = (SchemaGrammar*) fGrammarResolver->getGrammar(uri);
    if (!sGrammar || sGrammar->getGrammarType() == Grammar::DTDGrammarType)
        return false;

    SchemaElementDecl* anElementDecl = (SchemaElementDecl*) sGrammar->getElemDecl(uriId, localpart, 0, Grammar::TOP_LEVEL_SCOPE);
    if (!anElementDecl)
        return false;

    SchemaElementDecl* pElemDecl = anElementDecl->getSubstitutionGroupElem();
    bool foundIt = false;

    while (pElemDecl) //(substitutionGroupFullName)
    {
        if (XMLString::equals(pElemDecl->getBaseName(), exemplar->getLocalPart()) &&
            (pElemDecl->getURI() == exemplar->getURI()))
        {
            // time to check for block value on element
            if((pElemDecl->getBlockSet() & SchemaSymbols::XSD_SUBSTITUTION) != 0)
                return false;

            foundIt = true;
            break;
        }

        pElemDecl = pElemDecl->getSubstitutionGroupElem();
    }//while

    if (!foundIt)
        return false;

    // this will contain anElement's complexType information.
    ComplexTypeInfo *aComplexType = anElementDecl->getComplexTypeInfo();
    int exemplarBlockSet = pElemDecl->getBlockSet();

    if(!aComplexType)
    {
        // check on simpleType case
        DatatypeValidator *anElementDV = anElementDecl->getDatatypeValidator();
        DatatypeValidator *exemplarDV = pElemDecl->getDatatypeValidator();

        return((anElementDV == 0) ||
            ((anElementDV == exemplarDV) ||
            ((exemplarBlockSet & SchemaSymbols::XSD_RESTRICTION) == 0)));
    }

    // 2.3 The set of all {derivation method}s involved in the derivation of D's {type definition} from C's {type definition} does not intersect with the union of the blocking constraint, C's {prohibited substitutions} (if C is complex, otherwise the empty set) and the {prohibited substitutions} (respectively the empty set) of any intermediate {type definition}s in the derivation of D's {type definition} from C's {type definition}.
    // prepare the combination of {derivation method} and
    // {disallowed substitution}
    int devMethod = 0;
    int blockConstraint = exemplarBlockSet;
        
    ComplexTypeInfo *exemplarComplexType = pElemDecl->getComplexTypeInfo();
    ComplexTypeInfo *tempType = aComplexType;;

    while (tempType != 0 &&
        tempType != exemplarComplexType) 
    {
        devMethod |= tempType->getDerivedBy();
        tempType = tempType->getBaseComplexTypeInfo();
        if (tempType) {
            blockConstraint |= tempType->getBlockSet();
        }
    }
    if (tempType != exemplarComplexType) {
        return false;
    }
    if ((devMethod & blockConstraint) != 0) {
        return false;
    }

    return true;
}


bool SubstitutionGroupComparator::isAllowedByWildcard(SchemaGrammar* const pGrammar,
                                                      QName* const element,
                                                      unsigned int wuri, bool wother)
{
    // whether the uri is allowed directly by the wildcard
    unsigned int uriId = element->getURI();

    if ((!wother && uriId == wuri) ||
        (wother &&
         uriId != wuri &&
         uriId != XMLContentModel::gEOCFakeId &&
         uriId != XMLContentModel::gEpsilonFakeId &&
         uriId != XMLElementDecl::fgPCDataElemId &&
         uriId != XMLElementDecl::fgInvalidElemId))
    {
        return true;
    }

    // get all elements that can substitute the current element
    RefHash2KeysTableOf<ElemVector>* theValidSubstitutionGroups = pGrammar->getValidSubstitutionGroups();

    if (!theValidSubstitutionGroups)
        return false;

    ValueVectorOf<SchemaElementDecl*>* subsElements = theValidSubstitutionGroups->get(element->getLocalPart(), uriId);

    if (!subsElements)
        return false;

    // then check whether there exists one element that is allowed by the wildcard
    int size = subsElements->size();

    for (int i = 0; i < size; i++)
    {
        unsigned int subUriId = subsElements->elementAt(i)->getElementName()->getURI();

        if ((!wother && subUriId == wuri) ||
            (wother &&
             subUriId != wuri &&
             subUriId != XMLContentModel::gEOCFakeId &&
             subUriId != XMLContentModel::gEpsilonFakeId &&
             subUriId != XMLElementDecl::fgPCDataElemId &&
             subUriId != XMLElementDecl::fgInvalidElemId))
        {
            return true;
        }
    }
    return false;
}

XERCES_CPP_NAMESPACE_END

/**
  * End of file SubstitutionGroupComparator.cpp
  */

