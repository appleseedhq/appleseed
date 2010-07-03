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
 * $Id: ValueStore.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/internal/XMLScanner.hpp>
#include <xercesc/framework/XMLValidator.hpp>
#include <xercesc/validators/datatype/DatatypeValidator.hpp>
#include <xercesc/validators/schema/identity/FieldActivator.hpp>
#include <xercesc/validators/schema/identity/ValueStore.hpp>
#include <xercesc/validators/schema/identity/IC_Field.hpp>
#include <xercesc/validators/schema/identity/IC_KeyRef.hpp>
#include <xercesc/validators/schema/identity/ValueStoreCache.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  ValueStore: Constructors and Destructor
// ---------------------------------------------------------------------------
ValueStore::ValueStore(IdentityConstraint* const ic,
                       XMLScanner* const scanner,
                       MemoryManager* const manager)
    : fDoReportError(false)
    , fValuesCount(0)
    , fIdentityConstraint(ic)
    , fValues(manager)
    , fValueTuples(0)
    , fKeyValueStore(0)
    , fScanner(scanner)
    , fMemoryManager(manager)
{
	fDoReportError = (scanner && scanner->getDoValidation());
}


ValueStore::~ValueStore()
{
    delete fValueTuples;
}

// ---------------------------------------------------------------------------
//  ValueStore: Helper methods
// ---------------------------------------------------------------------------
void ValueStore::addValue(IC_Field* const,
                          DatatypeValidator* const,
                          const XMLCh* const) {

}

void ValueStore::addValue(FieldActivator* const fieldActivator,
                          IC_Field* const field,
                          DatatypeValidator* const dv,
                          const XMLCh* const value) {

    if (!fieldActivator->getMayMatch(field) && fDoReportError) {
        fScanner->getValidator()->emitError(XMLValid::IC_FieldMultipleMatch);
    }

    // do we even know this field?
    int index = fValues.indexOf(field);

    if (index == -1) {

        if (fDoReportError) {
           fScanner->getValidator()->emitError(XMLValid::IC_UnknownField);
        }

        return;
    }

    // store value
    if (!fValues.getDatatypeValidatorAt(index) &&
        !fValues.getValueAt(index)) {
        fValuesCount++;
    }

    fValues.put(field, dv, value);

    if (fValuesCount == (int) fValues.size()) {

        // is this value as a group duplicated?
        if (contains(&fValues)) {
            duplicateValue();
        }

        // store values
        if (!fValueTuples) {
            fValueTuples = new (fMemoryManager) RefVectorOf<FieldValueMap>(4, true, fMemoryManager);
        }

        fValueTuples->addElement(new (fMemoryManager) FieldValueMap(fValues));
    }
}

void ValueStore::append(const ValueStore* const other) {

    if (!other->fValueTuples) {
        return;
    }

    unsigned int tupleSize = other->fValueTuples->size();

    for (unsigned int i=0; i<tupleSize; i++) {

	    FieldValueMap* valueMap = other->fValueTuples->elementAt(i);

        if (!contains(valueMap)) {

            if (!fValueTuples) {
                fValueTuples = new (fMemoryManager) RefVectorOf<FieldValueMap>(4, true, fMemoryManager);
            }

            fValueTuples->addElement(new (fMemoryManager) FieldValueMap(*valueMap));
        }
    }
}

void ValueStore::startValueScope() {

    fValuesCount = 0;

    int count = fIdentityConstraint->getFieldCount();

    for (int i = 0; i < count; i++) {
        fValues.put(fIdentityConstraint->getFieldAt(i), 0, 0);
    }
}

void ValueStore::endValueScope() {

    if (fValuesCount == 0) {

        if (fIdentityConstraint->getType() == IdentityConstraint::KEY && fDoReportError) {
            fScanner->getValidator()->emitError(XMLValid::IC_AbsentKeyValue,
                fIdentityConstraint->getElementName());
        }

        return;
    }

    // do we have enough values?
    if ((fValuesCount != fIdentityConstraint->getFieldCount()) && fDoReportError) {

        if(fIdentityConstraint->getType()==IdentityConstraint::KEY)
        {
			fScanner->getValidator()->emitError(XMLValid::IC_KeyNotEnoughValues,
                fIdentityConstraint->getElementName(), fIdentityConstraint->getIdentityConstraintName());
        }
    }
}

bool ValueStore::contains(const FieldValueMap* const other) {

    if (fValueTuples) {

        unsigned int otherSize = other->size();
        unsigned int tupleSize = fValueTuples->size();

        for (unsigned int i=0; i<tupleSize; i++) {

            FieldValueMap* valueMap = fValueTuples->elementAt(i);

            if (otherSize == valueMap->size()) {

                bool matchFound = true;

                for (unsigned int j=0; j<otherSize; j++) {
                    if (!isDuplicateOf(valueMap->getDatatypeValidatorAt(j), valueMap->getValueAt(j),
                                       other->getDatatypeValidatorAt(j), other->getValueAt(j))) {
                        matchFound = false;
                        break;
                    }
                }

                if (matchFound) { // found it
                    return true;
                }
            }
        }
    }

    return false;
}

bool ValueStore::isDuplicateOf(DatatypeValidator* const dv1, const XMLCh* const val1,
                               DatatypeValidator* const dv2, const XMLCh* const val2) {

    // if either validator's null, fall back on string comparison
    if(!dv1 || !dv2) {
        return (XMLString::equals(val1, val2));
    }

    bool val1IsEmpty = (val1==0 || *val1==0);
    bool val2IsEmpty = (val2==0 || *val2==0);

    if (val1IsEmpty && val2IsEmpty) {

        if (dv1 == dv2) {
            return true;
        }

        return false;
    }

    if (val1IsEmpty || val2IsEmpty) {
        return false;
    }

    // find the common ancestor, if there is one
    DatatypeValidator* tempVal1 = dv1;
    while(tempVal1)
    {
        DatatypeValidator* tempVal2 = dv2;
        for(; tempVal2 != NULL && tempVal2 != tempVal1; tempVal2 = tempVal2->getBaseValidator()) ;
        if (tempVal2) 
            return ((tempVal2->compare(val1, val2, fMemoryManager)) == 0);
        tempVal1=tempVal1->getBaseValidator();
    }

    // if we're here it means the types weren't related. They are different:
    return false;
}


// ---------------------------------------------------------------------------
//  ValueStore: Document handling methods
// ---------------------------------------------------------------------------
void ValueStore::endDcocumentFragment(ValueStoreCache* const valueStoreCache) {

    if (fIdentityConstraint->getType() == IdentityConstraint::KEYREF) {

        // verify references
        // get the key store corresponding (if it exists):
        fKeyValueStore = valueStoreCache->getGlobalValueStoreFor(((IC_KeyRef*) fIdentityConstraint)->getKey());

        if (!fKeyValueStore) {

            if (fDoReportError) {
                fScanner->getValidator()->emitError(XMLValid::IC_KeyRefOutOfScope,
                    fIdentityConstraint->getIdentityConstraintName());
            }

            return;
        }

        unsigned int count = (fValueTuples) ? fValueTuples->size() : 0;

        for (unsigned int i = 0; i < count; i++) {

            FieldValueMap* valueMap = fValueTuples->elementAt(i);

            if (!fKeyValueStore->contains(valueMap) && fDoReportError) {

                fScanner->getValidator()->emitError(XMLValid::IC_KeyNotFound,
                    fIdentityConstraint->getElementName());
            }
        }
    }
}

// ---------------------------------------------------------------------------
//  ValueStore: Error reporting methods
// ---------------------------------------------------------------------------
void ValueStore::reportNilError(IdentityConstraint* const ic) {

    if (fDoReportError && ic->getType() == IdentityConstraint::KEY) {
        fScanner->getValidator()->emitError(XMLValid::IC_KeyMatchesNillable,
                                            ic->getElementName());
    }
}

void ValueStore::duplicateValue() {

    if (fDoReportError) {

        switch (fIdentityConstraint->getType()) {
        case IdentityConstraint::UNIQUE:
            {
                fScanner->getValidator()->emitError(XMLValid::IC_DuplicateUnique,
                    fIdentityConstraint->getElementName());
                break;
            }
        case IdentityConstraint::KEY:
            {
                fScanner->getValidator()->emitError(XMLValid::IC_DuplicateKey,
                    fIdentityConstraint->getElementName());
                break;
            }
        }
    }
}

XERCES_CPP_NAMESPACE_END

/**
  * End of file ValueStore.cpp
  */

