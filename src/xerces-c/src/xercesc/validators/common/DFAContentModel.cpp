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
 * $Id: DFAContentModel.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/framework/XMLBuffer.hpp>
#include <xercesc/framework/XMLElementDecl.hpp>
#include <xercesc/framework/XMLValidator.hpp>
#include <xercesc/validators/common/CMAny.hpp>
#include <xercesc/validators/common/CMBinaryOp.hpp>
#include <xercesc/validators/common/CMLeaf.hpp>
#include <xercesc/validators/common/CMUnaryOp.hpp>
#include <xercesc/validators/common/DFAContentModel.hpp>
#include <xercesc/validators/common/ContentSpecNode.hpp>
#include <xercesc/validators/common/Grammar.hpp>
#include <xercesc/validators/schema/SchemaSymbols.hpp>
#include <xercesc/validators/schema/SubstitutionGroupComparator.hpp>
#include <xercesc/validators/schema/XercesElementWildcard.hpp>
#include <xercesc/util/RefHashTableOf.hpp>
#include <xercesc/util/HashCMStateSet.hpp>
#include <xercesc/util/XMLInteger.hpp>

XERCES_CPP_NAMESPACE_BEGIN


// ---------------------------------------------------------------------------
//  DFAContentModel: Constructors and Destructor
// ---------------------------------------------------------------------------
DFAContentModel::DFAContentModel( const bool             dtd
                                , ContentSpecNode* const elemContentSpec
                                , MemoryManager* const   manager) :

    fElemMap(0)
    , fElemMapType(0)
    , fElemMapSize(0)
    , fEmptyOk(false)
    , fEOCPos(0)
    , fFinalStateFlags(0)
    , fFollowList(0)
    , fHeadNode(0)
    , fLeafCount(0)
    , fLeafList(0)
    , fLeafListType(0)
    , fTransTable(0)
    , fTransTableSize(0)
    , fDTD(dtd)
    , fIsMixed(false)
    , fLeafNameTypeVector(0)
    , fMemoryManager(manager)
{
    // And build the DFA data structures
    buildDFA(elemContentSpec);
}

DFAContentModel::DFAContentModel( const bool             dtd
                                , ContentSpecNode* const elemContentSpec
                                , const bool             isMixed
                                , MemoryManager* const   manager):

    fElemMap(0)
    , fElemMapType(0)
    , fElemMapSize(0)
    , fEmptyOk(false)
    , fEOCPos(0)
    , fFinalStateFlags(0)
    , fFollowList(0)
    , fHeadNode(0)
    , fLeafCount(0)
    , fLeafList(0)
    , fLeafListType(0)
    , fTransTable(0)
    , fTransTableSize(0)
    , fDTD(dtd)
    , fIsMixed(isMixed)
    , fLeafNameTypeVector(0)
    , fMemoryManager(manager)
{
    // And build the DFA data structures
    buildDFA(elemContentSpec);
}

DFAContentModel::~DFAContentModel()
{
    //
    //  Clean up all the stuff that is not just temporary representation
    //  data that was cleaned up after building the DFA.
    //
    fMemoryManager->deallocate(fFinalStateFlags); //delete [] fFinalStateFlags;

    unsigned index;
    for (index = 0; index < fTransTableSize; index++)
        fMemoryManager->deallocate(fTransTable[index]); //delete [] fTransTable[index];
    fMemoryManager->deallocate(fTransTable); //delete [] fTransTable;

    for (index = 0; index < fLeafCount; index++)
        delete fElemMap[index];
    fMemoryManager->deallocate(fElemMap); //delete [] fElemMap;

    fMemoryManager->deallocate(fElemMapType); //delete [] fElemMapType;
    fMemoryManager->deallocate(fLeafListType); //delete [] fLeafListType;

	delete fLeafNameTypeVector;
}


// ---------------------------------------------------------------------------
//  DFAContentModel: Implementation of the ContentModel virtual interface
// ---------------------------------------------------------------------------
int
DFAContentModel::validateContent( QName** const        children
                                , const unsigned int   childCount
                                , const unsigned int) const
{
    //
    //  If there are no children, then either we fail on the 0th element
    //  or we return success. It depends upon whether this content model
    //  accepts empty content, which we determined earlier.
    //
    if (!childCount)
    {
        // success -1
        return fEmptyOk ? -1 : 0;
    }

    //
    //  Lets loop through the children in the array and move our way
    //  through the states. Note that we use the fElemMap array to map
    //  an element index to a state index.
    //
    unsigned int curState = 0;
    unsigned int nextState = 0;
    unsigned int childIndex = 0;
    for (; childIndex < childCount; childIndex++)
    {
        // Get the current element index out
        const QName* curElem = children[childIndex];
        const XMLCh* curElemRawName = 0;
        if (fDTD)
            curElemRawName = curElem->getRawName();

        // If this is text in a Schema mixed content model, skip it.
        if ( fIsMixed &&
            ( curElem->getURI() == XMLElementDecl::fgPCDataElemId))
            continue;

        // Look up this child in our element map
        unsigned int elemIndex = 0;
        for (; elemIndex < fElemMapSize; elemIndex++)
        {
            const QName* inElem  = fElemMap[elemIndex];
            if (fDTD) {
                if (XMLString::equals(inElem->getRawName(), curElemRawName)) {
                    nextState = fTransTable[curState][elemIndex];
                    if (nextState != XMLContentModel::gInvalidTrans)
                        break;
                }
            }
            else {
                ContentSpecNode::NodeTypes type = fElemMapType[elemIndex];
                if (type == ContentSpecNode::Leaf)
                {
                    if ((inElem->getURI() == curElem->getURI()) &&
                    (XMLString::equals(inElem->getLocalPart(), curElem->getLocalPart()))) {
                        nextState = fTransTable[curState][elemIndex];
                        if (nextState != XMLContentModel::gInvalidTrans)
                            break;
                    }
                }
                else if ((type & 0x0f)== ContentSpecNode::Any)
                {
                    nextState = fTransTable[curState][elemIndex];
                    if (nextState != XMLContentModel::gInvalidTrans)
                            break;
                }
                else if ((type & 0x0f) == ContentSpecNode::Any_NS)
                {
                    if (inElem->getURI() == curElem->getURI())
                    {
                        nextState = fTransTable[curState][elemIndex];
                        if (nextState != XMLContentModel::gInvalidTrans)
                            break;
                    }
                }
                else if ((type & 0x0f) == ContentSpecNode::Any_Other)
                {
                    if (inElem->getURI() != curElem->getURI()) {
                        nextState = fTransTable[curState][elemIndex];
                        if (nextState != XMLContentModel::gInvalidTrans)
                            break;
                    }
                }
            }
        }//for elemIndex

        // If "nextState" is -1, we found a match, but the transition is invalid
        if (nextState == XMLContentModel::gInvalidTrans)
            return childIndex;

        // If we didn't find it, then obviously not valid
        if (elemIndex == fElemMapSize)
            return childIndex;

        curState = nextState;
        nextState = 0;

    }//for childIndex

    //
    //  We transitioned all the way through the input list. However, that
    //  does not mean that we ended in a final state. So check whether
    //  our ending state is a final state.
    //
    if (!fFinalStateFlags[curState])
        return childIndex;

    //success
    return -1;
}

int DFAContentModel::validateContentSpecial(QName** const          children
                                            , const unsigned int      childCount
                                            , const unsigned int
                                            , GrammarResolver*  const pGrammarResolver
                                            , XMLStringPool*    const pStringPool) const
{

    SubstitutionGroupComparator comparator(pGrammarResolver, pStringPool);

    if (childCount == 0)
        return fEmptyOk ? -1 : 0;

    //
    //  Lets loop through the children in the array and move our way
    //  through the states. Note that we use the fElemMap array to map
    //  an element index to a state index.
    //
    unsigned int curState = 0;
    unsigned int nextState = 0;
    unsigned int childIndex = 0;
    for (; childIndex < childCount; childIndex++)
    {
        // Get the current element index out
        QName* curElem = children[childIndex];

        // If this is text in a Schema mixed content model, skip it.
        if ( fIsMixed &&
            ( curElem->getURI() == XMLElementDecl::fgPCDataElemId))
            continue;

        // Look up this child in our element map
        unsigned int elemIndex = 0;
        for (; elemIndex < fElemMapSize; elemIndex++)
        {
            QName* inElem  = fElemMap[elemIndex];
            ContentSpecNode::NodeTypes type = fElemMapType[elemIndex];
            if (type == ContentSpecNode::Leaf)
            {
                if (comparator.isEquivalentTo(curElem, inElem) )
                {
                    nextState = fTransTable[curState][elemIndex];
                    if (nextState != XMLContentModel::gInvalidTrans)
                        break;
                }

            }
            else if ((type & 0x0f)== ContentSpecNode::Any)
            {
                nextState = fTransTable[curState][elemIndex];
                if (nextState != XMLContentModel::gInvalidTrans)
                        break;
            }
            else if ((type & 0x0f) == ContentSpecNode::Any_NS)
            {
                if (inElem->getURI() == curElem->getURI())
                {
                    nextState = fTransTable[curState][elemIndex];
                    if (nextState != XMLContentModel::gInvalidTrans)
                        break;
                }
            }
            else if ((type & 0x0f) == ContentSpecNode::Any_Other)
            {
                if (inElem->getURI() != curElem->getURI())
                {
                    nextState = fTransTable[curState][elemIndex];
                    if (nextState != XMLContentModel::gInvalidTrans)
                        break;
                }
            }
        }//for elemIndex

        // If "nextState" is -1, we found a match, but the transition is invalid
        if (nextState == XMLContentModel::gInvalidTrans)
            return childIndex;

        // If we didn't find it, then obviously not valid
        if (elemIndex == fElemMapSize)
            return childIndex;

        curState = nextState;
        nextState = 0;

    }//for childIndex

    //
    //  We transitioned all the way through the input list. However, that
    //  does not mean that we ended in a final state. So check whether
    //  our ending state is a final state.
    //
    if (!fFinalStateFlags[curState])
        return childIndex;

    //success
    return -1;
}


// ---------------------------------------------------------------------------
//  DFAContentModel: Private helper methods
// ---------------------------------------------------------------------------
void DFAContentModel::buildDFA(ContentSpecNode* const curNode)
{
    unsigned int index;


    //
    //  The first step we need to take is to rewrite the content model using
    //  our CMNode objects, and in the process get rid of any repetition short
    //  cuts, converting them into '*' style repetitions or getting rid of
    //  repetitions altogether.
    //
    //  The conversions done are:
    //
    //  x+ -> (x|x*)
    //  x? -> (x|epsilon)
    //
    //  This is a relatively complex scenario. What is happening is that we
    //  create a top level binary node of which the special EOC value is set
    //  as the right side node. The the left side is set to the rewritten
    //  syntax tree. The source is the original content model info from the
    //  decl pool. The rewrite is done by buildSyntaxTree() which recurses the
    //  decl pool's content of the element and builds a new tree in the
    //  process.
    //
    //  Note that, during this operation, we set each non-epsilon leaf node's
    //  DFA state position and count the number of such leafs, which is left
    //  in the fLeafCount member.
    //
    CMLeaf* nodeEOC = new (fMemoryManager) CMLeaf
    (
        new (fMemoryManager) QName
        (
            XMLUni::fgZeroLenString
            , XMLUni::fgZeroLenString
            , XMLContentModel::gEOCFakeId
            , fMemoryManager
        )
        , ~0
        , true
        , fMemoryManager
    );
    CMNode* nodeOrgContent = buildSyntaxTree(curNode);
    fHeadNode = new (fMemoryManager) CMBinaryOp
    (
        ContentSpecNode::Sequence
        , nodeOrgContent
        , nodeEOC
        , fMemoryManager
    );

    //
    //  And handle specially the EOC node, which also must be numbered and
    //  counted as a non-epsilon leaf node. It could not be handled in the
    //  above tree build because it was created before all that started. We
    //  save the EOC position since its used during the DFA building loop.
    //
    fEOCPos = fLeafCount;
    nodeEOC->setPosition(fLeafCount++);

    //
    //  Ok, so now we have to iterate the new tree and do a little more work
    //  now that we know the leaf count. One thing we need to do is to
    //  calculate the first and last position sets of each node. This is
    //  cached away in each of the nodes.
    //
    //  Along the way we also set the leaf count in each node as the maximum
    //  state count. They must know this in order to create their first/last
    //  position sets.
    //
    //  We also need to build an array of references to the non-epsilon
    //  leaf nodes. Since we iterate here the same way as we did during the
    //  initial tree build (which built their position numbers, we will put
    //  them in the array according to their position values.
    //
    fLeafList = (CMLeaf**) fMemoryManager->allocate(fLeafCount*sizeof(CMLeaf*)); //new CMLeaf*[fLeafCount];
    fLeafListType = (ContentSpecNode::NodeTypes*) fMemoryManager->allocate
    (
        fLeafCount * sizeof(ContentSpecNode::NodeTypes)
    ); //new ContentSpecNode::NodeTypes[fLeafCount];
    postTreeBuildInit(fHeadNode, 0);

    //
    //  And, moving onward... We now need to build the follow position sets
    //  for all the nodes. So we allocate an array of pointers to state sets,
    //  one for each leaf node (i.e. each significant DFA position.)
    //
    fFollowList = (CMStateSet**) fMemoryManager->allocate
    (
        fLeafCount * sizeof(CMStateSet*)
    ); //new CMStateSet*[fLeafCount];
    for (index = 0; index < fLeafCount; index++)
        fFollowList[index] = new (fMemoryManager) CMStateSet(fLeafCount, fMemoryManager);
    calcFollowList(fHeadNode);

    //
    //  Check to see whether this content model can handle an empty content,
    //  which is something we need to optimize by looking now before we
    //  throw away the info that would tell us that.
    //
    //  If the left node of the head (the top level of the original content)
    //  is nullable, then its true.
    //
    fEmptyOk = nodeOrgContent->isNullable();

    //
    //  And finally the big push... Now we build the DFA using all the states
    //  and the tree we've built up. First we set up the various data
    //  structures we are going to use while we do this.
    //
    //  First of all we need an array of unique element ids in our content
    //  model. For each transition table entry, we need a set of contiguous
    //  indices to represent the transitions for a particular input element.
    //  So we need to a zero based range of indexes that map to element types.
    //  This element map provides that mapping.
    //
    fElemMap = (QName**) fMemoryManager->allocate
    (
        fLeafCount * sizeof(QName*)
    ); //new QName*[fLeafCount];
    fElemMapType = (ContentSpecNode::NodeTypes*) fMemoryManager->allocate
    (
        fLeafCount * sizeof(ContentSpecNode::NodeTypes)
    ); //new ContentSpecNode::NodeTypes[fLeafCount];
    fElemMapSize = 0;


    for (unsigned int outIndex = 0; outIndex < fLeafCount; outIndex++)
    {
        fElemMap[outIndex] = new (fMemoryManager) QName(fMemoryManager);

        if ( (fLeafListType[outIndex] & 0x0f) != ContentSpecNode::Leaf )
            if (!fLeafNameTypeVector)
                fLeafNameTypeVector = new (fMemoryManager) ContentLeafNameTypeVector(fMemoryManager);

        // Get the current leaf's element index
        const QName* element = fLeafList[outIndex]->getElement();
        const XMLCh* elementRawName = 0;
        if (fDTD && element)
            elementRawName = element->getRawName();

        // See if the current leaf node's element index is in the list
        unsigned int inIndex = 0;

        for (; inIndex < fElemMapSize; inIndex++)
        {
            const QName* inElem = fElemMap[inIndex];
            if (fDTD) {
                if (XMLString::equals(inElem->getRawName(), elementRawName)) {
                    break;
                }
            }
            else {
                if ((fElemMapType[inIndex] == fLeafListType[outIndex]) &&
                    (inElem->getURI() == element->getURI()) &&
                    (XMLString::equals(inElem->getLocalPart(), element->getLocalPart()))) {
                    break;
                }
            }
        }

        // If it was not in the list, then add it and bump the map size
        if (inIndex == fElemMapSize)
        {
            fElemMap[fElemMapSize]->setValues(*element);
            fElemMapType[fElemMapSize] = fLeafListType[outIndex];
            ++fElemMapSize;
        }
    }

    // set up the fLeafNameTypeVector object if there is one.
    if (fLeafNameTypeVector) {
        fLeafNameTypeVector->setValues(fElemMap, fElemMapType, fElemMapSize);
    }

    /***
     * Optimization(Jan, 2001); We sort fLeafList according to
     * elemIndex which is *uniquely* associated to each leaf.
     * We are *assuming* that each element appears in at least one leaf.
     **/
    // don't forget to delete it

    int *fLeafSorter = (int*) fMemoryManager->allocate
    (
        (fLeafCount + fElemMapSize) * sizeof(int)
    ); //new int[fLeafCount + fElemMapSize];
    unsigned int fSortCount = 0;

    for (unsigned int elemIndex = 0; elemIndex < fElemMapSize; elemIndex++)
    {
        const QName* element = fElemMap[elemIndex];
        const XMLCh* elementRawName = 0;
        if (fDTD && element)
            elementRawName = element->getRawName();

        for (unsigned int leafIndex = 0; leafIndex < fLeafCount; leafIndex++)
        {
            const QName* leaf = fLeafList[leafIndex]->getElement();            
            if (fDTD) {
                if (XMLString::equals(leaf->getRawName(), elementRawName)) {
                    fLeafSorter[fSortCount++] = leafIndex;
                }
            }
            else {
                if ((fElemMapType[elemIndex] == fLeafListType[leafIndex]) &&
                    (leaf->getURI() == element->getURI()) &&
                    (XMLString::equals(leaf->getLocalPart(), element->getLocalPart()))) {
                      fLeafSorter[fSortCount++] = leafIndex;
                }
            }
        }
        fLeafSorter[fSortCount++] = -1;
    }

    //
    //  Next lets create some arrays, some that that hold transient info
    //  during the DFA build and some that are permament. These are kind of
    //  sticky since we cannot know how big they will get, but we don't want
    //  to use any collection type classes because of performance.
    //
    //  Basically they will probably be about fLeafCount*2 on average, but can
    //  be as large as 2^(fLeafCount*2), worst case. So we start with
    //  fLeafCount*4 as a middle ground. This will be very unlikely to ever
    //  have to expand though, it if does, the overhead will be somewhat ugly.
    //
    unsigned int curArraySize = fLeafCount * 4;
    const CMStateSet** statesToDo = (const CMStateSet**) 
        fMemoryManager->allocate
        (
            curArraySize * sizeof(const CMStateSet*)
        ); //new const CMStateSet*[curArraySize];
    fFinalStateFlags = (bool*) fMemoryManager->allocate
    (
        curArraySize * sizeof(bool)
    ); //new bool[curArraySize];
    fTransTable = (unsigned int**) fMemoryManager->allocate
    (
        curArraySize * sizeof(unsigned int*)
    ); //new unsigned int*[curArraySize];

    //
    //  Ok we start with the initial set as the first pos set of the head node
    //  (which is the seq node that holds the content model and the EOC node.)
    //
    const CMStateSet* setT = new (fMemoryManager) CMStateSet(fHeadNode->getFirstPos());

    //
    //  Init our two state flags. Basically the unmarked state counter is
    //  always chasing the current state counter. When it catches up, that
    //  means we made a pass through that did not add any new states to the
    //  lists, at which time we are done. We could have used a expanding array
    //  of flags which we used to mark off states as we complete them, but
    //  this is easier though less readable maybe.
    //
    unsigned int unmarkedState = 0;
    unsigned int curState = 0;

    //
    //  Init the first transition table entry, and put the initial state
    //  into the states to do list, then bump the current state.
    //
    fTransTable[curState] = makeDefStateList();
    statesToDo[curState] = setT;
    curState++;

    //
    // the stateTable is an auxiliary means to fast
    // identification of new state created (instead
    // of squential loop statesToDo to find out),
    // while the role that statesToDo plays remain unchanged.
    //
    // TODO: in the future, we may change the 29 to something
    //       derived from curArraySize.
    RefHashTableOf<XMLInteger> *stateTable =
        new (fMemoryManager) RefHashTableOf<XMLInteger>
        (
            curArraySize
            , true
            , new (fMemoryManager) HashCMStateSet()
            , fMemoryManager
        );
    //stateTable->put((CMStateSet*)setT, new (fMemoryManager) XMLInteger(0));

    //
    //  Ok, almost done with the algorithm from hell... We now enter the
    //  loop where we go until the states done counter catches up with
    //  the states to do counter.
    //
    CMStateSet* newSet = 0;
    while (unmarkedState < curState)
    {
        //
        //  Get the next unmarked state out of the list of states to do.
        //  And get the associated transition table entry.
        //
        setT = statesToDo[unmarkedState];
        unsigned int* transEntry = fTransTable[unmarkedState];

        // Mark this one final if it contains the EOC state
        fFinalStateFlags[unmarkedState] = setT->getBit(fEOCPos);

        // Bump up the unmarked state count, marking this state done
        unmarkedState++;

        // Optimization(Jan, 2001)
        unsigned int sorterIndex = 0;
        // Optimization(Jan, 2001)

        // Loop through each possible input symbol in the element map
        for (unsigned int elemIndex = 0; elemIndex < fElemMapSize; elemIndex++)
        {
            //
            //  Build up a set of states which is the union of all of the
            //  follow sets of DFA positions that are in the current state. If
            //  we gave away the new set last time through then create a new
            //  one. Otherwise, zero out the existing one.
            //
            if (!newSet)
                newSet = new (fMemoryManager) CMStateSet
                (
                    fLeafCount
                    , fMemoryManager
                );
            else
                newSet->zeroBits();

#ifdef OBSOLETED
// unoptimized code
            for (unsigned int leafIndex = 0; leafIndex < fLeafCount; leafIndex++)
            {
                // If this leaf index (DFA position) is in the current set...
                if (setT->getBit(leafIndex))
                {
                    //
                    //  If this leaf is the current input symbol, then we want
                    //  to add its follow list to the set of states to transition
                    //  to from the current state.
                    //
                    const QName* leaf = fLeafList[leafIndex]->getElement();
                    const QName* element = fElemMap[elemIndex];
                    if (fDTD) {
                        if (XMLString::equals(leaf->getRawName(), element->getRawName())) {
                            *newSet |= *fFollowList[leafIndex];
                        }
                    }
                    else {
                        if ((leaf->getURI() == element->getURI()) &&
                            (XMLString::equals(leaf->getLocalPart(), element->getLocalPart()))) {
                            *newSet |= *fFollowList[leafIndex];
                        }
                    }
                }
            } // for leafIndex
#endif

            // Optimization(Jan, 2001)
            int leafIndex = fLeafSorter[sorterIndex++];

            while (leafIndex != -1)
            {
                // If this leaf index (DFA position) is in the current set...
                if (setT->getBit(leafIndex))
                {
                    //
                    //  If this leaf is the current input symbol, then we
                    //  want to add its follow list to the set of states to
                    //  transition to from the current state.
                    //
                    *newSet |= *fFollowList[leafIndex];
                }
                leafIndex = fLeafSorter[sorterIndex++];
            } // while (leafIndex != -1)

            //
            //  If this new set is not empty, then see if its in the list
            //  of states to do. If not, then add it.
            //
            if (!newSet->isEmpty())
            {
                //
                //  Search the 'states to do' list to see if this new
                //  state set is already in there.
                //
                /***
                unsigned int stateIndex = 0;
                for (; stateIndex < curState; stateIndex++)
                {
                    if (*statesToDo[stateIndex] == *newSet)
                        break;
                }
                ***/

                XMLInteger *stateObj = (XMLInteger*) (stateTable->get(newSet));
                unsigned int stateIndex = (stateObj == 0 ? curState : stateObj->intValue());

                // If we did not find it, then add it
                if (stateIndex == curState)
                {
                    //
                    //  Put this new state into the states to do and init
                    //  a new entry at the same index in the transition
                    //  table.
                    //
                    statesToDo[curState] = newSet;
                    fTransTable[curState] = makeDefStateList();
                    stateTable->put
                    (
                        newSet
                        , new (fMemoryManager) XMLInteger(curState)
                    );

                    // We now have a new state to do so bump the count
                    curState++;

                    //
                    //  Null out the new set to indicate we adopted it. This
                    //  will cause the creation of a new set on the next time
                    //  around the loop.
                    //
                    newSet = 0;
                }

                //
                //  Now set this state in the transition table's entry for this
                //  element (using its index), with the DFA state we will move
                //  to from the current state when we see this input element.
                //
                transEntry[elemIndex] = stateIndex;

                // Expand the arrays if we're full
                if (curState == curArraySize)
                {
                    //
                    //  Yikes, we overflowed the initial array size, so we've
                    //  got to expand all of these arrays. So adjust up the
                    //  size by 50% and allocate new arrays.
                    //
                    const unsigned int newSize = (unsigned int)(curArraySize * 1.5);
                    const CMStateSet** newToDo = (const CMStateSet**) 
                        fMemoryManager->allocate
                        (
                            newSize * sizeof(const CMStateSet*)
                        ); //new const CMStateSet*[newSize];
                    bool* newFinalFlags = (bool*) fMemoryManager->allocate
                    (
                        newSize * sizeof(bool)
                    ); //new bool[newSize];
                    unsigned int** newTransTable = (unsigned int**)
                        fMemoryManager->allocate
                        (
                            newSize * sizeof(unsigned int*)
                        ); //new unsigned int*[newSize];

                    // Copy over all of the existing content
                    for (unsigned int expIndex = 0; expIndex < curArraySize; expIndex++)
                    {
                        newToDo[expIndex] = statesToDo[expIndex];
                        newFinalFlags[expIndex] = fFinalStateFlags[expIndex];
                        newTransTable[expIndex] = fTransTable[expIndex];
                    }

                    // Clean up the old stuff
                    fMemoryManager->deallocate(statesToDo); //delete [] statesToDo;
                    fMemoryManager->deallocate(fFinalStateFlags); //delete [] fFinalStateFlags;
                    fMemoryManager->deallocate(fTransTable); //delete [] fTransTable;

                    // Store the new array size and pointers
                    curArraySize = newSize;
                    statesToDo = newToDo;
                    fFinalStateFlags = newFinalFlags;
                    fTransTable = newTransTable;
                } //if (curState == curArraySize)
            } //if (!newSet->isEmpty())
        } // for elemIndex
    } //while

    // Store the current state count in the trans table size
    fTransTableSize = curState;

    // If the last temp set was not stored, then clean it up
    if (newSet)
        delete newSet;

    //
    //  Now we can clean up all of the temporary data that was needed during
    //  DFA build.
    //

    //
    // Note on memory leak: Bugzilla#2707:
    // ===================================
    // The CMBinary, pointed to by fHeadNode, shall be released by
    // deleted by itself.
    //
    // Change has been made to postTreeBuildInit() such that fLeafList[]
    // would maintain its **OWN** copy of CMLeaf to avoid double deletion
    // of CMLeaf.
    //

    delete fHeadNode;

    for (index = 0; index < fLeafCount; index++)
        delete fFollowList[index];
    fMemoryManager->deallocate(fFollowList); //delete [] fFollowList;

    //
    // removeAll() will delete all data, XMLInteger,
    // while the keys are to be deleted by the
    // deletion of statesToDo.
    //
    delete stateTable;

    for (index = 0; index < curState; index++)
        delete (CMStateSet*)statesToDo[index];
    fMemoryManager->deallocate(statesToDo); //delete [] statesToDo;

    for (index = 0; index < fLeafCount; index++)
        delete fLeafList[index];
    fMemoryManager->deallocate(fLeafList); //delete [] fLeafList;

    fMemoryManager->deallocate(fLeafSorter); //delete [] fLeafSorter;

}


CMNode* DFAContentModel::buildSyntaxTree(ContentSpecNode* const curNode)
{
    // Initialize a return node pointer
    CMNode* retNode = 0;

    // Get the spec type of the passed node
    const ContentSpecNode::NodeTypes curType = curNode->getType();

    if ((curType & 0x0f) == ContentSpecNode::Any
        || (curType & 0x0f) == ContentSpecNode::Any_Other
        || (curType & 0x0f) == ContentSpecNode::Any_NS)
    {
        retNode = new (fMemoryManager) CMAny
        (
            curType
            , curNode->getElement()->getURI()
            , fLeafCount++
            , fMemoryManager
        );
    }
    else if (curType == ContentSpecNode::Leaf)
    {
        //
        //  Create a new leaf node, and pass it the current leaf count, which
        //  is its DFA state position. Bump the leaf count after storing it.
        //  This makes the positions zero based since we store first and then
        //  increment.
        //
        retNode = new (fMemoryManager) CMLeaf
        (
            curNode->getElement()
            , fLeafCount++
            , fMemoryManager
        );
    }
     else
    {
        //
        //  Its not a leaf, so we have to recurse its left and maybe right
        //  nodes. Save both values before we recurse and trash the node.
        //
        ContentSpecNode* leftNode = curNode->getFirst();
        ContentSpecNode* rightNode = curNode->getSecond();

        if (((curType & 0x0f) == ContentSpecNode::Choice)
        ||   ((curType & 0x0f) == ContentSpecNode::Sequence))
        {
            //
            //  Recurse on both children, and return a binary op node with the
            //  two created sub nodes as its children. The node type is the
            //  same type as the source.
            //
            CMNode* newLeft = buildSyntaxTree(leftNode);
            CMNode* newRight = buildSyntaxTree(rightNode);
            retNode = new (fMemoryManager) CMBinaryOp
            (
                curType
                , newLeft
                , newRight
                , fMemoryManager
            );
        }
         else if (curType == ContentSpecNode::ZeroOrMore
               || curType == ContentSpecNode::ZeroOrOne
               || curType == ContentSpecNode::OneOrMore)
        {
            // This one is fine as is, just change to our form
            retNode = new (fMemoryManager) CMUnaryOp
            (
                curType
                , buildSyntaxTree(leftNode)
                , fMemoryManager
            );
        }
         else
        {
            ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::CM_UnknownCMSpecType, fMemoryManager);
        }
    }
    return retNode;
}


void DFAContentModel::calcFollowList(CMNode* const curNode)
{
    // Get the spec type of the passed node
    const ContentSpecNode::NodeTypes curType = curNode->getType();

    if ((curType & 0x0f) == ContentSpecNode::Choice)
    {
        // Just recurse
        calcFollowList(((CMBinaryOp*)curNode)->getLeft());
        calcFollowList(((CMBinaryOp*)curNode)->getRight());
    }
    else if ((curType & 0x0f) == ContentSpecNode::Sequence)
    {
        // Recurse before we process this node
        calcFollowList(((CMBinaryOp*)curNode)->getLeft());
        calcFollowList(((CMBinaryOp*)curNode)->getRight());

        //
        //  Now handle our level. We use our left child's last pos set and our
        //  right child's first pos set, so get them now for convenience.
        //
        const CMStateSet& last  = ((CMBinaryOp*)curNode)->getLeft()->getLastPos();
        const CMStateSet& first = ((CMBinaryOp*)curNode)->getRight()->getFirstPos();

        //
        //  Now, for every position which is in our left child's last set
        //  add all of the states in our right child's first set to the
        //  follow set for that position.
        //
        for (unsigned int index = 0; index < fLeafCount; index++)
        {
            if (last.getBit(index))
                *fFollowList[index] |= first;
        }
    }
     else if (curType == ContentSpecNode::ZeroOrMore ||
		      curType == ContentSpecNode::OneOrMore   )
    {
        // Recurse first
        calcFollowList(((CMUnaryOp*)curNode)->getChild());

        //
        //  Now handle our level. We use our own first and last position
        //  sets, so get them up front.
        //
        const CMStateSet& first = curNode->getFirstPos();
        const CMStateSet& last  = curNode->getLastPos();

        //
        //  For every position which is in our last position set, add all
        //  of our first position states to the follow set for that
        //  position.
        //
        for (unsigned int index = 0; index < fLeafCount; index++)
        {
            if (last.getBit(index))
                *fFollowList[index] |= first;
        }
    }
     else if (curType == ContentSpecNode::ZeroOrOne)
    {
        // Recurse only
        calcFollowList(((CMUnaryOp*)curNode)->getChild());
    }
}


//
//  gInvalidTrans is used to represent bad transitions in the transition table
//  entry for each state. So each entry is initialized to that value. This
//  method creates a new entry and initializes it.
//
unsigned int* DFAContentModel::makeDefStateList() const
{
    unsigned int* retArray = (unsigned int*) fMemoryManager->allocate
    (
        fElemMapSize * sizeof(unsigned int)
    ); //new unsigned int[fElemMapSize];
    for (unsigned int index = 0; index < fElemMapSize; index++)
        retArray[index] = XMLContentModel::gInvalidTrans;
    return retArray;
}


int DFAContentModel::postTreeBuildInit(         CMNode* const   nodeCur
                                        , const unsigned int    curIndex)
{
    // Set the maximum states on this node
    nodeCur->setMaxStates(fLeafCount);

    // Get the spec type of the passed node
    const ContentSpecNode::NodeTypes curType = nodeCur->getType();

    // Get a copy of the index we can modify
    unsigned int newIndex = curIndex;

    // Recurse as required
    if ( ((curType & 0x0f) == ContentSpecNode::Any)       ||
         ((curType & 0x0f) == ContentSpecNode::Any_NS) ||
         ((curType & 0x0f) == ContentSpecNode::Any_Other)  )
    {
        fLeafList[newIndex] = new (fMemoryManager) CMLeaf
        (
            new (fMemoryManager) QName
            (
                XMLUni::fgZeroLenString
                , XMLUni::fgZeroLenString
                , ((CMAny*) nodeCur)->getURI()
                , fMemoryManager
            )
            , ((CMAny*)nodeCur)->getPosition()
            , true
            , fMemoryManager
        );
        fLeafListType[newIndex] = curType;
        ++newIndex;
    }
    else if (((curType & 0x0f) == ContentSpecNode::Choice)
         ||  ((curType & 0x0f) == ContentSpecNode::Sequence))
    {
        newIndex = postTreeBuildInit(((CMBinaryOp*)nodeCur)->getLeft(), newIndex);
        newIndex = postTreeBuildInit(((CMBinaryOp*)nodeCur)->getRight(), newIndex);
    }
    else if (curType == ContentSpecNode::ZeroOrMore ||
             curType == ContentSpecNode::ZeroOrOne  ||
             curType == ContentSpecNode::OneOrMore)
    {
        newIndex = postTreeBuildInit(((CMUnaryOp*)nodeCur)->getChild(), newIndex);
    }
    else if (curType == ContentSpecNode::Leaf)
    {
        //
        //  Put this node in the leaf list at the current index if its
        //  a non-epsilon leaf.
        //
        if (((CMLeaf*)nodeCur)->getElement()->getURI() != XMLContentModel::gEpsilonFakeId)
        {
            //
            // fLeafList make its own copy of the CMLeaf, so that
            // delete[] fLeafList and delete the owner of the nodeCur
            // will NOT delete the nodeCur --twice--,
            // thuse to make delete the owner of the nodeCur possible.
            //
            fLeafList[newIndex] = new (fMemoryManager) CMLeaf
            (
                ((CMLeaf*)nodeCur)->getElement()
                , ((CMLeaf*)nodeCur)->getPosition()
                , fMemoryManager
            );
            fLeafListType[newIndex] = ContentSpecNode::Leaf;
            ++newIndex;
        }
    }
    else
    {
        ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::CM_UnknownCMSpecType, fMemoryManager);
    }
    return newIndex;
}



ContentLeafNameTypeVector* DFAContentModel::getContentLeafNameTypeVector() const
{
   //later change it to return the data member
	return fLeafNameTypeVector;
}

void DFAContentModel::checkUniqueParticleAttribution (SchemaGrammar*    const pGrammar,
                                                      GrammarResolver*  const pGrammarResolver,
                                                      XMLStringPool*    const pStringPool,
                                                      XMLValidator*     const pValidator,
                                                      unsigned int*     const pContentSpecOrgURI,
                                                      const XMLCh*            pComplexTypeName /*= 0*/)
{

    SubstitutionGroupComparator comparator(pGrammarResolver, pStringPool);

    unsigned int i, j, k;

    // Rename the URI back
    for (i = 0; i < fElemMapSize; i++) {

        unsigned int orgURIIndex = fElemMap[i]->getURI();

        if ((orgURIIndex != XMLContentModel::gEOCFakeId) &&
            (orgURIIndex != XMLContentModel::gEpsilonFakeId) &&
            (orgURIIndex != XMLElementDecl::fgInvalidElemId) &&
            (orgURIIndex != XMLElementDecl::fgPCDataElemId)) {
            fElemMap[i]->setURI(pContentSpecOrgURI[orgURIIndex]);
        }
    }

    // Unique Particle Attribution
    // store the conflict results between any two elements in fElemMap
    // XMLContentModel::gInvalidTrans: not compared; 0: no conflict; 1: conflict
    unsigned int** fConflictTable = (unsigned int**) fMemoryManager->allocate
    (
        fElemMapSize * sizeof(unsigned int*)
    ); //new unsigned int*[fElemMapSize];

    // initialize the conflict table
    for (j = 0; j < fElemMapSize; j++) {
        fConflictTable[j] = (unsigned int*) fMemoryManager->allocate
        (
            fElemMapSize * sizeof(unsigned int)
        ); //new unsigned int[fElemMapSize];
        for (k = j+1; k < fElemMapSize; k++)
            fConflictTable[j][k] = XMLContentModel::gInvalidTrans;
    }

    // for each state, check whether it has overlap transitions
    for (i = 0; i < fTransTableSize; i++) {
        for (j = 0; j < fElemMapSize; j++) {
            for (k = j+1; k < fElemMapSize; k++) {
                if (fTransTable[i][j] != XMLContentModel::gInvalidTrans &&
                    fTransTable[i][k] != XMLContentModel::gInvalidTrans &&
                    fConflictTable[j][k] == XMLContentModel::gInvalidTrans) {

                    // If this is text in a Schema mixed content model, skip it.
                    if ( fIsMixed &&
                         (( fElemMap[j]->getURI() == XMLElementDecl::fgPCDataElemId) ||
                          ( fElemMap[k]->getURI() == XMLElementDecl::fgPCDataElemId)))
                        continue;

                    if (XercesElementWildcard::conflict(pGrammar,
                                                        fElemMapType[j],
                                                        fElemMap[j],
                                                        fElemMapType[k],
                                                        fElemMap[k],
                                                        &comparator)) {
                       fConflictTable[j][k] = 1;

                       XMLBuffer buf1(1023, fMemoryManager);
                       if (((fElemMapType[j] & 0x0f) == ContentSpecNode::Any) ||
                           ((fElemMapType[j] & 0x0f) == ContentSpecNode::Any_NS))
                           buf1.set(SchemaSymbols::fgATTVAL_TWOPOUNDANY);
                       else if ((fElemMapType[j] & 0x0f) == ContentSpecNode::Any_Other)
                           buf1.set(SchemaSymbols::fgATTVAL_TWOPOUNDOTHER);
                       else
                           buf1.set(fElemMap[j]->getRawName());

                       XMLBuffer buf2(1023, fMemoryManager);
                       if (((fElemMapType[k] & 0x0f) == ContentSpecNode::Any) ||
                           ((fElemMapType[k] & 0x0f) == ContentSpecNode::Any_NS))
                           buf2.set(SchemaSymbols::fgATTVAL_TWOPOUNDANY);
                       else if ((fElemMapType[k] & 0x0f) == ContentSpecNode::Any_Other)
                           buf2.set(SchemaSymbols::fgATTVAL_TWOPOUNDOTHER);
                       else
                           buf2.set(fElemMap[k]->getRawName());

                       pValidator->emitError(XMLValid::UniqueParticleAttributionFail,
                                             pComplexTypeName,
                                             buf1.getRawBuffer(),
                                             buf2.getRawBuffer());
                    }
                    else
                       fConflictTable[j][k] = 0;
                }
            }
        }
    }

    for (i = 0; i < fElemMapSize; i++)
        fMemoryManager->deallocate(fConflictTable[i]); //delete [] fConflictTable[i];
    fMemoryManager->deallocate(fConflictTable); //delete [] fConflictTable;
}

XERCES_CPP_NAMESPACE_END

