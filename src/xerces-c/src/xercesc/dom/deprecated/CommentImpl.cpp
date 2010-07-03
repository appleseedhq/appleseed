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
 * $Id: CommentImpl.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

/**
 * Represents an XML (or HTML) comment.
 *
 * @author Rania Y. Khalaf
 * @author Joseph Kesselman
 * @since  PR-DOM-Level-1-19980818.
 */

#include "CommentImpl.hpp"
#include "CharacterDataImpl.hpp"
#include "DOM_Node.hpp"
#include "DocumentImpl.hpp"
#include "DStringPool.hpp"

XERCES_CPP_NAMESPACE_BEGIN


static DOMString *gComment = 0; // will be lazily initialized to "#comment"
static XMLRegisterCleanup gCommentCleanup;

CommentImpl::CommentImpl(DocumentImpl *ownerDoc, const DOMString &dat)
    : CharacterDataImpl(ownerDoc, dat)
{
};


CommentImpl::CommentImpl(const CommentImpl &other, bool deep)
    : CharacterDataImpl(other, deep)
{
};


CommentImpl::~CommentImpl() {
};



NodeImpl * CommentImpl::cloneNode(bool deep)
{
    return new (getOwnerDocument()->getMemoryManager()) CommentImpl(*this, deep);
};


DOMString CommentImpl::getNodeName() {
    return DStringPool::getStaticString("#comment"
                                      , &gComment
                                      , reinitCommentImpl
                                      , gCommentCleanup);
}

short CommentImpl::getNodeType() {
    return DOM_Node::COMMENT_NODE;
};

// -----------------------------------------------------------------------
//  Notification that lazy data has been deleted
// -----------------------------------------------------------------------
void CommentImpl::reinitCommentImpl() {
	delete gComment;
	gComment = 0;
}

XERCES_CPP_NAMESPACE_END

