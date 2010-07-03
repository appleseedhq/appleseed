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
 * $Id: ModifierToken.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

#if !defined(MODIFIERTOKEN_HPP)
#define MODIFIERTOKEN_HPP

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/regx/Token.hpp>

XERCES_CPP_NAMESPACE_BEGIN

class XMLUTIL_EXPORT ModifierToken : public Token {
public:
	// -----------------------------------------------------------------------
    //  Public Constructors and Destructor
    // -----------------------------------------------------------------------
	ModifierToken(Token* const child, const int options, const int mask
        , MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager);
    ~ModifierToken();

	// -----------------------------------------------------------------------
    //  Getter methods
    // -----------------------------------------------------------------------
    int size() const;
	int getOptions() const;
	int getOptionsMask() const;
    Token* getChild(const int index) const;

private:
	// -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    ModifierToken(const ModifierToken&);
    ModifierToken& operator=(const ModifierToken&);

	// -----------------------------------------------------------------------
    //  Private data members
	// -----------------------------------------------------------------------
	int    fOptions;
	int    fOptionsMask;
	Token* fChild;
};


// ---------------------------------------------------------------------------
//  ModifierToken: getter methods
// ---------------------------------------------------------------------------
inline int ModifierToken::size() const {

    return 1;
}

inline int ModifierToken::getOptions() const {

    return fOptions;
}

inline int ModifierToken::getOptionsMask() const {

    return fOptionsMask;
}

inline Token* ModifierToken::getChild(const int) const {

    return fChild;
}

XERCES_CPP_NAMESPACE_END

#endif

/**
  * End of file ModifierToken.hpp
  */
