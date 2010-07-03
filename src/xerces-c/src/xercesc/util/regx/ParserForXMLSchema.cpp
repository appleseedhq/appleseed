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
 * $Id: ParserForXMLSchema.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/regx/ParserForXMLSchema.hpp>
#include <xercesc/util/regx/TokenFactory.hpp>
#include <xercesc/util/regx/RangeToken.hpp>
#include <xercesc/util/regx/TokenInc.hpp>
#include <xercesc/util/regx/RegxDefs.hpp>
#include <xercesc/util/ParseException.hpp>
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/util/PlatformUtils.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  ParserForXMLSchema: Constructors and Destructors
// ---------------------------------------------------------------------------
ParserForXMLSchema::ParserForXMLSchema(MemoryManager* const manager)
    : RegxParser(manager)
{

}

ParserForXMLSchema::~ParserForXMLSchema() {

}

// ---------------------------------------------------------------------------
//  ParserForXMLSchema: Parsing/Processing methods
// ---------------------------------------------------------------------------
Token* ParserForXMLSchema::processCaret() {

    processNext();
    return getTokenFactory()->createChar(chCaret);
}

Token* ParserForXMLSchema::processDollar() {

    processNext();
    return getTokenFactory()->createChar(chDollarSign);
}

Token* ParserForXMLSchema::processPlus(Token* const tok) {

    processNext();
    return getTokenFactory()->createConcat(tok,
                               getTokenFactory()->createClosure(tok));
}

Token* ParserForXMLSchema::processStar(Token* const tok) {

    processNext();
    return getTokenFactory()->createClosure(tok);
}

Token* ParserForXMLSchema::processQuestion(Token* const tok) {

    processNext();

    TokenFactory* tokFactory = getTokenFactory();
    Token* retTok = tokFactory->createUnion();
    retTok->addChild(tok, tokFactory);
    retTok->addChild(tokFactory->createToken(Token::T_EMPTY), tokFactory);
    return retTok;
}

Token* ParserForXMLSchema::processParen() {

    processNext();
    Token* retTok = getTokenFactory()->createParenthesis(parseRegx(true), 0);

    if (getState() != REGX_T_RPAREN) {
        ThrowXMLwithMemMgr(ParseException, XMLExcepts::Parser_Factor1, getMemoryManager());
    }

    processNext();
    return retTok;
}

RangeToken* ParserForXMLSchema::parseCharacterClass(const bool) {

    setParseContext(S_INBRACKETS);
    processNext();

    RangeToken* base = 0;
    RangeToken* tok = 0;
    bool isNRange = false;

    if (getState() == REGX_T_CHAR && getCharData() == chCaret) {

        isNRange = true;
        processNext();
        base = getTokenFactory()->createRange();
        base->addRange(0, Token::UTF16_MAX);
        tok = getTokenFactory()->createRange();
    }
    else {
        tok= getTokenFactory()->createRange();
    }

    int type;
    bool firstLoop = true;
    bool wasDecoded;

    while ( (type = getState()) != REGX_T_EOF) {

        wasDecoded = false;

        // single range | from-to-range | subtraction
        if (type == REGX_T_CHAR && getCharData() == chCloseSquare && !firstLoop) {

            if (isNRange) {

                base->subtractRanges(tok);
                tok = base;
            }
            break;
        }

        XMLInt32 ch = getCharData();
        bool     end = false;

        if (type == REGX_T_BACKSOLIDUS) {

            switch(ch) {
            case chLatin_d:
            case chLatin_D:
            case chLatin_w:
            case chLatin_W:
            case chLatin_s:
            case chLatin_S:
                {
                    tok->mergeRanges(getTokenForShorthand(ch));
                    end = true;
                }
                break;
            case chLatin_i:
            case chLatin_I:
            case chLatin_c:
            case chLatin_C:
                {
                    ch = processCInCharacterClass(tok, ch);
                    if (ch < 0) {
                        end = true;
                    }
                }
                break;
            case chLatin_p:
            case chLatin_P:
                {                    
                    RangeToken* tok2 = processBacksolidus_pP(ch);

                    if (tok2 == 0) {
                        ThrowXMLwithMemMgr(ParseException,XMLExcepts::Parser_Atom5, getMemoryManager());
                    }

                    tok->mergeRanges(tok2);
                    end = true;
                }
                break;
            case chDash:
                wasDecoded = true;
                // fall thru to default.
            default:
                ch = decodeEscaped();
            }
        } // end if REGX_T_BACKSOLIDUS
        else if (type == REGX_T_XMLSCHEMA_CC_SUBTRACTION && !firstLoop) {

            if (isNRange) {

                base->subtractRanges(tok);
                tok = base;
            }

            RangeToken* rangeTok = parseCharacterClass(false);
            tok->subtractRanges(rangeTok);

            if (getState() != REGX_T_CHAR || getCharData() != chCloseSquare) {
                ThrowXMLwithMemMgr(ParseException,XMLExcepts::Parser_CC5, getMemoryManager());
            }
            break;
        } // end if REGX_T_XMLSCHEMA...

        processNext();

        if (!end) {

            if (type == REGX_T_CHAR
                && (ch == chOpenSquare
                    || ch == chCloseSquare
                    || (ch == chDash && getCharData() == chCloseSquare && firstLoop))) {
                // if regex = [-] then invalid...
                // '[', ']', '-' not allowed and should be esacaped
                XMLCh chStr[] = { ch, chNull };
                ThrowXMLwithMemMgr2(ParseException,XMLExcepts::Parser_CC6, chStr, chStr, getMemoryManager());
            }
            if (ch == chDash && getCharData() == chDash && getState() != REGX_T_BACKSOLIDUS && !wasDecoded) {
                XMLCh chStr[] = { ch, chNull };
                ThrowXMLwithMemMgr2(ParseException,XMLExcepts::Parser_CC6, chStr, chStr, getMemoryManager());
            }

            if (getState() != REGX_T_CHAR || getCharData() != chDash) {
                tok->addRange(ch, ch);
            }
            else {

                processNext();
                if ((type = getState()) == REGX_T_EOF)
                    ThrowXMLwithMemMgr(ParseException,XMLExcepts::Parser_CC2, getMemoryManager());

                if (type == REGX_T_CHAR && getCharData() == chCloseSquare) {
                    tok->addRange(ch, ch);
                    tok->addRange(chDash, chDash);
                }
                else if (type == REGX_T_XMLSCHEMA_CC_SUBTRACTION) {

                    static const XMLCh dashStr[] = { chDash, chNull};
                    ThrowXMLwithMemMgr2(ParseException, XMLExcepts::Parser_CC6, dashStr, dashStr, getMemoryManager());
                }
                else {

                    XMLInt32 rangeEnd = getCharData();
                    XMLCh rangeEndStr[] = { rangeEnd, chNull };

                    if (type == REGX_T_CHAR) {

                        if (rangeEnd == chOpenSquare
                            || rangeEnd == chCloseSquare
                            || rangeEnd == chDash)
                            // '[', ']', '-' not allowed and should be esacaped
                            ThrowXMLwithMemMgr2(ParseException, XMLExcepts::Parser_CC6, rangeEndStr, rangeEndStr, getMemoryManager());
                    }
                    else if (type == REGX_T_BACKSOLIDUS) {
                        rangeEnd = decodeEscaped();
                    }

                    processNext();

                    if (ch > rangeEnd) {
                        XMLCh chStr[] = { ch, chNull };
                        ThrowXMLwithMemMgr2(ParseException,XMLExcepts::Parser_Ope3, rangeEndStr, chStr, getMemoryManager());
                    }

                    tok->addRange(ch, rangeEnd);
                }
            }
        }
        firstLoop = false;
    }

    if (getState() == REGX_T_EOF)
        ThrowXMLwithMemMgr(ParseException,XMLExcepts::Parser_CC2, getMemoryManager());

    tok->sortRanges();
    tok->compactRanges();
    setParseContext(S_NORMAL);
    processNext();

    return tok;
}

XMLInt32 ParserForXMLSchema::processCInCharacterClass(RangeToken* const tok,
                                                      const XMLInt32 ch)
{
    tok->mergeRanges(getTokenForShorthand(ch));
    return -1;
}

Token* ParserForXMLSchema::processLook(const unsigned short) {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_A() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_B() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_b() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_C() {

    processNext();
    return getTokenForShorthand(chLatin_C);
}

Token* ParserForXMLSchema::processBacksolidus_c() {

    processNext();
    return getTokenForShorthand(chLatin_c);
}

Token* ParserForXMLSchema::processBacksolidus_g() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_gt() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_I() {

    processNext();
    return getTokenForShorthand(chLatin_I);
}

Token* ParserForXMLSchema::processBacksolidus_i() {

    processNext();
    return getTokenForShorthand(chLatin_i);
}

Token* ParserForXMLSchema::processBacksolidus_lt() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_X() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_Z() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_z() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBackReference() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processCondition() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processIndependent() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processModifiers() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processParen2() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

RangeToken* ParserForXMLSchema::parseSetOperations() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

// ---------------------------------------------------------------------------
//  ParserForXMLSchema: Getter methods
// ---------------------------------------------------------------------------
Token* ParserForXMLSchema::getTokenForShorthand(const XMLInt32 ch) {

    switch(ch) {
    case chLatin_d:
        return getTokenFactory()->getRange(fgXMLDigit);
    case chLatin_D:
        return getTokenFactory()->getRange(fgXMLDigit, true);
    case chLatin_w:
        return getTokenFactory()->getRange(fgXMLWord);
    case chLatin_W:
        return getTokenFactory()->getRange(fgXMLWord, true);
    case chLatin_s:
        return getTokenFactory()->getRange(fgXMLSpace);
    case chLatin_S:
        return getTokenFactory()->getRange(fgXMLSpace, true);
    case chLatin_c:
        return getTokenFactory()->getRange(fgXMLNameChar);
    case chLatin_C:
        return getTokenFactory()->getRange(fgXMLNameChar, true);
    case chLatin_i:
        return getTokenFactory()->getRange(fgXMLInitialNameChar);
    case chLatin_I:
        return getTokenFactory()->getRange(fgXMLInitialNameChar, true);
    }

    return 0;
}

// ---------------------------------------------------------------------------
//  ParserForXMLSchema: Helper methods
// ---------------------------------------------------------------------------
bool ParserForXMLSchema::checkQuestion(const int) {

    return false;
}


XMLInt32 ParserForXMLSchema::decodeEscaped() {

    if (getState() != REGX_T_BACKSOLIDUS)
        ThrowXMLwithMemMgr(ParseException,XMLExcepts::Parser_Next1, getMemoryManager());

    XMLInt32 ch = getCharData();

    switch (ch) {
    case chLatin_n:
        ch = chLF;
        break;
    case chLatin_r:
        ch = chCR;
        break;
    case chLatin_t:
        ch = chHTab;
        break;
    case chBackSlash:
    case chPipe:
    case chPeriod:
    case chCaret:
    case chDash:
    case chQuestion:
    case chAsterisk:
    case chPlus:
    case chOpenCurly:
    case chCloseCurly:
    case chOpenParen:
    case chCloseParen:
    case chOpenSquare:
    case chCloseSquare:
        break;
    default:
		{
        XMLCh chString[] = {chBackSlash, ch, chNull};        
        ThrowXMLwithMemMgr1(ParseException,XMLExcepts::Parser_Process2, chString, getMemoryManager());
        }
    }

    return ch;
}

XERCES_CPP_NAMESPACE_END

/**
  * End of file ParserForXMLSchema.cpp
  */
