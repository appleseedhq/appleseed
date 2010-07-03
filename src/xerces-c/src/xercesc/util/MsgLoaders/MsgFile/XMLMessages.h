#ifndef XML_ERROR_MESSAGES_H
#define XML_ERROR_MESSAGES_H

#include <xercesc/util/XercesDefs.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// These are Fatal error messages

#define E_ExpectedCommentOrCDATA					"XMLBE00"
#define E_ExpectedAttrName						"XMLBE01"
#define E_ExpectedNotationName						"XMLBE02"
#define E_NoRepInMixed							"XMLBE03"
#define E_BadDefAttrDecl						"XMLBE04"
#define E_ExpectedDefAttrDecl						"XMLBE05"
#define E_AttListSyntaxError						"XMLBE06"
#define E_ExpectedEqSign						"XMLBE07"
#define E_DupAttrName							"XMLBE08"
#define E_BadIdForXMLLangAttr						"XMLBE09"
#define E_ExpectedElementName						"XMLBE0A"
#define E_MustStartWithXMLDecl						"XMLBE0B"
#define E_CommentsMustStartWith						"XMLBE0C"
#define E_InvalidDocumentStructure					"XMLBE0D"
#define E_ExpectedDeclString						"XMLBE0E"
#define E_BadXMLVersion							"XMLBE0F"
#define E_UnsupportedXMLVersion						"XMLBE10"
#define E_UnterminatedXMLDecl						"XMLBE11"
#define E_BadXMLEncoding						"XMLBE12"
#define E_BadStandalone							"XMLBE13"
#define E_UnterminatedComment						"XMLBE14"
#define E_PINameExpected						"XMLBE15"
#define E_UnterminatedPI						"XMLBE16"
#define E_InvalidCharacter						"XMLBE17"
#define E_UnexpectedEOF							"XMLBE18"
#define E_UnexpectedTextBeforeRoot					"XMLBE19"
#define E_UnterminatedStartTag						"XMLBE1A"
#define E_ExpectedAttrValue						"XMLBE1B"
#define E_UnterminatedEndTag						"XMLBE1C"
#define E_ExpectedAttributeType						"XMLBE1D"
#define E_ExpectedEndOfTagX						"XMLBE1E"
#define E_ExpectedMarkup						"XMLBE1F"
#define E_NotValidAfterContent						"XMLBE20"
#define E_ExpectedComment						"XMLBE21"
#define E_ExpectedCommentOrPI						"XMLBE22"
#define E_ExpectedWhitespace						"XMLBE23"
#define E_NoRootElemInDOCTYPE						"XMLBE24"
#define E_ExpectedQuotedString						"XMLBE25"
#define E_ExpectedPublicId						"XMLBE26"
#define E_InvalidPublicIdChar						"XMLBE27"
#define E_UnterminatedDOCTYPE						"XMLBE28"
#define E_InvalidCharacterInIntSubset					"XMLBE29"
#define E_ExpectedCDATA							"XMLBE2A"
#define E_InvalidInitialNameChar					"XMLBE2B"
#define E_InvalidNameChar						"XMLBE2C"
#define E_UnexpectedWhitespace						"XMLBE2D"
#define E_InvalidCharacterInAttrValue					"XMLBE2E"
#define E_ExpectedMarkupDecl						"XMLBE2F"
#define E_TextDeclNotLegalHere						"XMLBE30"
#define E_ConditionalSectInIntSubset					"XMLBE31"
#define E_ExpectedPEName						"XMLBE32"
#define E_UnterminatedEntityDecl					"XMLBE33"
#define E_InvalidCharacterRef						"XMLBE34"
#define E_UnterminatedCharRef						"XMLBE35"
#define E_ExpectedEntityRefName						"XMLBE36"
#define E_EntityNotFound						"XMLBE37"
#define E_NoUnparsedEntityRefs						"XMLBE38"
#define E_UnterminatedEntityRef						"XMLBE39"
#define E_RecursiveEntity						"XMLBE3A"
#define E_PartialMarkupInEntity						"XMLBE3B"
#define E_UnterminatedElementDecl					"XMLBE3C"
#define E_ExpectedContentSpecExpr					"XMLBE3D"
#define E_ExpectedAsterisk						"XMLBE3E"
#define E_UnterminatedContentModel					"XMLBE3F"
#define E_ExpectedSystemId						"XMLBE40"
#define E_ExpectedSystemOrPublicId					"XMLBE41"
#define E_UnterminatedNotationDecl					"XMLBE42"
#define E_ExpectedSeqChoiceLeaf						"XMLBE43"
#define E_ExpectedChoiceOrCloseParen					"XMLBE44"
#define E_ExpectedSeqOrCloseParen					"XMLBE45"
#define Gen_CouldNotOpenDTD						"XMLBE46"
#define V_NotSameAsFixedValue						"XMLBE47"
#define E_ExpectedEnumValue						"XMLBE48"
#define E_ExpectedEnumSepOrParen					"XMLBE49"
#define Gen_CouldNotOpenExtEntity					"XMLBE4A"
#define E_UnterminatedEntityLiteral					"XMLBE4B"
#define E_MoreEndThanStartTags						"XMLBE4C"
#define E_IllegalRefInStandalone					"XMLBE4D"
#define E_ExpectedOpenParen						"XMLBE4E"
#define E_SysException							"XMLBE4F"
#define E_AttrAlreadyUsedInSTag						"XMLBE50"
#define E_BracketInAttrValue						"XMLBE51"
#define E_XMLException							"XMLBE52"
#define E_Expected2ndSurrogateChar					"XMLBE53"
#define E_ExpectedEndOfConditional					"XMLBE54"
#define E_ExpectedIncOrIgn						"XMLBE55"
#define E_ExpectedINCLUDEBracket					"XMLBE56"
#define E_ExpectedTextDecl						"XMLBE57"
#define E_ExpectedXMLDecl						"XMLBE58"
#define E_UnexpectedEOE							"XMLBE59"
#define E_PEPropogated							"XMLBE5A"
#define E_ExtraCloseSquare						"XMLBE5B"
#define E_PERefInMarkupInIntSubset					"XMLBE5C"
#define E_EntityPropogated						"XMLBE5D"
#define E_ExpectedNumericalCharRef					"XMLBE5E"
#define E_ExpectedOpenSquareBracket					"XMLBE5F"
#define E_BadSequenceInCharData						"XMLBE60"
#define E_IllegalSequenceInComment					"XMLBE61"
#define E_UnterminatedCDATASection					"XMLBE62"
#define E_ExpectedNDATA							"XMLBE63"
#define E_NDATANotValidForPE						"XMLBE64"
#define E_HexRadixMustBeLowerCase					"XMLBE65"
#define E_DeclStringRep							"XMLBE66"
#define E_DeclStringsInWrongOrder					"XMLBE67"
#define E_NoExtRefsInAttValue						"XMLBE68"
#define E_XMLDeclMustBeLowerCase					"XMLBE69"
#define E_ExpectedEntityValue						"XMLBE6A"
#define E_BadDigitForRadix						"XMLBE6B"
#define E_EndedWithTagsOnStack						"XMLBE6C"
#define E_AmbiguousContentModel						"XMLBE6D"
#define E_NestedCDATA							"XMLBE6E"
#define Gen_NoDTDValidator						"XMLBE6F"
#define E_UnknownPrefix							"XMLBE70"



// These are Validity messages

#define V_ElementNotDefined						"XMLBE71"
#define V_AttNotDefined							"XMLBE72"
#define V_NotationNotDeclared						"XMLBE73"
#define V_RootElemNotLikeDocType					"XMLBE74"
#define V_RequiredAttrNotProvided					"XMLBE75"
#define V_ElementNotValidForContent					"XMLBE76"
#define V_BadIDAttrDefType						"XMLBE77"
#define V_InvalidEmptyAttValue						"XMLBE78"
#define V_ElementAlreadyExists						"XMLBE79"
#define V_MultipleIdAttrs						"XMLBE7A"
#define V_ReusedIDValue							"XMLBE7B"
#define V_IDNotDeclared							"XMLBE7C"
#define V_UnknownNotRefAttr						"XMLBE7D"
#define V_UndeclaredElemInDocType					"XMLBE7E"
#define V_EmptyNotValidForContent					"XMLBE7F"
#define V_AttNotDefinedForElement					"XMLBE80"
#define V_BadEntityRefAttr						"XMLBE81"
#define V_UnknownEntityRefAttr						"XMLBE82"
#define V_NotEnoughElemsForCM						"XMLBE83"



#define E_NoPIStartsWithXML						"XMLBE84"

// These are Warnings messages

#define W_NotationAlreadyExists						"XMLBE85"
#define W_AttListAlreadyExists						"XMLBE86"
#define W_ContradictoryEncoding						"XMLBE87"
#define W_UndeclaredElemInCM						"XMLBE88"
#define W_UndeclaredElemInAttList					"XMLBE89"


// These are Exception Messages

#define Array_BadIndex							"XMLBE8A"
#define Array_BadNewSize						"XMLBE8B"
#define CPtr_PointerIsZero						"XMLBE8C"
#define Enum_NoMoreElements						"XMLBE8D"
#define File_CouldNotOpenFile						"XMLBE8E"
#define File_CouldNotGetCurPos						"XMLBE8F"
#define File_CouldNotCloseFile						"XMLBE90"
#define File_CouldNotSeekToEnd						"XMLBE91"
#define File_CouldNotSeekToPos						"XMLBE92"
#define File_CouldNotDupHandle						"XMLBE93"
#define File_CouldNotReadFromFile					"XMLBE94"
#define File_CouldNotResetFile						"XMLBE95"
#define File_CouldNotGetSize						"XMLBE96"
#define File_CouldNotGetBasePathName					"XMLBE97"
#define HshTbl_ZeroModulus						"XMLBE98"
#define HshTbl_BadHashFromKey						"XMLBE99"
#define HshTbl_NoSuchKeyExists						"XMLBE9A"
#define Mutex_CouldNotCreate						"XMLBE9B"
#define Mutex_CouldNotClose						"XMLBE9C"
#define Mutex_CouldNotLock						"XMLBE9D"
#define Mutex_CouldNotUnlock						"XMLBE9E"
#define Mutex_CouldNotDestroy						"XMLBE9F"
#define Pool_ElemAlreadyExists						"XMLBEA0"
#define Pool_BadHashFromKey						"XMLBEA1"
#define Pool_InvalidId							"XMLBEA2"
#define Pool_ZeroModulus						"XMLBEA3"
#define Stack_BadIndex							"XMLBEA4"
#define Stack_EmptyStack						"XMLBEA5"
#define Str_ZeroSizedTargetBuf						"XMLBEA6"
#define Str_UnknownRadix						"XMLBEA7"
#define Str_TargetBufTooSmall						"XMLBEA8"
#define Str_StartIndexPastEnd						"XMLBEA9"
#define Strm_StdErrWriteFailure						"XMLBEAA"
#define Strm_StdOutWriteFailure						"XMLBEAB"
#define Strm_ConWriteFailure						"XMLBEAC"
#define StrPool_IllegalId						"XMLBEAD"
#define URL_MalformedURL						"XMLBEAE"
#define URL_UnsupportedProto						"XMLBEAF"
#define URL_OnlyLocalHost						"XMLBEB0"
#define Vector_BadIndex							"XMLBEB1"
#define Gen_UnexpectedEOF						"XMLBEB2"
#define Gen_ParseInProgress						"XMLBEB3"
#define AttrList_BadIndex						"XMLBEB4"
#define ElemStack_EmptyStack						"XMLBEB5"
#define ElemStack_BadIndex						"XMLBEB6"
#define ElemStack_StackUnderflow					"XMLBEB7"
#define ElemStack_NoParentPushed					"XMLBEB8"
#define RdrMgr_ReaderIdNotFound						"XMLBEB9"
#define Reader_BadAutoEncoding						"XMLBEBA"
#define Reader_CantCreateCvtrFor					"XMLBEBB"
#define Reader_CouldNotDecodeFirstLine					"XMLBEBC"
#define Reader_EOIInMultiSeq						"XMLBEBD"
/* #define Reader_EncodingNeedsCvtr					"XMLBEBE"   THIS GUY IS GONE */
#define Reader_BadUTF8Seq						"XMLBEBF"
#define Reader_BadSurrogateInUTF8					"XMLBEC0"
#define Scan_CouldNotOpenSource						"XMLBEC1"
#define Scan_UnbalancedStartEnd						"XMLBEC2"
#define AttDef_BadAttType						"XMLBEC3"
#define AttDef_BadDefAttType						"XMLBEC4"
#define BufMgr_NoMoreBuffers						"XMLBEC5"
#define BufMgr_BufferNotInPool						"XMLBEC6"
#define XMLRec_UnknownEncoding						"XMLBEC7"
#define Trans_CouldNotXCodeXMLData					"XMLBEC8"
#define Trans_CouldNotCreateDefCvtr					"XMLBEC9"
#define Bitset_BadIndex							"XMLBECA"
#define CM_BinOpHadUnaryType						"XMLBECB"
#define CM_MustBeMixedOrChildren					"XMLBECC"
#define CM_NoPCDATAHere							"XMLBECD"
#define CM_NotValidForSpecType						"XMLBECE"
#define CM_UnaryOpHadBinType						"XMLBECF"
#define CM_UnknownCMType						"XMLBED0"
#define CM_UnknownCMSpecType						"XMLBED1"
#define Val_InvalidElemId						"XMLBED2"

// These are the new messages for 3.0 code base
// Note ALL CAPITALIZED DEFINES ARE AS/400 UNIQUE MESSAGES
#define FILE_OPEN_PROBLEMS						"XMLBED3"
#define ICONV_CONVERT_PROBLEM						"XMLBED4"
#define ICONV_CCSID_PROBLEM						"XMLBED5"
#define E_Unexpected2ndSurrogateChar					"XMLBED6"
#define E_XMLDeclMustBeFirst						"XMLBED7"
#define E_XMLVersionRequired						"XMLBED8"
#define E_StandaloneNotLegal						"XMLBED9"
#define E_TooManyColonsInName						"XMLBEDA"
#define E_InvalidColonPos						"XMLBEDB"
#define E_ColonNotLegalWithNS						"XMLBEDC"
#define V_RepElemInMixed						"XMLBEDD"
#define Val_CantHaveIntSS						"XMLBEDE"
#define Scan_BadPScanToken						"XMLBEDF"
#define URL_NoProtocolPresent						"XMLBEE0"
#define URL_ExpectingTwoSlashes						"XMLBEE1"
#define URL_IncorrectEscapedCharRef					"XMLBEE2"
#define URL_UnterminatedHostComponent					"XMLBEE3"
#define URL_UnsupportedProto1						"XMLBEE4"
#define V_NoCharDataInCM						"XMLBEE5"
#define V_DoesNotMatchEnumList						"XMLBEE6"
#define V_AttrValNotName						"XMLBEE7"
#define V_NoMultipleValues						"XMLBEE8"
#define E_PartialTagMarkupError						"XMLBEE9"
#define E_EmptyMainEntity						"XMLBEEA"
#define E_CDATAOutsideOfContent						"XMLBEEB"
#define E_OnlyCharRefsAllowedHere					"XMLBEEC"
#define GENERAL_PANIC_MESSAGE   					"XMLBEED"

// NEW 3.1 MESSAGES					Woops, forgot XMLBEEE
#define NetAcc_InternalError						"XMLBEEF"
#define Reader_SrcOfsNotSupported					"XMLBEF0"
#define Trans_InvalidSizeReq						"XMLBEF1"
#define Trans_Unrepresentable						"XMLBEF2"
#define Trans_NotInSourceSet						"XMLBEF3"


char Errors[][8] =  {

  E_ExpectedCommentOrCDATA
 ,E_ExpectedAttrName	
 ,E_ExpectedNotationName
 ,E_NoRepInMixed
 ,E_BadDefAttrDecl
 ,E_ExpectedDefAttrDecl
 ,E_AttListSyntaxError
 ,E_ExpectedEqSign	
 ,E_DupAttrName	
 ,E_BadIdForXMLLangAttr	
 ,E_ExpectedElementName	
 ,E_MustStartWithXMLDecl
 ,E_CommentsMustStartWith
 ,E_InvalidDocumentStructure
 ,E_ExpectedDeclString	
 ,E_BadXMLVersion
 ,E_UnsupportedXMLVersion
 ,E_UnterminatedXMLDecl	
 ,E_BadXMLEncoding
 ,E_BadStandalone
 ,E_UnterminatedComment	
 ,E_PINameExpected
 ,E_UnterminatedPI
 ,E_InvalidCharacter
 ,E_UnexpectedTextBeforeRoot
 ,E_UnterminatedStartTag
 ,E_ExpectedAttrValue
 ,E_UnterminatedEndTag
 ,E_ExpectedAttributeType
 ,E_ExpectedEndOfTagX
 ,E_ExpectedMarkup
 ,E_NotValidAfterContent
 ,E_ExpectedComment
 ,E_ExpectedCommentOrPI	
 ,E_ExpectedWhitespace
 ,E_NoRootElemInDOCTYPE
 ,E_ExpectedQuotedString
 ,E_ExpectedPublicId
 ,E_InvalidPublicIdChar
 ,E_UnterminatedDOCTYPE	
 ,E_InvalidCharacterInIntSubset	
 ,E_ExpectedCDATA
 ,E_InvalidInitialNameChar
 ,E_InvalidNameChar
 ,E_UnexpectedWhitespace
 ,E_InvalidCharacterInAttrValue	
 ,E_ExpectedMarkupDecl
 ,E_TextDeclNotLegalHere
 ,E_ConditionalSectInIntSubset
 ,E_ExpectedPEName
 ,E_UnterminatedEntityDecl
 ,E_InvalidCharacterRef
 ,E_UnterminatedCharRef
 ,E_ExpectedEntityRefName
 ,E_EntityNotFound
 ,E_NoUnparsedEntityRefs
 ,E_UnterminatedEntityRef
 ,E_RecursiveEntity
 ,E_PartialMarkupInEntity
 ,E_UnterminatedElementDecl
 ,E_ExpectedContentSpecExpr
 ,E_ExpectedAsterisk
 ,E_UnterminatedContentModel
 ,E_ExpectedSystemId
 ,E_ExpectedSystemOrPublicId
 ,E_UnterminatedNotationDecl
 ,E_ExpectedSeqChoiceLeaf
 ,E_ExpectedChoiceOrCloseParen
 ,E_ExpectedSeqOrCloseParen
 ,E_ExpectedEnumValue
 ,E_ExpectedEnumSepOrParen
 ,E_UnterminatedEntityLiteral
 ,E_MoreEndThanStartTags
 ,E_IllegalRefInStandalone
 ,E_ExpectedOpenParen
 ,E_AttrAlreadyUsedInSTag
 ,E_BracketInAttrValue
 ,E_Expected2ndSurrogateChar
 ,E_ExpectedEndOfConditional
 ,E_ExpectedIncOrIgn
 ,E_ExpectedINCLUDEBracket
 ,E_ExpectedTextDecl
 ,E_ExpectedXMLDecl
 ,E_UnexpectedEOE
 ,E_PEPropogated
 ,E_ExtraCloseSquare
 ,E_PERefInMarkupInIntSubset
 ,E_EntityPropogated
 ,E_ExpectedNumericalCharRef
 ,E_ExpectedOpenSquareBracket
 ,E_BadSequenceInCharData
 ,E_IllegalSequenceInComment
 ,E_UnterminatedCDATASection
 ,E_ExpectedNDATA
 ,E_NDATANotValidForPE
 ,E_HexRadixMustBeLowerCase
 ,E_DeclStringRep
 ,E_DeclStringsInWrongOrder
 ,E_NoExtRefsInAttValue
 ,E_XMLDeclMustBeLowerCase
 ,E_ExpectedEntityValue
 ,E_BadDigitForRadix
 ,E_EndedWithTagsOnStack
 ,E_AmbiguousContentModel
 ,E_NestedCDATA
 ,E_UnknownPrefix
 ,E_PartialTagMarkupError
 ,E_EmptyMainEntity
 ,E_CDATAOutsideOfContent
 ,E_OnlyCharRefsAllowedHere
 ,E_Unexpected2ndSurrogateChar
 ,E_NoPIStartsWithXML
 ,E_XMLDeclMustBeFirst
 ,E_XMLVersionRequired
 ,E_StandaloneNotLegal
 ,E_TooManyColonsInName
 ,E_InvalidColonPos
 ,E_ColonNotLegalWithNS
 ,E_SysException
 ,E_XMLException
 ,E_UnexpectedEOF
};

char Invalid[][8] = {
 V_ElementNotDefined,
 V_AttNotDefined,
 V_NotationNotDeclared,
 V_RootElemNotLikeDocType,
 V_RequiredAttrNotProvided,
 V_ElementNotValidForContent,
 V_BadIDAttrDefType,
 V_InvalidEmptyAttValue,
 V_ElementAlreadyExists,
 V_MultipleIdAttrs,
 V_ReusedIDValue,
 V_IDNotDeclared,
 V_UnknownNotRefAttr,
 V_UndeclaredElemInDocType,
 V_EmptyNotValidForContent,
 V_AttNotDefinedForElement,
 V_BadEntityRefAttr,
 V_UnknownEntityRefAttr,
 V_NotEnoughElemsForCM
 ,V_NoCharDataInCM
 ,V_DoesNotMatchEnumList
 ,V_AttrValNotName
 ,V_NoMultipleValues
 ,V_NotSameAsFixedValue
 ,V_RepElemInMixed
};						


char Warnings[][8] = {
// W_NoPIStartsWithXML,
 W_NotationAlreadyExists,
 W_AttListAlreadyExists,
 W_ContradictoryEncoding,
 W_UndeclaredElemInCM,
 W_UndeclaredElemInAttList
};

char Exceptions[][8]= {
 Array_BadIndex,
 Array_BadNewSize,
 AttrList_BadIndex,
 AttDef_BadAttType,
 AttDef_BadDefAttType,
 Bitset_BadIndex,
 BufMgr_NoMoreBuffers,
 BufMgr_BufferNotInPool,
 CPtr_PointerIsZero,
 CM_BinOpHadUnaryType,
 CM_MustBeMixedOrChildren,
 CM_NoPCDATAHere,
 CM_NotValidForSpecType,
 CM_UnaryOpHadBinType,
 CM_UnknownCMType,
 CM_UnknownCMSpecType,
 ElemStack_EmptyStack,
 ElemStack_BadIndex,
 ElemStack_StackUnderflow,
 ElemStack_NoParentPushed,
 Enum_NoMoreElements,
 File_CouldNotOpenFile,
 File_CouldNotGetCurPos,
 File_CouldNotCloseFile	,
 File_CouldNotSeekToEnd	,
 File_CouldNotSeekToPos	,
 File_CouldNotDupHandle	,
 File_CouldNotReadFromFile,
 File_CouldNotResetFile	,
 File_CouldNotGetSize	,
 File_CouldNotGetBasePathName,
 Gen_ParseInProgress,
 Gen_NoDTDValidator,
 Gen_CouldNotOpenDTD,
 Gen_CouldNotOpenExtEntity,
 Gen_UnexpectedEOF,
 HshTbl_ZeroModulus,
 HshTbl_BadHashFromKey,
 HshTbl_NoSuchKeyExists,
 Mutex_CouldNotCreate,
 Mutex_CouldNotClose,
 Mutex_CouldNotLock,
 Mutex_CouldNotUnlock,
 Mutex_CouldNotDestroy,
 NetAcc_InternalError,
 Pool_ElemAlreadyExists,
 Pool_BadHashFromKey,
 Pool_InvalidId,
 Pool_ZeroModulus,
 RdrMgr_ReaderIdNotFound,
 Reader_BadAutoEncoding,
 Reader_CantCreateCvtrFor,
 Reader_CouldNotDecodeFirstLine,
 Reader_EOIInMultiSeq,
 Reader_BadUTF8Seq,
 Reader_BadSurrogateInUTF8,
 Reader_SrcOfsNotSupported,
 Scan_CouldNotOpenSource,
 Scan_UnbalancedStartEnd,
 Scan_BadPScanToken,
 Stack_BadIndex,
 Stack_EmptyStack,
 Str_ZeroSizedTargetBuf,
 Str_UnknownRadix,
 Str_TargetBufTooSmall,
 Str_StartIndexPastEnd,
 Strm_StdErrWriteFailure,
 Strm_StdOutWriteFailure,
 Strm_ConWriteFailure,
 StrPool_IllegalId,
 Trans_CouldNotXCodeXMLData,
 Trans_CouldNotCreateDefCvtr,
 Trans_InvalidSizeReq,
 Trans_Unrepresentable,
 Trans_NotInSourceSet,
 URL_MalformedURL,
 URL_UnsupportedProto,
 URL_UnsupportedProto1,
 URL_OnlyLocalHost,
 URL_NoProtocolPresent,
 URL_ExpectingTwoSlashes,
 URL_IncorrectEscapedCharRef,
 URL_UnterminatedHostComponent,
 Vector_BadIndex,
 Val_InvalidElemId,
 Val_CantHaveIntSS,
 XMLRec_UnknownEncoding
};

XERCES_CPP_NAMESPACE_END

#endif


