#if (__OS400_TGTVRM__>=510)                               /* @01a */
    #pragma datamodel(P128)                               /* @01a */
#endif                                                    /* @01a */

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
 * $Id: iconv_cnv.hpp 568078 2007-08-21 11:43:25Z amassari $
 */

#ifndef UCNV_H
#define UCNV_H

#include "utypes.h"

XERCES_CPP_NAMESPACE_BEGIN

/**
 * Creates a UConverter object with the names specified as a C string.
 * The actual name will be resolved with the alias file.
 * if <TT>NULL</TT> is passed for the converter name, it will create one with the
 * getDefaultName return value.
 * @param converterName : name of the uconv table
 * @param err outgoing error status <TT>MEMORY_ALLOCATION_ERROR, TABLE_NOT_FOUND</TT>
 * @return the created Unicode converter object, or <TT>NULL</TT> if an error occured
 * @see ucnv_openU
 * @see ucnv_openCCSID
 * @see ucnv_close
 */

U_CAPI
UConverter* U_EXPORT2 ucnv_open   (const char *converterName,
				   UErrorCode * err);


/**
 * Creates a Unicode converter with the names specified as unicode string. The name should be limited to
 * the ASCII-7 alphanumerics range. The actual name will be resolved with the alias file.
 * if <TT>NULL</TT> is passed for the converter name, it will create one with the
 * getDefaultName return value.
 * @param converterName : name of the uconv table in a zero terminated Unicode string
 * @param err outgoing error status <TT>MEMORY_ALLOCATION_ERROR, TABLE_NOT_FOUND</TT>
 * @return the created Unicode converter object, or <TT>NULL</TT> if an error occured
 * @see ucnv_open
 * @see ucnv_openCCSID
 * @see ucnv_close
 */
U_CAPI UConverter* U_EXPORT2 ucnv_openU (const UChar * name,
				       UErrorCode * err);



/**
 * Creates a UConverter object using a CCSID number.
 * @param codepage : codepage # of the uconv table
 * @param platform : codepage's platform (now only <TT>IBM</TT> supported)
 * @param err error status <TT>MEMORY_ALLOCATION_ERROR, TABLE_NOT_FOUND</TT>
 * @return the created Unicode converter object, or <TT>NULL</TT> if and error occured
 * @see ucnv_open
 * @see ucnv_openU
 * @see ucnv_close
 */


U_CAPI void  U_EXPORT2 ucnv_close (UConverter * converter);




/**
 * Returns the maximum length of bytes used by a character. This varies between 1 and 4
 * @param converter the Unicode converter
 * @return the maximum number of bytes allowed by this particular converter
 * @see ucnv_getMinCharSize
 */
U_CAPI int8_t U_EXPORT2
    ucnv_getMaxCharSize (const UConverter * converter);


/**
 * Returns the minimum byte length for characters in this codepage. This is either
 * 1 or 2 for all supported codepages.
 * @param converter the Unicode converter
 * @return the minimum number of bytes allowed by this particular converter
 * @see ucnv_getMaxCharSize
 */
U_CAPI int8_t U_EXPORT2
    ucnv_getMinCharSize (const UConverter * converter);


/**
 * Transcodes an array of unicode characters to an array of codepage characters.
 * The source pointer is an I/O parameter, it starts out pointing where the function is
 * to begin transcoding, and ends up pointing after the first sequence of the bytes
 * that it encounters that are semantically invalid.
 * if ucnv_setToUCallBack is called with an action other than <TT>STOP</TT>
 * before a call is made to this API, <TT>consumed</TT> and <TT>source</TT> should point to the same place
 * (unless <TT>target</TT> ends with an imcomplete sequence of bytes and <TT>flush</TT> is <TT>FALSE</TT>).
 * the <TT>target</TT> buffer buffer needs to be a least the size of the maximum # of bytes per characters
 * allowed by the target codepage.
 * @param converter the Unicode converter
 * @param converter the Unicode converter
 * @param target : I/O parameter. Input : Points to the beginning of the buffer to copy
 *  codepage characters to. Output : points to after the last codepage character copied
 *  to <TT>target</TT>.
 * @param targetLimit the pointer to the end of the <TT>target</TT> array
 * @param source the source Unicode character array
 * @param sourceLimit the pointer to the end of the source array
 * @param offsets if NULL is passed, nothing will happen to it, otherwise it needs to have the same number
 * of allocated cells as <TT>target</TT>. Will fill in offsets from target to source pointer
 * e.g: <TT>offsets[3]</TT> is equal to 6, it means that the <TT>target[3]</TT> was a result of transcoding <TT>source[6]</TT>
 * For output data carried across calls -1 will be placed for offsets.
 * @param flush <TT>TRUE</TT> if the buffer is the last buffer of the conversion interation
 * and the conversion will finish with this call, FALSE otherwise.
 * @param err the error status.  <TT>ILLEGAL_ARGUMENT_ERROR</TT> will be returned if the
 * converter is <TT>NULL</TT>.
 * @see ucnv_fromUChars
 * @see ucnv_convert
 * @see ucnv_getMinCharSize
 * @see ucnv_setToUCallBack
 */

U_CAPI
  void U_EXPORT2 ucnv_fromUnicode (UConverter * converter,
			 char **target,
			 const char *targetLimit,
			 const UChar ** source,
			 const UChar * sourceLimit,
			 int32_t* offsets,
			 int flush,
			 UErrorCode * err);


/**
 * Converts an array of codepage characters into an array of unicode characters.
 * The source pointer is an I/O parameter, it starts out pointing at the place
 * to begin translating, and ends up pointing after the first sequence of the bytes
 * that it encounters that are semantically invalid.
 * if ucnv_setFromUCallBack is called with an action other than STOP
 * before a call is made to this API, consumed and source should point to the same place
 * (unless target ends with an imcomplete sequence of bytes and flush is FALSE).
 * @param converter the Unicode converter
 * @param target : I/O parameter. Input : Points to the beginning of the buffer to copy
 *  Unicode characters to. Output : points to after the last UChar copied to target.
 * @param targetLimit the pointer to the end of the target array
 * @param source the source codepage character array
 * @param sourceLimit the pointer to the end of the source array
 * @param offsets if NULL is passed, nothing will happen to it, otherwise it needs to have the same number
 * of allocated cells as <TT>target</TT>. Will fill in offsets from target to source pointer
 * e.g: <TT>offsets[3]</TT> is equal to 6, it means that the <TT>target[3]</TT> was a result of transcoding <TT>source[6]</TT>
 * For output data carried across calls -1 will be placed for offsets.
 * @param flush TRUE if the buffer is the last buffer and the conversion will finish
 * in this call, FALSE otherwise.
 * @param err the error code status  <TT>ILLEGAL_ARGUMENT_ERROR</TT> will be returned if the
 * converter is <TT>NULL</TT>, or if <TT>targetLimit</TT> and <TT>sourceLimit</TT> are misaligned.
 * @see ucnv_toUChars
 * @see ucnv_getNextUChar
 * @see ucnv_convert
 * @see ucnv_setFromUCallBack
 */

U_CAPI
  void U_EXPORT2 ucnv_toUnicode (UConverter * converter,
		       UChar ** target,
		       const UChar * targetLimit,
		       const char **source,
		       const char *sourceLimit,
		       int32_t* offsets,
		       int flush,
		       UErrorCode * err);


/**
 * Transcodes the source Unicode string to the target string in a codepage encoding
 * with the specified Unicode converter.  For example, if a Unicode to/from JIS
 * converter is specified, the source string in Unicode will be transcoded to JIS
 * encoding.  The result will be stored in JIS encoding.
 * if any problems during conversion are encountered it will SUBSTITUTE with the default (initial)
 * substitute characters.
 * This function is a more convenient but less efficient version of \Ref{ucnv_fromUnicode}.
 * @param converter the Unicode converter
 * @param source the <TT>source</TT> Unicode string (zero Terminated)
 * @param target the <TT>target</TT> string in codepage encoding (<STRONG>not zero-terminated</STRONG> because some
 * codepage do not use '\0' as a string terminator
 * @param targetCapacity Input the number of bytes available in the <TT>target</TT> buffer
 * @param err the error status code.
 * <TT>INDEX_OUTOFBOUNDS_ERROR</TT> will be returned if the
 * the # of bytes provided are not enough for transcoding.
 * <TT>ILLEGAL_ARGUMENT_ERROR</TT> is returned if the converter is <TT>NULL</TT> or the source or target string is empty.
 * <TT>BUFFER_OVERFLOW_ERROR</TT> when <TT>targetSize</TT> turns out to be bigger than <TT>targetCapacity</TT>
 * @return number of bytes needed in target, regardless of <TT>targetCapacity</TT>
 * @see ucnv_fromUnicode
 * @see ucnv_convert
 */
U_CAPI
  int32_t U_EXPORT2 ucnv_fromUChars (const UConverter * converter,
			   char *target,
			   int32_t targetCapacity,
			   const UChar * source,
			   UErrorCode * err);





/**
 * Transcode the source string in codepage encoding to the target string in
 * Unicode encoding.  For example, if a Unicode to/from JIS
 * converter is specified, the source string in JIS encoding will be transcoded
 * to Unicode and placed into a provided target buffer.
 * if any problems during conversion are encountered it will SUBSTITUTE with the Unicode REPLACEMENT char
 * We recomment, the size of the target buffer needs to be at least as long as the maximum # of bytes per char
 * in this character set.
 * A zero-terminator will be placed at the end of the target buffer
 * This function is a more convenient but less efficient version of \Ref{ucnv_toUnicode}.
 * @param converter the Unicode converter
 * @param source the source string in codepage encoding
 * @param target the target string in Unicode encoding
 * @param targetCapacity capacity of the target buffer
 * @param sourceSize : Number of bytes in <TT>source</TT> to be transcoded
 * @param err the error status code
 * <TT>MEMORY_ALLOCATION_ERROR</TT> will be returned if the
 * the internal process buffer cannot be allocated for transcoding.
 * <TT>ILLEGAL_ARGUMENT_ERROR</TT> is returned if the converter is <TT>NULL</TT> or
 * if the source or target string is empty.
 * <TT>BUFFER_OVERFLOW_ERROR</TT> when the input buffer is prematurely exhausted and targetSize non-<TT>NULL</TT>.
 * @return the number of UChar needed in target (including the zero terminator)
 * @see ucnv_getNextUChar
 * @see ucnv_toUnicode
 * @see ucnv_convert
 */
U_CAPI
  int32_t U_EXPORT2 ucnv_toUChars (const UConverter * converter,
			 UChar * target,
			 int32_t targetCapacity,
			 const char *source,
			 int32_t sourceSize,
			 UErrorCode * err);

/********************************
 * Will convert a codepage buffer one character at a time.
 * This function was written to be efficient when transcoding small amounts of data at a time.
 * In that case it will be more efficient than \Ref{ucnv_toUnicode}.
 * When converting large buffers use \Ref{ucnv_toUnicode}.
 *@param converter an open UConverter
 *@param source the address of a pointer to the codepage buffer, will be updated to point after
 *the bytes consumed in the conversion call.
 *@param points to the end of the input buffer
 *@param err fills in error status (see ucnv_toUnicode)
 *@return a UChar resulting from the partial conversion of source
 *@see ucnv_toUnicode
 *@see ucnv_toUChars
 *@see ucnv_convert
 */
U_CAPI
  UChar U_EXPORT2 ucnv_getNextUChar (UConverter * converter,
			   const char **source,
			   const char *sourceLimit,
			   UErrorCode * err);


/**************************
* Will convert a sequence of bytes from one codepage to another.
* This is <STRONG>NOT AN EFFICIENT</STRONG> way to transcode.
* use \Ref{ucnv_toUnicode} and \Ref{ucnv_fromUnicode} for efficiency
* @param toConverterName: The name of the converter that will be used to encode the output buffer
* @param fromConverterName: The name of the converter that will be used to decode the input buffer
* @param target: Pointer to the output buffer to write to
* @param targetCapacity: on input contains the capacity of target
* @param source: Pointer to the input buffer
* @param sourceLength: on input contains the capacity of source
* @param err: fills in an error status
* @return  will be filled in with the number of bytes needed in target
* @see ucnv_fromUnicode
* @see ucnv_toUnicode
* @see ucnv_fromUChars
* @see ucnv_toUChars
* @see ucnv_getNextUChar
*/
U_CAPI
  int32_t U_EXPORT2 ucnv_convert (const char *toConverterName,
			const char *fromConverterName,
			char *target,
			int32_t targetCapacity,
			const char *source,
			int32_t sourceLength,
			UErrorCode * err);

XERCES_CPP_NAMESPACE_END

#endif

#if (__OS400_TGTVRM__>=510)                                /* @01a */  
     #pragma datamodel(pop)                                /* @01a */ 
#endif                                                     /* @01a */

