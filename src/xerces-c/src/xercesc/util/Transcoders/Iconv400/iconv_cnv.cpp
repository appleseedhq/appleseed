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
 * $Id: iconv_cnv.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/util/XercesDefs.hpp>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <iconv_util.hpp>
#include <iconv_cnv.hpp>
#include <qmhrtvm.h>
#include <qusec.h>
#include <xercesc/util/Platforms/OS400/OS400PlatformUtils.hpp>
#include <iconv.h>
#include <errno.h>
#define CHUNK_SIZE 5*1024

XERCES_CPP_NAMESPACE_BEGIN

void Convert_toUnicode(UConverter *,
				     UChar **,
				     const UChar *,
				     const char **,
				     const char *,
				     int32_t* offsets,
				     int,
				     UErrorCode *);

void Convert_fromUnicode(UConverter *,
				       char **,
				       const char *,
				       const UChar **,
				       const UChar *,
				       int32_t* offsets,
				       int,
				       UErrorCode *);

UChar getNextUChar(UConverter* converter,
					       const char** source,
					       const char* sourceLimit,
					       UErrorCode* err);

void T_UConverter_fromCodepageToCodepage (UConverter * outConverter,
						 UConverter * inConverter,
						 char **target,
						 const char *targetLimit,
						 const char **source,
						 const char *sourceLimit,
						 int32_t* offsets,
						 int flush,
						 UErrorCode * err);
void Converter_fromUnicode(UConverter * _this,
				 char **target,
				 const char *targetLimit,
				 const UChar ** source,
				 const UChar * sourceLimit,
				 int32_t *offsets,
				 int flush,
				 UErrorCode * err);

/*Calls through createConverter */
UConverter* ucnv_open (const char *name,
		       UErrorCode * err)
{
  if (U_FAILURE (*err))
    return NULL;

  /*In case "name" is NULL we want to open the default converter */
  if (name != NULL)
    return createConverter (name, err);
  else
    return createConverter (iconv_getDefaultCodepage(), err);
}

/*Extracts the UChar* to a char* and calls through createConverter */
UConverter*  ucnv_openU (const UChar * name,
			 UErrorCode * err)
{
  char asciiName[MAX_CONVERTER_NAME_LENGTH];

  if (U_FAILURE (*err))
    return NULL;
  if (name == NULL)
    return ucnv_open (NULL, err);
  if (u_strlen (name) > MAX_CONVERTER_NAME_LENGTH)
    {
      *err = U_ILLEGAL_ARGUMENT_ERROR;
      return NULL;
    }
  return ucnv_open (u_austrcpy (asciiName, name), err);
}

/*Decreases the reference counter in the shared immutable section of the object
 *and frees the mutable part*/

void ucnv_close (UConverter * converter)
{
/* for iconv we will close the handles and free the converter storage*/
  iconv_close(converter->sharedData->toiconv_handle);
  iconv_close(converter->sharedData->fromiconv_handle);
  if (converter == NULL)
    return;


  free (converter);

  return;
}

/* currently required for iconv suuport */
/* XMLReader calls this and uses fact that it is different than min
to go thru a calculation otherwise if max and min same then there is
a calculation speed up - we will keep the two routines but have them
return different sizes - later will ifdef XMLreader for ICONV to remove the calls*/
 int8_t  ucnv_getMaxCharSize (const UConverter * converter)
{
  return (4); /* dummy returns just need to be different in XMLParser - need something else for ICU replacement */
}
/* currently required for iconv support */
/* see note for ucnv_getMaxCharSize */
int8_t  ucnv_getMinCharSize (const UConverter * converter)
{
  return (1);
}

void   ucnv_fromUnicode (UConverter * _this,
			 char **target,
			 const char *targetLimit,
			 const UChar ** source,
			 const UChar * sourceLimit,
			 int32_t* offsets,
			 int flush,
			 UErrorCode * err)
{
  /*
   * Check parameters in for all conversions
   */
  if (U_FAILURE (*err))   return;
  if ((_this == NULL) || ((char *) targetLimit < *target) || (sourceLimit < *source))
    {
      *err = U_ILLEGAL_ARGUMENT_ERROR;
      return;
    }


  /*calls the specific conversion routines */
  Converter_fromUnicode(_this,target,targetLimit,source,sourceLimit,
					   offsets,flush,err);

  return;
}

void   ucnv_toUnicode (UConverter * _this,
		       UChar ** target,
		       const UChar * targetLimit,
		       const char **source,
		       const char *sourceLimit,
		       int32_t* offsets,
		       int flush,
		       UErrorCode * err)
{
  /*
   * Check parameters in for all conversions
   */
  if (U_FAILURE (*err))   return;
  if ((_this == NULL) || ((UChar *) targetLimit < *target) || (sourceLimit < *source))
    {
      *err = U_ILLEGAL_ARGUMENT_ERROR;
      return;
    }



  /*calls the specific conversion routines */
  Convert_toUnicode(_this,target,targetLimit,source,sourceLimit,
					  offsets,flush,err);
  return;
}

int32_t   ucnv_fromUChars (const UConverter * converter,
			   char *target,
			   int32_t targetSize,
			   const UChar * source,
			   UErrorCode * err)
{
  const UChar *mySource = source;
  const UChar *mySource_limit;
  int32_t mySourceLength = 0;
  UConverter myConverter;
  char *myTarget = target;
  int32_t targetCapacity = 0;

  if (U_FAILURE (*err))
    return 0;

  if ((converter == NULL) || (targetSize < 0))
    {
      *err = U_ILLEGAL_ARGUMENT_ERROR;
      return 0;
    }

  /*makes a local copy of the UConverter */
  myConverter = *converter;


  /*if the source is empty we return immediately */
  mySourceLength = u_strlen (source);
  if (mySourceLength == 0)
    {
      /*for consistency we still need to
       *store 0 in the targetCapacity
       *if the user requires it
       */
      return 0;
    }

  mySource_limit = mySource + mySourceLength;

  if (targetSize > 0)
    {
      ucnv_fromUnicode (&myConverter,
			&myTarget,
			target + targetSize,
			&mySource,
			mySource_limit,
			NULL,
			TRUE,
			err);
      targetCapacity = myTarget - target;
    }

  /*Updates targetCapacity to contain the number of bytes written to target */

  if (targetSize == 0)
    {
      *err = U_BUFFER_OVERFLOW_ERROR;
    }

  /* If the output buffer is exhausted, we need to stop writing
   * to it but continue the conversion in order to store in targetSize
   * the number of bytes that was required*/
  if (*err == U_BUFFER_OVERFLOW_ERROR)
    {
      char target2[CHUNK_SIZE];
      char *target2_alias = target2;
      const char *target2_limit = target2 + CHUNK_SIZE;

      /*We use a stack allocated buffer around which we loop
       *(in case the output is greater than CHUNK_SIZE)
       */

      while (*err == U_BUFFER_OVERFLOW_ERROR)
	{
	  *err = U_ZERO_ERROR;
	  target2_alias = target2;
	  ucnv_fromUnicode (&myConverter,
			    &target2_alias,
			    target2_limit,
			    &mySource,
			    mySource_limit,
			    NULL,
			    TRUE,
			    err);

	  /*updates the output parameter to contain the number of char required */
	  targetCapacity += (target2_alias - target2) + 1;
	}
      /*We will set the erro code to BUFFER_OVERFLOW_ERROR only if
       *nothing graver happened in the previous loop*/
      (targetCapacity)--;
      if (U_SUCCESS (*err))
	*err = U_BUFFER_OVERFLOW_ERROR;
    }

  return targetCapacity;
}

int32_t ucnv_toUChars (const UConverter * converter,
		       UChar * target,
		       int32_t targetSize,
		       const char *source,
		       int32_t sourceSize,
		       UErrorCode * err)
{
  const char *mySource = source;
  const char *mySource_limit = source + sourceSize;
  UConverter myConverter;
  UChar *myTarget = target;
  int32_t targetCapacity;

  if (U_FAILURE (*err))
    return 0;

  if ((converter == NULL) || (targetSize < 0) || (sourceSize < 0))
    {
      *err = U_ILLEGAL_ARGUMENT_ERROR;
      return 0;
    }
  /*Means there is no work to be done */
  if (sourceSize == 0)
    {
      /*for consistency we still need to
       *store 0 in the targetCapacity
       *if the user requires it
       */
      if (targetSize >= 1)
	{
	  target[0] = 0x0000;
	  return 1;
	}
      else
	return 0;
    }

  /*makes a local copy of the UConverter */
  myConverter = *converter;


  /*Not in pure pre-flight mode */
  if (targetSize > 0)
    {
     /*  Changed from (targetSize * 2) to (targetSize) */
      ucnv_toUnicode (&myConverter,
		      &myTarget,
		      target + (targetSize-1),	  /*Save a spot for the Null terminator */
		      &mySource,
		      mySource_limit,
		      NULL,
		      TRUE,
		      err);

      /*Null terminates the string */
      *(myTarget) = 0x0000;
    }


  /*Rigs targetCapacity to have at least one cell for zero termination */
  /*Updates targetCapacity to contain the number of bytes written to target */
  targetCapacity = 1;
  targetCapacity += myTarget - target;
  if (targetSize == 0)
    {
      *err = U_BUFFER_OVERFLOW_ERROR;
    }
  /* If the output buffer is exhausted, we need to stop writing
   * to it but if the input buffer is not exhausted,
   * we need to continue the conversion in order to store in targetSize
   * the number of bytes that was required
   */
  if (*err == U_BUFFER_OVERFLOW_ERROR)
    {
      UChar target2[CHUNK_SIZE];
      UChar *target2_alias = target2;
      const UChar *target2_limit = target2 + CHUNK_SIZE;

      /*We use a stack allocated buffer around which we loop
         (in case the output is greater than CHUNK_SIZE) */

      while (*err == U_BUFFER_OVERFLOW_ERROR)
	{
	  *err = U_ZERO_ERROR;
	  target2_alias = target2;
	  ucnv_toUnicode (&myConverter,
			  &target2_alias,
			  target2_limit,
			  &mySource,
			  mySource_limit,
			  NULL,
			  TRUE,
			  err);

	  /*updates the output parameter to contain the number of char required */
	  targetCapacity += target2_alias - target2 + 1;
	}
      (targetCapacity)--;	/*adjust for last one */
      if (U_SUCCESS (*err))
	*err = U_BUFFER_OVERFLOW_ERROR;
    }

  return targetCapacity;
}

UChar ucnv_getNextUChar (UConverter * converter,
			 const char **source,
			 const char *sourceLimit,
			 UErrorCode * err)
{


  /*calls the specific conversion routines */
  /*as dictated in a code review, avoids a switch statement */
  return getNextUChar(converter,source,sourceLimit,err);
}



/**************************
* Will convert a sequence of bytes from one codepage to another.
* @param toConverterName: The name of the converter that will be used to encode the output buffer
* @param fromConverterName: The name of the converter that will be used to decode the input buffer
* @param target: Pointer to the output buffer* written
* @param targetLength: on input contains the capacity of target, on output the number of bytes copied to target
* @param source: Pointer to the input buffer
* @param sourceLength: on input contains the capacity of source, on output the number of bytes processed in "source"
* @param internal: used internally to store store state data across calls
* @param err: fills in an error status
*/
void
T_UConverter_fromCodepageToCodepage (UConverter * outConverter,
				     UConverter * inConverter,
				     char **target,
				     const char *targetLimit,
				     const char **source,
				     const char *sourceLimit,
				     int32_t* offsets,
				     int flush,
				     UErrorCode * err)
{

  UChar out_chunk[CHUNK_SIZE];
  const UChar *out_chunk_limit = out_chunk + CHUNK_SIZE;
  UChar *out_chunk_alias;
  UChar const *out_chunk_alias2;


  if (U_FAILURE (*err))    return;


  /*loops until the input buffer is completely consumed
   *or if an error has be encountered
   *first we convert from inConverter codepage to Unicode
   *then from Unicode to outConverter codepage
   */
  while ((*source != sourceLimit) && U_SUCCESS (*err))
    {
      out_chunk_alias = out_chunk;
      ucnv_toUnicode (inConverter,
		      &out_chunk_alias,
		      out_chunk_limit,
		      source,
		      sourceLimit,
		      NULL,
		      flush,
		      err);

      /*BUFFER_OVERFLOW_ERROR means that the output "CHUNK" is full
       *we will require at least another loop (it's a recoverable error)
       */

      if (U_SUCCESS (*err) || (*err == U_BUFFER_OVERFLOW_ERROR))
	{
	  *err = U_ZERO_ERROR;
	  out_chunk_alias2 = out_chunk;

	  while ((out_chunk_alias2 != out_chunk_alias) && U_SUCCESS (*err))
	    {
	      ucnv_fromUnicode (outConverter,
				target,
				targetLimit,
				&out_chunk_alias2,
				out_chunk_alias,
				NULL,
				TRUE,
				err);

	    }
	}
      else
	break;
    }

  return;
}

int32_t  ucnv_convert(const char *toConverterName,
		      const char *fromConverterName,
		      char *target,
		      int32_t targetSize,
		      const char *source,
		      int32_t sourceSize,
		      UErrorCode * err)
{
  const char *mySource = source;
  const char *mySource_limit = source + sourceSize;
  int32_t mySourceLength = 0;
  UConverter *inConverter;
  UConverter *outConverter;
  char *myTarget = target;
  int32_t targetCapacity = 0;

  if (U_FAILURE (*err))
    return 0;

  if ((targetSize < 0) || (sourceSize < 0))
    {
      *err = U_ILLEGAL_ARGUMENT_ERROR;
      return 0;
    }

  /*if there is no input data, we're done */
  if (sourceSize == 0)
    {
      /*in case the caller passed an output ptr
       *we update it
       */
      return 0;
    }

  /*create the converters */
  inConverter = ucnv_open (fromConverterName, err);
  if (U_FAILURE (*err)) return 0;
  outConverter = ucnv_open (toConverterName, err);
  if (U_FAILURE (*err))
    {
      ucnv_close (inConverter);
      return 0;
    }


  if (targetSize > 0)
    {
      T_UConverter_fromCodepageToCodepage (outConverter,
					   inConverter,
					   &myTarget,
					   target + targetSize,
					   &mySource,
					   mySource_limit,
					   NULL,
					   TRUE,
					   err);
    }


  /*Updates targetCapacity to contain the number of bytes written to target */
  targetCapacity = myTarget - target;
  if (targetSize == 0)
    {
      *err = U_BUFFER_OVERFLOW_ERROR;
    }

  /* If the output buffer is exhausted, we need to stop writing
   * to it but continue the conversion in order to store in targetSize
   * the number of bytes that was required*/
  if (*err == U_BUFFER_OVERFLOW_ERROR)
    {
      char target2[CHUNK_SIZE];
      char *target2_alias = target2;
      const char *target2_limit = target2 + CHUNK_SIZE;

      /*We use a stack allocated buffer around which we loop
       *(in case the output is greater than CHUNK_SIZE)
       */

      while (*err == U_BUFFER_OVERFLOW_ERROR)
	{
	  *err = U_ZERO_ERROR;
	  target2_alias = target2;
	  T_UConverter_fromCodepageToCodepage (outConverter,
					       inConverter,
					       &target2_alias,
					       target2_limit,
					       &mySource,
					       mySource_limit,
					       NULL,
					       TRUE,
					       err);

	  /*updates the output parameter to contain the number of char required */
	  targetCapacity += (target2_alias - target2) + 1;
	}
      /*We will set the erro code to BUFFER_OVERFLOW_ERROR only if
       *nothing graver happened in the previous loop*/
      (targetCapacity)--;
      if (U_SUCCESS (*err))
	*err = U_BUFFER_OVERFLOW_ERROR;
    }

  ucnv_close (inConverter);
  ucnv_close (outConverter);

  return targetCapacity;
}

void Converter_fromUnicode(UConverter * _this,
				 char **target,
				 const char *targetLimit,
				 const UChar ** source,
				 const UChar * sourceLimit,
				 int32_t *offsets,
				 int flush,
				 UErrorCode * err)
{
  int chardone;
  const UChar *mySource = *source;
  unsigned char *myTarget = (unsigned char *) *target;

  int32_t targetLength = targetLimit - (char *) myTarget;
  int32_t sourceLength = (sourceLimit - mySource) * 2;
  unsigned char targetChar = 0x00;
  /* pick up the iconv handle and perform the conversion */
  errno = 0;
  chardone =iconv(_this->sharedData->fromiconv_handle,(char**)source,  (size_t*) &sourceLength,target,(size_t *)&targetLength);
  if (errno!=0)
    if (errno == E2BIG)
      {
	  *err = U_BUFFER_OVERFLOW_ERROR;
          return;
      }
    else

      if ((errno ==EBADDATA)|| (errno ==ECONVERT))

       {
        char errno_id[7];
        send_message(NULL,ICONV_CONVERT_PROBLEM,'d');
        convert_errno(errno_id,errno);
        send_message(NULL,errno_id,'d');
        *err = U_INVALID_CHAR_FOUND;
	return;
	}


  return;
       }

void Convert_toUnicode(UConverter * _this,
				  UChar ** target,
				  const UChar * targetLimit,
				  const char **source,
				  const char *sourceLimit,
				  int32_t *offsets,
				  int flush,
				  UErrorCode * err)
{
  char *mySource = (char *) *source;
  UChar *myTarget = *target;

  int32_t targetLength = (targetLimit - myTarget)*2;  /* multiply by 2 */
  int32_t sourceLength = (sourceLimit - (char *) mySource);
  int chardone;
  /* pick up the iconv handle */
  errno = 0;
  chardone =iconv(_this->sharedData->toiconv_handle,(char**)source,  (size_t*) &sourceLength,(char **)target,(size_t *)&targetLength);
  if (errno!=0)
  {
    if (errno == E2BIG)
      {
	  *err = U_BUFFER_OVERFLOW_ERROR;
          return;
      }
    else

      if ((errno ==EBADDATA)|| (errno ==ECONVERT))

       {
        char errno_id[7];
        send_message(NULL,ICONV_CONVERT_PROBLEM,'d');
        convert_errno(errno_id,errno);
        send_message(NULL,errno_id,'d');
        *err = U_INVALID_CHAR_FOUND;
        return;
	
	}

}


  return;
}

UChar getNextUChar(UConverter* converter,
					       const char** source,
					       const char* sourceLimit,
					       UErrorCode* err)
{
  UChar myUChar;
  UChar* myUCharptr;
  size_t numberibytes=sizeof(UChar);
  size_t numberobytes=sizeof(UChar);
  int chardone;
  if ((*source)+1 > sourceLimit)
    {
      *err = U_INDEX_OUTOFBOUNDS_ERROR;
      return 0xFFFD;
    }


  /*pick up the iconv handle */
  /* convert the requested character - need to cache characters 6 will do - XMLReader is using this function to get header to process*/
  myUCharptr  = &myUChar;
 chardone =iconv(converter->sharedData->toiconv_handle,(char**)source,  (size_t*) &numberibytes,(char **)&myUCharptr,(size_t *)&numberobytes);
  if (myUChar != 0xFFFD) return myUChar;
  else
    {
      UChar* myUCharPtr = &myUChar;
      const char* sourceFinal = *source;

      *err = U_INVALID_CHAR_FOUND;


      /*makes the internal caching transparent to the user*/
      if (*err == U_INDEX_OUTOFBOUNDS_ERROR) *err = U_ZERO_ERROR;

      return myUChar;
    }
}

XERCES_CPP_NAMESPACE_END
