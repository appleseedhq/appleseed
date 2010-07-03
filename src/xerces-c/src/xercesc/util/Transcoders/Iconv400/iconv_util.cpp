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
 * $Id: iconv_util.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/util/XercesDefs.hpp>
#include <iconv.h>
#include <xercesc/util/Platforms/OS400/OS400PlatformUtils.hpp>
#include <stdlib.h>
#include <unistd.h>
#include <iconv_util.hpp>
#include <iconv_cnv.hpp>
#include <ctype.h>
#include <qlgusr.h>   // @15a


XERCES_CPP_NAMESPACE_BEGIN

#define COPYRIGHT_STRING_LENGTH  200
#define MAX_STRLEN 0x00FFFFF
#define CHARCCSIDSIZE 5
static UConverter* _defaultConverter = NULL;
static UErrorCode gErr = U_ZERO_ERROR;

#include "utypes.h"

// void   shareConverterData (UConverterSharedData * data,char *InDescriptor) ;
// UConverterSharedData *getSharedConverterData(char *Descriptor);
#define defaultConverter (_defaultConverter==NULL)?_defaultConverter=ucnv_open(NULL, &gErr):_defaultConverter


/* Cleans up the default converter if it has been allocated.
   This is a little messy, but I know the code well enough to
   do anything neater.
 */
void cleanupDefaultConverter()
{
	if (_defaultConverter != NULL) {
		ucnv_close(_defaultConverter);
		_defaultConverter = NULL;
	}
}

static char DEFAULT_CONVERTER_NAME[60] = "";
const char* iconv_getDefaultCodepage()
{
  strcpy(DEFAULT_CONVERTER_NAME, "ibm037");
  return DEFAULT_CONVERTER_NAME;
}
/* placeholder function to be deleted in 3_0 */


/* used by String.c */




int32_t
u_strlen(const UChar *s)
{
  int32_t  i = 0;

  while(*s++)
    i++;
  return  i;
}

/* note sure if needed -  think that this is needed in cnv.c */
char* u_austrcpy(char *s1,
         const UChar *ucs2 )
{
  char * anchor = s1;     /* save the start of result string */
  UErrorCode err = U_ZERO_ERROR;
  int32_t len;
  len = ucnv_fromUChars(defaultConverter,
                s1,
                MAX_STRLEN,
                ucs2,
                &err);

  s1[len] = '\0';
  return s1;

}

/*Logic determines if the converter is Algorithmic AND/OR cached
 *depending on that:
 * -we either go to get data from disk and cache it (Data=TRUE, Cached=False)
 * -Get it from a Hashphkramtable (Data=X, Cached=TRUE)
 * -Call dataConverter initializer (Data=TRUE, Cached=TRUE)
 * -Call AlgorithmicConverter initializer (Data=FALSE, Cached=TRUE)
 */
// int QlgCvtTextDescToDesc(int,int,char *,int,char *,int,int);  @14d



UConverter *
  createConverter (const char *converterName, UErrorCode * err)
{
  char realName[MAX_CONVERTER_NAME_LENGTH];
  char uppercased_converterName[MAX_CONVERTER_NAME_LENGTH];
  UConverter *myUConverter = NULL;
  UConverterSharedData *mySharedConverterData = NULL;

  /* following parameters are passed to the convert text decscriptor
     to descriptor          */
  int  InType;
  int  OutType=1;
  char *InDescriptor;
  int  InDescSize;
  char OutDescriptor[CHARCCSIDSIZE]; /* 5 = size of a CCSID id on AS/400*/
  int  OutDescSize;
  int  JobCCSID=0;
  char *inchar, *outchar; /* input and output conversion pointers*/
  int tempchar;
  if (U_FAILURE (*err))
    return NULL;
/******************************************************************/
/* assume name is in EBCDIC. The convetername is assumed to be
/* either upper/lower case ebcdic  and the text converter requires
/* upper case - Since we do not know we will convert to upper case
** just in case
/******************************************************************/
inchar = (char*)converterName;
outchar = &uppercased_converterName[0];
while (*inchar) /*except for null terminator uppercase inputs*/
{
   tempchar = (int)(*inchar);
   *outchar=(char)toupper(tempchar);
   inchar ++;
   outchar ++;
}
  if (outchar -&uppercased_converterName[0]<=MAX_CONVERTER_NAME_LENGTH)
    *outchar = '\0'; /* replace the terminator                 */
  if(!strcmp(uppercased_converterName, "IBM-37"))  /* We special-case this one */
  {
      memcpy(uppercased_converterName, "IBM-037", 7);
      memset(uppercased_converterName + 7, '\0',1);
  }
/******************************************************************/
/* convert name to AS/400 CCSID
** if CCSID is returned then we can complete building the converter
** otherwise we will return the same error as ICU converters
*/
/******************************************************************/
 InType = 11; /*set for IANA initially */
 if (QlgCvtTextDescToDesc(InType, OutType,(char *)uppercased_converterName,                          strlen(uppercased_converterName),&OutDescriptor[0],sizeof(OutDescriptor),JobCCSID)<0)
    {
     InType = 3; /* change to AIX 4.1 if we fail above */
     if (QlgCvtTextDescToDesc(InType, OutType,(char *)uppercased_converterName,                          strlen(uppercased_converterName),&OutDescriptor[0],sizeof(OutDescriptor),JobCCSID)<0)
     {
      *err = U_INVALID_TABLE_FILE;
      send_message((char *)converterName,ICONV_CCSID_PROBLEM,'d');
      return NULL;
     }
    }
/*****************************************************************/
/* create a new converter                                        */
/*****************************************************************/
 myUConverter = createNewConverter(&OutDescriptor[0], err);

 if (U_FAILURE (*err) || (myUConverter == NULL))
   {
     return NULL;
   }
 return myUConverter;

}


UConverter* createNewConverter(const char *name, UErrorCode *err)
{
  char temp[33];
  int32_t i = 0;
  const int8_t *myByteArray = NULL;
  const uint16_t *myIndexArray = NULL;
  int32_t myValuesCount = 0;
  int32_t myIndexCount = 0;
  UConverter *myConverter = NULL;
  int32_t myCheck;
  int8_t errorLevel = 0;
  char throwAway[COPYRIGHT_STRING_LENGTH];
  char UNICODE_CCSID_ID_O[33]="IBMCCSID134880000000";
  char UNICODE_CCSID_ID_I[33]="IBMCCSID13488";
  char my_CCSID_ID[33]="\0";
  char CONVERT_ID_START[9] ="IBMCCSID"; /* include null terminator for strcat */
  char DEFAULTS[9] = "00000000";
 if (U_FAILURE (*err))
    return NULL;
/* create the base shell for the converter */
  myConverter = (UConverter *) malloc (sizeof (UConverter));
  if (myConverter == NULL)
    {
      *err = U_MEMORY_ALLOCATION_ERROR;
      return NULL;
    }
/* create the "shared area' and link into the shell */
  myConverter->sharedData =
    (UConverterSharedData *) malloc (sizeof (UConverterSharedData));
  if (myConverter->sharedData == NULL)
    {
      free (myConverter);
      *err = U_MEMORY_ALLOCATION_ERROR;
      return NULL;
    }

  memset(my_CCSID_ID, '\0', 33);  /* sizeof(my_CCSID_ID)); */
  memcpy(my_CCSID_ID,&CONVERT_ID_START[0],strlen(CONVERT_ID_START)+1);
  memcpy(&my_CCSID_ID[8],&name[0],5);
  memcpy(&my_CCSID_ID[13],&DEFAULTS[0],7);
  memset(temp, '\0', 33);
  memcpy(temp, my_CCSID_ID, 13);

  myConverter->sharedData->toiconv_handle = iconv_open(UNICODE_CCSID_ID_I,my_CCSID_ID);
/*  memset(&my_CCSID_ID[13],'0',19);  clear out the non necessary data to reverse */
  my_CCSID_ID[13] = '\0';
  myConverter->sharedData->fromiconv_handle = iconv_open(temp,UNICODE_CCSID_ID_O);
  if ((myConverter->sharedData->toiconv_handle.return_value ==-1) ||(myConverter->sharedData->fromiconv_handle.return_value==-1))

      {
	/*If it isn't any of the above, the file is invalid */
	*err = U_INVALID_TABLE_FILE;
	free (myConverter->sharedData);
	free (myConverter);
      }

  return myConverter;


}

XERCES_CPP_NAMESPACE_END
