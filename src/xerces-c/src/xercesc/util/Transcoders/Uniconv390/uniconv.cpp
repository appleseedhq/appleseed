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
 * $Id: uniconv.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <cunhc.h>           
#include "ccsid.h"
#include "uniconv.h"

XERCES_CPP_NAMESPACE_BEGIN

#define WORK_BUFFER_SIZE 16*1024
#define DDA_NEEDED CUNBCPRM_DDA_REQ
#define RETRY_THRESHOLD 10000

// This is utility routine which strips '-', '_' and spaces from the name and
// also upper cases the name. It also returns the length of the string.
static int stripNameCopy(const char *s,char *d,int max)
{
   int si=0;
   int di=0;

   while ( (s[si] != '\0') && (di < max) ) {
      if ( (s[si] == ' ') || (s[si] == '_') || (s[si] == '-') )
         si++;
      else {
         d[di] = toupper(s[si]);
         si++;di++;
      }
   }
   d[di] = 0;
   if (s[si] != '\0')
      return -1;
   return si;
}

// This takes a name and does a lookup into the ccsid table (from ccsid.h)
// to find the corresponding ccsid. It also checks if the string ends in s390
// and returns that information to the caller.
// The lookup into the table is done via a binary search since we know that the
// table was nicely sorted for us.
static int getccsid(const char *s,int * is390)
{
   char tmpstr[_AE_MAX_CODESET_NAME_LENGTH];
   int start;
   int limit;
   int index;
   int result;
   int thelen;

   // Clean up the name....
   if (s == NULL)
      return -1;
   if ((thelen = stripNameCopy(s,tmpstr,_AE_MAX_CODESET_NAME_LENGTH-1)) == -1)
      return -1;

   // Check for the S390 string in the name
   *is390 = 0;
   if ( (strstr((char *)tmpstr, "S390")) != NULL )
      *is390 = 1;

   // Now lookup the name via a binary search
   start = 0;
   limit = _AE_NUM_OF_CODESETS;
   index = limit/2;
   while ( ((result=strcoll(tmpstr, CCSID_MAPPING[index].NAME)) != 0) &&
            (start < limit-1) ) {
      if (result < 0)
         limit = index;
      else
          start = index;
      index = (start+limit)/2;
   }

   if (result != 0 && start >= limit-1)
      return -1;

   return CCSID_MAPPING[index].CCSID;
}
// **********************************************************************
// These are the character conversion services
// **********************************************************************

// "Open" the conversion. Allocate memory to hold the handle which
// unicode services requires. Call unicode services with a 0 length
// so that it can initialize it's handle.
// Note that unicode services must always be called in a loop since
// it could be busy reloading its tables.
uniconv_t uniconv_open(const char *destenc, const char *srcenc) {
   CUNBCPRM  defparms = {CUNBCPRM_DEFAULT};
   CUNBCPRM * tmpp;
   void * handle_area;
   char *cptr;
   int srcis390;
   int destis390;

   errno = 0;
   handle_area = malloc (sizeof(CUNBCPRM)+DDA_NEEDED+WORK_BUFFER_SIZE+8);
   tmpp = (CUNBCPRM *) handle_area;
   if (tmpp==NULL)
      return (uniconv_t)-1;

   // initialize the parm area with defaults, then start filling it
   // in with our values.
   memcpy(tmpp,&defparms,sizeof(defparms));
   tmpp->Src_Buf_Len= 0;
   // get the ccsids.
   if ( ((tmpp->Src_CCSID=getccsid(srcenc,&srcis390)) == -1) ||
        ((tmpp->Targ_CCSID=getccsid(destenc,&destis390)) == -1) ) {
      errno=ENOENT;
      free(handle_area);
      return (uniconv_t)-1;
   }
   tmpp->Wrk_Buf_Ptr=(void*) (((unsigned int) handle_area) + sizeof(CUNBCPRM)+DDA_NEEDED +8);
   tmpp->Wrk_Buf_Len=WORK_BUFFER_SIZE;
   // Doubleword align the DDA area
   tmpp->DDA_Buf_Ptr=(void*) ((unsigned int) handle_area + sizeof(CUNBCPRM) +7);
   tmpp->DDA_Buf_Ptr = (void*) ((unsigned int) tmpp->DDA_Buf_Ptr & ~7);
   tmpp->DDA_Buf_Len=DDA_NEEDED;
   // This flag tells the services to automatically refresh the handle if it
   // becomes invalid.

// Use next two lines of code on old z/OS levels where Flag1 is is char field
// tmpp->Flag1|=CUNBCPRM_REFRESH_AT_INV_HANDLE_START;
// tmpp->Flag1|=CUNBCPRM_SUB_ACTION_SUBSTITUTE;

// Use next two lines of code on later z/OS levels where Flag1 is bit field
   tmpp->Flag1.Inv_Handle = 1;
   tmpp->Flag1.Sub_Action = 1;

   /* Determine which technique to use */
   if ( (srcis390) || (destis390) )
      // This technique causes it to swap LF and NL.
      memcpy(tmpp->Technique,"L       ",8);
   else
      memcpy(tmpp->Technique,"        ",8);

   // Retry if the services are busy reloading their tables.
   int retry_count = 0;
   while (retry_count < RETRY_THRESHOLD) {
      CUNLCNV(tmpp);
      if (tmpp->Return_Code == CUN_RC_OK)
         break;
      else if ( (tmpp->Return_Code == CUN_RC_WARN) &&
                ( (tmpp->Reason_Code == CUN_RS_NO_HANDLE) ||
                  (tmpp->Reason_Code == CUN_RS_INV_HANDLE_NOSET) ||
                  (tmpp->Reason_Code == CUN_RS_INV_HANDLE_SET) ) )
         // Let it loop around again
         retry_count++;
      else
         break;
   }

   if (tmpp->Return_Code != CUN_RC_OK) {
      free(handle_area);
      errno=EINVAL;
      handle_area = (uniconv_t)-1;
   }

   return handle_area;
}

// All that is required for close is to free the handle buffer.
int uniconv_close(uniconv_t handle_area) {
   errno = 0;
   if (((int)handle_area) <= 0) {
      errno=EBADF;
      return -1;
   }
   free(handle_area);
   return 0;
}

// This does the real conversion.
// Note that unicode services must always be called in a loop since
// it could be busy reloading its tables.
int uniconv(uniconv_t cd, char **inbuf,  size_t *inbytesleft,
                          char **outbuf, size_t *outbytesleft) {
   CUNBCPRM * tmpp;
   size_t startinlen = *inbytesleft;
   size_t startoutlen = *outbytesleft;
   errno = 0;

   if (((int)cd) <= 0) {
      errno=EBADF;
      return -1;
   }

   // Fill in the parameter area with current values
   tmpp = (CUNBCPRM *) cd;
   tmpp->Src_Buf_Ptr = *inbuf;
   tmpp->Src_Buf_Len = *inbytesleft;
   tmpp->Targ_Buf_Ptr = *outbuf;
   tmpp->Targ_Buf_Len = *outbytesleft;

   // Retry if the services are busy reloading their tables.
   int retry_count = 0;
   while (retry_count < RETRY_THRESHOLD) {
      CUNLCNV(tmpp);
      if (tmpp->Return_Code == CUN_RC_OK)
         break;
      else if ( (tmpp->Return_Code == CUN_RC_WARN) &&
                ( (tmpp->Reason_Code == CUN_RS_NO_HANDLE) ||
                  (tmpp->Reason_Code == CUN_RS_INV_HANDLE_NOSET) ||
                  (tmpp->Reason_Code == CUN_RS_INV_HANDLE_SET) ) )
         // Let it loop around again
         retry_count++;
      else
         break;
   }
   *inbuf        = (char *)tmpp->Src_Buf_Ptr;
   *inbytesleft  = tmpp->Src_Buf_Len;
   *outbuf       = (char *)tmpp->Targ_Buf_Ptr;
   *outbytesleft = tmpp->Targ_Buf_Len;

   if (tmpp->Return_Code != CUN_RC_OK) {
      if (tmpp->Reason_Code == CUN_RS_TRG_EXH)
         errno=E2BIG;
      else if (tmpp->Reason_Code == CUN_RS_MBC_INCOMPLETE)
         errno=EINVAL;
      else {
         errno=EBADF;
         return -1;
      }
   }
   return (startinlen-*inbytesleft);
}

// **********************************************************************
// These are the case conversion services.
// **********************************************************************

// This "opens" the case conversion. It allocates the parameter area
// then does a dummy call to unicode services so that it can set up
// the handle.
// Note that unicode services must always be called in a loop since
// it could be busy reloading its tables.
static inline uniconv_t uniconv_case_open(unsigned char direction) {
CUNBAPRM  defparms = {CUNBAPRM_DEFAULT};
CUNBAPRM * tmpp;
void * handle_area;

   errno = 0;
   handle_area = malloc (sizeof(CUNBAPRM)+CUNBAPRM_DDA_REQ);
   tmpp = (CUNBAPRM *) handle_area;
   if (tmpp==NULL)
      return (uniconv_t)-1;
   // initialize the parm area with defaults, then start filling it
   // in with our values.
   memcpy(tmpp,&defparms,sizeof(defparms));
   tmpp->DDA_Buf_Ptr=(void*) ((unsigned int) handle_area + sizeof(CUNBAPRM));
   tmpp->DDA_Buf_Len=CUNBAPRM_DDA_REQ;
   // This flag tells the services to automatically refresh the handle if it
   // becomes invalid.

// Use next line of code on old z/OS levels where Flag1 is char field
// tmpp->Flag1|=CUNBAPRM_REFRESH_AT_INV_HANDLE_START;

// Use next line of code on later z/OS levels where Flag1 is bit field
   tmpp->Flag1.Inv_Handle = 1;

   unichar_t inchar = 0x61;
   unichar_t outchar;
   tmpp->Src_Buf_Ptr=&inchar;
   tmpp->Targ_Buf_Ptr=&outchar;
   tmpp->Targ_Buf_Len=sizeof(unichar_t);
   tmpp->Src_Buf_Len=sizeof(unichar_t);
   tmpp->Conv_Type=direction;

   // Retry if the services are busy reloading their tables.
   int retry_count = 0;
   while (true) {
      CUNLASE ( tmpp );

      if (tmpp->Return_Code == CUN_RC_OK) {
         break;
      } else if ( (tmpp->Return_Code == CUN_RC_WARN) &&
                  ( (tmpp->Reason_Code == CUN_RS_NO_HANDLE) ||
                    (tmpp->Reason_Code == CUN_RS_INV_HANDLE_NOSET) ||
                    (tmpp->Reason_Code == CUN_RS_INV_HANDLE_SET) ) ) {
         // Let it loop around again
         retry_count++;
         if (retry_count > RETRY_THRESHOLD) {
            errno = ENOSYS;
            break;
         }
      } else {
         errno = ENOSYS;
         break;
      }
   }
   if (tmpp->Return_Code != CUN_RC_OK) {
      free(handle_area);
      errno=EINVAL;
      handle_area = (uniconv_t)-1;
   }

   return handle_area;
}

// These are the actual external interfaces for the open function
uniconv_t uniconv_toupper_open() {
   return uniconv_case_open(CUNBAPRM_TO_UPPER);
}
uniconv_t uniconv_tolower_open() {
   return uniconv_case_open(CUNBAPRM_TO_LOWER);
}
// This closes the case conversion. All it does is free the handle buffer.
int _uniconv_case_close(uniconv_t handle_area) {
   errno = 0;
   if (((int)handle_area) <= 0) {
      errno=EBADF;
      return -1;
   }
   free(handle_area);
   return 0;
}

// This does the actual case conversion. The direction is already
// stored in the handle buffer.
// Note that unicode services must always be called in a loop since
// it could be busy reloading its tables.
unichar_t uniconv_caseit (uniconv_t cd,unichar_t inchar) {
   unichar_t outchar;
   CUNBAPRM * tmpp;

   errno = 0;
   if (((int)cd) <= 0) {
      errno=EBADF;
      return -1;
   }
   tmpp = (CUNBAPRM *) cd;
   tmpp->Src_Buf_Ptr=&inchar;
   tmpp->Targ_Buf_Ptr=&outchar;
   tmpp->Targ_Buf_Len=sizeof(unichar_t);
   tmpp->Src_Buf_Len=sizeof(unichar_t);

   // Retry if the services are busy reloading their tables.
   int retry_count = 0;
   while (true) {
      CUNLASE ( tmpp );

      if (tmpp->Return_Code == CUN_RC_OK) {
         break;
      }
      else if ( (tmpp->Return_Code == CUN_RC_WARN) &&
                ( (tmpp->Reason_Code == CUN_RS_NO_HANDLE) ||
                  (tmpp->Reason_Code == CUN_RS_INV_HANDLE_NOSET) ||
                  (tmpp->Reason_Code == CUN_RS_INV_HANDLE_SET) ) ) {
         // Let it loop around again
         retry_count++;
         if (retry_count > RETRY_THRESHOLD) {
            errno = ENOSYS;
            break;
         }
      } else {
         errno = ENOSYS;
         break;
      }
   }
   return outchar;
}

XERCES_CPP_NAMESPACE_END
