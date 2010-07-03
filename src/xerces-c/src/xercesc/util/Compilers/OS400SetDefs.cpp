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
 * $Id: OS400SetDefs.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------

#include <sys/types.h>
#include <ctype.h>
 int
strcasecmp (const char *string1,const char * string2)
 {
  char *s1, *s2;
  int result;

  s1 = (char *)string1;
  s2 = (char *)string2;

  while ((result = tolower (*s1) - tolower (*s2)) == 0)
    {
      if (*s1++ == '\0')
        return 0;
      s2++;
    }
  return (result);
}
int
strncasecmp (const char *string1,const char *string2,size_t count)
{
  register char *s1, *s2;
  register int r;
  register unsigned int rcount;
  rcount = (unsigned int) count; 
  if (rcount > 0)
    {
      s1 = (char *)string1;
      s2 = (char *)string2;
      do
	{
	  if ((r = tolower (*s1) - tolower (*s2)) != 0)
	    return r;
	  if (*s1++ == '\0')
	    break;
	  s2++;
	}
      while (--rcount != 0);
    }
  return (0);
}
/* des not appear as though the following is needed */
#ifndef __OS400__
int stricmp(const char* str1, const char*  str2)
{
 return strcasecmp(str1, str2);
}

int strnicmp(const char* str1, const char* str2, const unsigned int count)
{
	if (count == 0)
		return 0;

	return strncasecmp( str1, str2, (size_t)count);
}
#endif
