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
 * $Id: CodeWarriorDefs.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

#include <xercesc/util/XercesDefs.hpp>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

// These functions are needed because MacOS doesn't define them
//	(these routines are defined in CW 8 by extras.h, but there is no MachO
//	library for extras).

#if __MACH__
// Compare lexigraphically two strings
int stricmp(const char *s1, const char *s2)
{
    char c1, c2;
    while (1)
    {
        c1 = tolower(*s1++);
        c2 = tolower(*s2++);
        if (c1 < c2) return -1;
        if (c1 > c2) return 1;
        if (c1 == 0) return 0;
    }
}

// Compare lexigraphically two strings up to a max length
int strnicmp(const char *s1, const char *s2, const unsigned int n)
{
    int i;
    char c1, c2;
    for (i=0; i<n; i++)
    {
        c1 = tolower(*s1++);
        c2 = tolower(*s2++);
        if (c1 < c2) return -1;
        if (c1 > c2) return 1;
        if (!c1) return 0;
    }
    return 0;
}
#endif


#if defined(_WIN32) || defined(WIN32)
int mbswcslen(const char * s, const unsigned int n)
{
	int     result;
	char *  source;
	int count = -1;
	size_t  source_len;
	
	source_len = strlen(s);

    source      = (char *)s;
    
    for (count = 0; count < n; count++)
    {
    	if (*source)
    	{
        	result = mbtowc(0, source, source_len);
        	if (result > 0)
        	{
        		source += result;
        		source_len -= result;
        	}
        	else
        		return((size_t)-1);								/*- mm 011102 -*/
        }
        else
        	break;
    }

	return(count);
}

int wcsmbslen(const wchar_t * pwcs, const unsigned int n)
{
	int     count = 0;
	int     result;
	wchar_t * source;
	char    temp[3];
	
	if (!pwcs)
		return (0);
	
	source = (wchar_t*)pwcs;
	
	while(count <= n)
	{
		if (!*source)
		{
			break;
		}
		else
		{
			result = wctomb(temp, *source++);
			if ((count + result) <= n)
			{
				count += result;
			}
			else
				break;
		}
	}
			
	return(count);
}
#endif




