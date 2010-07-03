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
 * $Id: XercesSampleStartupFragment.c 568078 2007-08-21 11:43:25Z amassari $
 */

/*
	The code in this file should be compiled into your Metrowerks Runtime PPC code.
	The net result will be that you can build generic unix-style C code without
	Macintosh modifications to setup argc/argv. The Xerces sample projects expect
	this modification.
	
	To use:
	
		- Add the code below to your Metrowerks Startup.c file (in runtime libraries)
		- Rebuild the various targets of project MSL All.mcp.
		- In the PPC Linker section of your project, set the start code to be
		  __start_ccommand instead of the typical __start. This will cause ccommand
		  to be invoked before main() is started.
		  
	This code has been submitted as a suggestion to Metrowerks: hopefully they'll
	add it to the startup code so this modification won't always be necessary.
	
	!!!NOTE!!!
	
	If the above seems too odious for your needs, you may also modify the "main" file
	of each sample by adding a line such as the following near the start of the main()
	function:
	
		argc = ccommand(&argv);

	This will invoke the command line dialog manually.
	
	- 8/3/00 James Berry <jberry@criticalpath.com>
*/


//	External declaration
int ccommand(char ***arg);

//	Forward declaration
pascal void __start_ccommand(void);

/*
 *	__start_ccommand	-	Optional special startup routine for Metrowerks C++ (PowerPC)
 *
 *	This routine should be specified as the PEF main routine in the container
 *	for any monolithic application that requires arguments via argc/argv.
 *  The program startup/termination sequence is:
 *
 *	1.	Register the exception-handling info for the application
 *	2.	Call all static initializers
 *	3.	Call ccommand to set up default values for 'argc' and 'argv' and call main()
 *	4.	Call exit() to perform required cleanup and termination, including
 *		destroying all static objects, closing open files, closing console window, etc.
 *
 *	We defer all details of proper program termination to the ANSI exit() routine.
 *
 */
pascal void __start_ccommand(void)
{
	int argc;
	char **argv;

	//	set the stack frame back-link to 0 to improve debugger stack display
	clear_stackframe_backlink();

	//	register this code fragment with the Exception Handling mechanism
	fragmentID = __register_fragment(__code_start__, __code_end__,
									__data_start__, __data_end__,
									__exception_table_start__, __exception_table_end__,
									__RTOC());
	
	__init_critical_regions();
	//	call all static initializers
	__sinit();
	
	argc = ccommand(&argv);
	
	//	call main(argc, argv)
	main(argc, argv);
	
	//	call exit() to terminate the program properly--will not return
	exit(0);
	
	//	unregister this code fragment with the Exception Handling mechanism
//	__unregister_fragment(fragmentID);
}


