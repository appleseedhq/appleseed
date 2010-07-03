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

/**
 * $Id: OS400PlatformUtils.cpp 568078 2007-08-21 11:43:25Z amassari $
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#define MY_XP_CPLUSPLUS
#include    <xercesc/util/XMLUniDefs.hpp>
#include    <xercesc/util/XercesDefs.hpp>
#include    <pthread.h>
#include    <xercesc/util/PlatformUtils.hpp>
#include    <xercesc/util/RuntimeException.hpp>
#include    <xercesc/util/Janitor.hpp>
#include    <xercesc/util/XMLString.hpp>
#include    <xercesc/util/XMLHolder.hpp>
#include    <xercesc/util/PanicHandler.hpp>
#include    <stdio.h>
#include    <stdlib.h>
#include    <errno.h>
#include    <unistd.h>
#include    <qp0z1170.h>
#include    <mimchint.h>
#include    <string.h>
#include    <qmhsndpm.h>
#include    <qmhrtvm.h>
#include    <qusec.h>
#include    <unistd.h>
#include    <string.h>
#include    <except.h>
#include    <mih/cmpswp.h>
#include    "OS400PlatformUtils.hpp"
#include    <xercesc/util/OutOfMemoryException.hpp>
#include    <qlgusr.h>   // @1aa
#include    <qusrjobi.h>  // @1aa
#include    <qusec.h>  // @1aa
#include    "utypes.h"  // @1aa
#include    "iconv_util.hpp"  // @1aa
#include    "iconv_cnv.hpp"  // @1aa
#include    <qmhrtvm.h>   // @1aa

#if defined (XML_USE_ICONV400_TRANSCODER)
    #include <xercesc/util/Transcoders/Iconv400/Iconv400TransService.hpp>
XERCES_CPP_NAMESPACE_BEGIN    
	void cleanupDefaultConverter();
XERCES_CPP_NAMESPACE_END    
#elif defined (XML_USE_ICU_TRANSCODER)
    #include <xercesc/util/Transcoders/ICU/ICUTransService.hpp>
#else
 Transcoder not Specified - For OS/400 must be either ICU or Iconv400
#endif

#if defined(XML_USE_MSGFILE_MESSAGELOADER)
	 #include <xercesc/util/MsgLoaders/MsgFile/MsgLoader.hpp>
#elif defined(XML_USE_INMEM_MESSAGELOADER)
	 #include <xercesc/util/MsgLoaders/InMemory/InMemMsgLoader.hpp>
#else
	 #include <xercesc/util/MsgLoaders/ICU/ICUMsgLoader.hpp>
#endif

#if defined (XML_USE_NETACCESSOR_SOCKET)
    #include <xercesc/util/NetAccessors/Socket/SocketNetAccessor.hpp>
#endif

XERCES_CPP_NAMESPACE_BEGIN

char* PackingRepText(const char * const repText1,
		     const char * const repText2,
		     const char * const repText3,
		     const char * const repText4);
// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Platform init method
// ---------------------------------------------------------------------------
XMLNetAccessor* XMLPlatformUtils::makeNetAccessor()
{
#if defined (XML_USE_NETACCESSOR_SOCKET)
    return new (fgMemoryManager) SocketNetAccessor();
#else
    return 0;
#endif
}



//
//  This method is called very early in the bootstrapping process. This guy
//  must create a transcoding service and return it. It cannot use any string
//  methods, any transcoding services, throw any exceptions, etc... It just
//  makes a transcoding service and returns it, or returns zero on failure.
//

XMLTransService* XMLPlatformUtils::makeTransService()
#if defined (XML_USE_ICU_TRANSCODER)
{
    return new (fgMemoryManager) ICUTransService;
}
#elif defined (XML_USE_ICONV400_TRANSCODER)
{
    return new (fgMemoryManager) Iconv400TransService;
}
#else
{
    return new (fgMemoryManager) IconvTransService;
}
#endif

//
//  This method is called by the platform independent part of this class
//  when client code asks to have one of the supported message sets loaded.
//  In our case, we use the ICU based message loader mechanism.
//
XMLMsgLoader* XMLPlatformUtils::loadAMsgSet(const XMLCh* const msgDomain)
{
    XMLMsgLoader* retVal;
    try
    {
#if defined(XML_USE_MSGFILE_MESSAGELOADER)        
        retVal = new (fgMemoryManager) MsgCatalogLoader(msgDomain);
#elif defined (XML_USE_ICU_MESSAGELOADER)
	retVal = new (fgMemoryManager) ICUMsgLoader(msgDomain);
#elif defined (XML_USE_ICONV_MESSAGELOADER)
	retVal = new (fgMemoryManager) MsgCatalogLoader(msgDomain);
#else
	retVal = new (fgMemoryManager) InMemMsgLoader(msgDomain);
#endif
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...)
    {
        panic( PanicHandler::Panic_CantLoadMsgDomain );
    }
    return retVal;
}

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: The panic method
// ---------------------------------------------------------------------------
void XMLPlatformUtils::panic(const PanicHandler::PanicReasons reason)
{
	
    if (fgUserPanicHandler)
    {
    	fgUserPanicHandler->panic(reason);
    }
 	
    //
    //  We just print a message and exit, Note we are currently dependent on
    // the number of reasons being under 10 for this teo work
    //
    else
    {
        struct reason_code
        {
            char reason_char;
            char endofstring;
        }reason_code;

        reason_code.reason_char = '0';
        reason_code.endofstring = '\0';
        reason_code.reason_char = reason_code.reason_char + reason;
        send_message((char*)&reason_code,GENERAL_PANIC_MESSAGE,'e');
    }

}
// ---------------------------------------------------------------------------
//  XMLPlatformUtils: File Methods
// ---------------------------------------------------------------------------
unsigned int XMLPlatformUtils::curFilePos(FileHandle theFile
                                          , MemoryManager* const manager)
{
    // Get the current position
    int curPos = ftell( (FILE*)theFile);
    if (curPos == -1)
	ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetSize, manager);

    return (unsigned int)curPos;
}

void XMLPlatformUtils::closeFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    if (fclose((FILE*)theFile))
	ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotCloseFile, manager);
}


unsigned int XMLPlatformUtils::fileSize(FileHandle theFile
                                        , MemoryManager* const manager)
{
    // Get the current position
    long  int curPos = ftell((FILE*)theFile);
    if (curPos == -1)
		ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetCurPos, manager);

    // Seek to the end and save that value for return
     if (fseek( (FILE*)theFile, 0, SEEK_END) )
		ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotSeekToEnd, manager);

    long int retVal = ftell( (FILE*)theFile);
    if (retVal == -1)
		ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotSeekToEnd, manager);

    // And put the pointer back
    if (fseek( (FILE*)theFile, curPos, SEEK_SET) )
		ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotSeekToPos, manager);

    return (unsigned int)retVal;
}

static char JOBCCSID_CONVERTER_NAME[60] = "";  // @1aa
static UConverter *jobCCSID_Converter = NULL;  // @1aa
void get_jobCCSID_converter()   // @1aa
{
  Qwc_JOBI0400_t job_attr;                 /* Job attribute receive. @1aa */
  Qus_EC_t       recv_error;               /* Error on receive?   @1aa  */
  UErrorCode uerr = U_ZERO_ERROR;          /* Error structure for creating converter @1aa */

  recv_error.Bytes_Provided = sizeof(recv_error);  // @1aa

  QUSRJOBI(&job_attr,
	   sizeof(job_attr),
	   "JOBI0400",
	   "*                         ",
	   "                ",
	   &recv_error);   // extract job ccsid (integer form) @1aa

  if ( recv_error.Bytes_Available != 0 )  // @1aa
  {  /* The 'QUSRJOBI' API failed for some reason.  */
      strcpy(JOBCCSID_CONVERTER_NAME, "ibm037");  // use internal default @1aa  
  }
  else  // @1aa
  {  /* Get the default job CCSID.   */
      int JobCCSID = job_attr.Default_Coded_Char_Set_Id;   // @1aa
      char jobCCSID_str[30];   // @1aa

      sprintf(jobCCSID_str, "%05d", JobCCSID);  // @1aa
      // convert job CCSID (integer format) to IANA text format.
      int rc = QlgCvtTextDescToDesc(0,
				    11,
				    (char *)jobCCSID_str,
				    strlen(jobCCSID_str),
				    &JOBCCSID_CONVERTER_NAME[0],
				    sizeof(JOBCCSID_CONVERTER_NAME),
				    JobCCSID);  // @1aa
      if (rc < 0)  // @1aa
	  strcpy(JOBCCSID_CONVERTER_NAME, "ibm037"); // @1aa   

  }

  // create converter for job CCSID
  jobCCSID_Converter = createConverter (JOBCCSID_CONVERTER_NAME, &uerr); // @1aa

  if (U_FAILURE (uerr))  // @1aa
  {
      jobCCSID_Converter = NULL; // don't use jobCCSID_Converter @1aa  
  }

  return;    // @1aa
}

#include <qmhrtvm.h>
#include <qusec.h>
#include <errno.h> // for UErrorCode  @1aa 

FileHandle XMLPlatformUtils::openFile(const XMLCh* const FILENAME
                                      , MemoryManager* const manager)
{   char errno_id[7];  // @1aa
    int numChars = (u_strlen(FILENAME)*2) + 2;  // @1aa @1bc add 2 bytes for null terminator 
    char* saved_myTarget = new char[numChars];  // target for transcoding to job CCSID @1aa
    char* myTarget = (char *) saved_myTarget;  // ucnv_fromUnicode can change myTarget pointer @1aa
    memset(myTarget,'\0',numChars); // make sure storage is zeroed out to allow for
                                    // automatic null terminator @1ba 
    char* myTargetLimit = myTarget + (numChars - 1);    // @1aa
    const UChar *mySource = (UChar *) FILENAME;

    int mySourceChars = u_strlen(mySource);  // @1aa 
    const UChar* mySourceLimit = (UChar *) mySource + u_strlen(mySource);    // @1aa

    UErrorCode err = U_ZERO_ERROR;    /* Error structure for creating converter @1aa */

    if (jobCCSID_Converter == NULL)  // should only need to create this converter once. @1aa
    {
	  get_jobCCSID_converter();   // @1aa
    }
 
    // need to do open with pathname/filename in job CCSID, not IBM037.  The other
    // openFile( ) is dead code, so didn't bother with converting the pathname/fileName there.
    // Alternative to using converter code is to use XLATEMB MI instruction.  @1aa
    const char* TMPFILENAME;
    if (jobCCSID_Converter != NULL) {
       ucnv_fromUnicode (jobCCSID_Converter,
			&myTarget,
			myTargetLimit,
			&mySource,
			mySourceLimit,
			NULL,
			TRUE,
			&err);  // @1aa

       TMPFILENAME = saved_myTarget;  // @1aa
       if (U_FAILURE (err)) {
	   TMPFILENAME = XMLString::transcode(FILENAME);  // transcode to IBM037 as backup @1aa.
       }
 
    }
    else {
	TMPFILENAME = XMLString::transcode(FILENAME);  // transcode to IBM037 as backup @1aa.
    }

    //    ArrayJanitor<char> janText((char*)saved_myTarget);  @1dd
    errno = 0;
    FileHandle retVal = (FILE*)fopen( TMPFILENAME , "rb" );

    if (retVal == NULL)
    {
	send_message((char*)TMPFILENAME,FILE_OPEN_PROBLEMS,'d');
	convert_errno(errno_id,errno);
	send_message(NULL,errno_id,'d');
	delete [] saved_myTarget;  // @1aa
	return 0;
    }
    delete [] saved_myTarget;  // @1aa

    return retVal;
}

// If this version of openFile is ever called, need to convert the filename
// to job CCSID similar to what's done in the other openFile method.  @1aa.
FileHandle XMLPlatformUtils::openFile(const char* const fileName
                                      , MemoryManager* const manager)
{   char errno_id[7];
    errno = 0;
    FileHandle retVal = (FILE*)fopen( fileName , "rb" );

    if (retVal == NULL)
    {
     send_message((char*)fileName,FILE_OPEN_PROBLEMS,'d');
     convert_errno(errno_id,errno);
     send_message(NULL,errno_id,'d');
        return 0;
    }
    return retVal;
}

FileHandle XMLPlatformUtils::openFileToWrite(const XMLCh* const fileName
                                             , MemoryManager* const manager)
{
    const char* tmpFileName = XMLString::transcode(fileName, manager);
    ArrayJanitor<char> janText((char*)tmpFileName, manager);

    return openFileToWrite(tmpFileName);
}

FileHandle XMLPlatformUtils::openFileToWrite(const char* const fileName
                                             , MemoryManager* const manager)
{
    char errno_id[7];
    errno = 0;
    FileHandle retVal = (FILE*)fopen( fileName , "wb" );

    if (retVal == NULL)
    {
     send_message((char*)fileName,FILE_OPEN_PROBLEMS,'d');
     convert_errno(errno_id,errno);
     send_message(NULL,errno_id,'d');
        return 0;
    }

    return retVal;
}

unsigned int
XMLPlatformUtils::readFileBuffer(  FileHandle      theFile
                                , const unsigned int    toRead
                                , XMLByte* const  toFill
                                , MemoryManager* const manager)
{
    size_t noOfItemsRead = fread( (void*) toFill, 1, toRead, (FILE*)theFile);

    if(ferror((FILE*)theFile))
    {
		ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotReadFromFile, manager);
    }
    return (unsigned int)noOfItemsRead;
}

void
XMLPlatformUtils::writeBufferToFile( FileHandle     const  theFile
                                   , long                  toWrite
                                   , const XMLByte* const  toFlush
                                   , MemoryManager* const  manager)
{
    if (!theFile        ||
        (toWrite <= 0 ) ||
        !toFlush         )
        return;

    const XMLByte* tmpFlush = (const XMLByte*) toFlush;
    size_t bytesWritten = 0;

    while (true)
    {
        bytesWritten=fwrite(tmpFlush, sizeof(XMLByte), toWrite, (FILE*)theFile);

        if(ferror((FILE*)theFile))
        {
            ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotWriteToFile, manager);
        }

        if (bytesWritten < toWrite) //incomplete write
        {
            tmpFlush+=bytesWritten;
            toWrite-=bytesWritten;
            bytesWritten=0;
        }
        else
            return;
    }

    return;
}

void XMLPlatformUtils::resetFile(FileHandle theFile
                                 , MemoryManager* const manager)
{
    // Seek to the start of the file
    if (fseek((FILE*)theFile, 0, SEEK_SET) )
		ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotResetFile, manager);
}



// ---------------------------------------------------------------------------
//  XMLPlatformUtils: File system methods
// ---------------------------------------------------------------------------

//-------------------------
//-- BEGIN realpath code --
//-------------------------

#include <limits.h>
#include <unistd.h>
#include <errno.h>

//
// realpath
//
// Resolve a file name into its absolute name
// This function doesn't exist in the iSeries C runtime library so this partial replacement was
//  written to handle most of its function.  This implementation doesn't resolve symbolic links
//  to their "real" paths because for the purposes of reading files the symlinks will work just
//  fine.
//
char *realpath(const char *file_name, char *resolved_name) {
    // Input: file_name - the name of a file or directory
    // Output: resolved_name - file_name with a fully qualified path and all "extraneous" path stuff removed
    // Returned value: Same as resolved_name unless there is an error in which case NULL is returned
    //
    // Possible errors:  (no)==we don't check that
    //  EACCES      Read or search permission was denied for a component of the path name.
    //  EINVAL     File_name or resolved_name is a null pointer.
    //  (no) ELOOP     Too many symbolic links are encountered in translating file_name.
    //  ENAMETOOLONG     The length of file_name or resolved_name exceeds PATH_MAX or a path name component is longer than NAME_MAX.
    //  ENOENT     The file_name parameter does not exist or points to an empty string.
    //  (no) ENOTDIR     A component of the file_name prefix is not a directory.
    // If there is an error in resolving the name the return value will be NULL (i.e., 0) and resolved_name
    //  will be changed to an empty string (a 0 will be written into its first character position)

    // Note for future expansion:  "readlink" for links

    // Check for null name errors
     if (resolved_name == NULL) {
        errno = EINVAL;
        return(NULL);
     }
    // Assumption: At this point resolved_name points to a character array large enough to hold at least PATH_MAX characters
     if (file_name == NULL) {
        errno = EINVAL;
        *resolved_name = '\0';
        return(NULL);
     }
    // Assumption: At this point file_name is a valid null terminated string

    // Check for empty name error
     if (*file_name == '\0') {
        errno = ENOENT;
        *resolved_name = '\0';
        return(NULL);
     }

    char *from = (char*)file_name;
    char *to = resolved_name;
    int fromIdx=0, toIdx=0;

    if (*file_name == '/') {
        // If file_name starts with a '/', it's an absolute path
        // Everything's already set up properly
        to[toIdx++] = '/';
    } // if
    else {
        // file_name doesn't start with a '/' so it is relative to the current directory
        // Prepend the current working directory before the file name
        getcwd(to, PATH_MAX);
        size_t cwd_len = strlen(resolved_name);
        // Assumption: getcwd returns a non-empty, valid, null terminated string

        // Add '/' on the cwd if needed
        // Note: I think the only time a '/' will end the cwd is if it is just "/"
        //  but this covers otherwise just in case
        toIdx = cwd_len;
        if ((toIdx < PATH_MAX-1) &&
            (to[toIdx] != '/')) {
             to[toIdx++] = '/';
        } // if
    } // else

    // The target ends in a '/' at this point
    // Skip any leading '/'s in the source
    while (from[fromIdx] == '/') {
        fromIdx++;
    } // while

    // Copy from the source to the target removing extraneous "."s, "/"s, and ".."s as we go
    // Assumption: looking ahead for characters is OK because all the string end with '/0'
    while (from[fromIdx] != '\0' ) {
        // Assumption - at top of loop we are either at '..', '.', or 'xxxxxxxx' (where x is any non-'/' character), each either followed by a '/' or a closing '\0'
        //  "../" => "/", also strips trailing ".."
        if ((from[fromIdx] == '.') &&
            (from[fromIdx+1] == '.') &&
            ((from[fromIdx+2] == '/') || (from[fromIdx+2] == '\0'))) {
            fromIdx += (from[fromIdx+2] == '\0') ? 2 : 3;
            // Back up to the previous '/' in the target except if we are at the first '/' already
            if (toIdx >= 2) {
                // Back up past the current '/'
                --toIdx;
                // Look back until we find the previous '/'
                while(to[toIdx-1] != '/') {
                     --toIdx;
                } // while
            } // if
        } // if

        // "./" => "/", also strips trailing "."
        else if ((from[fromIdx] == '.') &&
                 ((from[fromIdx+1] == '/') || (from[fromIdx+1] == '\0'))) {
             fromIdx += (from[fromIdx+1] == '\0') ? 1 : 2;
        } // else if

        else {
            // Copy the characters up to the next '/' or to the closing '\0'
            while((from[fromIdx] != '/') && (from[fromIdx] != '\0')) {
                // Check that the file name won't be too long (allow for the '\0' too)
                if (toIdx >= PATH_MAX-1) {
                    errno = ENAMETOOLONG;
                    *resolved_name = '\0';
                    return(NULL);
                } // if

                to[toIdx++] = from[fromIdx++];
            } // while
        } // else

        if (from[fromIdx] == '/') {
            // Skip any remaining '/'s in the source
            while (from[fromIdx] == '/') {
                fromIdx++;
            } // while

            // insert a '/' if there's more path to come (and room for it)
            if (from[fromIdx] != '\0') {
                // Check that the file name won't be too long (allow for the '\0' too)
                if (toIdx >= PATH_MAX-1) {
                    errno = ENAMETOOLONG;
                    *resolved_name = '\0';
                    return(NULL);
                } // if

                to[toIdx++] = '/';
            } // if
        } // if

    } // while

    // Remove a trailing '/' (except from "/")
    if ((toIdx > 1) &&
        (to[toIdx-1] == '/'))
        --toIdx;

    // End the string properly
    to[toIdx] = '\0';

    // Check if the file exists
    if (access(resolved_name,F_OK)) {
        errno = EACCES;
        *resolved_name = '\0';
        return(NULL);
    } // if

    // The file exists, return its name.
     return(resolved_name);
}

//-----------------------
//-- END realpath code --
//-----------------------




XMLCh* XMLPlatformUtils::getFullPath(const XMLCh* const srcPath,
                                     MemoryManager* const manager)
{

    //
    //  NOTE: THe path provided has always already been opened successfully,
    //  so we know that its not some pathological freaky path. It comes in
    //  in native format, and goes out as Unicode always
    //
    char* newSrc = XMLString::transcode(srcPath, manager);
     ArrayJanitor<char> janText(newSrc, manager);
    // Use a local buffer that is big enough for the largest legal path
    char absPath[PATH_MAX + 1];
	//get the absolute path
    char* retPath = realpath(newSrc, &absPath[0]);	
	
    if (!retPath)
    {
		ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::File_CouldNotGetBasePathName, manager);
    }
    return XMLString::transcode(absPath, manager);


}

bool XMLPlatformUtils::isRelative(const XMLCh* const toCheck
                                  , MemoryManager* const manager)
{
    // Check for pathological case of empty path
    if (!toCheck[0])
        return false;

    //
    //  If it starts with a slash, then it cannot be relative. This covers
    //  both something like "\Test\File.xml" and an NT Lan type remote path
    //  that starts with a node like "\\MyNode\Test\File.xml".
    //
    if (*toCheck == chForwardSlash)
        return false;

    // Else assume its a relative path
    return true;
}

XMLCh* XMLPlatformUtils::getCurrentDirectory(MemoryManager* const manager)
{
    char  dirBuf[PATH_MAX + 2];
    char  *curDir = getcwd(&dirBuf[0], PATH_MAX + 1);

    if (!curDir)
    {
        ThrowXMLwithMemMgr(XMLPlatformUtilsException,
                 XMLExcepts::File_CouldNotGetBasePathName, manager);
    }

    return XMLString::transcode(curDir, manager);
}

inline bool XMLPlatformUtils::isAnySlash(XMLCh c) 
{
    return ( chBackSlash == c || chForwardSlash == c);
}

void send_message (char * text, char * messageid, char type)
{


           short textsize;
           char* buffer;
           char* anchor;
           char* id;
           char message_id[8] = "CPF9897";/* id for raw txt
                                             message                */
           char message_file_name[21];
           char message_type[11] ="*DIAG     ";/* send diagnostic
                                                           message   */
           char call_stack[11] ="*         " ;/* current callstack*/
           int call_stack_counter= 0;/* sent to current call stack */
           char message_key[4]; /* return value - not used          */
            struct {
             int bytes_available;
             int bytes_used;
             char exception_id[7];
             char reserved;
             char exception_data[1];
                    } error_code;
           int msg_size;
          char* msg_type;
         error_code.bytes_available = sizeof(error_code);
/* check input parameters and set up the message information */
         if (messageid != 0)  /* was a message id passed   */
	 {
           if (strncmp(messageid,"CPF",3) &&
               strncmp(messageid,"CPE",3))
             strcpy(message_file_name,"QXMLMSG   *LIBL     ");
           else
             strcpy(message_file_name,"QCPFMSG   QSYS      ");


           id = messageid; /* yes - use the id, will be
                           in QCPFMSG                              */

	 }

         else  /* just use what we have for immediate text          */
	 {
           id = &message_id[0];
           strcpy(message_file_name,"QCPFMSG   QSYS      ");

	 }
         if (type == 'e')  /* is this the terminating exception     */
              msg_type = "*COMP      ";/* set it as completion      */
         else            /* currently all other messages are
                             diagnostics                             */
              msg_type = "*DIAG      ";
         if (text != 0)                  /* was a text field passed           */

         {
          textsize = strlen(text);
          msg_size = textsize + sizeof(short);
          buffer = (char*)malloc(msg_size);
          anchor = buffer;
          memcpy(buffer, (void*)&textsize, sizeof(short));
          buffer +=sizeof(short);
          memcpy(buffer, text, textsize);
         }
         else
           msg_size = 0;
         #pragma exception_handler(jsendprob, 0, _C1_ALL, _C2_ALL,_CTLA_HANDLE)

              QMHSNDPM((char *)id,&message_file_name,anchor,
                     msg_size,(char*)msg_type,(char*)&call_stack,
                     call_stack_counter,&message_key,&error_code);

   jsendprob:

#pragma disable_handler

    return ;

}

void abnormal_termination(int termcode)
{
   send_message(NULL,"CPF9899",'e'); /* send final exception that we have terminated*/
}

// ---------------------------------------------------------------------------
//  XMLPlatformUtils: Timing Methods
// ---------------------------------------------------------------------------
unsigned long XMLPlatformUtils::getCurrentMillis()
{
 _MI_Time mt;
         struct timeval tv;
         int rc;

         mattod(mt);
   rc = Qp0zCvtToTimeval(&tv, mt, QP0Z_CVTTIME_TO_TIMESTAMP);
   return((tv.tv_sec*1000 )+ (tv.tv_usec/1000));
}

// -----------------------------------------------------------------------
//  Mutex methods
// -----------------------------------------------------------------------

#if !defined (APP_NO_THREADS)

typedef XMLHolder<pthread_mutex_t>  MutexHolderType;

static MutexHolderType* gAtomicOpMutex = 0;

void XMLPlatformUtils::platformInit()
{
    //
    // The gAtomicOpMutex mutex needs to be created
	// because compareAndSwap and incrementlocation and decrementlocation
	// does not have the atomic system calls for usage
    // Normally, mutexes are created on first use, but there is a
    // circular dependency between compareAndExchange() and
    // mutex creation that must be broken.

    gAtomicOpMutex = new (fgMemoryManager) MutexHolderType;

    if (pthread_mutex_init(&gAtomicOpMutex->fInstance, NULL)) {
		delete gAtomicOpMutex;
		gAtomicOpMutex = 0;
        panic( PanicHandler::Panic_SystemInit );
	}
}

class  RecursiveMutex : public XMemory
{
public:
    pthread_mutex_t   mutex;
    int               recursionCount;
    pthread_t         tid;
    MemoryManager*    fManager;

    RecursiveMutex(MemoryManager* manager) :
        fManager(manager)
    {
		       if (pthread_mutex_init(&mutex, NULL))
			    XMLPlatformUtils::panic(PanicHandler::Panic_MutexErr);
                       recursionCount = 0;
                       tid.reservedHiId = 0;
		       tid.reservedLoId = 0;
                       tid.reservedHandle = 0;
                     }

    ~RecursiveMutex() {
			if (pthread_mutex_destroy(&mutex))
			    ThrowXMLwithMemMgr(XMLPlatformUtilsException, XMLExcepts::Mutex_CouldNotDestroy, fManager);
                      }

     void lock()      {
			  if (pthread_equal(tid, pthread_self()))
			  {
			      recursionCount++;
			      return;
			  }
			  if (pthread_mutex_lock(&mutex) != 0)
			      XMLPlatformUtils::panic(PanicHandler::Panic_MutexErr);
			  tid = pthread_self();
			  recursionCount = 1;
		      }


     void unlock()    {
                          if (--recursionCount > 0)
                              return;

			  if (pthread_mutex_unlock(&mutex) != 0)
			      XMLPlatformUtils::panic(PanicHandler::Panic_MutexErr);
                          tid.reservedHandle= 0;
			  tid.reservedHiId = 0;
			  tid.reservedLoId = 0;
                       }
   };

void* XMLPlatformUtils::makeMutex(MemoryManager* manager)
{
    return new (manager) RecursiveMutex(manager);
}


void XMLPlatformUtils::closeMutex(void* const mtxHandle)
{
    if (mtxHandle == NULL)
        return;
    RecursiveMutex *rm = (RecursiveMutex *)mtxHandle;
    delete rm;
}


void XMLPlatformUtils::lockMutex(void* const mtxHandle)
{
    if (mtxHandle == NULL)
        return;
    RecursiveMutex *rm = (RecursiveMutex *)mtxHandle;
    rm->lock();
}

void XMLPlatformUtils::unlockMutex(void* const mtxHandle)
{
    if (mtxHandle == NULL)
        return;
    RecursiveMutex *rm = (RecursiveMutex *)mtxHandle;
    rm->unlock();
}

// -----------------------------------------------------------------------
//  Miscellaneous synchronization methods
// -----------------------------------------------------------------------
//atomic system calls in Solaris is only restricted to kernel libraries
//So, to make operations thread safe we implement static mutex and lock
//the atomic operations. It makes the process slow but what's the alternative!
void* XMLPlatformUtils::compareAndSwap ( void**      toFill ,
                    const void* const newValue ,
                    const void* const toCompare)
{
    //return ((void*)cas32( (uint32_t*)toFill,  (uint32_t)toCompare, (uint32_t)newValue) );
    // the below calls are temporarily made till the above functions are part of user library
    // Currently its supported only in the kernel mode

    if (pthread_mutex_lock( &gAtomicOpMutex->fInstance))
        panic(PanicHandler::Panic_SynchronizationErr);

    void *retVal = *toFill;
    if (*toFill == toCompare)
              *toFill = (void *)newValue;

    if (pthread_mutex_unlock( &gAtomicOpMutex->fInstance))
        panic(PanicHandler::Panic_SynchronizationErr);

    return retVal;
}

int XMLPlatformUtils::atomicIncrement(int &location)
{
    int current = location;		
    int new_loc = current+1;		

    while (_CMPSWP(&current,			
                   &location,	
                   new_loc) == 0)		
        new_loc = current+1;
    int tmp = new_loc;

    return tmp;
}
int XMLPlatformUtils::atomicDecrement(int &location)
{

    int current = location;		
    int new_loc = current-1;		

    while (_CMPSWP(&current,			
                   &location,	
                   new_loc) == 0)		
        new_loc = current-1;
    int tmp = new_loc;

    return tmp;
}


#else // #if !defined (APP_NO_THREADS)

void XMLPlatformUtils::platformInit()
{
   // do nothing
}

void XMLPlatformUtils::closeMutex(void* const)
{
}

void XMLPlatformUtils::lockMutex(void* constmanager)
{
}

void* XMLPlatformUtils::makeMutex(MemoryManager*)
{
        return 0;
}

void XMLPlatformUtils::unlockMutex(void* const)
{
}

void* XMLPlatformUtils::compareAndSwap ( void**      toFill,
                                   const void* const newValue,
                                   const void* const toCompare)
{
    void *retVal = *toFill;
    if (*toFill == toCompare)
       *toFill = (void *)newValue;
    return retVal;
}

int XMLPlatformUtils::atomicIncrement(int &location)
{
    return ++location;
}

int XMLPlatformUtils::atomicDecrement(int &location)
{
    return --location;
}

#endif // APP_NO_THREADS


/*
 * convert the errno value to a cpf message identifier by converting the
 * error to its decimal equivalent and appending "CPE" to the front
 * note that the caller passes the storage for the message id as a parm
 */
void convert_errno(char* errno_id,int errnum)
	      {
sprintf(errno_id,"CPE%d04" ,errnum );
return;
	      }

FileHandle XMLPlatformUtils::openStdInHandle(MemoryManager* const manager)
{
    return (FileHandle)fdopen(dup(0), "r");
}

void XMLPlatformUtils::platformTerm()
{
#if !defined (APP_NO_THREADS)
	pthread_mutex_destroy(&gAtomicOpMutex->fInstance);
    delete gAtomicOpMutex;
	gAtomicOpMutex = 0;
#endif

#if defined (XML_USE_ICONV400_TRANSCODER)
	cleanupDefaultConverter();
#endif
}

#include <xercesc/util/LogicalPath.c>

XERCES_CPP_NAMESPACE_END
