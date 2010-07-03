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
 * $Id: XMLInitializer.hpp 568078 2007-08-21 11:43:25Z amassari $
 */


#if !defined(XMLINITIALIZER_HPP)
#define XMLINITIALIZER_HPP

#include <xercesc/util/XercesDefs.hpp>

XERCES_CPP_NAMESPACE_BEGIN


/**
  * Utilities that must be implemented in a class-specific way.
  *
  * This class contains methods that must be implemented by different
  * classes that have static data (class or local) that they need
  * to initialize when XMLPlatformUtils::Initialize is invoked.
  */
class XMLUTIL_EXPORT XMLInitializer
{
protected :
    /** @name Initialization methods */
    //@{

    /** Perform per-class initialization of static data
      *
      * Initialization <b>must</b> be called in XMLPlatformUtils::Initialize.
      */
    static void InitializeAllStaticData();

    //@}

    friend class XMLPlatformUtils;

private :
    // -----------------------------------------------------------------------
    //  Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    XMLInitializer();
    XMLInitializer(const XMLInitializer& toCopy);
    XMLInitializer& operator=(const XMLInitializer&);

    /** @name Private static initialization methods */
    //@{

    static void initializeMsgLoader4DOM();
    static void initializeDOMImplementationImpl();
    static void initializeDOMImplementationRegistry();
    static void initializeEmptyNodeList();
    static void initializeDOMNormalizerMsgLoader();
    static void initializeValidatorMsgLoader();
    static void initializeXSValueStatics();
    static void initializeScannerMsgLoader();
    static void initializeEncodingValidator();
    static void initializeExceptionMsgLoader();
    static void initializeDVFactory();
    static void initializeGeneralAttrCheckMap();
    static void initializeXSDErrReporterMsgLoader();
    static void initializeDTDGrammarDfltEntities();
    static void initializeRangeTokenMap();
    static void initializeRegularExpression();
    static void initializeAnyType();

    //@}
};


XERCES_CPP_NAMESPACE_END

#endif
