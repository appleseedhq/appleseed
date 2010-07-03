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
 * $Id: XMLInitializer.cpp 568078 2007-08-21 11:43:25Z amassari $
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XMLInitializer.hpp>
#include <xercesc/util/PlatformUtils.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  XMLInitializer: Initialization methods
// ---------------------------------------------------------------------------
void XMLInitializer::InitializeAllStaticData()
{
    try {
        initializeMsgLoader4DOM();
        initializeRangeTokenMap();
        initializeRegularExpression();
        initializeDOMImplementationImpl();
        initializeDOMImplementationRegistry();
        initializeEmptyNodeList();
        initializeDOMNormalizerMsgLoader();
        initializeValidatorMsgLoader();
        initializeXSValueStatics();
        initializeScannerMsgLoader();
        initializeEncodingValidator();
        initializeExceptionMsgLoader();
        initializeDVFactory();
        initializeGeneralAttrCheckMap();
        initializeXSDErrReporterMsgLoader();
        initializeDTDGrammarDfltEntities();
        initializeAnyType();
    }
    catch(...) {
        XMLPlatformUtils::panic(PanicHandler::Panic_AllStaticInitErr);
    }
}


XERCES_CPP_NAMESPACE_END
