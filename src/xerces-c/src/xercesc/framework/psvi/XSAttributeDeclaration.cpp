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
 * $Log$
 * Revision 1.15  2004/09/08 13:56:07  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.14  2004/07/06 14:58:15  cargilld
 * Rename VALUE_CONSTRAINT enumeration names to avoid naming conflict with AIX system header which already uses VC_DEFAULT as a macro.  Will need to document that this fix breaks source code compatibility.
 *
 * Revision 1.13  2004/05/04 19:02:40  cargilld
 * Enable IDs to work on all kinds of schema components
 *
 * Revision 1.12  2004/01/29 11:46:30  cargilld
 * Code cleanup changes to get rid of various compiler diagnostic messages.
 *
 * Revision 1.11  2004/01/06 03:55:26  knoaman
 * Various PSVI fixes.
 *
 * Revision 1.10  2003/12/30 20:41:06  neilg
 * do not report anything about default/fixed values for non-global attribute declarations
 *
 * Revision 1.9  2003/12/29 17:06:31  knoaman
 * PSVI: return value constraint only if global declaration
 *
 * Revision 1.8  2003/12/29 16:15:42  knoaman
 * More PSVI updates
 *
 * Revision 1.7  2003/11/21 22:34:45  neilg
 * More schema component model implementation, thanks to David Cargill.
 * In particular, this cleans up and completes the XSModel, XSNamespaceItem,
 * XSAttributeDeclaration and XSAttributeGroup implementations.
 *
 * Revision 1.6  2003/11/21 17:19:30  knoaman
 * PSVI update.
 *
 * Revision 1.5  2003/11/14 22:47:53  neilg
 * fix bogus log message from previous commit...
 *
 * Revision 1.4  2003/11/14 22:33:30  neilg
 * Second phase of schema component model implementation.  
 * Implement XSModel, XSNamespaceItem, and the plumbing necessary
 * to connect them to the other components.
 * Thanks to David Cargill.
 *
 * Revision 1.3  2003/11/06 15:30:04  neilg
 * first part of PSVI/schema component model implementation, thanks to David Cargill.  This covers setting the PSVIHandler on parser objects, as well as implementing XSNotation, XSSimpleTypeDefinition, XSIDCDefinition, and most of XSWildcard, XSComplexTypeDefinition, XSElementDeclaration, XSAttributeDeclaration and XSAttributeUse.
 *
 * Revision 1.2  2003/09/17 17:45:37  neilg
 * remove spurious inlines; hopefully this will make Solaris/AIX compilers happy.
 *
 * Revision 1.1  2003/09/16 14:33:36  neilg
 * PSVI/schema component model classes, with Makefile/configuration changes necessary to build them
 *
 */

#include <xercesc/framework/psvi/XSAttributeDeclaration.hpp>
#include <xercesc/framework/psvi/XSModel.hpp>
#include <xercesc/framework/psvi/XSNamespaceItem.hpp>
#include <xercesc/util/StringPool.hpp>
#include <xercesc/validators/schema/SchemaGrammar.hpp>
#include <xercesc/validators/schema/SchemaAttDef.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  XSAttributeDeclaration: Constructors and Destructor
// ---------------------------------------------------------------------------
XSAttributeDeclaration::XSAttributeDeclaration(SchemaAttDef* const           attDef,
                                               XSSimpleTypeDefinition* const typeDef,
                                               XSAnnotation* const           annot,
                                               XSModel* const                xsModel,
                                               XSConstants::SCOPE            scope,
                                               XSComplexTypeDefinition*      enclosingCTDefinition,
                                               MemoryManager * const         manager)
    : XSObject(XSConstants::ATTRIBUTE_DECLARATION, xsModel, manager)
    , fAttDef(attDef)
    , fTypeDefinition(typeDef)
    , fAnnotation(annot) 
    , fScope(scope)
    , fEnclosingCTDefinition(enclosingCTDefinition)        
{
}

XSAttributeDeclaration::~XSAttributeDeclaration() 
{
    // don't delete fTypeDefinition - deleted by XSModel
}

// ---------------------------------------------------------------------------
//  XSAttributeDeclaration: XSObject virtual methods
// ---------------------------------------------------------------------------
const XMLCh *XSAttributeDeclaration::getName() 
{
    return fAttDef->getAttName()->getLocalPart();
}

const XMLCh *XSAttributeDeclaration::getNamespace() 
{
    return fXSModel->getURIStringPool()->getValueForId(fAttDef->getAttName()->getURI());
}

XSNamespaceItem *XSAttributeDeclaration::getNamespaceItem() 
{
    return fXSModel->getNamespaceItem(getNamespace());
}

// ---------------------------------------------------------------------------
//  XSAttributeDeclaration: access methods
// ---------------------------------------------------------------------------

XSConstants::VALUE_CONSTRAINT XSAttributeDeclaration::getConstraintType() const
{
    if (fScope != XSConstants::SCOPE_GLOBAL)
        return XSConstants::VALUE_CONSTRAINT_NONE;

    if (fAttDef->getDefaultType() == XMLAttDef::Default)
        return XSConstants::VALUE_CONSTRAINT_DEFAULT;

    if ((fAttDef->getDefaultType() == XMLAttDef::Fixed) ||
        (fAttDef->getDefaultType() == XMLAttDef::Required_And_Fixed))
        return XSConstants::VALUE_CONSTRAINT_FIXED;

    return XSConstants::VALUE_CONSTRAINT_NONE;
}

const XMLCh *XSAttributeDeclaration::getConstraintValue()
{
    if (fScope == XSConstants::SCOPE_GLOBAL)
        return fAttDef->getValue();

    return 0;
}

bool XSAttributeDeclaration::getRequired() const
{
    if (fAttDef->getDefaultType() == XMLAttDef::Required ||
        fAttDef->getDefaultType() == XMLAttDef::Required_And_Fixed)
        return true;

    return false;
}

XERCES_CPP_NAMESPACE_END


