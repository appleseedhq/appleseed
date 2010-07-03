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

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/DOM.hpp> for the entire
//  DOM API, or xercesc/dom/DOM*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//


#if !defined(DOMTYPEINFOIMPL_HPP)
#define DOMTYPEINFOIMPL_HPP

//------------------------------------------------------------------------------------
//  Includes
//------------------------------------------------------------------------------------
#include <xercesc/dom/DOMTypeInfo.hpp>
#include <xercesc/dom/DOMPSVITypeInfo.hpp>

XERCES_CPP_NAMESPACE_BEGIN

class DOMDocumentImpl;

class CDOM_EXPORT DOMTypeInfoImpl : public DOMTypeInfo, public DOMPSVITypeInfo
{
public:

    //-----------------------------------------------------------------------------------
    //  Constructor
    //-----------------------------------------------------------------------------------
    DOMTypeInfoImpl(const XMLCh* namespaceUri=0, const XMLCh* name=0);
    DOMTypeInfoImpl(DOMDocumentImpl* ownerDoc, const DOMPSVITypeInfo* sourcePSVI);

    static DOMTypeInfoImpl  g_DtdValidatedElement;
    static DOMTypeInfoImpl  g_DtdNotValidatedAttribute;
    static DOMTypeInfoImpl  g_DtdValidatedCDATAAttribute;
    static DOMTypeInfoImpl  g_DtdValidatedIDAttribute;
    static DOMTypeInfoImpl  g_DtdValidatedIDREFAttribute;
    static DOMTypeInfoImpl  g_DtdValidatedIDREFSAttribute;
    static DOMTypeInfoImpl  g_DtdValidatedENTITYAttribute;
    static DOMTypeInfoImpl  g_DtdValidatedENTITIESAttribute;
    static DOMTypeInfoImpl  g_DtdValidatedNMTOKENAttribute;
    static DOMTypeInfoImpl  g_DtdValidatedNMTOKENSAttribute;
    static DOMTypeInfoImpl  g_DtdValidatedNOTATIONAttribute;
    static DOMTypeInfoImpl  g_DtdValidatedENUMERATIONAttribute;

    //@{
    // -----------------------------------------------------------------------
    //  Getter methods
    // -----------------------------------------------------------------------
    /**
     * Returns The name of a type declared for the associated <code>DOMElement</code> 
     * or <code>DOMAttr</code>, or null if undeclared.
     *
     * <p><b>"Experimental - subject to change"</b></p>
     *
     * @return The name of a type declared for the associated <code>DOMElement</code> 
     * or <code>DOMAttribute</code>, or null if undeclared.
     * @since DOM level 3
     */
    virtual const XMLCh* getName() const;

    /**
     * The namespace of the type declared for the associated <code>DOMElement</code> 
     * or <code>DOMAttr</code> or null if the <code>DOMElement</code> does not have 
     * declaration or if no namespace information is available.
     *
     * <p><b>"Experimental - subject to change"</b></p>
     *
     * @return The namespace of the type declared for the associated <code>DOMElement</code> 
     * or <code>DOMAttr</code> or null if the <code>DOMElement</code> does not have 
     * declaration or if no namespace information is available.
     * @since DOM level 3
     */
    virtual const XMLCh* getNamespace() const;

    /**
     * Returns the string value of the specified PSVI property associated to a 
     * <code>DOMElement</code> or <code>DOMAttr</code>, or null if not available.
     *
     * <p><b>"Experimental - subject to change"</b></p>
     *
     * @return the string value of the specified PSVI property associated to a 
     * <code>DOMElement</code> or <code>DOMAttr</code>, or null if not available.
     */
    virtual const XMLCh* getStringProperty(PSVIProperty prop) const;

    /**
     * Returns the numeric value of the specified PSVI property associated to a 
     * <code>DOMElement</code> or <code>DOMAttr</code>, or 0 if not available.
     *
     * <p><b>"Experimental - subject to change"</b></p>
     *
     * @return the numeric value of the specified PSVI property associated to a 
     * <code>DOMElement</code> or <code>DOMAttr</code>, or 0 if not available.
     */
    virtual int getNumericProperty(PSVIProperty prop) const;
    //@}

    //@{
    // -----------------------------------------------------------------------
    //  Setter methods
    // -----------------------------------------------------------------------

    /**
     * Set the value for a string PSVI property.
     *
     * <p><b>"Experimental - subject to change"</b></p>
     *
     */
    virtual void setStringProperty(PSVIProperty prop, const XMLCh* value);

    /**
     * Set the value for a numeric PSVI property.
     *
     * <p><b>"Experimental - subject to change"</b></p>
     *
     */
    virtual void setNumericProperty(PSVIProperty prop, int value);
    //@}
  
  private:
    int             fBitFields;
    const XMLCh*    fTypeName;
    const XMLCh*    fTypeNamespace;
    const XMLCh*    fMemberTypeName;
    const XMLCh*    fMemberTypeNamespace;
    const XMLCh*    fDefaultValue;
    const XMLCh*    fNormalizedValue;
    
    // -----------------------------------------------------------------------
    // Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    DOMTypeInfoImpl (const DOMTypeInfoImpl&);
    DOMTypeInfoImpl & operator = (const DOMTypeInfoImpl &);
};

XERCES_CPP_NAMESPACE_END

#endif

/**
 * End of file DOMTypeInfo.hpp
 */
