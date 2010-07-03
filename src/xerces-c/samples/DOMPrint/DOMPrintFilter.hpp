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
 * $Id: DOMPrintFilter.hpp 568078 2007-08-21 11:43:25Z amassari $
 * $Log$
 * Revision 1.5  2004/09/08 13:55:31  peiyongz
 * Apache License Version 2.0
 *
 * Revision 1.4  2002/11/05 21:46:19  tng
 * Explicit code using namespace in application.
 *
 * Revision 1.3  2002/06/04 14:22:51  peiyongz
 * Implement setter/getter from DOMWriterFilter
 *
 * Revision 1.2  2002/06/03 22:40:07  peiyongz
 * *** empty log message ***
 *
 * Revision 1.1  2002/05/29 13:33:32  peiyongz
 * DOM3 Save Interface: DOMWriter/DOMWriterFilter
 *
 */

//////////////////////////////////////////////////////////////////////
// DOMPrintFilter.hpp: a sample implementation of DOMWriterFilter.
//
//////////////////////////////////////////////////////////////////////

#ifndef DOMPrintFilter_HEADER_GUARD_
#define DOMPrintFilter_HEADER_GUARD_

#include <xercesc/dom/DOMWriterFilter.hpp>

XERCES_CPP_NAMESPACE_USE

class DOMPrintFilter : public DOMWriterFilter {
public:

    /** @name Constructors */
	DOMPrintFilter(unsigned long whatToShow = DOMNodeFilter::SHOW_ALL);
    //@{

    /** @name Destructors */
	~DOMPrintFilter(){};
    //@{

	/** @ interface from DOMWriterFilter */
	virtual short acceptNode(const DOMNode*) const;
    //@{

	virtual unsigned long getWhatToShow() const {return fWhatToShow;};

	virtual void          setWhatToShow(unsigned long toShow) {fWhatToShow = toShow;};

private:
	// unimplemented copy ctor and assignement operator
	DOMPrintFilter(const DOMPrintFilter&);
	DOMPrintFilter & operator = (const DOMPrintFilter&);

	unsigned long fWhatToShow;

};

#endif
