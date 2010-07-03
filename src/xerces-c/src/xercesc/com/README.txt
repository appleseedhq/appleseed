
COM Wrapper
-----------

xerces-com.dll needs to be registered before its first use.  This can be done using either through the References Dialog in Microsoft(r) Visual Basic (tm) or Microsoft(r) Excel or using REGSVR32.EXE,
provided with many Windows development environments.

To register using Microsoft(r) Visual Basic 6.0, select Project/References from the menu to show the References dialog, press the Browse dialog and then select xerces-com.dll.  In Microsoft(r) Excel,
select Tools/Macro/Visual Basic Editor to show the VBA editor and then select Tools/References for the references dialog.

To register using REGSVR32 from the command line, type REGSVR32 followed by the path name of xerces-com.dll.

After registration, the xerces-com will appear on the References dialog as "Xerces XML Parser".

xerces-com was designed to support the same interfaces as Microsoft XML 2.0.  For many Visual Basic applications, the only changes necessary are to add the reference to Xerces XML Parser, remove the
reference to Microsoft XML 2.0, and to change the line that creates the parser object.

Each of the following lines would create an instance of a Microsoft XML 2.0 parser:

Dim xmlparser as new MSXML.DOMDocument
Dim xmlparser as new DOMDocument
Set object = CreateObject("MSXML.DOMDocument")
Set object = CreateObject("MSXML2.DOMDocument")

The following lines would create an instance of a Xerces parser:

Dim xmlparser as new Xerces.DOMDocument
Dim xmlparser as new DOMDocument
Set object = CreateObject("Xerces.DOMDocument")
set object = CreateObject("Xerces.DOMDocument.2.3.0")

If you leave both MSXML and Xerces in the same project, you may need to disambiguate interfaces by prepending MSXML or Xerces.

For many applications, MSXML and Xerces will be interchangable.  However, there may be documents that MSXML rejects and Xerces accepts and vice versa and the DOM presented to the application may be
slightly different.  Specific examples include:

MSXML represents entity references as entity reference elements,
Xerces expands the entity references.

MSXML represents the XML declaration as a processing instruction node,
Xerces does not represent the XML declaration.

MSXML will suppress whitespace nodes regardless of validation setting when preserveWhitespace = false,
Xerces will only suppress whitespace when validation = true.
