#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
# 
#      http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#

#
# $Id: sanityTest.pl 696141 2008-09-17 03:06:33Z dbertoni $
#


#  Author(s):   Mike Strosaker
#
#  Verify that a Xerces-C build is successful.
#  Command-Line Parameter:
#      The OS type ("win" or "unix")
#
#  Prerequisites:
#      Set the path to include the appropriate executables
#      and the library path to include the libraries

$os     = $ARGV[0];

# flush output
$| = 1;

if (lc($os) eq "win") {
    $pathsep = "\\";
}
else {
    $pathsep = "/";
}

chdir "samples".$pathsep."data";

#
#  Run the samples
#

#  Run SAXCount
system ("SAXCount");
system ("SAXCount -v=never personal.xml");
system ("SAXCount personal.xml");
system ("SAXCount -n -s personal-schema.xml");

#  Run SAXPrint
system ("SAXPrint");
system ("SAXPrint -v=never personal.xml");
system ("SAXPrint personal.xml");
system ("SAXPrint -n -s personal-schema.xml");

#  Run SAX2Count
system ("SAX2Count");
system ("SAX2Count -v=never personal.xml");
system ("SAX2Count personal.xml");
system ("SAX2Count -p personal-schema.xml");

#  Run SAX2Print
system ("SAX2Print");
system ("SAX2Print -v=never personal.xml");
system ("SAX2Print personal.xml");
system ("SAX2Print -p personal-schema.xml");
system ("SAX2Print personal.xsd");
system ("SAX2Print -sa personal.xsd");

#  Run MemParse
system ("MemParse");
system ("MemParse -v=never");

#  Run Redirect
system ("Redirect");
system ("Redirect personal.xml");

#  Run DOMCount
system ("DOMCount");
system ("DOMCount -v=never personal.xml");
system ("DOMCount personal.xml");
system ("DOMCount -n -s personal-schema.xml");

#  Run DOMPrint
system ("DOMPrint");
system ("DOMPrint -wfpp=on -wddc=off -v=never personal.xml");
system ("DOMPrint -wfpp=on -wddc=off personal.xml");
system ("DOMPrint -wfpp=on -wddc=on  personal.xml");
system ("DOMPrint -wfpp=on -wddc=off -n -s personal-schema.xml");
system ("DOMPrint -v=never -xpath=//name personal.xml");

#  Run StdInParse
system ("StdInParse < personal.xml");
system ("StdInParse -v=never < personal.xml");
system ("StdInParse -n -s < personal-schema.xml");

#  Run PParse
system ("PParse");
system ("PParse personal.xml");
system ("PParse -n -s personal-schema.xml");

#  Run EnumVal
system ("EnumVal");
system ("EnumVal personal.xml");

#  Run SEnumVal
system ("SEnumVal");
system ("SEnumVal personal-schema.xml");

#  Run CreateDOMDocument
system ("CreateDOMDocument");

#  Run SAXPrint.  This tests long element names.
print  ("\nTest of a long element name.\n");
system ("SAXPrint -v=never long.xml");
print  ("\n\n");

#
#  Run the test cases
#

#  Run DOMMemTest
print  ("DOMMemTest\n");
system ("DOMMemTest");

#  Run DOMTest
print  ("DOMTest\n");
system ("DOMTest");

#  Run RangeTest
print  ("RangeTest\n");
system ("RangeTest");

#  Run DOMTraversalTest
print  ("DOMTraversalTest\n");
system ("DOMTraversalTest");

#  Run XSerializerTest
system ("XSerializerTest");
system ("XSerializerTest -v=never  personal.xml");
system ("XSerializerTest -v=always personal.xml");
system ("XSerializerTest -v=never  personal-schema.xml");
system ("XSerializerTest -v=always personal-schema.xml");
system ("XSerializerTest -v=always -f personal-schema.xml");

#  Run XSValueTest
system ("XSValueTest");

#  Run InitTestTerm
system ("InitTermTest");
print  ("1");
system ("InitTermTest personal.xml");
print  ("2");
system ("InitTermTest -n -s personal-schema.xml");
print  ("3");
system ("InitTermTest -n -s -f personal-schema.xml");

#  Run ThreadTest
system ("ThreadTest");
print  ("1");
system ("ThreadTest -parser=sax -v=never -quiet -threads 10 -time 20 personal.xml");
print  ("2");
system ("ThreadTest -parser=dom -v=never -quiet -threads 10 -time 20 personal.xml");
print  ("3");
system ("ThreadTest -parser=sax2 -v=never -quiet -threads 10 -time 20 personal.xml");
print  ("4");
system ("ThreadTest -parser=sax -v=always -quiet -threads 10 -time 20 personal.xml");
print  ("5");
system ("ThreadTest -parser=dom -v=always -quiet -threads 10 -time 20 personal.xml");
print  ("6");
system ("ThreadTest -parser=sax2 -v=always -quiet -threads 10 -time 20 personal.xml");
print  ("7");
system ("ThreadTest -parser=sax -gc -v=always -quiet -threads 10 -time 20 personal.xml");
print  ("8");
system ("ThreadTest -parser=dom -gc -v=always -quiet -threads 10 -time 20 personal.xml");
print  ("9");
system ("ThreadTest -parser=sax2 -gc -v=always -quiet -threads 10 -time 20 personal.xml");
print  ("10");
system ("ThreadTest -parser=sax -n -s -f -v=always -quiet -threads 10 -time 20 personal-schema.xml");
print  ("11");
system ("ThreadTest -parser=dom -n -s -f -v=always -quiet -threads 10 -time 20 personal-schema.xml");
print  ("12");
system ("ThreadTest -parser=sax2 -n -s -f -v=always -quiet -threads 10 -time 20 personal-schema.xml");
print  ("13");
system ("ThreadTest -parser=sax -gc -n -s -f -v=always -quiet -threads 10 -time 20 personal-schema.xml");
print  ("14");
system ("ThreadTest -parser=dom -gc -n -s -f -v=always -quiet -threads 10 -time 20 personal-schema.xml");
print  ("15");
system ("ThreadTest -parser=sax2 -gc -n -s -f -v=always -quiet -threads 10 -time 20 personal-schema.xml");

#  Run MemHandlerTest
system ("MemHandlerTest");
system ("MemHandlerTest -v=always -n -r=2 personal.xml");
system ("MemHandlerTest -v=always -n -r=2 -s -f personal-schema.xml");

chdir "..".$pathsep."..";

chdir "tests".$pathsep."src".$pathsep."DOM".$pathsep."TypeInfo";
system ("DOMTypeInfoTest");

chdir "..".$pathsep."..";

