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

push(@INC, "/home/xml4c/bin", "/home/xml4c/bin/perl/perl-RUN/opt/perl5/lib", "/Development2/cupert/bin/perl/perl-RUN/opt/perl5/lib", "/Development/cupert/usr/local/perl/perl-RUN/opt/perl5/lib");

require "getopt.pl";
require 5.0;

$|=1;   # Force a flush after every print

#
# Setup global variables
#
&Getopt('sopcxmntrb');
my $XERCESCROOT    = $opt_s;
my $targetdir      = $opt_o;
my $ICUROOT        = $ENV{'ICUROOT'};
my $ICUIsPresent   = (($opt_t =~ m/icu/i || $opt_m =~ m/icu/i) && length($ICUROOT) > 0);
my $ICUResourceDir = "$XERCESCROOT/src/xercesc/util/MsgLoaders/ICU/resources";
    
# Check for the environment variables and exit if error
if (!length($XERCESCROOT) || !length($targetdir) || (length($opt_h) > 0) ) {
    print ("Usage is: packageBinaries <options>\n");
    print ("  where options are:\n");
    print ("    -s <source_directory>\n");
    print ("    -o <target_directory>\n");
    print ("    -c <C compiler name> (e.g. gcc, cc, xlc_r, VC6, VC7, VC7.1, VC8, ecl or icl)\n");
    print ("    -x <C++ compiler name> (e.g. g++, CC, aCC, c++, xlC_r, cl, ecl, ecc, icl, VC6, VC7, VC7.1 or VC8)\n");
    print ("    -m <message loader> can be 'inmem' \(default\), 'icu' or 'iconv'\n");
    print ("    -n <net accessor> can be 'fileonly' or 'socket' \(default\)\n");
    print ("    -t <transcoder> can be 'icu' or 'native' \(default\)\n");
    print ("    -r <thread option> can be 'pthread' \(default\)or 'dce' (only used on HP-11)\n");
    print ("    -b <bitsToBuild> (accepts '64', '32')\n");
    print ("    -j suppress building of ICU (speeds up builds when debugging)\n");
    print ("    -h to get help on these commands\n\n");
    print ("Example: Under unix's\n");
    print ("    perl packageBinaries.pl -s \$HOME/xerces-c-src2_8");
    print (" -o \$HOME/xerces-c2_8-linux -c gcc -x g++ -m inmem -n fileonly -t native\n\n");
    print ("Example: Under Windows\n");
    print ("    perl packageBinaries.pl -s \\xerces-c-src2_8");
    print (" -o\\xerces-c2_5-win32 [-n fileonly] [-t icu]\n\n");
    print ("Note:\n");
    print ("    Under Windows, by default the XercesLib project files is\n");
    print ("    configured to use Win32 resource file based message loader,\n");
    print ("    WinSocket based net accessor and native Win32 based transcoder.\n");
    print ("    The two options mentioned in the example above are the only\n");
    print ("    options relevant under Windows on the command line for this script.\n");
    exit(-1);
}

if (($opt_t =~ m/icu/i || $opt_m =~ m/icu/i) && (length($ICUROOT) == 0)) {
    print "You have specified an ICU build but you have not defined your ICU install directory.\n";
    print "To build with ICU, you must set an environment variable called ICUROOT\n";
    print "Cannot proceed any further.\n";
    exit(-1);
}

# Check if the source directory exists or not
if (!(-e $XERCESCROOT)) {
    print ("The directory $XERCESCROOT does not exist. Cannot proceed any further.\n");
    exit(-1);
}

# Check if the target directory exists, exit if it does
if (-e $targetdir) {
    print ("Error: The target directory \'$targetdir\' already exists.\n");
    print ("       You must start with a clean directory to package your product.\n");
    exit(1);
}

#
# Find out the platform from 'uname -s'
#
open(PLATFORM, "uname -s|");
$platform = <PLATFORM>;
chomp($platform);
close (PLATFORM);

#Fix the backslashes on the Windows platform
$XERCESCROOT =~ s/\\/\//g;
$ICUROOT =~ s/\\/\//g;
$targetdir =~ s/\\/\//g;

print "Packaging binaries for \`" . $platform . "\` in " . $targetdir . " ...\n";   # "

#Construct the name of the zip file by extracting the last directory name
$zipfiles = $targetdir;
$zipfiles =~ s/.*(\/|\\)([\w-\.]*)$/$2/g;
$zipfiles = $zipfiles . "/*";

#
#   WINDOWS builds happen here, as long as they
#     aren't using gcc...
#
#  Tasks:
#
#    Preparation
#          Initialize Vars
#          Make the target directory and its main subdirectories
#          'FileOnly' NetAccessor
#
#   Build
#          ICU and/or Resource bundle library for ICUMsgLoader
#          Xerces-C libraries, samples and tests
#
#   Population
#          include
#          ICU and/or ICUMsgLoader
#          Xerces-C libraries, samples and tests
#          Documentation
#
if ($platform =~ m/Windows/  || ($platform =~ m/CYGWIN/ && !($opt_c =~ m/gcc/))) {
#
# Preparation Begin
#
#    Preparation::Initialize Vars
#
    my $PlatformName = 'Win32'; 
    my $DevStudioVer = "6.0";
    my $VCBuildDir     = "VC6";
    my $Transcoder     = 0;
    my $MsgLoader     = 0;

    my $ReleaseBuildDir  = undef;
    my $DebugBuildDir    = undef;
    my $ProjectDir          = undef;
                        
    if ($opt_b eq "64") {
        $PlatformName = 'Win64';
    }
      
    if ($opt_x eq "" || $opt_x =~ m/VC6/i )
    {
        $DevStudioVer   = "6.0";
        $VCBuildDir     = "VC6"; 
        $ProjectDir     = "$XERCESCROOT/Projects/Win32/$VCBuildDir/xerces-all";
    }
    elsif ($opt_x =~ m/VC7.1/i ) 
    {
        $DevStudioVer   = "7.1";
        $VCBuildDir     = "VC7.1"; 
        $ProjectDir     = "$XERCESCROOT/Projects/Win32/$VCBuildDir/xerces-all";
    }    
    elsif ($opt_x =~ m/VC7/i ) 
    {
        $DevStudioVer   = "7.0";
        $VCBuildDir     = "VC7"; 
        $ProjectDir     = "$XERCESCROOT/Projects/Win32/$VCBuildDir/xerces-all";
    }
 	elsif ($opt_x =~ m/VC8/i ) 
    {
        $DevStudioVer   = "8.0";
        $VCBuildDir     = "VC8"; 
        $ProjectDir     = "$XERCESCROOT/Projects/Win32/$VCBuildDir/xerces-all";
    }        
    elsif ($opt_x =~ m/ecl/i || $opt_x =~ m/icl/i )
    {
        $DevStudioVer   = "6.1";
        $VCBuildDir     = "VC6"; 
        $PlatformName   = "Win64";
        $ProjectDir     = "$XERCESCROOT/Projects/Win32/$VCBuildDir/xerces-all/all";
    }
    else
    {
        print ("Error: Invalid compilers used \n");
        print ("-x <C++ compiler name> VC6, VC7, VC7.1, VC8, ecl and icl \n");        
        exit(1);            	    	
    }

    $ReleaseBuildDir = "$XERCESCROOT/Build/$PlatformName/$VCBuildDir/Release";
    $DebugBuildDir   = "$XERCESCROOT/Build/$PlatformName/$VCBuildDir/Debug";    
        
    if ($opt_t =~ m/icu/i ) {
        $Transcoder = 1;
    }

    if ($opt_m =~ m/icu/i) {
        $MsgLoader = 1;
    }
                
    print "PlatformName    =$PlatformName\n";
    print "DevStudioVer    =$DevStudioVer\n";
    print "VCBuildDir        =$VCBuildDir\n";
    print "ReleaseBuildDir =$ReleaseBuildDir\n";
    print "DebugBuildDir   =$DebugBuildDir\n";
    print "ProjectDir          =$ProjectDir\n";
    print "Transcoder        =$Transcoder\n";
    print "MsgLoader        =$MsgLoader\n";
        
    if (-e "$targetdir.zip") {
        print ("Deleting old target file \'$targetdir.zip\' \n");
        unlink("$targetdir.zip");
    }
            
    #
    # Preparation::Make the target directory and its main subdirectories
    #
    createCommonDir();

    # windows specifics
    psystem ("mkdir $targetdir/samples/Projects");
    psystem ("mkdir $targetdir/samples/Projects/Win32");
    psystem ("mkdir $targetdir/samples/Projects/Win32/$VCBuildDir");

    #
    # Preparation:: 'FileOnly' NetAccessor 
    #                     has been specified, then the project files have to be changed.
    # 
    if ($opt_n =~ m/fileonly/i) {
        if ($DevStudioVer eq "6.0") { 
            changeWindowsProjectForFileOnlyNA("$XERCESCROOT/Projects/Win32/VC6/xerces-all/XercesLib/XercesLib.dsp");
        } elsif ($DevStudioVer eq "7.0") {
            changeWindowsProjectForFileOnlyNA_VC7("$XERCESCROOT/Projects/Win32/VC7/xerces-all/XercesLib/XercesLib.vcproj");
        } elsif ($DevStudioVer eq "7.1") {
            changeWindowsProjectForFileOnlyNA_VC7("$XERCESCROOT/Projects/Win32/VC7.1/xerces-all/XercesLib/XercesLib.vcproj");        
        } elsif ($DevStudioVer eq "8.0") {
            changeWindowsProjectForFileOnlyNA_VC7("$XERCESCROOT/Projects/Win32/VC8/xerces-all/XercesLib/XercesLib.vcproj");
        }                
        #else: for now we do not build FO with ecl
    }

#
# Build Begin
#
#        Build::ICU and/or Resource bundle library for ICUMsgLoader
#
    if ($ICUIsPresent) {

        # Make the icu dlls
        if (!(length($opt_j) > 0)) {  
            #Clean up all the dependency files, causes problems for nmake
            print ("Building ICU from $ICUROOT ...\n");
            pchdir ("$ICUROOT");
            psystem ("del /s /f *.dep *.ncb *.plg *.opt");

            # we ship both debug and release dlls
            if ($DevStudioVer eq "6.0") {
                pchdir ("$ICUROOT/source/allinone");
                if ($PlatformName eq "Win64")
                {
                    psystem("msdev allinone.dsw /MAKE \"all - $PlatformName Release\" /USEENV /REBUILD /OUT buildlog_release.txt");
                    psystem("msdev allinone.dsw /MAKE \"all - $PlatformName Debug\" /USEENV /REBUILD /OUT buildlog_debug.txt");
                }
                else
                {
                    psystem("msdev allinone.dsw /MAKE \"all - $PlatformName Release\" /REBUILD /OUT buildlog_release.txt");            	
                    psystem("msdev allinone.dsw /MAKE \"all - $PlatformName Debug\" /REBUILD /OUT buildlog_debug.txt");            	
                }
                psystem("type buildlog_release.txt");
                psystem("type buildlog_debug.txt");
            } elsif ($DevStudioVer eq "7.0") {
                pchdir ("$ICUROOT/source/allinone");

                psystem("devenv /rebuild Release /out buildlog_release.txt /project all allinone.sln");
                psystem("type buildlog_release.txt");

                psystem("devenv /rebuild debug /out buildlog_debug.txt /project all allinone.sln");
                psystem("type buildlog_debug.txt");
            } elsif ($DevStudioVer eq "7.1") {
            	# ICU only has allinone.sln for VC7.0
            	# So the build with ICU on VC7.1 may fail until the VC7.1 version is available            	
                pchdir ("$ICUROOT/source/allinone");
                
                psystem("devenv.com allinone.sln /rebuild Release /out buildlog_release.txt");
                psystem("type buildlog_release.txt");

				psystem("devenv.com allinone.sln /rebuild debug /out buildlog_debug.txt");                
                psystem("type buildlog_debug.txt");      
            } elsif ($DevStudioVer eq "8.0") {
            	# ICU only has allinone.sln for VC7.0
            	# So the build with ICU on VC8.0 may fail until the VC8.0 version is available            	
                pchdir ("$ICUROOT/source/allinone");
                
                psystem("devenv.com allinone.sln /rebuild Release /out buildlog_release.txt");
                psystem("type buildlog_release.txt");

				psystem("devenv.com allinone.sln /rebuild debug /out buildlog_debug.txt");                
                psystem("type buildlog_debug.txt");                                          
            } else { #"6.1"
                pchdir ("$ICUROOT/source/allinone/all");            	
 	            #ship release dlls only
                psystem("nmake -f all_win64_release.mak \"CFG=all - $PlatformName Release\" CPP=$opt_x.exe >buildlog_release.txt 2>&1");
                psystem("type buildlog_release.txt");
                
                #debug is disabled
                #psystem("nmake -f all_win64_debug.mak \"CFG=all - $PlatformName Debug\" CPP=$opt_x.exe >buildlog_debug.txt 2>&1");
                #psystem("type buildlog_debug.txt");
           }

        } #$opt_j

        #
        # Build resource bundle library for ICUMsgLoader
        # this can only be built when ICUIsPresent
        if ($opt_m =~ m/icu/i) {
            pchdir ("$ICUResourceDir");
            # clean up intermediate files to make it rebuildable
            psystem ("del /s /f *.DLL *.dll *.res *.DAT *.lib *.obj");            
            psystem( "nmake /f resources.mak > buildlog.txt 2>&1 ");
            system("type buildlog.txt");
            # to follow 2 digits convention
            psystem("ren XercesMessages2_8_0.DLL XercesMessages2_8.DLL");
            psystem("ren XercesMessages2_8_0.lib XercesMessages2_8.lib");
        }

        #
        # Adjust project file
        #
        if ($DevStudioVer eq "6.0") {
            change_windows_project_for_ICU("$XERCESCROOT/Projects/Win32/VC6/xerces-all/XercesLib/XercesLib.dsp", $Transcoder , $MsgLoader);
        } elsif ($DevStudioVer eq "7.0") {
            change_windows_project_for_ICU_VC7("$XERCESCROOT/Projects/Win32/VC7/xerces-all/XercesLib/XercesLib.vcproj", $Transcoder , $MsgLoader);
        } elsif ($DevStudioVer eq "7.1") {
            change_windows_project_for_ICU_VC7("$XERCESCROOT/Projects/Win32/VC7.1/xerces-all/XercesLib/XercesLib.vcproj", $Transcoder , $MsgLoader);            
        } elsif ($DevStudioVer eq "8.0") {
            change_windows_project_for_ICU_VC7("$XERCESCROOT/Projects/Win32/VC8/xerces-all/XercesLib/XercesLib.vcproj", $Transcoder , $MsgLoader);                        
        } else { # "6.1"
            change_windows_makefile_for_ICU("$XERCESCROOT/Projects/Win32/VC6/xerces-all/XercesLib/XercesLib.mak", $Transcoder, $MsgLoader);
        }
                
    } #$ICUIsPresent

    #
    # Build::Make Xerces-C libraries, samples and tests
    #        

    # Clean up all the dependency files, causes problems for nmake
    # Also clean up all MSVC-generated project files that just cache the IDE state
    pchdir ("$XERCESCROOT");
    psystem ("del /s /f *.dep *.ncb *.plg *.opt");

    #
    #                             release                       debug 
    # ===========================================================================
    #                  vc6      vc7     ecl        vc6       vc7      ecl
    # ===========================================================================
    # xercesc          yes      yes     yes        yes       yes      no
    # depdom           yes      yes     yes        yes       yes      no
    # samples          yes      yes     yes        no        no       no
    # tests            yes      yes     yes        no        no       no
    #
    pchdir ("$ProjectDir");
    
    if ($DevStudioVer eq "6.0") {
        if ($PlatformName eq "Win64") { # /USEENV
            psystem("msdev xerces-all.dsw /MAKE \"all - $PlatformName Release\" /USEENV /REBUILD /OUT buildlog_release.txt");
            psystem("msdev xerces-all.dsw /MAKE \"XercesLib - $PlatformName Debug\" /USEENV /REBUILD /OUT buildlog_debug.txt");
            psystem("msdev xerces-all.dsw /MAKE \"XercesDeprecatedDOMLib - $PlatformName Debug\" /USEENV /REBUILD /OUT buildlog_depdom_debug.txt");
        }
        else {
            psystem("msdev xerces-all.dsw /MAKE \"all - $PlatformName Release\" /REBUILD /OUT buildlog_release.txt");
            psystem("msdev xerces-all.dsw /MAKE \"XercesLib - $PlatformName Debug\" /REBUILD /OUT buildlog_debug.txt");
            psystem("msdev xerces-all.dsw /MAKE \"XercesDeprecatedDOMLib - $PlatformName Debug\" /REBUILD /OUT buildlog_depdom_debug.txt");
        }	
    } elsif ($DevStudioVer eq "7.0") {
        psystem("devenv /rebuild Release /out buildlog_release.txt /project all xerces-all.sln");
        psystem("devenv /rebuild debug /out buildlog_debug.txt /project XercesLib xerces-all.sln");        
        psystem("devenv /rebuild debug /out buildlog_depdom_debug.txt /project XercesDeprecatedDOMLib xerces-all.sln");
    } elsif ($DevStudioVer eq "7.1") {
        psystem("devenv /rebuild Release /out buildlog_release.txt /project all xerces-all.sln");
        psystem("devenv /rebuild debug /out buildlog_debug.txt /project XercesLib xerces-all.sln");        
        psystem("devenv /rebuild debug /out buildlog_depdom_debug.txt /project XercesDeprecatedDOMLib xerces-all.sln");        
        psystem("devenv /rebuild debug /out buildlog_depdom_debug.txt /project XercesDeprecatedDOMLib xerces-all.sln");        
    } elsif ($DevStudioVer eq "8.0") {
        psystem("devenv /rebuild Release /out buildlog_release.txt /project all xerces-all.sln");
        psystem("devenv /rebuild debug /out buildlog_debug.txt /project XercesLib xerces-all.sln");        
        psystem("devenv /rebuild debug /out buildlog_depdom_debug.txt /project XercesDeprecatedDOMLib xerces-all.sln");        
        
    } else { # "6.1"
        psystem( "nmake -f all.mak \"CFG=all - $PlatformName Release\" CPP=$opt_x.exe >buildlog_release.txt 2>&1");
    }

    system("type buildlog_release.txt");
    system("type buildlog_debug.txt");
    system("type buildlog_depdom_debug.txt");


#
# Population Begin
#
#    Population::include
#      
    pchdir ($targetdir);
    print "\nBuild is being copied from \'" . $ReleaseBuildDir . "\'";
    
    populateInclude();

#
#    Population::ICU and/or ICUMsgLoader    
#
    if ($ICUIsPresent) {    	
    	
        print ("\n\nCopying icu outputs ...\n");        	
        # Copy the ICU dlls and libs
        psystem("cp -fv $ICUROOT/bin/icuuc32.dll $targetdir/bin");
        psystem("cp -fv $ICUROOT/bin/icuuc32d.dll $targetdir/bin");

        # it seems icudt32*.DLL is generated (upper case dll)
        # but just case, try lower case .dll as well
        psystem("cp -fv $ICUROOT/bin/icudt32*.DLL $targetdir/bin");
        psystem("cp -fv $ICUROOT/bin/icudt32*.dll $targetdir/bin");

        psystem("cp -fv $ICUROOT/lib/icuuc.lib $targetdir/lib");
        psystem("cp -fv $ICUROOT/lib/icuucd.lib $targetdir/lib");

        psystem("cp -Rfv $ICUROOT/include/* $targetdir/include");
            
        # Copy the Resouce Bundle for ICUMsgLoader
        if ( $opt_m =~ m/icu/i) {           
            pchdir ("$ICUResourceDir");    	
            psystem("cp -fv XercesMessages*.DLL $ReleaseBuildDir");
            psystem("cp -fv XercesMessages*.lib    $ReleaseBuildDir");

            psystem("cp -fv XercesMessages*.res   $targetdir/msg");
            psystem("cp -fv XercesMessages*.DLL $targetdir/bin");
            psystem("cp -fv XercesMessages*.lib    $targetdir/lib");                               
        }        	

    } #ICUIsPresent

    #
    # Population::Xerces-c
    # 
    print ("\n\nCopying Xerces-c outputs ...\n");            
    psystem("cp -fv $ReleaseBuildDir/*.dll               $targetdir/bin");
    psystem("cp -fv $ReleaseBuildDir/*.exe               $targetdir/bin");        
    psystem("cp -fv $ReleaseBuildDir/xerces-c_*.lib      $targetdir/lib");
    psystem("cp -fv $ReleaseBuildDir/xerces-depdom_*.lib $targetdir/lib");

    psystem("cp -fv $DebugBuildDir/*.dll                 $targetdir/bin");
    psystem("cp -fv $DebugBuildDir/xerces-c_*.lib        $targetdir/lib");
    psystem("cp -fv $DebugBuildDir/xerces-depdom_*.lib   $targetdir/lib");
               
    # Populate the etc output directory like config.status and the map file
    print ("\n\nCopying misc output to etc ...\n");
    psystem("cp -fv $XERCESCROOT/Build/Win32/$VCBuildDir/Release/obj/*.map $targetdir/etc");

    # Populate the samples directory
    populateSamples();

    # windows specifics: sample projects
    psystem("cp -Rfv $XERCESCROOT/samples/Projects/Win32/$VCBuildDir/* $targetdir/samples/Projects/Win32/$VCBuildDir");
   
    # Populate the scripts and docs directory
    populateMisc();
    
    # Now package it all up using ZIP
    pchdir ("$targetdir/..");
    print ("\n\nZIPping up all files ...\n");
    $zipname = $targetdir . ".zip";
    psystem ("zip -r $zipname $zipfiles");
}
#
#     End of Windows Builds.

#
#  UNIX builds happen here ...
#
#  Tasks:
#
#    Preparation
#          Initialize Vars
#          Make the target directory and its main subdirectories
#
#   Build
#          ICU and/or Resource bundle library for ICUMsgLoader
#          Xerces-C libraries, samples and tests
#
#   Population
#          include
#          ICU and/or ICUMsgLoader
#          Xerces-C libraries, samples and tests
#          Documentation
#
if ( ($platform =~ m/AIX/i)      || 
     ($platform =~ m/HP-UX/i) || 
     ($platform =~ m/BeOS/i)   ||
     ($platform =~ m/SunOS/i) || 
     ($platform =~ m/Linux/i)    || 
     ($platform =~ m/ptx/i)        ||
     ($platform =~ m/Darwin/i)  || 
     ($platform =~ m/CYGWIN/ && ($opt_c =~ m/gcc/))) {
#
# Preparation Begin
#
#    Population::Initialize Vars
#

    # Echo the current PATH to see what compiler it picks up
    psystem ("echo PATH=$ENV{'PATH'}");

    # Set defaults for platform-specific options.
    if ($platform =~ m/AIX/i) {
        $platform = "aix";
        if ($opt_c eq "") {$opt_c = "xlc_r"; }
        if ($opt_x eq "") {$opt_x = "xlC_r"; }

        if ($opt_x eq "xlC_rv5compat") {
            $icu_cxxflags = '"-w -O2 -qmaxmem=-1 -qnamemangling=v5"';
            $icu_cflags   = '"-w -O2 -qmaxmem=-1 -qnamemangling=v5"';        	
        } else {
            $icu_cxxflags = '"-w -O2 -qmaxmem=-1"';
            $icu_cflags = '"-w -O2 -qmaxmem=-1"';
        }

        if ($opt_m =~ m/icu/i) {
        	$ENV{'LIBPATH'}="$ICUROOT/lib:$XERCESCROOT/lib:$ENV{'LIBPATH'}";
        	$ENV{'PATH'}="$ICUROOT/bin:$ENV{'PATH'}";        	
        }
        psystem ("echo LIBPATH=$ENV{'LIBPATH'}");
    }

    # Mac OS X
    if ($platform =~ m/Darwin/i) {
        $platform = "macosx";
		
        # Set option defaults
        if ($opt_c eq "")	{$opt_c = 'cc'; }
        if ($opt_x eq "")	{$opt_x = 'g++'; }
        if ($opt_n eq "") 	{$opt_n = 'native'; }		# native net accessor
        if ($opt_t eq "")	{$opt_t = 'native'; }		# native transcoder
		
        # Code for mac os specific tools
        if ($TAR eq "") {
            $TAR = 'gnutar'; 
        }

        if ($MAKE eq "") {
            $MAKE = 'make'; 
        }
    } # Mac OS
    
    if ($platform eq 'HP-UX') {
        # Find out the operating system version from 'uname -r'
        open(OSVERSION, "uname -r|");
        $osversion = <OSVERSION>;
        chomp($osversion);
        close (OSVERSION);
        $platform = 'hp-11' if ($osversion =~ m/11\./);
        $platform = 'hp-10' if ($osversion =~ m/10\./);

        if ($opt_c eq "") {$opt_c = "cc"; }
        if ($opt_x eq "") {
            $opt_x = "CC";
            if ($platform eq "hp-11") {
                $opt_x = "aCC";
            }
        }
        if ($opt_m eq "") {
            $opt_m = "inmem";
        }

        $icu_cxxflags = '"-w +O2 +Ofltacc"';
        $icu_cflags = '"-w +O2 +Ofltacc"';

        if ($opt_m =~ m/icu/i) {
        	$ENV{'SHLIB_PATH'}="$ICUROOT/lib:$XERCESCROOT/lib:$ENV{'SHLIB_PATH'}";
        	$ENV{'PATH'}="$ICUROOT/bin:$ENV{'PATH'}";        	        	
        }

        psystem ("echo SHLIB_PATH=$ENV{'SHLIB_PATH'}");
    }# HP-UX
    
    if ($platform =~ m/BeOS/i) {
        $platform = "beos";
        if ($opt_c eq "") {$opt_c = "gcc";}
        if ($opt_x eq "") {$opt_x = "g++";}
        $icu_cxxflags = '"-w -O"';
        $icu_cflags = '"-w -O"';
        psystem ("echo LIBRARY_PATH=$ENV{'LIBRARY_PATH'}");
    }# BeOS
    
    if ($platform =~ m/Linux/i) {
        $platform = "linux";
        if ($opt_c eq "") {$opt_c = "gcc";}
        if ($opt_x eq "") {$opt_x = "g++";}
        
        if ($opt_x eq "icc"){
            $icu_cxxflags = '"-w -O0"';
            $icu_cflags = '"-w -O0"';        	
        } else {
            $icu_cxxflags = '"-w -O"';
            $icu_cflags = '"-w -O"';
        }

        if ($opt_m =~ m/icu/i) {
        	$ENV{'LD_LIBRARY_PATH'}="$ICUROOT/lib:$XERCESCROOT/lib:$ENV{'LD_LIBRARY_PATH'}";
        	$ENV{'PATH'}="$ICUROOT/bin:$ENV{'PATH'}";        	        	
        }

        psystem ("echo LD_LIBRARY_PATH=$ENV{'LD_LIBRARY_PATH'}");
    } # Linux

    if ($platform =~ m/SunOS/i) {
        $platform = "solaris";
        if ($opXt_c eq "") {$opt_c = "cc";}
        if ($opt_x eq "") {$opt_x = "CC";}

        if ($opt_m =~ m/icu/i) {
        	$ENV{'LD_LIBRARY_PATH'}="$ICUROOT/lib:$XERCESCROOT/lib:$ENV{'LD_LIBRARY_PATH'}";
        	$ENV{'PATH'}="$ICUROOT/bin:$ENV{'PATH'}";        	        	
        }
        
        $icu_cxxflags = '"-w -O3"';
        $icu_cflags = '"-w -xO3"'; 

        psystem ("echo LD_LIBRARY_PATH=$ENV{'LD_LIBRARY_PATH'}");
    } # SunOS

    if ($platform =~ m/ptx/i) {
        # Check if the patches have been applied or not
        $platform = "ptx";
        if (!(-d "$XERCESCROOT/src/xercesc/util/Platforms/PTX")) {
            print ("Error: Could not locate PTX-specific XML4C directory.\n");
            print ("    The PTX-specific patches must be applied to both XML4C and ICU before a build can succeed.\n");
            exit(-1);
        }
        # Generally speaking, ICU must be built, before XML4C can be built, for ptx.
        # If this case causes problems, we can revisit it in the future. Right now,
        # we fail only if ICUROOT is defined but mh-ptx is not present.
        if ($ICUIsPresent) {
            if (!(-e "$ICUROOT/source/config/mh-ptx")) {
                print ("Error: Could not locate PTX-specific ICU files.\n");
                print ("    The PTX-specific patches must be applied to both XML4C and ICU before a build can succeed.\n");
                exit(-1);
            }
        }
        $icu_cxxflags = '"-w -0"';
        $icu_cflags = '"-w -0"';
        
        # XMLINSTALL is a ptx-port-specific variable used for manipulating where the files are installed.
        if (!length($ENV{'XMLINSTALL'})) {
            print ("XMLINSTALL has not been explicitly defined. Setting it to \'$targetdir\'.\n");
            $ENV{'XMLINSTALL'} = $targetdir;
        }
        $XMLINSTALL = $ENV{'XMLINSTALL'};
    } #ptx
    
    if(($platform =~ m/CYGWIN/) && ($opt_c =~ m/gcc/)) {
        $MAKE = "make";
        $platform = "CYGWIN";
    }

    # Set defaults for platform-independent options.
    if ($opt_m eq "")	{$opt_m = "inmem"; }   # In memory  message loader.
    if ($opt_n eq "")	{$opt_n = "socket"; }  # Socket based net accessor.
    if ($opt_t eq "")	{$opt_t = "native"; }  # Native transcoding service.
    if ($opt_b eq "")	{$opt_b = "32"; }      # bitstobuild.
	
    # Set defaults for platform tools
    if ($TAR eq "") { 
        $TAR = 'tar'; 
    }
        
    if ($MAKE eq "")	{ 
        $MAKE = 'gmake'; 
    }

    # Check if the target directories already exist or not
    if (-e $targetdir.".tar") {
        print ("Error: The target file \'$targetdir.tar\' already exists.\n");
        print ("       You must delete the file \'$targetdir.tar\' to package your product.\n");
        exit(1);
    }

    if (-e $srctargetdir.".tar") {
        print ("Error: The target file \'$srctargetdir.tar\' already exists.\n");
        print ("       You must delete the file \'$srctargetdir.tar\' to package your product.\n");
        exit(1);
    }

    #
    # Preparation::Make the target directory and its main subdirectories    
    #
    createCommonDir();
  
#
# Build Begin
#
#        Build::ICU and/or Resource bundle library for ICUMsgLoader
#    
    if ($ICUIsPresent ) {

        # Make the icu dlls
        if (!(length($opt_j) > 0)) {      	
            print ("Building ICU from $ICUROOT ...\n");

            # First make the ICU files executable
            pchdir ("$ICUROOT/source");
            psystem ("chmod +x configure config.*");
            psystem ("chmod +x install-sh");
            $ENV{'ICU_DATA'} = "$ICUROOT/data";

            if ($platform =~ m/ptx/i) {
                psystem ("chmod +x runConfigureICU");
                psystem ("sh ./runConfigureICU PTX");
            }
            elsif ($platform eq 'hp-11') {

                my $cXX = $opt_x;                 
                if ($opt_x eq "aCC05") {
                    $cXX = "aCC";
                }
                                
                if ($opt_b eq "32") {
                    psystem ("CC=$opt_c CXX=$cXX CXXFLAGS=$icu_cxxflags CFLAGS=$icu_cflags sh ./configure --prefix=$ICUROOT --disable-64bit-libs");
                }
                else {
                    psystem ("CC=$opt_c CXX=$cXX CXXFLAGS=$icu_cxxflags CFLAGS=$icu_cflags sh ./configure --prefix=$ICUROOT");
                }         
           }elsif ($platform eq 'aix') {

                my $cXX = $opt_x;                 
                if ($opt_x eq "xlC_rv5compat") {
                    $cXX = "xlC_r";
                }

                if ($opt_b eq "32") {
                    psystem ("CC=$opt_c CXX=$cXX CXXFLAGS=$icu_cxxflags CFLAGS=$icu_cflags sh ./configure --prefix=$ICUROOT --disable-64bit-libs");
                }
                else {
                    psystem ("CC=$opt_c CXX=$cXX CXXFLAGS=$icu_cxxflags CFLAGS=$icu_cflags sh ./configure --prefix=$ICUROOT");
                }
           }elsif ($platform eq 'solaris') {                       
                if ($opt_b eq "32") {               	
                    psystem ("CC=$opt_c CXX=$opt_x CXXFLAGS=$icu_cxxflags CFLAGS=$icu_cflags sh ./configure --prefix=$ICUROOT --disable-64bit-libs");
                }
                else {
                    $icu_cxxflags = '"-w -O3 -xarch=v9"';
                    $icu_cflags = '"-w -xO3 -xarch=v9"';                 	
                    psystem ("CC=$opt_c CXX=$opt_x CXXFLAGS=$icu_cxxflags CFLAGS=$icu_cflags sh ./configure --prefix=$ICUROOT");
                }                                         
            } else {
            # set the 32 bit or 64 bit
                if ($opt_b eq "32") {
                    psystem ("CC=$opt_c CXX=$opt_x CXXFLAGS=$icu_cxxflags CFLAGS=$icu_cflags sh ./configure --prefix=$ICUROOT --disable-64bit-libs");
                }
                else {
                    psystem ("CC=$opt_c CXX=$opt_x CXXFLAGS=$icu_cxxflags CFLAGS=$icu_cflags sh ./configure --prefix=$ICUROOT");
                }
            }
        
            psystem ("$MAKE clean");                  # Clean up the build, may want to comment this line out!
            psystem ("rm -f $ICUROOT/data/*.o"); # make clean is not enough
            psystem ("rm -f $ICUROOT/data/*.c"); # same for .c files
            psystem ("$MAKE");                           # This will take a long time!
            psystem ("$MAKE install");                  # Make this separate since this breaks on Solaris
       
        } #opt_j

        #
        # resource bundle library for ICUMsgLoader is built by Makefile
        # clean up intermediate files to make it rebuildable  
        # 
        if ($opt_m =~ m/icu/i) {
            pchdir ("$ICUResourceDir");
            psystem ("rm -f *.o *.res *.c *.lst *dll.mak");            
        }
            
    }  #$ICUIsPresent

    # For ptx, ICUROOT must now be set to XMLINSTALL for further work.
    if ($platform =~ m/ptx/i) {
        $ENV{'ICUROOT'} = $ENV{'XMLINSTALL'};
    }

    #
    # Build::Make Xerces-C libraries, samples and tests
    #        
    print("\n\nBuild the xerces-c library ...\n");
    pchdir ("$XERCESCROOT/src/xercesc");
    psystem ("chmod +x run* con* install-sh");
               
    if (length($opt_r) > 0) {
        psystem ("./runConfigure -p$platform -c$opt_c -x$opt_x -m$opt_m -n$opt_n -t$opt_t -r$opt_r -b$opt_b");
    } else {
        psystem ("./runConfigure -p$platform -c$opt_c -x$opt_x -m$opt_m -n$opt_n -t$opt_t -b$opt_b");
    }

    psystem ("$MAKE clean");     # May want to comment this line out to speed up
    psystem ("$MAKE");
                
    #                
    #   Move ICU libs into lib dir, so samples will link.  This matches the structure of
    #   the eventual binary packaging, even though we are doing it in the build directory.
    #
    if ($ICUIsPresent) {
        copyICUOnUNIX("$XERCESCROOT/lib");
        # src/xercesc/util/MsgLoader/ICU/resources/Makefile has built and 
        # copied the message library to $XERCESCROOT/lib, we need copy over here.
    }# $ICUIsPresent

    # build the samples
    print("\n\nBuild the samples ...\n");
    pchdir ("$XERCESCROOT/samples");
    psystem ("chmod +x run* con* install-sh");
    psystem ("./runConfigure -p$platform -c$opt_c -x$opt_x -b$opt_b");
    psystem ("$MAKE clean");     # May want to comment this line out to speed up
    psystem ("$MAKE");

    # build the tests
    print("\n\nBuild the tests ...\n");
    pchdir ("$XERCESCROOT/tests");
    psystem ("chmod +x run* con* install-sh");
    psystem ("./runConfigure -p$platform -c$opt_c -x$opt_x -b$opt_b");
    psystem ("$MAKE clean");     # May want to comment this line out to speed up
    psystem ("$MAKE");

    pchdir ($targetdir);

#
# Population Begin
#
#    Population::include
#
    populateInclude();   

    if ($ICUIsPresent) {
        print "\nICU files are being copied from \'$ICUROOT\'";
        psystem("cp -Rf $ICUROOT/include/* $targetdir/include");
    }

#
#    Population::ICU and/or ICUMsgLoader    
#

    #
    # Create symbolic link for those ICU libraries
    #
    if ($ICUIsPresent) {
        # ICU
        copyICUOnUNIX("$targetdir/lib");
                     
        # Copy the Resouce Bundle for ICUMsgLoader
        if ( $opt_m =~ m/icu/i) {
            print ("\n\nCopying ICU message bundles ...\n");        	
            psystem("cp -f $XERCESCROOT/msg/XercesMessages*.res $targetdir/msg");
           
            psystem("cp -f $XERCESCROOT/lib/libXercesMessages28.0.so .");
            psystem("find . -name 'libXercesMessages28.0.so' -exec ln -s {} libXercesMessages28.so \\;");
            psystem("find . -name 'libXercesMessages28.so'   -exec ln -s {} libXercesMessages.so \\;");

            psystem("cp -f $XERCESCROOT/lib/libXercesMessages.so.28.0 .");
            psystem("find . -name 'libXercesMessages.so.28.0' -exec ln -s {} libXercesMessages.so.28 \\;");
            psystem("find . -name 'libXercesMessages.so.28'   -exec ln -s {} libXercesMessages.so \\;");

            psystem("cp -f $XERCESCROOT/lib/libXercesMessages.sl.28.0 .");
            psystem("find . -name 'libXercesMessages.sl.28.0' -exec ln -s {} libXercesMessages.sl.28 \\;");
            psystem("find . -name 'libXercesMessages.sl.28'   -exec ln -s {} libXercesMessages.sl \\;");

            psystem("cp -f $XERCESCROOT/lib/libXercesMessages28.0.a .");
            psystem("find . -name 'libXercesMessages28.0.a'   -exec ln -s {} libXercesMessages28.a \\;");
            psystem("find . -name 'libXercesMessages28.a'     -exec ln -s {} libXercesMessages.a \\;");
                               
        }        	

    }
  
    #
    # Population::Xerces-c
    # 
        
    print ("\n\nCopying binary outputs ...\n");
    psystem("cp -Rf $XERCESCROOT/bin/* $targetdir/bin");
    psystem("rm -rf $targetdir/bin/obj");

    # Populate the library output directory
    print ("\n\nCopying library outputs ...\n");
    pchdir ("$targetdir/lib");
    psystem("rm -f libxerces-c* ");

    if ((-e "$XERCESCROOT/lib/libxerces-c.so.28.0" )) {
        psystem("cp -f $XERCESCROOT/lib/libxerces-c.so.28.0 .");
        psystem("ln -s libxerces-c.so.28.0 libxerces-c.so.28 ");
        psystem("ln -s libxerces-c.so.28   libxerces-c.so    ");
    }

    if ((-e "$XERCESCROOT/lib/libxerces-depdom.so.28.0" )) {
        psystem("cp -f $XERCESCROOT/lib/libxerces-depdom.so.28.0 .");
        psystem("ln -s libxerces-depdom.so.28.0 libxerces-depdom.so.28 ");
        psystem("ln -s libxerces-depdom.so.28   libxerces-depdom.so    ");
    }

    if ((-e "$XERCESCROOT/lib/libxerces-c.sl.28.0" )) {
        psystem("cp -f $XERCESCROOT/lib/libxerces-c.sl.28.0 .");
        psystem("ln -s libxerces-c.sl.28.0 libxerces-c.sl.28 ");
        psystem("ln -s libxerces-c.sl.28   libxerces-c.sl    ");
    }

    if ((-e "$XERCESCROOT/lib/libxerces-depdom.sl.28.0" )) {
        psystem("cp -f $XERCESCROOT/lib/libxerces-depdom.sl.28.0 .");
        psystem("ln -s libxerces-depdom.sl.28.0 libxerces-depdom.sl.28 ");
        psystem("ln -s libxerces-depdom.sl.28   libxerces-depdom.sl    ");
    }
                
    if ((-e "$XERCESCROOT/lib/libxerces-c28.0.so" )) {
        psystem("cp -f $XERCESCROOT/lib/libxerces-c28.0.so .");
        psystem("ln -s libxerces-c28.0.so libxerces-c28.so  ");
        psystem("ln -s libxerces-c28.so   libxerces-c.so    ");
    }

    if ((-e "$XERCESCROOT/lib/libxerces-depdom28.0.so" )) {
        psystem("cp -f $XERCESCROOT/lib/libxerces-depdom28.0.so .");
        psystem("ln -s libxerces-depdom28.0.so libxerces-depdom28.so  ");
        psystem("ln -s libxerces-depdom28.so   libxerces-depdom.so    ");
    }
    
    if ((-e "$XERCESCROOT/lib/libxerces-c28.0.a" )) {
        psystem("cp -f $XERCESCROOT/lib/libxerces-c28.0.a . ");
        psystem("ln -s libxerces-c28.0.a  libxerces-c28.a ");
        psystem("ln -s libxerces-c28.a    libxerces-c.a ");
    }
        
    if ((-e "$XERCESCROOT/lib/libxerces-depdom28.0.a" )) {
        psystem("cp -f $XERCESCROOT/lib/libxerces-depdom28.0.a . ");
        psystem("ln -s libxerces-depdom28.0.a  libxerces-depdom28.a ");
        psystem("ln -s libxerces-depdom28.a    libxerces-depdom.a ");
    }        
    
    # Mac OS X
    if ((-e "$XERCESCROOT/lib/libxerces-c.28.0.dylib" )) {
        psystem("cp -f $XERCESCROOT/lib/libxerces-c.28.0.dylib .");
        psystem("ln -s libxerces-c.28.0.dylib libxerces-c.28.dylib ");
        psystem("ln -s libxerces-c.28.dylib   libxerces-c.dylib    ");
    }

    # Populate the Message Catalog Files
    if ( $opt_m =~ m/iconv/i ) {
        print ("\n\nCopying Message Catalog Files ...\n");
        pchdir ("$targetdir/msg");
        psystem("rm -f XercesMessages* ");    	
        psystem("cp -f $XERCESCROOT/msg/XercesMessages*.cat .");
    }

    # Populate the etc output directory like config.status and the map file
    print ("\n\nCopying misc output to etc ...\n");
    psystem("cp -Rf $XERCESCROOT/src/xercesc/config.status $targetdir/etc");
    psystem("cp -Rf $XERCESCROOT/obj/*.map $targetdir/etc");

    # Populate the samples directory
    populateSamples();
    
    # UNIX specifics
    foreach $iii ('config.guess', 'config.h.in', 'config.sub', 'configure', 'configure.in',
                  'install-sh', 'runConfigure', 'Makefile.in', 'Makefile.incl', 'Makefile') {
        psystem("cp -f $XERCESCROOT/samples/$iii $targetdir/samples");
    }

    # Populate the scripts and docs directory
    populateMisc();
   
    # Change the directory permissions
    psystem ("chmod 644 `find $targetdir -type f`");
    psystem ("chmod 755 $targetdir/bin/* $targetdir/lib/*");
    psystem ("chmod +x $targetdir/samples/runConfigure $targetdir/samples/configure $targetdir/samples/install-sh");
    psystem ("chmod +x $targetdir/samples/config.sub $targetdir/samples/config.guess $targetdir/samples/config.status");
    psystem ("chmod 755 `find $targetdir -type d`");

    # Now package it all up using tar
    print ("\n\nTARing up all files ...\n");
    pchdir ("$targetdir/..");
    $zipname = $targetdir . ".tar";
    $platformzipname = $zipname;

    psystem ("$TAR -cvf $platformzipname $zipfiles");

    # Finally compress the files
    print ("Compressing $platformzipname ...\n");
    psystem ("gzip $platformzipname");
} # end of UNIX build

sub createCommonDir() {
    	
    print ("\n\nCreating  directories ...\n");
        	
    psystem ("mkdir $targetdir");
    psystem ("mkdir $targetdir/bin");
    psystem ("mkdir $targetdir/lib");
    psystem ("mkdir $targetdir/msg");
    psystem ("mkdir $targetdir/etc");

    psystem ("mkdir $targetdir/include");
    psystem ("mkdir $targetdir/include/xercesc");

    psystem ("mkdir $targetdir/samples");
    psystem ("mkdir $targetdir/samples/data");
    psystem ("mkdir $targetdir/samples/SAXCount");
    psystem ("mkdir $targetdir/samples/SAX2Count");
    psystem ("mkdir $targetdir/samples/SAXPrint");
    psystem ("mkdir $targetdir/samples/SAX2Print");
    psystem ("mkdir $targetdir/samples/DOMCount");
    psystem ("mkdir $targetdir/samples/DOMPrint");
    psystem ("mkdir $targetdir/samples/Redirect");
    psystem ("mkdir $targetdir/samples/MemParse");
    psystem ("mkdir $targetdir/samples/PParse");
    psystem ("mkdir $targetdir/samples/StdInParse");
    psystem ("mkdir $targetdir/samples/EnumVal");
    psystem ("mkdir $targetdir/samples/SEnumVal");
    psystem ("mkdir $targetdir/samples/CreateDOMDocument");
    psystem ("mkdir $targetdir/samples/PSVIWriter");    
    psystem ("mkdir $targetdir/samples/SCMPrint");        

    psystem ("mkdir $targetdir/scripts");

    psystem ("mkdir $targetdir/doc");
    psystem ("mkdir $targetdir/doc/html");
    psystem ("mkdir $targetdir/doc/html/apiDocs");

}

sub populateInclude() {
      
    print ("\n\nCopying headers files ...\n");

    @headerDirectories =
     qw'sax
        sax2
        framework
        framework/psvi        
        dom
        dom/deprecated
        internal
        parsers
        util
        util/Compilers
        util/MsgLoaders
        util/MsgLoaders/ICU
        util/MsgLoaders/InMemory
        util/MsgLoaders/MsgCatalog
        util/MsgLoaders/Win32
        util/Platforms
        util/Platforms/AIX
        util/Platforms/HPUX
        util/Platforms/BeOS
        util/Platforms/Linux
        util/Platforms/MacOS
        util/Platforms/OS2
        util/Platforms/OS390
        util/Platforms/PTX
        util/Platforms/Solaris
        util/Platforms/Tandem
        util/Platforms/Win32
        util/regx
        util/Transcoders
        util/Transcoders/ICU
        util/Transcoders/Iconv
        util/Transcoders/Win32
        validators
        validators/common
        validators/datatype
        validators/DTD
        validators/schema
        validators/schema/identity';

    foreach $dir (@headerDirectories) {
        $inclDir = "include/xercesc/$dir";
        if (! (-e $inclDir)) {
            psystem("mkdir $inclDir");
        }
        $srcDir = "$XERCESCROOT/src/xercesc/$dir";

        # Weed out directories that have no files to copy, to avoid a bunch of
        # warnings from the cp command in the build output.
        opendir(dir, $srcDir);
        @allfiles = readdir(dir);
        closedir(dir);
        foreach $fileKind ("hpp", "c") {
            $matches = grep(/\.$fileKind$/, @allfiles);
            if ($matches > 0) {
                psystem("cp -f $srcDir/*.$fileKind  $inclDir/");
            }
        }
    }

    psystem("cp -Rf $XERCESCROOT/version.incl $targetdir");
    
    #
    #  Remove internal implementation headers from the DOM include directory.
    #
    psystem ("rm -rf $targetdir/include/xercesc/dom/impl");
    psystem ("rm -f  $targetdir/include/xercesc/dom/deprecated/*Impl.hpp");
    psystem ("rm -f  $targetdir/include/xercesc/dom/deprecated/DS*.hpp");
     
}
    	
sub populateSamples() {

    print ("\n\nCopying sample files ...\n");    
    
    psystem("cp -Rf $XERCESCROOT/samples/SAXCount/* $targetdir/samples/SAXCount");
    psystem("rm -f $targetdir/samples/SAXCount/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/SAX2Count/* $targetdir/samples/SAX2Count");
    psystem("rm -f $targetdir/samples/SAX2Count/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/SAXPrint/* $targetdir/samples/SAXPrint");
    psystem("rm -f $targetdir/samples/SAXPrint/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/SAX2Print/* $targetdir/samples/SAX2Print");
    psystem("rm -f $targetdir/samples/SAX2Print/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/DOMCount/* $targetdir/samples/DOMCount");
    psystem("rm -f $targetdir/samples/DOMCount/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/DOMPrint/* $targetdir/samples/DOMPrint");
    psystem("rm -f $targetdir/samples/DOMPrint/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/Redirect/* $targetdir/samples/Redirect");
    psystem("rm -f $targetdir/samples/Redirect/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/MemParse/* $targetdir/samples/MemParse");
    psystem("rm -f $targetdir/samples/MemParse/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/PParse/* $targetdir/samples/PParse");
    psystem("rm -f $targetdir/samples/PParse/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/StdInParse/* $targetdir/samples/StdInParse");
    psystem("rm -f $targetdir/samples/StdInParse/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/EnumVal/* $targetdir/samples/EnumVal");
    psystem("rm -f $targetdir/samples/EnumVal/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/SEnumVal/* $targetdir/samples/SEnumVal");
    psystem("rm -f $targetdir/samples/SEnumVal/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/CreateDOMDocument/* $targetdir/samples/CreateDOMDocument");
    psystem("rm -f $targetdir/samples/CreateDOMDocument/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/PSVIWriter/* $targetdir/samples/PSVIWriter");
    psystem("rm -f $targetdir/samples/PSVIWriter/Makefile");
    psystem("cp -Rf $XERCESCROOT/samples/SCMPrint/* $targetdir/samples/SCMPrint");
    psystem("rm -f $targetdir/samples/SCMPrint/Makefile");
        
    psystem("cp -Rf $XERCESCROOT/samples/data/* $targetdir/samples/data");	
           
}

sub populateMisc() {

    # Populate the scripts directory
    print ("\n\nCopying script files ...\n");
    psystem("cp $XERCESCROOT/scripts/sanityTest* $targetdir/scripts");

    # Populate the docs directory
    print ("\n\nCopying documentation ...\n");
    psystem("cp -Rf $XERCESCROOT/doc/* $targetdir/doc");
    psystem("cp $XERCESCROOT/Readme.html $targetdir");
    psystem("cp $XERCESCROOT/credits.txt $targetdir");
    psystem("cp $XERCESCROOT/LICENSE     $targetdir");
    psystem("cp $XERCESCROOT/NOTICE      $targetdir");
       
    psystem("rm -f $targetdir/doc/Doxyfile");
    psystem("rm -rf $targetdir/doc/style");
    psystem("rm -f $targetdir/doc/*.xml");
    psystem("rm -f $targetdir/doc/*.ent");
    psystem("rm -f $targetdir/doc/*.gif");
       
}

#
# copy ICU file to the directory specified
#	
sub copyICUOnUNIX() {
	
        pchdir ("$_[0]");

        #
        # copy icudata dll
        # For ICU 3.2:
        # on AIX,              it is called libicudata32.0.a
        # on Solaris/Linux,    it is called libicudata.so.32.0
        # on HP,               it is called libicudata.sl.32.0
        #
        psystem("rm -f libicudata*");
        psystem("cp -f $ICUROOT/lib/libicudata32.0.so .");
        psystem("cp -f $ICUROOT/lib/libicudata32.0.a .");        
        psystem("cp -f $ICUROOT/lib/libicudata.so.32.0 .");
        psystem("cp -f $ICUROOT/lib/libicudata.sl.32.0 .");
        
        psystem("find . -name 'libicudata32.0.so' -exec ln -s {} libicudata.so \\;");
        psystem("find . -name 'libicudata32.0.so' -exec ln -s {} libicudata32.so \\;");

        psystem("find . -name 'libicudata32.0.a'  -exec ln -s {} libicudata.a \\;");
        psystem("find . -name 'libicudata32.0.a'  -exec ln -s {} libicudata32.a \\;");
        
        psystem("find . -name 'libicudata.so.32.0' -exec ln -s {} libicudata.so \\;");
        psystem("find . -name 'libicudata.so.32.0' -exec ln -s {} libicudata.so.32 \\;");

        psystem("find . -name 'libicudata.sl.32.0' -exec ln -s {} libicudata.sl \\;");
        psystem("find . -name 'libicudata.sl.32.0' -exec ln -s {} libicudata.sl.32 \\;");

        #
        # copy icuuc dll
        # on AIX,              it is called libicuuc32.0.a
        # on Solaris/Linux,    it is called libicuuc.so.32.0
        # on HP,               it is called libicuuc.sl.32.0
        #
        psystem("rm -f libicuuc*");
        psystem("cp -f $ICUROOT/lib/libicuuc32.0.so .");
        psystem("cp -f $ICUROOT/lib/libicuuc32.0.a  .");        
        psystem("cp -f $ICUROOT/lib/libicuuc.so.32.0  .");
        psystem("cp -f $ICUROOT/lib/libicuuc.sl.32.0  .");
        
        psystem("find . -name 'libicuuc32.0.so' -exec ln -s {} libicuuc.so \\;");
        psystem("find . -name 'libicuuc32.0.so' -exec ln -s {} libicuuc32.so \\;");
        
        psystem("find . -name 'libicuuc32.0.a'  -exec ln -s {} libicuuc.a \\;");
        psystem("find . -name 'libicuuc32.0.a'  -exec ln -s {} libicuuc32.a \\;");
                
        psystem("find . -name 'libicuuc.so.32.0' -exec ln -s {} libicuuc.so \\;");
        psystem("find . -name 'libicuuc.so.32.0' -exec ln -s {} libicuuc.so.32 \\;");

        psystem("find . -name 'libicuuc.sl.32.0' -exec ln -s {} libicuuc.sl \\;");
        psystem("find . -name 'libicuuc.sl.32.0' -exec ln -s {} libicuuc.sl.32 \\;");
               
}

	
sub change_windows_project_for_ICU() {
    my ($thefile, $Transcoder, $MsgLoader) = @_;

    print "\nConverting Windows Xerces library project ($thefile) for ICU usage...";
    my $thefiledotbak = $thefile . ".bak";
    rename ($thefile, $thefiledotbak);

    open (FIZZLE, $thefiledotbak);
    open (FIZZLEOUT, ">$thefile");
    while ($line = <FIZZLE>) {
        if ($line =~ /Win32 Debug/) {
            $icuuc = "icuucd";
            }
        if ($line =~ /Win32 Release/) {
            $icuuc = "icuuc";
            }

        $line =~ s[/D "PROJ_XMLPARSER"][/I "$ICUROOT\\include" /D "PROJ_XMLPARSER"];
        $line =~ s[Debug/xerces-c_2D.lib"][Debug/xerces-c_2D.lib" /libpath:"$ICUROOT\\lib" /libpath:"$ICUROOT\\source\\data" /libpath:"$XERCESCROOT\\src\\xercesc\\util\\MsgLoaders\\ICU\\resources"];
        $line =~ s[Release/xerces-c_2.lib"][Release/xerces-c_2.lib" /libpath:"$ICUROOT\\lib" /libpath:"$ICUROOT\\source\\data" /libpath:"$XERCESCROOT\\src\\xercesc\\util\\MsgLoaders\\ICU\\resources"];       
       
        if ($MsgLoader)
        {
            $line =~ s/user32.lib/user32.lib $icuuc.lib XercesMessages2_8.lib/g;
        }        
        elsif ($Transcoder)
        {
            $line =~ s/user32.lib/user32.lib $icuuc.lib/g;
        }
        
        if ($Transcoder)
        {
            $line =~ s/XML_USE_WIN32_TRANSCODER/XML_USE_ICU_TRANSCODER/g;
            $line =~ s/Transcoders\\Win32\\Win32TransService.cpp/Transcoders\\ICU\\ICUTransService.cpp/g;
            $line =~ s/Transcoders\\Win32\\Win32TransService.hpp/Transcoders\\ICU\\ICUTransService.hpp/g;
        }

        if ($MsgLoader)
        {
            $line =~ s/XML_USE_WIN32_MSGLOADER/XML_USE_ICU_MESSAGELOADER/g;
            $line =~ s/MsgLoaders\\Win32\\Win32MsgLoader.cpp/MsgLoaders\\ICU\\ICUMsgLoader.cpp/g;
            $line =~ s/MsgLoaders\\Win32\\Win32MsgLoader.hpp/MsgLoaders\\ICU\\ICUMsgLoader.hpp/g;
        }

        print FIZZLEOUT $line;
    }
    close (FIZZLEOUT);
    close (FIZZLE);
    unlink ($thefiledotbak);
}

sub change_windows_makefile_for_ICU() {
    my ($thefile, $Transcoder, $MsgLoader) = @_;
    print "\nConverting Windows Xerces library makefile ($thefile) for ICU usage...";
    my $thefiledotbak = $thefile . ".bak";
    rename ($thefile, $thefiledotbak);

    open (FIZZLE, $thefiledotbak);
    open (FIZZLEOUT, ">$thefile");
    while ($line = <FIZZLE>) {
        if ($line =~ /Win64 Debug/ ){
            $icuuc = "icuucd";
            }
        if ($line =~ /Win64 Release/ ) {
            $icuuc = "icuuc";
        }

        $line =~ s[/D "PROJ_XMLPARSER"][/I "$ICUROOT\\include" /D "PROJ_XMLPARSER"];
        $line =~ s[/machine:IA64][/libpath:"$ICUROOT\\lib" /libpath:"$ICUROOT\\source\\data" /libpath:"$XERCESCROOT\\src\\xercesc\\util\\MsgLoaders\\ICU\\resources" /machine:IA64];

        if ($MsgLoader)
        {
            $line =~ s/user32.lib/user32.lib $icuuc.lib XercesMessages2_8.lib/g;
        }        
        elsif ($Transcoder)
        {
            $line =~ s/user32.lib/user32.lib $icuuc.lib/g;
        }
            
        if ($Transcoder) {
            $line =~ s/XML_USE_WIN32_TRANSCODER/XML_USE_ICU_TRANSCODER/g;
            $line =~ s/Transcoders\\Win32\\Win32TransService/Transcoders\\ICU\\ICUTransService/g;
            $line =~ s/Win32TransService/ICUTransService/g;
        }

        if ($MsgLoader)
        {
            $line =~ s/XML_USE_WIN32_MSGLOADER/XML_USE_ICU_MESSAGELOADER/g;
            $line =~ s/MsgLoaders\\Win32\\Win32MsgLoader/MsgLoaders\\ICU\\ICUMsgLoader/g;
            $line =~ s/Win32MsgLoader/ICUMsgLoader/g;
        }

        print FIZZLEOUT $line;
    }
    close (FIZZLEOUT);
    close (FIZZLE);
    unlink ($thefiledotbak);
}

sub change_windows_project_for_ICU_VC7() {
    my ($thefile, $Transcoder, $MsgLoader) = @_;
    print "\nConverting Windows Xerces library project ($thefile) for ICU usage...";
    my $thefiledotbak = $thefile . ".bak";
    rename ($thefile, $thefiledotbak);

    open (FIZZLE, $thefiledotbak);
    open (FIZZLEOUT, ">$thefile");
    while ($line = <FIZZLE>) {
        if ($line =~ /Release\|Win32/) {
            $icuuc = "icuuc";
            }
        if ($line =~ /Debug\|Win32/) {
            $icuuc = "icuucd";
            }
        $line =~ s/AdditionalIncludeDirectories=\"([^"]*)/AdditionalIncludeDirectories=\"$ICUROOT\\include;$1/;
        $line =~ s/AdditionalLibraryDirectories=\"([^"]*)/AdditionalLibraryDirectories=\"$ICUROOT\\lib;$ICUROOT\\source\\data;$XERCESCROOT\\src\\xercesc\\util\\MsgLoaders\\ICU\\resources;$1/;
        
        if ($MsgLoader)
        {
            $line =~ s/AdditionalDependencies=\"([^"]*)/AdditionalDependencies=\"$icuuc.lib XercesMessages2_8.lib $1/;
        }        
        elsif ($Transcoder)
        {
            $line =~ s/AdditionalDependencies=\"([^"]*)/AdditionalDependencies=\"$icuuc.lib $1/;
        }

        if ($Transcoder) {
            $line =~ s/XML_USE_WIN32_TRANSCODER/XML_USE_ICU_TRANSCODER/g;
            $line =~ s/Transcoders\\Win32\\Win32TransService.cpp/Transcoders\\ICU\\ICUTransService.cpp/g;
            $line =~ s/Transcoders\\Win32\\Win32TransService.hpp/Transcoders\\ICU\\ICUTransService.hpp/g;
        }

        if ($MsgLoader)
        {
            $line =~ s/XML_USE_WIN32_MSGLOADER/XML_USE_ICU_MESSAGELOADER/g;
            $line =~ s/MsgLoaders\\Win32\\Win32MsgLoader/MsgLoaders\\ICU\\ICUMsgLoader/g;
            $line =~ s/Win32MsgLoader/ICUMsgLoader/g;
        }

        print FIZZLEOUT $line;
    }
    close (FIZZLEOUT);
    close (FIZZLE);
    unlink ($thefiledotbak);
}

#
# This subroutine is used to munge the XercesLib project file to remove all
# traces of WinSock based NetAccessor. Once no NetAccessor is specified, the
# project file is configured for using the 'FileOnly' NetAccessor.
#
# For this function to work the assumption is that project file in CMVC is
# preconfigured to already use the WinSock based NetAccessor. So, the changes
# that need to be done are:
#   - to remove references to any #defines
#   - to remove references to wsock32.lib
#   - to remove references to the source files for the WinSock based NetAccessor.
#

sub changeWindowsProjectForFileOnlyNA() {
    my ($thefile) = @_;
    print "\nConfiguring Xerces library project ($thefile) for FileOnly NetAccessor...";
    my $thefiledotbak = $thefile . ".bak";
    rename ($thefile, $thefiledotbak);

    open (PROJFILEIN, $thefiledotbak);
    open (PROJFILEOUT, ">$thefile");

    while ($aline = <PROJFILEIN>) {
        # By skipping over lines between the NetAccessors group
        # we can references to the WinSock based NetAccessor files.
        if ($aline =~ m/^# Begin Group \"NetAccessors\"/g) {
            # ...found it. Write out the line as a place holder. Also...
            print PROJFILEOUT $aline;
            # ...preserve the next two lines.
            $aline = <PROJFILEIN>;
            print PROJFILEOUT $aline;
            $aline = <PROJFILEIN>;
            print PROJFILEOUT $aline;
            # Skip over the lines till you hit the WinSock NetAccessor 'End Group'.
            while ($aline = <PROJFILEIN>) { # read the next line
                last if ($aline =~ m/^# End Group/g);
            }
            # We need to preserve the 'End Group' line. The last statement of the
            # enclosing while loop prints it out.
        }

        # From the remaining lines, remove any references to the #defines and
        # the WinSock library.
        $aline =~ s/\/D \"XML_USE_NETACCESSOR_WINSOCK\" //g;  # "
        if ($aline =~ /( )+ws2_32.lib( )*\"/) { # end of line
          $aline =~ s/( )+ws2_32.lib( )*\"/\"/g;
        } else { # beginning or middle of line
          $aline =~ s/ws2_32.lib( )*//g;
        }

        print PROJFILEOUT $aline;
    }
    close (PROJFILEOUT);
    close (PROJFILEIN);
    unlink ($thefiledotbak);
}

sub changeWindowsProjectForFileOnlyNA_VC7() {
    my ($thefile) = @_;
    print "\nConfiguring Xerces library project ($thefile) for FileOnly NetAccessor...\n";
    my $thefiledotbak = $thefile . ".bak";
    rename ($thefile, $thefiledotbak);

    open (PROJFILEIN, $thefiledotbak);
    open (PROJFILEOUT, ">$thefile");

    while ($aline = <PROJFILEIN>) {
        # By skipping over lines between the NetAccessors group
        # we can references to the WinSock based NetAccessor files.
        if ($aline =~ m/^# Begin Group \"NetAccessors\"/g) {
            # ...found it. Write out the line as a place holder. Also...
            print PROJFILEOUT $aline;
            # ...preserve the next two lines.
            $aline = <PROJFILEIN>;
            print PROJFILEOUT $aline;
            $aline = <PROJFILEIN>;
            print PROJFILEOUT $aline;
            # Skip over the lines till you hit the WinSock NetAccessor 'End Group'.
            while ($aline = <PROJFILEIN>) { # read the next line
                last if ($aline =~ m/^# End Group/g);
            }
            # We need to preserve the 'End Group' line. The last statement of the
            # enclosing while loop prints it out.
        }

        # From the remaining lines, remove any references to the #defines and
        # the WinSock library.
        if ($aline =~ /\;XML_USE_NETACCESSOR_WINSOCK/) { # end or middle of line
          $aline =~ s/\;XML_USE_NETACCESSOR_WINSOCK//g;
        } else { # beginning of line
          $aline =~ s/\XML_USE_NETACCESSOR_WINSOCK\;*//g;
        }

        if ($aline =~ /\s+ws2_32\.lib\s*\"/) { # end of line
          $aline =~ s/\s+ws2_32\.lib\s*\"/\"/g;
        } else { # beginning or middle of line
          $aline =~ s/ws2_32\.lib\s*//g;
        }

        print PROJFILEOUT $aline;
    }
    close (PROJFILEOUT);
    close (PROJFILEIN);
    unlink ($thefiledotbak);
}

#
#  psystem subroutine both prints and executes a system command.
#
sub psystem() {
    print("$_[0]\n");
    system($_[0]);
    }

#
#  chdir subroutine both prints and executes a chdir
#
sub pchdir() {
    print("chdir $_[0]\n");
    chdir $_[0];
    }