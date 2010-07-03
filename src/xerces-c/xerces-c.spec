%define tarversion 2_8_0

# threads
# values: pthreads, none
%define threads pthread

Summary:	Xerces-C++ validating XML parser
Name:		xerces-c
Version:	2.8.0
Release:	1
URL:		http://xerces.apache.org/xerces-c/
Source0:	%{name}-src_%{tarversion}.tar.gz
License:        Apache
Group:		Libraries
BuildRoot:	%{_tmppath}/%{name}-root
Prefix:		/usr

%description
Xerces-C++ is a validating XML parser written in a portable subset of C++.
Xerces-C++ makes it easy to give your application the ability to read and
write XML data. A shared library is provided for parsing, generating,
manipulating, and validating XML documents.

The parser provides high performance, modularity, and scalability. Source
code, samples and API documentation are provided with the parser. For
portability, care has been taken to make minimal use of templates, no RTTI,
and minimal use of #ifdefs.

%package devel
Requires:	xerces-c = %{version}
Group:		Development/Libraries
Summary:	Header files for Xerces-C++ validating XML parser

%description devel
Header files you can use to develop XML applications with Xerces-C++.

Xerces-C++ is a validating XML parser written in a portable subset of C++.
Xerces-C++ makes it easy to give your application the ability to read and
write XML data. A shared library is provided for parsing, generating,
manipulating, and validating XML documents.

%package doc
Group:		Documentation
Summary:	Documentation for Xerces-C++ validating XML parser

%description doc
Documentation for Xerces-C++.

Xerces-C++ is a validating XML parser written in a portable subset of C++.
Xerces-C++ makes it easy to give your application the ability to read and
write XML data. A shared library is provided for parsing, generating,
manipulating, and validating XML documents.

%prep
%setup -q -n %{name}-src_%{tarversion}

%build
%ifarch alpha ppc64 s390x sparc64 x86_64 ia64
  %define rcopts -b 64
%else
  %define rcopts -b 32
%endif

export XERCESCROOT=$RPM_BUILD_DIR/%{name}-src_%{tarversion}
cd $XERCESCROOT/src/xercesc
./runConfigure %{rcopts} -c %{__cc} -x %{__cxx} -p %{_os} -C --libdir="%{_libdir}" -minmem -nsocket -tnative -r%{threads} -P%{_prefix}
%{__make} DESTDIR=$RPM_BUILD_ROOT
cd $XERCESCROOT/samples
./runConfigure %{rcopts} -c %{__cc} -x %{__cxx} -p %{_os} -r%{threads}
%{__make} DESTDIR=$RPM_BUILD_ROOT

%install
export XERCESCROOT=$RPM_BUILD_DIR/%{name}-src_%{tarversion}
cd $XERCESCROOT/src/xercesc
%{__make} DESTDIR=$RPM_BUILD_ROOT TARGET=$RPM_BUILD_ROOT install
if [ ! -e $RPM_BUILD_ROOT%{_prefix}/%{_lib} ]; then
        %{__mv} $RPM_BUILD_ROOT%{_prefix}/lib $RPM_BUILD_ROOT%{_prefix}/%{_lib}
fi
%{__mkdir_p} $RPM_BUILD_ROOT%{_prefix}/bin
#we don't want obj directory
%ifos solaris2.8 solaris2.9 solaris2.10
  %define find gfind
%else
  %define find find
%endif
%{__install} `%{find} $XERCESCROOT/bin -maxdepth 1 -type f` $RPM_BUILD_ROOT%{_prefix}/bin
%{__mkdir_p} $RPM_BUILD_ROOT%{_prefix}/share/%{name}
%{__cp} -a $XERCESCROOT/samples $RPM_BUILD_ROOT%{_prefix}/share/%{name}

%clean
%{__rm} -rf $RPM_BUILD_ROOT

%ifnos solaris2.8 solaris2.9 solaris2.10
%post -p /sbin/ldconfig
%endif

%ifnos solaris2.8 solaris2.9 solaris2.10
%postun -p /sbin/ldconfig
%endif

%files
%defattr(755,root,root)
%{_bindir}/*
%{_libdir}/libxerces-*.so.*

%files devel
%defattr(-,root,root)
%{_includedir}
%{_prefix}/share/%{name}/samples
%{_libdir}/libxerces-*.so

%files doc
%defattr(-,root,root)
%doc LICENSE NOTICE STATUS credits.txt Readme.html doc/

%changelog
* Fri Jun  6 2003 Tuan Hoang <tqhoang@bigfoot.com>
- updated for new Xerces-C filename and directory format
- fixed date format in changelog section

* Fri Mar 14 2003 Tinny Ng <tng@ca.ibm.com>
- changed to 2.3

* Wed Dec 18 2002 Albert Strasheim <albert@stonethree.com>
- added symlink to libxerces-c.so in lib directory

* Fri Dec 13 2002 Albert Strasheim <albert@stonethree.com>
- added seperate doc package
- major cleanups

* Tue Sep 03 2002  <thomas@linux.de>
- fixed missing DESTDIR in Makefile.util.submodule

* Mon Sep 02 2002  <thomas@linux.de>
- Initial build.
