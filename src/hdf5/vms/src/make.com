$!#
$!# Copyright by The HDF Group.
$!# Copyright by the Board of Trustees of the University of Illinois.
$!# All rights reserved.
$!#
$!# This file is part of HDF5.  The full HDF5 copyright notice, including
$!# terms governing use, modification, and redistribution, is contained in
$!# the files COPYING and Copyright.html.  COPYING can be found at the root
$!# of the source code distribution tree; Copyright.html can be found at the
$!# root level of an installed copy of the electronic HDF5 document set and
$!# is linked from the top-level documents page.  It can also be found at
$!# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
$!# access to either file, you may request a copy from help@hdfgroup.org.
$!#
$! Makefile for VMS systems.
$!
$! Make HDF5 library
$!
$! The next two lines should be uncommented only when building by hand in the
$! current directory. Use build.com in the vms directory to build 
$! the distribution. Make sure that location of the zlib library is correct.
$!
$! define zlib_dir sys$sysusers:[pourmal.zlib-1_2_3]
$! ccopt = "/float=ieee_float/define=H5_VMS/include=zlib_dir"
$ ccc := cc 'ccopt 
$ ccc h5detect.c
$ link h5detect
$ type sys$input
        Running h5detect to create h5tinit.c
$ define/user_mode sys$output h5tinit.c
$ run h5detect
$!
$ ccc h5make_libsettings.c
$ link h5make_libsettings
$ type sys$input
        Running h5make_libsettings to create H5lib_settings.h
$ define/user_mode sys$output H5lib_settings.h
$ run h5make_libsettings
$!
$ type sys$input
         Creating  HDF5 library
$!
$ cobj= "H5, H5checksum, H5dbg, H5system, H5timer, H5trace,"+-
        "H5Abtree2, H5A, H5AC, H5Adense, H5Adeprec, H5Aint, H5Atest,"+-
        "H5B2, H5B2cache, H5B2dbg, H5B2hdr, H5B2int, H5B2stat, H5B2test, H5B, H5Bcache, H5Bdbg,"+-
        "H5C, H5CS,"+-
        "H5Dbtree, H5D, H5Dchunk, H5Dcompact, H5Dcontig, H5Ddbg, H5Ddeprec, H5Defl, H5Dfill,"+-
        "H5Dint, H5Dio, H5Dlayout, H5Dmpio, H5Doh, H5Dscatgath, H5Dselect, H5Dtest,"+-
        "H5E, H5Edeprec, H5Eint,"+-
        "H5Faccum, H5F, H5Fdbg, H5FD, H5FDcore,"+-
        "H5FDdirect, H5FDfamily, H5FDint, H5FDlog, H5FDmpi, H5FDmpio,"+-
        "H5FDmpiposix, H5FDmulti, H5FDsec2, H5FDspace, H5FDstdio,"+-
        "H5FDwindows, H5Fefc, H5Ffake, H5Fio, H5FL, H5Fmount, H5Fmpi, H5FO, H5Fquery, H5FS, "+-
        "H5FScache, H5FSdbg, H5Fsfile, H5FSsection, H5FSstat, H5FStest, H5Fsuper, H5Fsuper_cache, H5Ftest,"+-
        "H5Gbtree2, H5G, H5Gcache, H5Gcompact, H5Gdense, H5Gdeprec, H5Gent, H5Gint,"+-
        "H5Glink, H5Gloc, H5Gname, H5Gnode, H5Gobj, H5Goh, H5Groot, H5Gstab, H5Gtest, H5Gtraverse,"+-
        "H5HFbtree2, H5HF, H5HFcache, H5HFdbg, H5HFdblock, H5HFdtable, H5HFhdr, H5HFhuge,"+-
        "H5HFiblock, H5HFiter, H5HFman, H5HFsection, H5HFspace, H5HFstat, H5HFtest, H5HFtiny,"+-
        "H5HG, H5HGcache, H5HGdbg, H5HL, H5HLcache, H5HLdbg, H5HLint, H5HP, H5I, H5Itest, H5L, H5Lexternal"
$ cobj1= "H5MFaggr, H5MF, H5MFdbg, H5MFsection, H5MM, H5MP, H5MPtest,"+-
        "H5Oainfo, H5Oalloc, H5Oattr, H5Oattribute,"+-
        "H5Obogus, H5Obtreek, H5O, H5Ocache, H5Ochunk, H5Ocont, H5Ocopy, H5Odbg, H5Odrvinfo,"+-
        "H5Odtype, H5Oefl, H5Ofill, H5Oginfo, H5Olayout,"+-
        "H5Olinfo, H5Olink, H5Omessage, H5Omtime"
$ cobj2= "H5Oname, H5Onull, H5Opline, H5Orefcount, H5Osdspace, H5Oshared, H5Oshmesg,"+-
        "H5Ostab, H5Otest, H5Ounknown,"+-
        "H5Pacpl, H5P, H5Pdapl, H5Pdcpl, H5Pdeprec, H5Pdxpl, H5Pfapl, H5Pfcpl, H5Pfmpl,"+-
        "H5Pgcpl, H5Pint, H5Plapl, H5Plcpl, H5Pocpl, H5Pocpypl, H5Pstrcpl, H5Ptest,"+-
        "H5R, H5RC, H5Rdeprec, H5RS,"+-
        "H5Sall, H5S, H5Sdbg, H5Shyper, H5SL, H5SMbtree2, H5SM, H5SMcache, H5SMmessage, H5Smpio,"+- 
        "H5SMtest,H5Snone, H5Spoint, H5Sselect, H5ST, H5Stest,"+-
        "H5Tarray, H5Tbit, H5T, H5Tcommit,"+-
        "H5Tcompound, H5Tconv, H5Tcset, H5Tdbg, H5Tdeprec, H5Tenum, H5Tfields, H5Tfixed,"+-
        "H5Tfloat, H5Tinit, H5Tnative, H5Toffset, H5Toh, H5Topaque, H5Torder,"+-
        "H5Tpad, H5Tprecis, H5TS, H5Tstrpad, H5Tvisit, H5Tvlen, H5V, H5WB, H5Z,"+-
        "H5Zdeflate, H5Zfletcher32, H5Znbit, H5Zscaleoffset, H5Zshuffle, H5Zszip, H5Ztrans"
$!
$ ccc 'cobj
$ ccc 'cobj1
$ ccc 'cobj2 
$ library/create []hdf5
$ library/insert []hdf5 'cobj
$ library/insert []hdf5 'cobj1
$ library/insert []hdf5 'cobj2
$ type sys$input
	Done
$!
