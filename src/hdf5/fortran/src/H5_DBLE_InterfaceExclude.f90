!****h* fortran/src/H5_DBLE_InterfaceExclude.f90
!
! NAME
!   H5_DBLE_INTERFACE
!
! FUNCTION
!   This module is used for when the default REAL is of type DOUBLE PRECISION.
!   We do not include the double precision interfaces if the defaut REAL is
!   DOUBLE PRECISION since this would lead to a non-unique conflict with the
!   generic interfaces declared as REAL and those declared as DOUBLE PRECISION.
!
! NOTES
!   Empty module.
!
! COPYRIGHT
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!   Copyright by The HDF Group.                                               *
!   Copyright by the Board of Trustees of the University of Illinois.         *
!   All rights reserved.                                                      *
!                                                                             *
!   This file is part of HDF5.  The full HDF5 copyright notice, including     *
!   terms governing use, modification, and redistribution, is contained in    *
!   the files COPYING and Copyright.html.  COPYING can be found at the root   *
!   of the source code distribution tree; Copyright.html can be found at the  *
!   root level of an installed copy of the electronic HDF5 document set and   *
!   is linked from the top-level documents page.  It can also be found at     *
!   http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
!   access to either file, you may request a copy from help@hdfgroup.org.     *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! AUTHOR
!  M.S. Breitenfeld
!
!*****

MODULE H5_DBLE_INTERFACE


END MODULE H5_DBLE_INTERFACE
