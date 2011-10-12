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
!
! The following example demonstrates how to create and close an HDF5 file.
! It creates a file called 'file.h5',  and then closes the file.
!

     PROGRAM FILEEXAMPLE

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE

     CHARACTER(LEN=8), PARAMETER :: filename = "filef.h5" ! File name
     INTEGER(HID_T) :: file_id                            ! File identifier

     INTEGER     ::   error  ! Error flag

!
!    Initialize FORTRAN interface.
!
     CALL h5open_f (error)
     !
     ! Create a new file using default properties.
     !
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

     !
     ! Terminate access to the file.
     !
     CALL h5fclose_f(file_id, error)
!
!    Close FORTRAN interface.
!
     CALL h5close_f(error)
     END PROGRAM FILEEXAMPLE
