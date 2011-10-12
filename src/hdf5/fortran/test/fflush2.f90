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
! Purpose:	This is the second half of a two-part test that makes sure
!		that a file can be read after an application crashes as long
!		as the file was flushed first.  This half tries to read the
!		file created by the first half.
!

     PROGRAM FFLUSH2EXAMPLE

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE

     CHARACTER(LEN=7), PARAMETER :: filename = "fflush1"
     CHARACTER(LEN=80) :: fix_filename

     !
     !data space rank and dimensions
     !
     INTEGER, PARAMETER :: RANK = 2
     INTEGER, PARAMETER :: NX = 4
     INTEGER, PARAMETER :: NY = 5

     !
     ! File identifiers
     !
     INTEGER(HID_T) :: file_id

     !
     ! Group identifier
     !
     INTEGER(HID_T) :: gid

     !
     ! dataset identifier
     !
     INTEGER(HID_T) :: dset_id


     !
     ! data type identifier
     !
     INTEGER(HID_T) :: dtype_id

     !
     !flag to check operation success
     !
     INTEGER     ::   error

     !
     !general purpose integer
     !
     INTEGER     ::   i, j, total_error = 0

     !
     !data buffers
     !
     INTEGER, DIMENSION(NX,NY) :: data_out
     INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
     data_dims(1) = NX
     data_dims(2) = NY

     !
     !Initialize FORTRAN predifined datatypes
     !
     CALL h5open_f(error)
          CALL check("h5open_f",error,total_error)

     !
     !Open the file.
     !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              CALL h5_exit_f (1)
          endif
	  print *, "filename=", filename, "fix_filename=", fix_filename
     CALL h5fopen_f(fix_filename, H5F_ACC_RDONLY_F, file_id, error)
          CALL check("h5fopen_f",error,total_error)

     !
     !Open the dataset
     !
     CALL h5dopen_f(file_id, "/D", dset_id, error)
          CALL check("h5dopen_f",error,total_error)

     !
     !Get dataset's data type.
     !
     CALL h5dget_type_f(dset_id, dtype_id, error)
          CALL check("h5dget_type_f",error,total_error)

     !
     !Read the dataset.
     !
     CALL h5dread_f(dset_id, dtype_id, data_out, data_dims, error)
          CALL check("h5dread_f",error,total_error)

     !
     !Print the dataset.
     !
     do i = 1, NX
          write(*,*) (data_out(i,j), j = 1, NY)
     end do
!
!result of the print statement
!
! 0,  1,  2,  3,  4
! 1,  2,  3,  4,  5
! 2,  3,  4,  5,  6
! 3,  4,  5,  6,  7

     !
     !Open the group.
     !
     CALL h5gopen_f(file_id, "G", gid, error)
          CALL check("h5gopen_f",error,total_error)

     !
     !In case error happens, exit.
     !
     IF (error == -1) CALL h5_exit_f (1)

     !
     !Close the datatype
     !
     CALL h5tclose_f(dtype_id, error)
          CALL check("h5tclose_f",error,total_error)

     !
     !Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)
          CALL check("h5dclose_f",error,total_error)

     !
     !Close the group.
     !
     CALL h5gclose_f(gid, error)
          CALL check("h5gclose_f",error,total_error)

     !
     !Close the file.
     !
     CALL h5fclose_f(file_id, error)
          CALL check("h5fclose_f",error,total_error)

     !
     !Close FORTRAN predifined datatypes
      !
     CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
     CALL h5close_f(error)
         CALL check("h5close_types_f",error,total_error)

     ! if errors detected, exit with non-zero code.
     IF (total_error .ne. 0) CALL h5_exit_f (1)

     END PROGRAM FFLUSH2EXAMPLE
