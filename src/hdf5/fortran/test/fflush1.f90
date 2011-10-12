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
! Purpose:	This is the first half of a two-part test that makes sure
!		that a file can be read after an application crashes as long
!		as the file was flushed first.  We simulate by exit the
!              the program using stop statement
!

     PROGRAM FFLUSH1EXAMPLE

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE

     !
     !the respective filename is "fflush1.h5"
     !
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
     ! data space identifier
     !
     INTEGER(HID_T) :: dataspace
     !
     !The dimensions for the dataset.
     !
     INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/NX,NY/)

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
     INTEGER, DIMENSION(NX,NY) :: data_in
     INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
     data_dims(1) = NX
     data_dims(2) = NY

     !
     !Initialize FORTRAN predifined datatypes
     !
     CALL h5open_f(error)
          CALL check("h5open_f",error,total_error)

     !
     !Initialize data_in buffer
     !
     do i = 1, NX
          do j = 1, NY
               data_in(i,j) =  (i-1) + (j-1)
          end do
     end do

     !
     !Create file "fflush1.h5" using default properties.
     !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              CALL h5_exit_f (1)
          endif
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
          CALL check("h5fcreate_f",error,total_error)

     !
     !Create group "/G" inside file "fflush1.h5".
     !
     CALL h5gcreate_f(file_id, "/G", gid, error)
          CALL check("h5gcreate_f",error,total_error)

     !
     !Create data space for the dataset.
     !
     CALL h5screate_simple_f(RANK, dims, dataspace, error)
          CALL check("h5screate_simple_f",error,total_error)

     !
     !Create dataset "/D" inside file "fflush1.h5".
     !
     CALL h5dcreate_f(file_id, "/D", H5T_NATIVE_INTEGER, dataspace, &
                      dset_id, error)
          CALL check("h5dcreate_f",error,total_error)

     !
     ! Write data_in to the dataset
     !
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data_in, data_dims, error)
          CALL check("h5dwrite_f",error,total_error)

     !
     !flush and exit without closing the library
     !
     CALL H5fflush_f(file_id, H5F_SCOPE_GLOBAL_F, error)
          CALL check("h5fflush_f",error,total_error)

     ! if errors detected, exit with non-zero code.
     IF (total_error .ne. 0) CALL h5_exit_f (1)


     001 STOP


     END PROGRAM FFLUSH1EXAMPLE

