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
! The following example shows how to create an empty dataset.
! It creates a file called 'dsetf.h5', defines the
! dataset dataspace, creates a dataset which is a 4x6 integer array,
! and then closes the dataspace, the dataset, and the file.
!

     PROGRAM DSETEXAMPLE

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE

     CHARACTER(LEN=8), PARAMETER :: filename = "dsetf.h5" ! File name
     CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"     ! Dataset name

     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: dset_id       ! Dataset identifier
     INTEGER(HID_T) :: dspace_id     ! Dataspace identifier


     INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/4,6/) ! Dataset dimensions
     INTEGER     ::   rank = 2                        ! Dataset rank

     INTEGER     ::   error ! Error flag

     !
     ! Initialize FORTRAN interface.
     !
     CALL h5open_f(error)

     !
     ! Create a new file using default properties.
     !
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

     !
     ! Create the dataspace.
     !
     CALL h5screate_simple_f(rank, dims, dspace_id, error)

     !
     ! Create the dataset with default properties.
     !
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dspace_id, &
                      dset_id, error)

     !
     ! End access to the dataset and release resources used by it.
     !
     CALL h5dclose_f(dset_id, error)

     !
     ! Terminate access to the data space.
     !
     CALL h5sclose_f(dspace_id, error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     ! Close FORTRAN interface.
     !
     CALL h5close_f(error)

     END PROGRAM DSETEXAMPLE


