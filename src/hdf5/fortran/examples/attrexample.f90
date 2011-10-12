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
! This example shows how to create and write a dataset attribute.
! It opens the existing file 'dset.h5', obtains the identifier of
! the dataset "/dset", defines attribute's dataspace,
! creates dataset attribute, writes the attribute, and then closes
! the attribute's dataspace, attribute, dataset, and file.

     PROGRAM ATTREXAMPLE


     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE

     CHARACTER(LEN=8), PARAMETER :: filename = "dsetf.h5" ! File name
     CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"     ! Dataset name
     CHARACTER(LEN=9), PARAMETER :: aname = "attr_long"   ! Attribute name

     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: dset_id       ! Dataset identifier
     INTEGER(HID_T) :: attr_id       ! Attribute identifier
     INTEGER(HID_T) :: aspace_id     ! Attribute Dataspace identifier
     INTEGER(HID_T) :: atype_id      ! Attribute Dataspace identifier
     INTEGER(HSIZE_T), DIMENSION(1) :: adims = (/2/) ! Attribute dimension
     INTEGER     ::   arank = 1                      ! Attribure rank
     INTEGER(SIZE_T) :: attrlen    ! Length of the attribute string

     CHARACTER(LEN=80), DIMENSION(2) ::  attr_data  ! Attribute data

     INTEGER     ::   error ! Error flag
     INTEGER(HSIZE_T), DIMENSION(1) :: data_dims


     !
     ! Initialize attribute's data
     !
     attr_data(1) = "Dataset character attribute"
     attr_data(2) = "Some other string here     "
     attrlen = 80
     !
     ! Initialize FORTRAN interface.
     !
     CALL h5open_f(error)

     !
     ! Open an existing file.
     !
     CALL h5fopen_f (filename, H5F_ACC_RDWR_F, file_id, error)

     !
     ! Open an existing dataset.
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)

     !
     ! Create scalar data space for the attribute.
     !
     CALL h5screate_simple_f(arank, adims, aspace_id, error)
     !
     ! Create datatype for the attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
     CALL h5tset_size_f(atype_id, attrlen, error)

     !
     ! Create dataset attribute.
     !
     CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, &
                      attr_id, error)

     !
     ! Write the attribute data.
     !
     data_dims(1) = 2
     CALL h5awrite_f(attr_id, atype_id, attr_data, data_dims, error)

     !
     ! Close the attribute.
     !
     CALL h5aclose_f(attr_id, error)

     !
     ! Terminate access to the data space.
     !
     CALL h5sclose_f(aspace_id, error)

     !
     ! End access to the dataset and release resources used by it.
     !
     CALL h5dclose_f(dset_id, error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     ! Close FORTRAN interface.
     !
     CALL h5close_f(error)

     END PROGRAM ATTREXAMPLE

