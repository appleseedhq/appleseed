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
! This program creates a dataset that is one dimensional array of
! structures  {
!                 character*2
!                 integer
!                 double precision
!                 real
!                                   }
! Data is written and read back by fields.
!

     PROGRAM COMPOUNDEXAMPLE

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE

     CHARACTER(LEN=11), PARAMETER :: filename = "compound.h5" ! File name
     CHARACTER(LEN=8), PARAMETER :: dsetname = "Compound"     ! Dataset name
     INTEGER, PARAMETER :: dimsize = 6 ! Size of the dataset

     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: dset_id       ! Dataset identifier
     INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
     INTEGER(HID_T) :: dtype_id      ! Compound datatype identifier
     INTEGER(HID_T) :: dt1_id      ! Memory datatype identifier (for character field)
     INTEGER(HID_T) :: dt2_id      ! Memory datatype identifier (for integer field)
     INTEGER(HID_T) :: dt3_id      ! Memory datatype identifier (for double precision field)
     INTEGER(HID_T) :: dt4_id      ! Memory datatype identifier (for real field)
     INTEGER(HID_T) :: dt5_id      ! Memory datatype identifier
     INTEGER(HID_T) :: plist_id    ! Dataset trasfer property
     INTEGER(SIZE_T) :: typesize


     INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/dimsize/) ! Dataset dimensions
     INTEGER     ::   rank = 1                            ! Dataset rank

     INTEGER     ::   error ! Error flag
     INTEGER(SIZE_T)     ::   type_size  ! Size of the datatype
     INTEGER(SIZE_T)     ::   type_sizec  ! Size of the character datatype
     INTEGER(SIZE_T)     ::   type_sizei  ! Size of the integer datatype
     INTEGER(SIZE_T)     ::   type_sized  ! Size of the double precision datatype
     INTEGER(SIZE_T)     ::   type_sizer  ! Size of the real datatype
     INTEGER(SIZE_T)     ::   offset     ! Member's offset
     CHARACTER(LEN=2), DIMENSION(dimsize)      :: char_member
     CHARACTER(LEN=2), DIMENSION(dimsize)      :: char_member_out ! Buffer to read data out
     INTEGER, DIMENSION(dimsize)          :: int_member
     DOUBLE PRECISION, DIMENSION(dimsize) :: double_member
     REAL, DIMENSION(dimsize)             :: real_member
     INTEGER :: i
     INTEGER(HSIZE_T), DIMENSION(1) :: data_dims
     data_dims(1) = dimsize
     !
     ! Initialize data buffer.
     !
     do i = 1, dimsize
        char_member(i)(1:1) = char(65+i)
        char_member(i)(2:2) = char(65+i)
        char_member_out(i)(1:1)   = char(65)
        char_member_out(i)(2:2)   = char(65)
        int_member(i)   = i
        double_member(i)   = 2.* i
        real_member(i)   = 3. * i
     enddo

     !
     ! Initialize FORTRAN interface.
     !
     CALL h5open_f(error)
     !
     ! Set dataset transfer property to preserve partially initialized fields
     ! during write/read to/from dataset with compound datatype.
     !
     CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
     CALL h5pset_preserve_f(plist_id, .TRUE., error)

     !
     ! Create a new file using default properties.
     !
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)

     !
     ! Create the dataspace.
     !
     CALL h5screate_simple_f(rank, dims, dspace_id, error)
     !
     ! Create compound datatype.
     !
     ! First calculate total size by calculating sizes of each member
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, dt5_id, error)
     typesize = 2
     CALL h5tset_size_f(dt5_id, typesize, error)
     CALL h5tget_size_f(dt5_id, type_sizec, error)
     CALL h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, error)
     CALL h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, error)
     CALL h5tget_size_f(H5T_NATIVE_REAL, type_sizer, error)
     type_size = type_sizec + type_sizei + type_sized + type_sizer
     CALL h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
     !
     ! Insert memebers
     !
     ! CHARACTER*2 memeber
     !
     offset = 0
     CALL h5tinsert_f(dtype_id, "char_field", offset, dt5_id, error)
     !
     ! INTEGER member
     !
     offset = offset + type_sizec ! Offset of the second memeber is 2
     CALL h5tinsert_f(dtype_id, "integer_field", offset, H5T_NATIVE_INTEGER, error)
     !
     ! DOUBLE PRECISION member
     !
     offset = offset + type_sizei  ! Offset of the third memeber is 6
     CALL h5tinsert_f(dtype_id, "double_field", offset, H5T_NATIVE_DOUBLE, error)
     !
     ! REAL member
     !
     offset = offset + type_sized  ! Offset of the last member is 14
     CALL h5tinsert_f(dtype_id, "real_field", offset, H5T_NATIVE_REAL, error)

     !
     ! Create the dataset with compound datatype.
     !
     CALL h5dcreate_f(file_id, dsetname, dtype_id, dspace_id, &
                      dset_id, error)
     !
     ! Create memory types. We have to create a compound datatype
     ! for each member we want to write.
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sizec, dt1_id, error)
     offset = 0
     CALL h5tinsert_f(dt1_id, "char_field", offset, dt5_id, error)
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
     offset = 0
     CALL h5tinsert_f(dt2_id, "integer_field", offset, H5T_NATIVE_INTEGER, error)
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sized, dt3_id, error)
     offset = 0
     CALL h5tinsert_f(dt3_id, "double_field", offset, H5T_NATIVE_DOUBLE, error)
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sizer, dt4_id, error)
     offset = 0
     CALL h5tinsert_f(dt4_id, "real_field", offset, H5T_NATIVE_REAL, error)
     !
     ! Write data by fields in the datatype. Fields order is not important.
     !
     CALL h5dwrite_f(dset_id, dt4_id, real_member, data_dims, error, xfer_prp = plist_id)
     CALL h5dwrite_f(dset_id, dt1_id, char_member, data_dims, error, xfer_prp = plist_id)
     CALL h5dwrite_f(dset_id, dt3_id, double_member, data_dims, error, xfer_prp = plist_id)
     CALL h5dwrite_f(dset_id, dt2_id, int_member, data_dims, error, xfer_prp = plist_id)

     !
     ! End access to the dataset and release resources used by it.
     !
     CALL h5dclose_f(dset_id, error)

     !
     ! Terminate access to the data space.
     !
     CALL h5sclose_f(dspace_id, error)
     !
     ! Terminate access to the datatype
     !
     CALL h5tclose_f(dtype_id, error)
     CALL h5tclose_f(dt1_id, error)
     CALL h5tclose_f(dt2_id, error)
     CALL h5tclose_f(dt3_id, error)
     CALL h5tclose_f(dt4_id, error)
     CALL h5tclose_f(dt5_id, error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)

     !
     ! Open the file.
     !
     CALL h5fopen_f (filename, H5F_ACC_RDWR_F, file_id, error)
     !
     ! Open the dataset.
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)
     !
     ! Create memeory datatyoe to read character member of the compound datatype.
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, dt2_id, error)
     typesize = 2
     CALL h5tset_size_f(dt2_id, typesize, error)
     CALL h5tget_size_f(dt2_id, type_size, error)
     CALL h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
     offset = 0
     CALL h5tinsert_f(dt1_id, "char_field", offset, dt2_id, error)
     !
     ! Read part of the datatset and display it.
     !
     CALL h5dread_f(dset_id, dt1_id, char_member_out, data_dims, error)
     write(*,*) (char_member_out(i), i=1, dimsize)

     !
     ! Close all open objects.
     !
     CALL h5dclose_f(dset_id, error)
     CALL h5tclose_f(dt1_id, error)
     CALL h5tclose_f(dt2_id, error)
     CALL h5fclose_f(file_id, error)
     !
     ! Close FORTRAN interface.
     !
     CALL h5close_f(error)

     END PROGRAM COMPOUNDEXAMPLE


