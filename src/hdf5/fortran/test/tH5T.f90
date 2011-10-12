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
    SUBROUTINE compoundtest(cleanup, total_error)
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
! The following H5T interface functions are tested:
! h5tcopy_f, h5tset(get)_size_f, h5tcreate_f, h5tinsert_f,  h5tclose_f,
! h5tget_class_f, h5tget_member_name_f, h5tget_member_offset_f, h5tget_member_type_f,
! h5tequal_f, h5tinsert_array_f, h5tcommit_f, h5tencode_f, h5tdecode_f


     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(OUT) :: total_error

     CHARACTER(LEN=8), PARAMETER :: filename = "compound" ! File name
     CHARACTER(LEN=80) :: fix_filename
     CHARACTER(LEN=8), PARAMETER :: dsetname = "Compound"     ! Dataset name
     INTEGER, PARAMETER :: dimsize = 6 ! Size of the dataset
     INTEGER, PARAMETER :: COMP_NUM_MEMBERS = 4 ! Number of members in the compound datatype

     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: dset_id       ! Dataset identifier
     INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
     INTEGER(HID_T) :: dtype_id      ! Compound datatype identifier
     INTEGER(HID_T) :: dtarray_id    ! Compound datatype identifier
     INTEGER(HID_T) :: arrayt_id    ! Array datatype identifier
     INTEGER(HID_T) :: dt1_id      ! Memory datatype identifier (for character field)
     INTEGER(HID_T) :: dt2_id      ! Memory datatype identifier (for integer field)
     INTEGER(HID_T) :: dt3_id      ! Memory datatype identifier (for double precision field)
     INTEGER(HID_T) :: dt4_id      ! Memory datatype identifier (for real field)
     INTEGER(HID_T) :: dt5_id      ! Memory datatype identifier
     INTEGER(HID_T) :: membtype_id ! Datatype identifier
     INTEGER(HID_T) :: plist_id    ! Dataset trasfer property


     INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/dimsize/) ! Dataset dimensions
     INTEGER     ::   rank = 1                            ! Dataset rank

     INTEGER     ::   error ! Error flag
     INTEGER(SIZE_T)     ::   type_size  ! Size of the datatype
     INTEGER(SIZE_T)     ::   type_sizec  ! Size of the character datatype
     INTEGER(SIZE_T)     ::   type_sizei  ! Size of the integer datatype
     INTEGER(SIZE_T)     ::   type_sized  ! Size of the double precision datatype
     INTEGER(SIZE_T)     ::   type_sizer  ! Size of the real datatype
     INTEGER(SIZE_T)     ::   offset     ! Member's offset
     INTEGER(SIZE_T)     ::   offset_out     ! Member's offset
     CHARACTER(LEN=2), DIMENSION(dimsize)      :: char_member
     CHARACTER(LEN=2), DIMENSION(dimsize)      :: char_member_out ! Buffer to read data out
     INTEGER, DIMENSION(dimsize)          :: int_member
     INTEGER, DIMENSION(dimsize)          :: int_member_out
     DOUBLE PRECISION, DIMENSION(dimsize) :: double_member
     DOUBLE PRECISION, DIMENSION(dimsize) :: double_member_out
     REAL, DIMENSION(dimsize)             :: real_member
     REAL, DIMENSION(dimsize)             :: real_member_out
     INTEGER :: i
     INTEGER :: class ! Datatype class
     INTEGER :: num_members ! Number of members in the compound datatype
     CHARACTER(LEN=256) :: member_name
     INTEGER :: len ! Lenght of the name of the compound datatype member
     INTEGER :: member_index ! index of the field
     INTEGER(HSIZE_T), DIMENSION(3) :: array_dims=(/2,3,4/)
     INTEGER :: array_dims_range = 3
     INTEGER :: elements = 24 ! number of elements in the array_dims array.
     INTEGER(SIZE_T) :: sizechar
     INTEGER(HSIZE_T), DIMENSION(1) :: data_dims
     LOGICAL :: flag = .TRUE.

     CHARACTER(LEN=1024) :: cmpd_buf
     INTEGER(SIZE_T) :: cmpd_buf_size=0
     INTEGER(HID_T) :: decoded_sid1
     INTEGER(HID_T) :: decoded_tid1

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
        int_member_out(i)   = 0
        double_member(i)   = 2.* i
        double_member_out(i)   = 0.
        real_member(i)   = 3. * i
        real_member_out(i)   = 0.
     enddo

     !
     ! Set dataset transfer property to preserve partially initialized fields
     ! during write/read to/from dataset with compound datatype.
     !
     CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
         CALL check("h5pcreate_f", error, total_error)
     CALL h5pset_preserve_f(plist_id, flag, error)
         CALL check("h5pset_preserve_f", error, total_error)
     !
     ! Create a new file using default properties.
     !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
         CALL check("h5fcreate_f", error, total_error)

     !
     ! Create the dataspace.
     !
     CALL h5screate_simple_f(rank, dims, dspace_id, error)
         CALL check("h5screate_simple_f", error, total_error)
     !
     ! Create compound datatype.
     !
     ! First calculate total size by calculating sizes of each member
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, dt5_id, error)
         CALL check("h5tcopy_f", error, total_error)
     sizechar = 2
     CALL h5tset_size_f(dt5_id, sizechar, error)
         CALL check("h5tset_size_f", error, total_error)
     CALL h5tget_size_f(dt5_id, type_sizec, error)
         CALL check("h5tget_size_f", error, total_error)
     CALL h5tget_size_f(H5T_NATIVE_INTEGER, type_sizei, error)
         CALL check("h5tget_size_f", error, total_error)
     CALL h5tget_size_f(H5T_NATIVE_DOUBLE, type_sized, error)
         CALL check("h5tget_size_f", error, total_error)
     CALL h5tget_size_f(H5T_NATIVE_REAL, type_sizer, error)
         CALL check("h5tget_size_f", error, total_error)
     !write(*,*) "get sizes", type_sizec, type_sizei, type_sizer, type_sized
     type_size = type_sizec + type_sizei + type_sized + type_sizer
     CALL h5tcreate_f(H5T_COMPOUND_F, type_size, dtype_id, error)
         CALL check("h5tcreate_f", error, total_error)
     !
     ! Insert memebers
     !
     ! CHARACTER*2 memeber
     !
     offset = 0
     CALL h5tinsert_f(dtype_id, "char_field", offset, dt5_id, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     ! INTEGER member
     !
     offset = offset + type_sizec ! Offset of the second memeber is 2
     CALL h5tinsert_f(dtype_id, "integer_field", offset, H5T_NATIVE_INTEGER, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     ! DOUBLE PRECISION member
     !
     offset = offset + type_sizei  ! Offset of the third memeber is 6
     CALL h5tinsert_f(dtype_id, "double_field", offset, H5T_NATIVE_DOUBLE, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     ! REAL member
     !
     offset = offset + type_sized  ! Offset of the last member is 14
     CALL h5tinsert_f(dtype_id, "real_field", offset, H5T_NATIVE_REAL, error)
     CALL check("h5tinsert_f", error, total_error)

!!$     !/*-----------------------------------------------------------------------
!!$     ! * Test encoding and decoding compound  datatypes
!!$     ! *-----------------------------------------------------------------------
!!$     !*/
!!$     !    /* Encode compound type in a buffer */
!!$
!!$     !         First find the buffer size
!!$
!!$     CALL H5Tencode_f(dtype_id, cmpd_buf, cmpd_buf_size, error)
!!$     CALL check("H5Tencode_f", error, total_error)
!!$
!!$     ! /* Try decoding bogus buffer */
!!$
!!$     CALL H5Tdecode_f(cmpd_buf, decoded_tid1, error)
!!$     CALL VERIFY("H5Tdecode_f", error, -1, total_error)
!!$
!!$     CALL H5Tencode_f(dtype_id, cmpd_buf, cmpd_buf_size, error)
!!$     CALL check("H5Tencode_f", error, total_error)
!!$
!!$     ! /* Decode from the compound buffer and return an object handle */
!!$     CALL H5Tdecode_f(cmpd_buf, decoded_tid1, error)
!!$     CALL check("H5Tdecode_f", error, total_error)
!!$
!!$     ! /* Verify that the datatype was copied exactly */
!!$
!!$     CALL H5Tequal_f(decoded_tid1, dtype_id, flag, error)
!!$     CALL check("H5Tequal_f", error, total_error)
!!$     CALL VerifyLogical("H5Tequal_f", flag, .TRUE., total_error)

     !
     ! Create the dataset with compound datatype.
     !
     CALL h5dcreate_f(file_id, dsetname, dtype_id, dspace_id, &
                      dset_id, error)
         CALL check("h5dcreate_f", error, total_error)
     !
     ! Create memory types. We have to create a compound datatype
     ! for each member we want to write.
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sizec, dt1_id, error)
         CALL check("h5tcreate_f", error, total_error)
     offset = 0
     CALL h5tinsert_f(dt1_id, "char_field", offset, dt5_id, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt2_id, error)
         CALL check("h5tcreate_f", error, total_error)
     offset = 0
     CALL h5tinsert_f(dt2_id, "integer_field", offset, H5T_NATIVE_INTEGER, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sized, dt3_id, error)
         CALL check("h5tcreate_f", error, total_error)
     offset = 0
     CALL h5tinsert_f(dt3_id, "double_field", offset, H5T_NATIVE_DOUBLE, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sizer, dt4_id, error)
         CALL check("h5tcreate_f", error, total_error)
     offset = 0
     CALL h5tinsert_f(dt4_id, "real_field", offset, H5T_NATIVE_REAL, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     ! Write data by fields in the datatype. Fields order is not important.
     !
     CALL h5dwrite_f(dset_id, dt4_id, real_member, data_dims, error, xfer_prp = plist_id)
         CALL check("h5dwrite_f", error, total_error)
     CALL h5dwrite_f(dset_id, dt1_id, char_member, data_dims, error, xfer_prp = plist_id)
         CALL check("h5dwrite_f", error, total_error)
     CALL h5dwrite_f(dset_id, dt3_id, double_member, data_dims, error, xfer_prp = plist_id)
         CALL check("h5dwrite_f", error, total_error)
     CALL h5dwrite_f(dset_id, dt2_id, int_member, data_dims, error, xfer_prp = plist_id)
         CALL check("h5dwrite_f", error, total_error)

     !
     ! End access to the dataset and release resources used by it.
     !
     CALL h5dclose_f(dset_id, error)
         CALL check("h5dclose_f", error, total_error)

     !
     ! Terminate access to the data space.
     !
     CALL h5sclose_f(dspace_id, error)
         CALL check("h5sclose_f", error, total_error)
     !
     ! Terminate access to the datatype
     !
     CALL h5tclose_f(dtype_id, error)
         CALL check("h5tclose_f", error, total_error)
     CALL h5tclose_f(dt1_id, error)
         CALL check("h5tclose_f", error, total_error)
     CALL h5tclose_f(dt2_id, error)
         CALL check("h5tclose_f", error, total_error)
     CALL h5tclose_f(dt3_id, error)
         CALL check("h5tclose_f", error, total_error)
     CALL h5tclose_f(dt4_id, error)
         CALL check("h5tclose_f", error, total_error)

     !
     ! Create and store compound datatype with the character and
     ! array members.
     !
     type_size = type_sizec + elements*type_sizer ! Size of compound datatype
     CALL h5tcreate_f(H5T_COMPOUND_F, type_size, dtarray_id, error)
         CALL check("h5tcreate_f", error, total_error)
     offset = 0
     CALL h5tinsert_f(dtarray_id, "char_field", offset, H5T_NATIVE_CHARACTER, error)
         CALL check("h5tinsert_f", error, total_error)
     offset = type_sizec
     CALL h5tarray_create_f(H5T_NATIVE_REAL, array_dims_range, array_dims, arrayt_id, error)
         CALL check("h5tarray_create_f", error, total_error)
     CALL h5tinsert_f(dtarray_id,"array_field", offset, arrayt_id, error)
         CALL check("h5tinsert_f", error, total_error)
     CALL h5tcommit_f(file_id, "Compound_with_array_member", dtarray_id, error)
         CALL check("h5tcommit_f", error, total_error)
     CALL h5tclose_f(arrayt_id, error)
         CALL check("h5tclose_f", error, total_error)
     CALL h5tclose_f(dtarray_id, error)
         CALL check("h5tclose_f", error, total_error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)
         CALL check("h5fclose_f", error, total_error)

     !
     ! Open the file.
     !
     CALL h5fopen_f (fix_filename, H5F_ACC_RDWR_F, file_id, error)
         CALL check("h5fopen_f", error, total_error)
     !
     ! Open the dataset.
     !
     CALL h5dopen_f(file_id, dsetname, dset_id, error)
         CALL check("h5dopen_f", error, total_error)
     !
     ! Get datatype of the open dataset.
     ! Check it class, number of members,  and member's names.
     !
     CALL h5dget_type_f(dset_id, dtype_id, error)
         CALL check("h5dget_type_f", error, total_error)
     CALL h5tget_class_f(dtype_id, class, error)
         CALL check("h5dget_class_f", error, total_error)
         if (class .ne. H5T_COMPOUND_F) then
            write(*,*) " Wrong class type returned"
            total_error = total_error + 1
         endif
     CALL h5tget_nmembers_f(dtype_id, num_members, error)
         CALL check("h5dget_nmembers_f", error, total_error)
         if (num_members .ne. COMP_NUM_MEMBERS ) then
            write(*,*) " Wrong number of members returned"
            total_error = total_error + 1
         endif
     !
     !  Go through the members and find out their names and offsets.
     !  Also see if name corresponds to the index
     !
     do i = 1, num_members
        CALL h5tget_member_name_f(dtype_id, i-1, member_name, len, error)
         CALL check("h5tget_member_name_f", error, total_error)
        CALL h5tget_member_offset_f(dtype_id, i-1, offset_out, error)
         CALL check("h5tget_member_offset_f", error, total_error)
        CALL h5tget_member_index_f(dtype_id, member_name(1:len), member_index, error)
         CALL check("h5tget_member_index_f", error, total_error)
         if(member_index .ne. i-1) then
            write(*,*) "Index returned is incorrect"
            write(*,*) member_index, i-1
            total_error = total_error + 1
            endif

        CHECK_NAME: SELECT CASE (member_name(1:len))
        CASE("char_field")
             if(offset_out .ne. 0) then
               write(*,*) "Offset of the char member is incorrect"
               total_error = total_error + 1
             endif
             CALL h5tget_member_type_f(dtype_id, i-1, membtype_id, error)
              CALL check("h5tget_member_type_f", error, total_error)
             CALL h5tequal_f(membtype_id, dt5_id, flag, error)
              CALL check("h5tequal_f", error, total_error)
             if(.not. flag) then
                write(*,*) "Wrong member type returned for character member"
                total_error = total_error + 1
             endif
             CALL h5tget_member_class_f(dtype_id, i-1, class, error)
              CALL check("h5tget_member_class_f",error, total_error)
              if (class .ne. H5T_STRING_F) then
                 write(*,*) "Wrong class returned for character member"
                 total_error = total_error + 1
              endif
        CASE("integer_field")
             if(offset_out .ne. type_sizec) then
               write(*,*) "Offset of the integer member is incorrect"
               total_error = total_error + 1
             endif
             CALL h5tget_member_type_f(dtype_id, i-1, membtype_id, error)
              CALL check("h5tget_member_type_f", error, total_error)
             CALL h5tequal_f(membtype_id, H5T_NATIVE_INTEGER, flag, error)
              CALL check("h5tequal_f", error, total_error)
             if(.not. flag) then
                write(*,*) "Wrong member type returned for integer memebr"
                total_error = total_error + 1
             endif
             CALL h5tget_member_class_f(dtype_id, i-1, class, error)
              CALL check("h5tget_member_class_f",error, total_error)
              if (class .ne. H5T_INTEGER_F) then
                 write(*,*) "Wrong class returned for INTEGER member"
                 total_error = total_error + 1
              endif
        CASE("double_field")
             if(offset_out .ne. (type_sizec+type_sizei)) then
               write(*,*) "Offset of the double precision member is incorrect"
               total_error = total_error + 1
             endif
             CALL h5tget_member_type_f(dtype_id, i-1, membtype_id, error)
              CALL check("h5tget_member_type_f", error, total_error)
             CALL h5tequal_f(membtype_id, H5T_NATIVE_DOUBLE, flag, error)
              CALL check("h5tequal_f", error, total_error)
             if(.not. flag) then
                write(*,*) "Wrong member type returned for double precision memebr"
                total_error = total_error + 1
             endif
             CALL h5tget_member_class_f(dtype_id, i-1, class, error)
              CALL check("h5tget_member_class_f",error, total_error)
              if (class .ne. H5T_FLOAT_F) then
                 write(*,*) "Wrong class returned for double precision member"
                 total_error = total_error + 1
              endif
        CASE("real_field")
             if(offset_out .ne. (type_sizec+type_sizei+type_sized)) then
               write(*,*) "Offset of the real member is incorrect"
               total_error = total_error + 1
             endif
             CALL h5tget_member_type_f(dtype_id, i-1, membtype_id, error)
              CALL check("h5tget_member_type_f", error, total_error)
             CALL h5tequal_f(membtype_id, H5T_NATIVE_REAL, flag, error)
              CALL check("h5tequal_f", error, total_error)
             if(.not. flag) then
                write(*,*) "Wrong member type returned for real memebr"
                total_error = total_error + 1
             endif
             CALL h5tget_member_class_f(dtype_id, i-1, class, error)
              CALL check("h5tget_member_class_f",error, total_error)
              if (class .ne. H5T_FLOAT_F) then
                 write(*,*) "Wrong class returned for real member"
                 total_error = total_error + 1
              endif
        CASE DEFAULT
               write(*,*) "Wrong member's name"
               total_error = total_error + 1

        END SELECT CHECK_NAME

     enddo
     !
     ! Create memory datatype to read character member of the compound datatype.
     !
     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, dt2_id, error)
         CALL check("h5tcopy_f", error, total_error)
     sizechar = 2
     CALL h5tset_size_f(dt2_id, sizechar, error)
         CALL check("h5tset_size_f", error, total_error)
     CALL h5tget_size_f(dt2_id, type_size, error)
         CALL check("h5tget_size_f", error, total_error)
     CALL h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
         CALL check("h5tcreate_f", error, total_error)
     offset = 0
     CALL h5tinsert_f(dt1_id, "char_field", offset, dt2_id, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     ! Read part of the dataset
     !
     CALL h5dread_f(dset_id, dt1_id, char_member_out, data_dims, error)
         CALL check("h5dread_f", error, total_error)
         do i = 1, dimsize
            if (char_member_out(i) .ne. char_member(i)) then
                write(*,*) " Wrong character data is read back "
                total_error = total_error + 1
            endif
         enddo
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sizei, dt5_id, error)
         CALL check("h5tcreate_f", error, total_error)
     offset = 0
     CALL h5tinsert_f(dt5_id, "integer_field", offset, H5T_NATIVE_INTEGER, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     ! Read part of the dataset
     !
     CALL h5dread_f(dset_id, dt5_id, int_member_out, data_dims, error)
         CALL check("h5dread_f", error, total_error)
         do i = 1, dimsize
            if (int_member_out(i) .ne. int_member(i)) then
                write(*,*) " Wrong integer data is read back "
                total_error = total_error + 1
            endif
         enddo
     !
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sized, dt3_id, error)
         CALL check("h5tcreate_f", error, total_error)
     offset = 0
     CALL h5tinsert_f(dt3_id, "double_field", offset, H5T_NATIVE_DOUBLE, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     ! Read part of the dataset
     !
     CALL h5dread_f(dset_id, dt3_id, double_member_out, data_dims, error)
         CALL check("h5dread_f", error, total_error)
         do i = 1, dimsize
            if (double_member_out(i) .ne. double_member(i)) then
                write(*,*) " Wrong double precision data is read back "
                total_error = total_error + 1
            endif
         enddo
     !
     !
     CALL h5tcreate_f(H5T_COMPOUND_F, type_sizer, dt4_id, error)
         CALL check("h5tcreate_f", error, total_error)
     offset = 0
     CALL h5tinsert_f(dt4_id, "real_field", offset, H5T_NATIVE_REAL, error)
         CALL check("h5tinsert_f", error, total_error)
     !
     ! Read part of the dataset
     !
     CALL h5dread_f(dset_id, dt4_id, real_member_out, data_dims, error)
         CALL check("h5dread_f", error, total_error)
         do i = 1, dimsize
            if (real_member_out(i) .ne. real_member(i)) then
                write(*,*) " Wrong real precision data is read back "
                total_error = total_error + 1
            endif
         enddo
     !
     ! *-----------------------------------------------------------------------
     ! * Test encoding and decoding compound datatypes
     ! *-----------------------------------------------------------------------
     !
     !    /* Encode compound type in a buffer */
     !         -- First find the buffer size

     CALL H5Tencode_f(dtype_id, cmpd_buf, cmpd_buf_size, error)
     CALL check("H5Tencode_f", error, total_error)

     ! /* Try decoding bogus buffer */

     CALL H5Tdecode_f(cmpd_buf, decoded_tid1, error)
     CALL VERIFY("H5Tdecode_f", error, -1, total_error)

     CALL H5Tencode_f(dtype_id, cmpd_buf, cmpd_buf_size, error)
     CALL check("H5Tencode_f", error, total_error)

     ! /* Decode from the compound buffer and return an object handle */
     CALL H5Tdecode_f(cmpd_buf, decoded_tid1, error)
     CALL check("H5Tdecode_f", error, total_error)

     ! /* Verify that the datatype was copied exactly */

     CALL H5Tequal_f(decoded_tid1, dtype_id, flag, error)
     CALL check("H5Tequal_f", error, total_error)
     CALL VerifyLogical("H5Tequal_f", flag, .TRUE., total_error)
     !
     ! Close all open objects.
     !
     CALL h5dclose_f(dset_id, error)
         CALL check("h5dclose_f", error, total_error)
     CALL h5tclose_f(dt1_id, error)
         CALL check("h5tclose_f", error, total_error)
     CALL h5tclose_f(dt2_id, error)
         CALL check("h5tclose_f", error, total_error)
     CALL h5tclose_f(dt3_id, error)
         CALL check("h5tclose_f", error, total_error)
     CALL h5tclose_f(dt4_id, error)
         CALL check("h5tclose_f", error, total_error)
     CALL h5tclose_f(dt5_id, error)
         CALL check("h5tclose_f", error, total_error)
     CALL h5fclose_f(file_id, error)
         CALL check("h5fclose_f", error, total_error)

          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
     RETURN
     END SUBROUTINE compoundtest




    SUBROUTINE basic_data_type_test(cleanup, total_error)

!   This subroutine tests following functionalities:
!   H5tget_precision_f, H5tset_precision_f, H5tget_offset_f
!   H5tset_offset_f, H5tget_pad_f, H5tset_pad_f, H5tget_sign_f,
!   H5tset_sign_f, H5tget_ebias_f,H5tset_ebias_f, H5tget_norm_f,
!   H5tset_norm_f, H5tget_inpad_f, H5tset_inpad_f, H5tget_cset_f,
!   H5tset_cset_f, H5tget_strpad_f, H5tset_strpad_f

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(OUT) :: total_error

     INTEGER(HID_T) :: dtype1_id, dtype2_id, dtype3_id, dtype4_id, dtype5_id
                                     ! datatype identifiers
     INTEGER(SIZE_T) :: precision ! Datatype precision
     INTEGER(SIZE_T) :: setprecision ! Datatype precision
     INTEGER(SIZE_T) :: offset ! Datatype offset
     INTEGER(SIZE_T) :: setoffset ! Datatype offset
     INTEGER :: lsbpad !padding type of the least significant bit
     INTEGER :: msbpad !padding type of the most significant bit
     INTEGER :: sign !sign type for an integer type
     INTEGER(SIZE_T) :: ebias1 !Datatype exponent bias of a floating-point type
     INTEGER(SIZE_T) :: ebias2 !Datatype exponent bias of a floating-point type
     INTEGER(SIZE_T) :: setebias
     INTEGER :: norm   !mantissa normalization of a floating-point datatype
     INTEGER :: inpad   !padding type for unused bits in floating-point datatypes.
     INTEGER :: cset   !character set type of a string datatype
     INTEGER :: strpad !string padding method for a string datatype
     INTEGER :: error !error flag


     !
     ! Create a datatype
     !
     CALL h5tcopy_f(H5T_STD_U16BE, dtype1_id, error)
     CALL check("h5tcopy_f",error,total_error)
     !
     !datatype type_id should be modifiable after h5tcopy_f
     !
     setprecision = 12
     CALL h5tset_precision_f(dtype1_id, setprecision, error)
     CALL check("h5set_precision_f",error,total_error)
     CALL h5tget_precision_f(dtype1_id,precision, error)
     CALL check("h5get_precision_f",error,total_error)
     if(precision .ne. 12) then
         write (*,*) "got precision is not correct"
         total_error = total_error + 1
     end if

     CALL h5tcopy_f(H5T_STD_I32LE, dtype2_id, error)
     CALL check("h5tcopy_f",error,total_error)
     setprecision = 12
     CALL h5tset_precision_f(dtype2_id, setprecision, error)
     CALL check("h5set_precision_f",error,total_error)

     setoffset = 2
     CALL h5tset_offset_f(dtype1_id, setoffset, error)
     CALL check("h5set_offset_f",error,total_error)
     setoffset = 10
     CALL h5tset_offset_f(dtype2_id, setoffset, error)
     CALL check("h5set_offset_f",error,total_error)
     CALL h5tget_offset_f(dtype2_id,offset, error)
     CALL check("h5get_offset_f",error,total_error)
     if(offset .ne. 10) then
         write (*,*) "got offset is not correct"
         total_error = total_error + 1
     end if

     CALL h5tset_pad_f(dtype2_id,H5T_PAD_ONE_F, H5T_PAD_ONE_F, error)
     CALL check("h5set_pad_f",error,total_error)
     CALL h5tget_pad_f(dtype2_id,lsbpad,msbpad, error)
     CALL check("h5get_pad_f",error,total_error)
     if((lsbpad .ne. H5T_PAD_ONE_F) .and. (msbpad .ne. H5T_PAD_ONE_F)) then
         write (*,*) "got pad is not correct"
         total_error = total_error + 1
     end if

!     CALL h5tset_sign_f(dtype2_id,H5T_SGN_2_F, error)
!     CALL check("h5set_sign_f",error,total_error)
!     CALL h5tget_sign_f(dtype2_id,sign, error)
     CALL h5tget_sign_f(H5T_NATIVE_INTEGER, sign, error)
     CALL check("h5tget_sign_f",error,total_error)
     if(sign .ne. H5T_SGN_2_F ) then
         write (*,*) "got sign is not correct"
         total_error = total_error + 1
     end if

     CALL h5tcopy_f(H5T_IEEE_F64BE, dtype3_id, error)
     CALL check("h5tcopy_f",error,total_error)
     CALL h5tcopy_f(H5T_IEEE_F32LE, dtype4_id, error)
     CALL check("h5tcopy_f",error,total_error)

     setebias = 257
     CALL h5tset_ebias_f(dtype3_id, setebias, error)
     CALL check("h5tset_ebias_f",error,total_error)
     setebias = 1
     CALL h5tset_ebias_f(dtype4_id, setebias, error)
     CALL check("h5tset_ebias_f",error,total_error)
     CALL h5tget_ebias_f(dtype3_id, ebias1, error)
     CALL check("h5tget_ebias_f",error,total_error)
     if(ebias1 .ne. 257 ) then
         write (*,*) "got ebias is not correct"
         total_error = total_error + 1
     end if
     CALL h5tget_ebias_f(dtype4_id, ebias2, error)
     CALL check("h5tget_ebias_f",error,total_error)
     if(ebias2 .ne. 1 ) then
         write (*,*) "got ebias is not correct"
         total_error = total_error + 1
     end if

     !attention:
     !It seems that I can't use H5T_NORM_IMPLIED_F to set the norm value
     !because I got error for the get_norm function
!     CALL h5tset_norm_f(dtype3_id,H5T_NORM_IMPLIED_F , error)
!     CALL check("h5tset_norm_f",error,total_error)
!     CALL h5tget_norm_f(dtype3_id, norm, error)
!     CALL check("h5tget_norm_f",error,total_error)
!     if(norm .ne. H5T_NORM_IMPLIED_F ) then
!         write (*,*) "got norm is not correct"
!         total_error = total_error + 1
!     end if
     CALL h5tset_norm_f(dtype3_id, H5T_NORM_MSBSET_F , error)
     CALL check("h5tset_norm_f",error,total_error)
     CALL h5tget_norm_f(dtype3_id, norm, error)
     CALL check("h5tget_norm_f",error,total_error)
     if(norm .ne. H5T_NORM_MSBSET_F ) then
         write (*,*) "got norm is not correct"
         total_error = total_error + 1
     end if

     CALL h5tset_norm_f(dtype3_id, H5T_NORM_NONE_F , error)
     CALL check("h5tset_norm_f",error,total_error)
     CALL h5tget_norm_f(dtype3_id, norm, error)
     CALL check("h5tget_norm_f",error,total_error)
     if(norm .ne. H5T_NORM_NONE_F ) then
         write (*,*) "got norm is not correct"
         total_error = total_error + 1
    end if

     CALL h5tset_inpad_f(dtype3_id, H5T_PAD_ZERO_F , error)
     CALL check("h5tset_inpad_f",error,total_error)
     CALL h5tget_inpad_f(dtype3_id, inpad , error)
     CALL check("h5tget_inpad_f",error,total_error)
     if(inpad .ne. H5T_PAD_ZERO_F ) then
         write (*,*) "got inpad is not correct"
         total_error = total_error + 1
     end if

     CALL h5tset_inpad_f(dtype3_id,H5T_PAD_ONE_F  , error)
     CALL check("h5tset_inpad_f",error,total_error)
     CALL h5tget_inpad_f(dtype3_id, inpad , error)
     CALL check("h5tget_inpad_f",error,total_error)
     if(inpad .ne. H5T_PAD_ONE_F ) then
         write (*,*) "got inpad is not correct"
         total_error = total_error + 1
     end if

     CALL h5tset_inpad_f(dtype3_id,H5T_PAD_BACKGROUND_F  , error)
     CALL check("h5tset_inpad_f",error,total_error)
     CALL h5tget_inpad_f(dtype3_id, inpad , error)
     CALL check("h5tget_inpad_f",error,total_error)
     if(inpad .ne. H5T_PAD_BACKGROUND_F ) then
         write (*,*) "got inpad is not correct"
         total_error = total_error + 1
     end if

!     we should not apply h5tset_cset_f to non_character data typemake

!     CALL h5tset_cset_f(dtype4_id, H5T_CSET_ASCII_F, error)
!     CALL check("h5tset_cset_f",error,total_error)
!     CALL h5tget_cset_f(dtype4_id, cset, error)
!     CALL check("h5tget_cset_f",error,total_error)
!     if(cset .ne. H5T_CSET_ASCII_F ) then
!         write (*,*) "got cset is not correct"
!         total_error = total_error + 1
!     end if

     CALL h5tcopy_f(H5T_NATIVE_CHARACTER, dtype5_id, error)
     CALL check("h5tcopy_f",error,total_error)
     CALL h5tset_cset_f(dtype5_id, H5T_CSET_ASCII_F, error)
     CALL check("h5tset_cset_f",error,total_error)
     CALL h5tget_cset_f(dtype5_id, cset, error)
     CALL check("h5tget_cset_f",error,total_error)
     if(cset .ne. H5T_CSET_ASCII_F ) then
         write (*,*) "got cset is not correct"
         total_error = total_error + 1
     end if
     CALL h5tset_strpad_f(dtype5_id, H5T_STR_NULLPAD_F, error)
     CALL check("h5tset_strpad_f",error,total_error)
     CALL h5tget_strpad_f(dtype5_id, strpad, error)
     CALL check("h5tget_strpad_f",error,total_error)
     if(strpad .ne. H5T_STR_NULLPAD_F ) then
         write (*,*) "got strpad is not correct"
         total_error = total_error + 1
     end if

     CALL h5tset_strpad_f(dtype5_id, H5T_STR_SPACEPAD_F, error)
     CALL check("h5tset_strpad_f",error,total_error)
     CALL h5tget_strpad_f(dtype5_id, strpad, error)
     CALL check("h5tget_strpad_f",error,total_error)
     if(strpad .ne. H5T_STR_SPACEPAD_F ) then
         write (*,*) "got strpad is not correct"
         total_error = total_error + 1
     end if

    CALL h5tclose_f(dtype1_id, error)
    CALL check("h5tclose_f", error, total_error)
    CALL h5tclose_f(dtype2_id, error)
    CALL check("h5tclose_f", error, total_error)
    CALL h5tclose_f(dtype3_id, error)
    CALL check("h5tclose_f", error, total_error)
    CALL h5tclose_f(dtype4_id, error)
    CALL check("h5tclose_f", error, total_error)
    CALL h5tclose_f(dtype5_id, error)
    CALL check("h5tclose_f", error, total_error)


     RETURN
     END SUBROUTINE basic_data_type_test

    SUBROUTINE enumtest(cleanup, total_error)

    USE HDF5
    IMPLICIT NONE

    LOGICAL, INTENT(IN)  :: cleanup
    INTEGER, INTENT(OUT) :: total_error
    CHARACTER(LEN=4), PARAMETER :: filename="enum"
    CHARACTER(LEN=80) :: fix_filename
    CHARACTER(LEN=8), PARAMETER :: dsetname="enumdset"
    CHARACTER(LEN=4)            :: true ="TRUE"
    CHARACTER(LEN=5)            :: false="FALSE"
    CHARACTER(LEN=5)            :: mem_name

    INTEGER(HID_T) :: file_id
    INTEGER(HID_T) :: dset_id
    INTEGER(HID_T) :: dspace_id
    INTEGER(HID_T) :: dtype_id, dtype, native_type
    INTEGER        :: error
    INTEGER        :: value
    INTEGER(HSIZE_T), DIMENSION(1) :: dsize
    INTEGER(SIZE_T) :: buf_size
    INTEGER, DIMENSION(2) :: data
    INTEGER(HSIZE_T), DIMENSION(7) :: dims
    INTEGER :: order1, order2
    INTEGER(SIZE_T) :: type_size1, type_size2
    INTEGER :: class

    dims(1) = 2
    dsize(1) = 2
    data(1) = 1
    data(2) = 0
     !
     ! Create a new file using default properties.
     !
    CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
    IF (error .NE. 0) THEN
       WRITE(*,*) "Cannot modify filename"
       STOP
    ENDIF
    CALL h5fcreate_f(fix_filename,H5F_ACC_TRUNC_F,file_id,error)
    CALL check("h5fcreate_f", error, total_error)
    !
    ! Create enumeration datatype with tow values
    !
    CALL h5tenum_create_f(H5T_NATIVE_INTEGER,dtype_id,error)
    CALL check("h5tenum_create_f", error, total_error)
    CALL h5tenum_insert_f(dtype_id,true,DATA(1),error)
    CALL check("h5tenum_insert_f", error, total_error)
    CALL h5tenum_insert_f(dtype_id,false,DATA(2),error)
    CALL check("h5tenum_insert_f", error, total_error)
    !
    ! Create write  and close a dataset with enum datatype
    !
    CALL h5screate_simple_f(1,dsize,dspace_id,error)
    CALL check("h5screate_simple_f", error, total_error)
    CALL h5dcreate_f(file_id,dsetname,dtype_id,dspace_id,dset_id,error)
    CALL check("h5dcreate_f", error, total_error)
    CALL h5dwrite_f(dset_id,dtype_id,DATA,dims,error)
    CALL check("h5dwrite_f", error, total_error)

    CALL H5Dget_type_f(dset_id, dtype, error)
    CALL check("H5Dget_type_f", error, total_error)

    CALL H5Tget_native_type_f(dtype, H5T_DIR_ASCEND_F, native_type, error)
    CALL check("H5Tget_native_type_f",error, total_error)

    !/* Verify the datatype retrieved and converted */
    CALL H5Tget_order_f(native_type, order1, error)
    CALL check("H5Tget_order_f",error, total_error)
    CALL H5Tget_order_f(H5T_NATIVE_INTEGER, order2, error)
    CALL check("H5Tget_order_f",error, total_error)
    CALL VERIFY("H5Tget_native_type_f",order1, order2, total_error)

    ! this test depends on whether -i8 was specified

!!$    CALL H5Tget_size_f(native_type, type_size1, error)
!!$    CALL check("H5Tget_size_f",error, total_error)
!!$    CALL H5Tget_size_f(H5T_STD_I32BE, type_size2, error)
!!$    CALL check("H5Tget_size_f",error, total_error)
!!$    CALL VERIFY("H5Tget_native_type_f", INT(type_size1), INT(type_size2), total_error)

    CALL H5Tget_class_f(native_type, class, error)
    CALL check("H5Tget_class_f",error, total_error)
    CALL VERIFY("H5Tget_native_type_f", INT(class), INT(H5T_ENUM_F), total_error)

    CALL h5dclose_f(dset_id,error)
    CALL check("h5dclose_f", error, total_error)
    CALL h5sclose_f(dspace_id,error)
    CALL check("h5sclose_f", error, total_error)
    !
    ! Get value of "TRUE"
    !
    CALL h5tenum_valueof_f(dtype_id, "TRUE", value, error)
    CALL check("h5tenum_valueof_f", error, total_error)
    IF (value .NE. 1) THEN
       WRITE(*,*) " Value of TRUE is not 1, error"
       total_error = total_error + 1
    ENDIF
    !
    !  Get name of 0
    !
    value = 0
    buf_size = 5
    CALL h5tenum_nameof_f(dtype_id,  value, buf_size, mem_name, error)
    CALL check("h5tenum_nameof_f", error, total_error)
    IF (mem_name .NE. "FALSE") THEN
       WRITE(*,*) " Wrong name for 0 value"
       total_error = total_error + 1
    ENDIF

    CALL h5tclose_f(dtype_id,error)
    CALL check("h5tclose_f", error, total_error)
    CALL h5fclose_f(file_id,error)
    CALL check("h5fclose_f", error, total_error)

    IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)

    RETURN
  END SUBROUTINE enumtest


!/*-------------------------------------------------------------------------
! * Function:    test_derived_flt
! *
! * Purpose:     Tests user-define and query functions of floating-point types.
! *              test h5tget/set_fields_f.
! *
! * Return:      Success:        0
! *
! *              Failure:        number of errors
! *
! * Fortran Programmer:  M.S. Breitenfeld
! *                      September 9, 2008
! *
! * Modifications:
! *
! *-------------------------------------------------------------------------
! */

SUBROUTINE test_derived_flt(cleanup, total_error)

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(OUT) :: total_error
  INTEGER(hid_t) :: file=-1, tid1=-1, tid2=-1
  INTEGER(hid_t) :: dxpl_id=-1
  INTEGER(size_t) :: spos, epos, esize, mpos, msize, size

  CHARACTER(LEN=15), PARAMETER :: filename="h5t_derived_flt"
  CHARACTER(LEN=80) :: fix_filename

  INTEGER(SIZE_T) :: precision1, offset1, ebias1, size1
  INTEGER(SIZE_T) :: precision2, offset2, ebias2, size2

  INTEGER :: error

  !/* Create File */
  CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
  IF (error .NE. 0) THEN
     WRITE(*,*) "Cannot modify filename"
     STOP
  ENDIF

  CALL h5fcreate_f(fix_filename,H5F_ACC_TRUNC_F,file,error)
  CALL check("h5fcreate_f", error, total_error)

  CALL h5pcreate_f(H5P_DATASET_XFER_F, dxpl_id, error)
  CALL check("h5pcreate_f", error, total_error)

  CALL h5tcopy_f(H5T_IEEE_F64LE, tid1, error)
  CALL check("h5tcopy_f",error,total_error)

  CALL h5tcopy_f(H5T_IEEE_F32LE, tid2, error)
  CALL check("h5tcopy_f",error,total_error)

  !/*------------------------------------------------------------------------
  ! *                   1st floating-point type
  ! * size=7 byte, precision=42 bits, offset=3 bits, mantissa size=31 bits,
  ! * mantissa position=3, exponent size=10 bits, exponent position=34,
  ! * exponent bias=511.  It can be illustrated in little-endian order as
  ! *
  ! *          6       5       4       3       2       1       0
  ! *    ???????? ???SEEEE EEEEEEMM MMMMMMMM MMMMMMMM MMMMMMMM MMMMM???
  ! *
  ! * To create a new floating-point type, the following properties must be
  ! * set in the order of
  ! *   set fields -> set offset -> set precision -> set size.
  ! * All these properties must be set before the type can function. Other
  ! * properties can be set anytime.  Derived type size cannot be expanded
  ! * bigger than original size but can be decreased.  There should be no
  ! * holes among the significant bits.  Exponent bias usually is set
  ! * 2^(n-1)-1, where n is the exponent size.
  ! *-----------------------------------------------------------------------*/

  CALL H5Tset_fields_f(tid1, INT(44,size_t), INT(34,size_t), INT(10,size_t), &
       INT(3,size_t), INT(31,size_t), error)
  CALL check("H5Tset_fields_f",error,total_error)

  CALL H5Tset_offset_f(tid1, INT(3,size_t), error)
  CALL check("H5Tset_offset_f",error,total_error)

  CALL H5Tset_precision_f(tid1, INT(42,size_t), error)
  CALL check("H5Tset_precision_f",error,total_error)

  CALL H5Tset_size_f(tid1, INT(7,size_t), error)
  CALL check("H5Tset_size_f",error,total_error)

  CALL H5Tset_ebias_f(tid1, INT(511,size_t), error)
  CALL check("H5Tset_ebias_f",error,total_error)

  CALL H5Tset_pad_f(tid1, H5T_PAD_ZERO_F, H5T_PAD_ZERO_F, error)
  CALL check("H5Tset_pad_f",error,total_error)

  CALL h5tcommit_f(file, "new float type 1", tid1, error)
  CALL check("h5tcommit_f", error, total_error)

  CALL h5tclose_f(tid1, error)
  CALL check("h5tclose_f", error, total_error)

  CALL H5Topen_f(file, "new float type 1", tid1, error)
  CALL check("H5Topen_f", error, total_error)

  CALL H5Tget_fields_f(tid1, spos, epos, esize, mpos, msize, error)
  CALL check("H5Tget_fields_f", error, total_error)

  IF(spos.NE.44 .OR. epos.NE.34 .OR. esize.NE.10 .OR. mpos.NE.3 .OR. msize.NE.31)THEN
     CALL VERIFY("H5Tget_fields_f", -1, 0, total_error)
  ENDIF

  CALL H5Tget_precision_f(tid1, precision1, error)
  CALL check("H5Tget_precision_f", error, total_error)
  CALL VERIFY("H5Tget_precision_f", INT(precision1), 42, total_error)

  CALL H5Tget_offset_f(tid1, offset1, error)
  CALL check("H5Tget_offset_f", error, total_error)
  CALL VERIFY("H5Tget_offset_f", INT(offset1), 3, total_error)

  CALL H5Tget_size_f(tid1, size1, error)
  CALL check("H5Tget_size_f", error, total_error)
  CALL VERIFY("H5Tget_size_f", INT(size1), 7, total_error)

  CALL H5Tget_ebias_f(tid1, ebias1, error)
  CALL check("H5Tget_ebias_f", error, total_error)
  CALL VERIFY("H5Tget_ebias_f", INT(ebias1), 511, total_error)

  !/*--------------------------------------------------------------------------
  ! *                   2nd floating-point type
  ! * size=3 byte, precision=24 bits, offset=0 bits, mantissa size=16 bits,
  ! * mantissa position=0, exponent size=7 bits, exponent position=16, exponent
  ! * bias=63. It can be illustrated in little-endian order as
  ! *
  ! *          2       1       0
  ! *    SEEEEEEE MMMMMMMM MMMMMMMM
  ! *--------------------------------------------------------------------------*/

  CALL H5Tset_fields_f(tid2, INT(23,size_t), INT(16,size_t), INT(7,size_t), &
       INT(0,size_t), INT(16,size_t), error)
  CALL check("H5Tset_fields_f",error,total_error)

  CALL H5Tset_offset_f(tid2, INT(0,size_t), error)
  CALL check("H5Tset_offset_f",error,total_error)

  CALL H5Tset_precision_f(tid2, INT(24,size_t), error)
  CALL check("H5Tset_precision_f",error,total_error)

  CALL H5Tset_size_f(tid2, INT(3,size_t), error)
  CALL check("H5Tset_size_f",error,total_error)

  CALL H5Tset_ebias_f(tid2, INT(63,size_t), error)
  CALL check("H5Tset_ebias_f",error,total_error)

  CALL H5Tset_pad_f(tid2, H5T_PAD_ZERO_F, H5T_PAD_ZERO_F, error)
  CALL check("H5Tset_pad_f",error,total_error)

  CALL h5tcommit_f(file, "new float type 2", tid2, error)
  CALL check("h5tcommit_f", error, total_error)

  CALL h5tclose_f(tid2, error)
  CALL check("h5tclose_f", error, total_error)

  CALL H5Topen_f(file, "new float type 2", tid2, error)
  CALL check("H5Topen_f", error, total_error)

  CALL H5Tget_fields_f(tid2, spos, epos, esize, mpos, msize, error)
  CALL check("H5Tget_fields_f", error, total_error)

  IF(spos.NE.23 .OR. epos.NE.16 .OR. esize.NE.7 .OR. mpos.NE.0 .OR. msize.NE.16)THEN
     CALL VERIFY("H5Tget_fields_f", -1, 0, total_error)
  ENDIF

  CALL H5Tget_precision_f(tid2, precision2, error)
  CALL check("H5Tget_precision_f", error, total_error)
  CALL VERIFY("H5Tget_precision_f", INT(precision2), 24, total_error)

  CALL H5Tget_offset_f(tid2, offset2, error)
  CALL check("H5Tget_offset_f", error, total_error)
  CALL VERIFY("H5Tget_offset_f", INT(offset2), 0, total_error)

  CALL H5Tget_size_f(tid2, size2, error)
  CALL check("H5Tget_size_f", error, total_error)
  CALL VERIFY("H5Tget_size_f", INT(size2), 3, total_error)

  CALL H5Tget_ebias_f(tid2, ebias2, error)
  CALL check("H5Tget_ebias_f", error, total_error)
  CALL VERIFY("H5Tget_ebias_f", INT(ebias2), 63, total_error)

  CALL h5tclose_f(tid1, error)
  CALL check("h5tclose_f", error, total_error)

  CALL h5tclose_f(tid2, error)
  CALL check("h5tclose_f", error, total_error)

  CALL H5Pclose_f(dxpl_id, error)
  CALL check("H5Pclose_f", error, total_error)

  CALL h5fclose_f(file,error)
  CALL check("h5fclose_f", error, total_error)

  IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)

END SUBROUTINE test_derived_flt
