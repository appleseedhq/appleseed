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
SUBROUTINE attribute_test_1_8(cleanup, total_error)

!   This subroutine tests following 1.8 functionalities:
!   h5acreate_f,  h5awrite_f, h5aclose_f,h5aread_f, h5aopen_name_f,
!   h5aget_name_f,h5aget_space_f, h5aget_type_f, H5Pset_shared_mesg_nindexes_f,
!   H5Pset_shared_mesg_index_f
!

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  CHARACTER(LEN=5), PARAMETER :: filename = "atest"    !File name
  CHARACTER(LEN=9), PARAMETER :: dsetname = "atestdset"        !Dataset name
  CHARACTER(LEN=11), PARAMETER :: aname = "attr_string"   !String Attribute name
  CHARACTER(LEN=14), PARAMETER :: aname2 = "attr_character"!Character Attribute name
  CHARACTER(LEN=11), PARAMETER :: aname3 = "attr_double"   !DOuble Attribute name
  CHARACTER(LEN=9), PARAMETER :: aname4 = "attr_real"      !Real Attribute name
  CHARACTER(LEN=12), PARAMETER :: aname5 = "attr_integer"  !Integer Attribute name
  CHARACTER(LEN=9), PARAMETER :: aname6 = "attr_null"     !Null Attribute name

  !
  !data space rank and dimensions
  !
  INTEGER, PARAMETER :: RANK = 2
  INTEGER, PARAMETER :: NX = 4
  INTEGER, PARAMETER :: NY = 5

  !
  !general purpose integer
  !
  INTEGER     ::   i, j
  INTEGER     ::   error ! Error flag

  ! NEW STARTS HERE
  INTEGER(HID_T) :: fapl = -1, fapl2 = -1
  INTEGER(HID_T) :: fcpl = -1, fcpl2 = -1
  INTEGER(HID_T) :: my_fapl, my_fcpl
  LOGICAL, DIMENSION(1:2) :: new_format = (/.TRUE.,.FALSE./)
  LOGICAL, DIMENSION(1:2) :: use_shared = (/.TRUE.,.FALSE./)

  INTEGER :: ret_total_error

! ********************
! test_attr equivelent
! ********************

!  WRITE(*,*) "TESTING ATTRIBUTES"

  CALL H5Pcreate_f(H5P_FILE_ACCESS_F,fapl,error)
  CALL check("h5Pcreate_f",error,total_error)
  CALL h5pcopy_f(fapl, fapl2, error)
  CALL check("h5pcopy_f",error,total_error)

  CALL H5Pcreate_f(H5P_FILE_CREATE_F,fcpl,error)
  CALL check("h5Pcreate_f",error,total_error)

  CALL h5pcopy_f(fcpl, fcpl2, error)
  CALL check("h5pcopy_f",error,total_error)

  CALL H5Pset_shared_mesg_nindexes_f(fcpl2,1,error)
  CALL check("H5Pset_shared_mesg_nindexes_f",error, total_error)

  CALL H5Pset_shared_mesg_index_f(fcpl2, 0, H5O_SHMESG_ATTR_FLAG_F, 1, error)
  CALL check(" H5Pset_shared_mesg_index_f",error, total_error)

  DO i = 1, 2

     IF (new_format(i)) THEN
        WRITE(*,'(1X,A)') "Testing with new file format:"
        my_fapl = fapl2
     ELSE
        WRITE(*,'(1X,A)') "Testing with old file format:"
        my_fapl = fapl
     END IF
     ret_total_error = 0
     CALL test_attr_basic_write(my_fapl, ret_total_error)
     CALL write_test_status(ret_total_error, &
          '  - Tests INT attributes on both datasets and groups', &
          total_error)

!!$        CALL test_attr_basic_read(my_fapl)
!!$        CALL test_attr_flush(my_fapl)
!!$        CALL test_attr_plist(my_fapl) ! this is next
!!$        CALL test_attr_compound_write(my_fapl)
!!$        CALL test_attr_compound_read(my_fapl)
!!$        CALL test_attr_scalar_write(my_fapl)
!!$        CALL test_attr_scalar_read(my_fapl)
!!$        CALL test_attr_mult_write(my_fapl)
!!$        CALL test_attr_mult_read(my_fapl)
!!$        CALL test_attr_iterate(my_fapl)
!!$        CALL test_attr_delete(my_fapl)
!!$        CALL test_attr_dtype_shared(my_fapl)
     IF(new_format(i)) THEN
        DO j = 1, 2
           IF (use_shared(j)) THEN
              WRITE(*,*) " - Testing with shared attributes:"
              my_fcpl = fcpl2
           ELSE
              WRITE(*,*) " - Testing without shared attributes:"
              my_fcpl = fcpl
           END IF
!!$              CALL test_attr_dense_create(my_fcpl, my_fapl)

           ret_total_error = 0
           CALL test_attr_dense_open(my_fcpl, my_fapl, ret_total_error)
           CALL write_test_status(ret_total_error, &
                '   - Testing INT attributes on both datasets and groups', &
                total_error)

!!$              CALL test_attr_dense_delete(my_fcpl, my_fapl)
!!$              CALL test_attr_dense_rename(my_fcpl, my_fapl)
!!$              CALL test_attr_dense_unlink(my_fcpl, my_fapl)
!!$              CALL test_attr_dense_limits(my_fcpl, my_fapl)
!!$              CALL test_attr_big(my_fcpl, my_fapl)
           ret_total_error = 0
           CALL test_attr_null_space(my_fcpl, my_fapl, ret_total_error)
           CALL write_test_status(ret_total_error, &
                '   - Testing storing attribute with "null" dataspace', &
                total_error)
!!$              CALL test_attr_deprec(fcpl, my_fapl)
           ret_total_error = 0
           CALL test_attr_many(new_format(i), my_fcpl, my_fapl, ret_total_error)
           CALL write_test_status(ret_total_error, &
                '   - Testing storing lots of attributes', &
                total_error)

           ret_total_error = 0
           CALL test_attr_corder_create_basic(my_fcpl, my_fapl, ret_total_error)
           CALL write_test_status(ret_total_error, &
                '   - Testing creating objects with attribute creation order', &
                total_error)

           ret_total_error = 0
           CALL test_attr_corder_create_compact(my_fcpl, my_fapl, ret_total_error)
           CALL write_test_status(ret_total_error, &
                '   - Testing compact storage on objects with attribute creation order', &
                total_error)
!!$              CALL test_attr_corder_create_dense(my_fcpl, my_fapl)
!!$              CALL test_attr_corder_create_reopen(my_fcpl, my_fapl)
!!$              CALL test_attr_corder_transition(my_fcpl, my_fapl)
!!$              CALL test_attr_corder_delete(my_fcpl, my_fapl)
           ret_total_error = 0
           CALL test_attr_info_by_idx(new_format(i), my_fcpl, my_fapl, ret_total_error)
           CALL write_test_status(ret_total_error, &
                '   - Testing querying attribute info by index', &
                total_error)

           ret_total_error = 0
           CALL test_attr_delete_by_idx(new_format(i), my_fcpl, my_fapl, ret_total_error)
           CALL write_test_status(ret_total_error, &
                '   - Testing deleting attribute by index', &
                total_error)

!!$              CALL test_attr_iterate2(new_format, my_fcpl, my_fapl)
!!$              CALL test_attr_open_by_idx(new_format, my_fcpl, my_fapl)
!!$              CALL test_attr_open_by_name(new_format, my_fcpl, my_fapl)
           ret_total_error = 0
           CALL test_attr_create_by_name(new_format(i), my_fcpl, my_fapl, ret_total_error)
           CALL write_test_status(ret_total_error, &
                '   - Testing creating attributes by name', &
                total_error)

             ! /* More complex tests with both "new format" and "shared" attributes */
           IF( use_shared(j) ) THEN
!!$                 CALL test_attr_shared_write(my_fcpl, my_fapl)
              ret_total_error = 0
              CALL test_attr_shared_rename(my_fcpl, my_fapl, ret_total_error)
              CALL write_test_status(ret_total_error,&
                   '   - Testing renaming shared attributes in "compact" & "dense" storage', &
                   total_error)

              ret_total_error = 0
              CALL test_attr_shared_delete(my_fcpl, my_fapl, ret_total_error)
              CALL write_test_status(ret_total_error,&
                   '   - Testing deleting shared attributes in "compact" & "dense" storage', &
                   total_error)


!!$                 CALL test_attr_shared_unlink(my_fcpl, my_fapl)
           END IF
!!$              CALL test_attr_bug1(my_fcpl, my_fapl)
        END DO
!!$        ELSE
!!$           CALL test_attr_big(fcpl, my_fapl)
!!$           CALL test_attr_null_space(fcpl, my_fapl)
!!$           CALL test_attr_deprec(fcpl, my_fapl)
!!$           CALL test_attr_many(new_format, fcpl, my_fapl)
!!$           CALL test_attr_info_by_idx(new_format, fcpl, my_fapl)
!!$           CALL test_attr_delete_by_idx(new_format, fcpl, my_fapl)
!!$           CALL test_attr_iterate2(new_format, fcpl, my_fapl)
!!$           CALL test_attr_open_by_idx(new_format, fcpl, my_fapl)
!!$           CALL test_attr_open_by_name(new_format, fcpl, my_fapl)
!!$           CALL test_attr_create_by_name(new_format, fcpl, my_fapl)
!!$           CALL test_attr_bug1(fcpl, my_fapl)

     END IF
  ENDDO

  CALL H5Pclose_f(fcpl, error)
  CALL CHECK("H5Pclose", error,total_error)
  CALL H5Pclose_f(fcpl2, error)
  CALL CHECK("H5Pclose", error,total_error)

  IF(cleanup) CALL h5_cleanup_f("tattr", H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)


  RETURN
END SUBROUTINE attribute_test_1_8

SUBROUTINE test_attr_corder_create_compact(fcpl,fapl, total_error)

!/****************************************************************
!**
!**  test_attr_corder_create_compact(): Test basic H5A (attribute) code.
!**      Tests compact attribute storage on objects with attribute creation order info
!**
!****************************************************************/

! Needed for get_info_by_name

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
! - - - arg types - - -

  INTEGER(HID_T), INTENT(IN) :: fcpl
  INTEGER(HID_T), INTENT(IN) :: fapl

  CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid
  INTEGER(HID_T) :: dcpl
  INTEGER(HID_T) :: sid

  INTEGER :: error
  INTEGER, INTENT(INOUT) :: total_error

  CHARACTER(LEN=8) :: DSET1_NAME = "Dataset1"
  CHARACTER(LEN=8) :: DSET2_NAME = "Dataset2"
  CHARACTER(LEN=8) :: DSET3_NAME = "Dataset3"
  INTEGER, PARAMETER :: NUM_DSETS = 3

  INTEGER :: curr_dset

  INTEGER(HID_T) :: dset1, dset2, dset3
  INTEGER(HID_T) :: my_dataset

  INTEGER :: u

  INTEGER :: max_compact ! Maximum # of links to store in group compactly
  INTEGER :: min_dense   ! Minimum # of links to store in group "densely"

  CHARACTER(LEN=7) :: attrname
  CHARACTER(LEN=2) :: chr2
  INTEGER(HID_T) :: attr        !String Attribute identifier
  INTEGER(HSIZE_T), DIMENSION(7) :: data_dims

  LOGICAL :: f_corder_valid ! Indicates whether the the creation order data is valid for this attribute
  INTEGER :: corder ! Is a positive integer containing the creation order of the attribute
  INTEGER :: cset ! Indicates the character set used for the attribute’s name
  INTEGER(HSIZE_T) :: data_size   ! indicates the size, in the number of characters

  data_dims = 0

!  WRITE(*,*) "     - Testing Compact Storage of Attributes with Creation Order Info"
  ! /* Create file */
  CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, fcpl, fapl)
  CALL check("h5fcreate_f",error,total_error)
  ! /* Create dataset creation property list */
  CALL H5Pcreate_f(H5P_DATASET_CREATE_F,dcpl,error)
  CALL check("h5Pcreate_f",error,total_error)

  CALL H5Pset_attr_creation_order_f(dcpl, IOR(H5P_CRT_ORDER_TRACKED_F, H5P_CRT_ORDER_INDEXED_F), error)
  CALL check("H5Pset_attr_creation_order",error,total_error)

  ! /* Query the attribute creation properties */
  CALL H5Pget_attr_phase_change_f(dcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f",error,total_error)

  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)

  CALL h5dcreate_f(fid, DSET1_NAME, H5T_NATIVE_CHARACTER, sid, dset1, error, dcpl_id=dcpl )
  CALL check("h5dcreate_f",error,total_error)

  CALL h5dcreate_f(fid, DSET2_NAME, H5T_NATIVE_CHARACTER, sid, dset2, error, dcpl )
  CALL check("h5dcreate_f",error,total_error)

  CALL h5dcreate_f(fid, DSET3_NAME, H5T_NATIVE_CHARACTER, sid, dset3, error, dcpl_id=dcpl )
  CALL check("h5dcreate_f",error,total_error)

  DO curr_dset = 0,NUM_DSETS-1
     SELECT CASE (curr_dset)
     CASE (0)
        my_dataset = dset1
     CASE (1)
        my_dataset = dset2
     CASE (2)
        my_dataset = dset3
!     CASE DEFAULT
!        CALL HDassert(0.AND."Toomanydatasets!")
     END SELECT
!!$    is_empty = H5O_is_attr_empty_test(my_dataset)
!!$    CALL VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test")
!!$    is_dense = H5O_is_attr_dense_test(my_dataset)
!!$    CALL VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test")
    DO u = 0, max_compact - 1
       ! /* Create attribute */
       WRITE(chr2,'(I2.2)') u
       attrname = 'attr '//chr2

       CALL h5acreate_f(my_dataset, attrname, H5T_NATIVE_INTEGER, sid, attr, error)
       CALL check("h5acreate_f",error,total_error)

       data_dims(1) = 1
       CALL h5awrite_f(attr, H5T_NATIVE_INTEGER, u, data_dims, error)
       CALL check("h5awrite_f",error,total_error)

       CALL h5aclose_f(attr, error)
       CALL check("h5aclose_f",error,total_error)

!!$      ret = H5O_num_attrs_test(my_dataset, nattrs)
!!$      CALL CHECK(ret, FAIL, "H5O_num_attrs_test")
!!$      CALL VERIFY(nattrs, (u + 1))
!!$      is_empty = H5O_is_attr_empty_test(my_dataset)
!!$      CALL VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test")
!!$      is_dense = H5O_is_attr_dense_test(my_dataset)
!!$      CALL VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test")
    END DO
  END DO

  !  /* Close Datasets */
  CALL h5dclose_f(dset1, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5dclose_f(dset2, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5dclose_f(dset3, error)
  CALL check("h5dclose_f",error,total_error)

  !   /* Close file */
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)

  ! /* Close dataspace */
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f",error,total_error)

  ! /* Close property list */
  CALL h5pclose_f(dcpl, error)
  CALL check("h5pclose_f",error,total_error)

  CALL h5fopen_f(FileName, H5F_ACC_RDWR_F, fid, error, fapl)
  CALL check("h5open_f",error,total_error)

  CALL h5dopen_f(fid, DSET1_NAME, dset1, error)
  CALL check("h5dopen_f",error,total_error)
  CALL h5dopen_f(fid, DSET2_NAME, dset2, error)
  CALL check("h5dopen_f",error,total_error)
  CALL h5dopen_f(fid, DSET3_NAME, dset3, error)
  CALL check("h5dopen_f",error,total_error)
  DO curr_dset = 0,NUM_DSETS-1
     SELECT CASE (curr_dset)
     CASE (0)
        my_dataset = dset1
     CASE (1)
        my_dataset = dset2
     CASE (2)
        my_dataset = dset3
     CASE DEFAULT
        WRITE(*,*) " WARNING: To many data sets! "
     END SELECT
!!$    ret = H5O_num_attrs_test(my_dataset, nattrs)
!!$    CALL CHECK(ret, FAIL, "H5O_num_attrs_test")
!!$    CALL VERIFY(nattrs, max_compact, "H5O_num_attrs_test")
!!$    is_empty = H5O_is_attr_empty_test(my_dataset)
!!$    CALL VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test")
!!$    is_dense = H5O_is_attr_dense_test(my_dataset)
!!$    CALL VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test")

     DO u = 0,max_compact-1
        WRITE(chr2,'(I2.2)') u
        attrname = 'attr '//chr2
        ! /* Retrieve information for attribute */

        CALL H5Aget_info_by_name_f(my_dataset, ".", attrname, &
             f_corder_valid, corder, cset, data_size, error, lapl_id = H5P_DEFAULT_F ) !with optional

        CALL check("H5Aget_info_by_name_f", error, total_error)

        ! /* Verify creation order of attribute */

        CALL verifyLogical("H5Aget_info_by_name_f", f_corder_valid, .TRUE., total_error)
        CALL verify("H5Aget_info_by_name_f", corder, u, total_error)


        ! /* Retrieve information for attribute */

        CALL H5Aget_info_by_name_f(my_dataset, ".", attrname, &
             f_corder_valid, corder, cset, data_size, error) ! without optional

        CALL check("H5Aget_info_by_name_f", error, total_error)

        ! /* Verify creation order of attribute */

        CALL verifyLogical("H5Aget_info_by_name_f", f_corder_valid, .TRUE., total_error)
        CALL verify("H5Aget_info_by_name_f", corder, u, total_error)

     END DO
  END DO
  !  /* Close Datasets */
  CALL h5dclose_f(dset1, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5dclose_f(dset2, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5dclose_f(dset3, error)
  CALL check("h5dclose_f",error,total_error)

  !   /* Close file */
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)

END SUBROUTINE test_attr_corder_create_compact

SUBROUTINE test_attr_null_space(fcpl, fapl, total_error)
!/****************************************************************
!**
!**  test_attr_null_space(): Test basic H5A (attribute) code.
!**      Tests storing attribute with "null" dataspace
!**
!****************************************************************/
  USE HDF5
  IMPLICIT NONE

  INTEGER(HID_T), INTENT(IN) :: fcpl
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error

  CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid
  INTEGER(HID_T) :: sid, null_sid
  INTEGER(HID_T) :: dataset

  CHARACTER(LEN=8) :: DSET1_NAME = "Dataset1"
  INTEGER, PARAMETER :: NUM_DSETS = 3


  INTEGER :: error

  INTEGER :: value_scalar
  INTEGER, DIMENSION(1) :: value
  INTEGER(HID_T) :: attr        !String Attribute identifier
  INTEGER(HID_T) :: attr_sid
  INTEGER(HSIZE_T), DIMENSION(7) :: data_dims

  INTEGER(HSIZE_T) :: storage_size   ! attributes storage requirements

  LOGICAL :: f_corder_valid ! Indicates whether the the creation order data is valid for this attribute
  INTEGER :: corder ! Is a positive integer containing the creation order of the attribute
  INTEGER :: cset ! Indicates the character set used for the attribute’s name
  INTEGER(HSIZE_T) :: data_size   ! indicates the size, in the number of characters

  LOGICAL :: equal

  ! test: H5Sextent_equal_f

  data_dims = 0

  ! /* Output message about test being performed */
!  WRITE(*,*) "     - Testing Storing Attributes with 'null' dataspace"
  ! /* Create file */
  CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, fcpl, fapl)
  CALL check("h5fcreate_f",error,total_error)
  ! /* Close file */
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)

  ! /* Re-open file */
  CALL h5fopen_f(FileName, H5F_ACC_RDWR_F, fid, error)
  CALL check("h5open_f",error,total_error)
  ! /* Create dataspace for dataset attributes */
  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)
  ! /* Create "null" dataspace for attribute */
  CALL h5screate_f(H5S_NULL_F, null_sid, error)
  CALL check("h5screate_f",error,total_error)
  ! /* Create a dataset */
  CALL h5dcreate_f(fid, DSET1_NAME, H5T_NATIVE_CHARACTER, sid, dataset, error)
  CALL check("h5dcreate_f",error,total_error)
  ! /* Add attribute with 'null' dataspace */

  ! /* Create attribute */
  CALL h5acreate_f(dataset, "null attr", H5T_NATIVE_INTEGER, null_sid, attr, error)
  CALL check("h5acreate_f",error,total_error)

  ! /* Try to read data from the attribute */
  ! /* (shouldn't fail, but should leave buffer alone) */
  value(1) = 103
  CALL h5aread_f(attr, H5T_NATIVE_INTEGER, value, data_dims, error)
  CALL check("h5aread_f",error,total_error)
  CALL verify("h5aread_f",value(1),103,total_error)

! /* Try to read data from the attribute again but*/
! /* for a scalar */

  value_scalar = 104
  data_dims(1) = 1
  CALL h5aread_f(attr, H5T_NATIVE_INTEGER, value_scalar, data_dims, error)
  CALL check("h5aread_f",error,total_error)
  CALL verify("h5aread_f",value_scalar,104,total_error)

  CALL h5aget_space_f(attr, attr_sid, error)
  CALL check("h5aget_space_f",error,total_error)

  CALL H5Sextent_equal_f(attr_sid, null_sid, equal, error)
  CALL check("H5Sextent_equal_f",error,total_error)
  CALL Verifylogical("H5Sextent_equal_f",equal,.TRUE.,total_error)

!!$  ret = H5Sclose(attr_sid)
!!$  CALL CHECK(ret, FAIL, "H5Sclose")

  CALL h5aget_storage_size_f(attr, storage_size, error)
  CALL check("h5aget_storage_size_f",error,total_error)
  CALL VERIFY("h5aget_storage_size_f",INT(storage_size),0,total_error)

  CALL h5aget_info_f(attr, f_corder_valid, corder, cset, data_size,  error)
  CALL check("h5aget_info_f", error, total_error)

  ! /* Check the attribute's information */
  CALL VERIFY("h5aget_info_f.corder",corder,0,total_error)

  CALL VERIFY("h5aget_info_f.cset", cset, H5T_CSET_ASCII_F, total_error)
  CALL h5aget_storage_size_f(attr, storage_size, error)
  CALL check("h5aget_storage_size_f",error,total_error)
  CALL VERIFY("h5aget_info_f.data_size", INT(data_size), INT(storage_size), total_error)
  CALL h5aclose_f(attr,error)
  CALL check("h5aclose_f",error,total_error)

  CALL H5Sclose_f(attr_sid, error)
  CALL check("H5Sclose_f",error,total_error)

  CALL H5Dclose_f(dataset, error)
  CALL check("H5Dclose_f", error,total_error)


  CALL H5Fclose_f(fid, error)
  CALL check("H5Fclose_f", error,total_error)

  CALL H5Sclose_f(sid, error)
  CALL check("H5Sclose_f", error,total_error)

  CALL H5Sclose_f(null_sid, error)
  CALL check("H5Sclose_f", error,total_error)

END SUBROUTINE test_attr_null_space


SUBROUTINE test_attr_create_by_name(new_format,fcpl,fapl, total_error)

!/****************************************************************
!**
!**  test_attr_create_by_name(): Test basic H5A (attribute) code.
!**      Tests creating attributes by name
!**
!****************************************************************/

  USE HDF5

  IMPLICIT NONE

  INTEGER(SIZE_T), PARAMETER :: NAME_BUF_SIZE = 7
  LOGICAL :: new_format
  INTEGER(HID_T), INTENT(IN) :: fcpl
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER :: max_compact,min_dense,u
  CHARACTER (LEN=NAME_BUF_SIZE) :: attrname
  CHARACTER(LEN=8) :: dsetname

  CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid
  INTEGER(HID_T) :: dcpl
  INTEGER(HID_T) :: sid

  CHARACTER(LEN=8) :: DSET1_NAME = "Dataset1"
  CHARACTER(LEN=8) :: DSET2_NAME = "Dataset2"
  CHARACTER(LEN=8) :: DSET3_NAME = "Dataset3"
  INTEGER, PARAMETER :: NUM_DSETS = 3

  INTEGER :: curr_dset

  INTEGER(HID_T) :: dset1, dset2, dset3
  INTEGER(HID_T) :: my_dataset
  INTEGER :: error

  INTEGER(HID_T) :: attr        !String Attribute identifier
  INTEGER(HSIZE_T), DIMENSION(7) :: data_dims


  CHARACTER(LEN=2) :: chr2
  LOGICAL, DIMENSION(1:2) :: use_index = (/.FALSE.,.TRUE./)
  INTEGER :: Input1
  INTEGER :: i

  data_dims = 0


  ! /* Create dataspace for dataset & attributes */
  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)

  ! /* Create dataset creation property list */
  CALL H5Pcreate_f(H5P_DATASET_CREATE_F,dcpl,error)
  CALL check("h5Pcreate_f",error,total_error)

  ! /* Query the attribute creation properties */

  CALL H5Pget_attr_phase_change_f(dcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f",error,total_error)

  ! /* Loop over using index for creation order value */
  DO i = 1, 2
     ! /* Print appropriate test message */
!!$     IF(use_index(i))THEN
!!$        WRITE(*,*) "       - Testing Creating Attributes By Name w/Creation Order Index"
!!$     ELSE
!!$        WRITE(*,*) "       - Testing Creating Attributes By Name w/o Creation Order Index"
!!$     ENDIF
     ! /* Create file */
     CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, fcpl, fapl)
     CALL check("h5fcreate_f",error,total_error)

     ! /* Set attribute creation order tracking & indexing for object */
     IF(new_format)THEN

        IF(use_index(i))THEN
           Input1 = H5P_CRT_ORDER_INDEXED_F
        ELSE
           Input1 = 0
        ENDIF

        CALL H5Pset_attr_creation_order_f(dcpl, IOR(H5P_CRT_ORDER_TRACKED_F, Input1), error)
        CALL check("H5Pset_attr_creation_order",error,total_error)

     ENDIF

     ! /* Create datasets */

     CALL h5dcreate_f(fid, DSET1_NAME, H5T_NATIVE_CHARACTER, sid, dset1, error, dcpl_id=dcpl )
     CALL check("h5dcreate_f2",error,total_error)

     CALL h5dcreate_f(fid, DSET2_NAME, H5T_NATIVE_CHARACTER, sid, dset2, error, dcpl_id=dcpl )
     CALL check("h5dcreate_f3",error,total_error)

     CALL h5dcreate_f(fid, DSET3_NAME, H5T_NATIVE_CHARACTER, sid, dset3, error, dcpl_id=dcpl )
     CALL check("h5dcreate_f4",error,total_error)


     ! /* Work on all the datasets */

     DO curr_dset = 0,NUM_DSETS-1
        SELECT CASE (curr_dset)
        CASE (0)
           my_dataset = dset1
           dsetname = DSET1_NAME
        CASE (1)
           my_dataset = dset2
           dsetname = DSET2_NAME
        CASE (2)
           my_dataset = dset3
           dsetname = DSET3_NAME
           !     CASE DEFAULT
           !        CALL HDassert(0.AND."Toomanydatasets!")
        END SELECT

        ! /* Check on dataset's attribute storage status */
!!$            is_empty = H5O_is_attr_empty_test(my_dataset);
!!$            VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
!!$            is_dense = H5O_is_attr_dense_test(my_dataset);
!!$            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        !/* Create attributes, up to limit of compact form */

        DO u = 0, max_compact - 1
           ! /* Create attribute */
           WRITE(chr2,'(I2.2)') u
           attrname = 'attr '//chr2
           CALL H5Acreate_by_name_f(fid, dsetname, attrname, H5T_NATIVE_INTEGER, sid, &
                attr, error, lapl_id=H5P_DEFAULT_F, acpl_id=H5P_DEFAULT_F, aapl_id=H5P_DEFAULT_F)
           CALL check("H5Acreate_by_name_f",error,total_error)

           ! /* Write data into the attribute */

           data_dims(1) = 1
           CALL h5awrite_f(attr, H5T_NATIVE_INTEGER, u, data_dims, error)
           CALL check("h5awrite_f",error,total_error)

           ! /* Close attribute */
           CALL h5aclose_f(attr, error)
           CALL check("h5aclose_f",error,total_error)

           ! /* Verify information for NEW attribute */
           CALL attr_info_by_idx_check(my_dataset, attrname, INT(u,HSIZE_T), use_index(i), total_error)
         !   CALL check("FAILED IN attr_info_by_idx_check",total_error)
        ENDDO

        ! /* Verify state of object */
!!$            ret = H5O_num_attrs_test(my_dataset, &nattrs);
!!$            CHECK(ret, FAIL, "H5O_num_attrs_test");
!!$            VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
!!$            is_empty = H5O_is_attr_empty_test(my_dataset);
!!$            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
!!$            is_dense = H5O_is_attr_dense_test(my_dataset);
!!$            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        ! /* Test opening attributes stored compactly */

        CALL attr_open_check(fid, dsetname, my_dataset, u, total_error)

     ENDDO


     ! /* Work on all the datasets */
     DO curr_dset = 0,NUM_DSETS-1
        SELECT CASE (curr_dset)
        CASE (0)
           my_dataset = dset1
           dsetname = DSET1_NAME
        CASE (1)
           my_dataset = dset2
           dsetname = DSET2_NAME
        CASE (2)
           my_dataset = dset3
           dsetname = DSET3_NAME
        END SELECT

        ! /* Create more attributes, to push into dense form */
        DO u = max_compact, max_compact* 2 - 1

           WRITE(chr2,'(I2.2)') u
           attrname = 'attr '//chr2

           CALL H5Acreate_by_name_f(fid, dsetname, attrname, H5T_NATIVE_INTEGER, sid, &
                attr, error, lapl_id=H5P_DEFAULT_F)
           CALL check("H5Acreate_by_name",error,total_error)

           ! /* Write data into the attribute */
           data_dims(1) = 1
           CALL h5awrite_f(attr, H5T_NATIVE_INTEGER, u, data_dims, error)
           CALL check("h5awrite_f",error,total_error)

           ! /* Close attribute */
           CALL h5aclose_f(attr, error)
           CALL check("h5aclose_f",error,total_error)

           ! /* Verify state of object */
!!$                if(u >= max_compact) {
!!$                    is_dense = H5O_is_attr_dense_test(my_dataset);
!!$                    VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");
!!$                } /* end if */
!!$
!!$                /* Verify information for new attribute */
!!$                ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
!!$                CHECK(ret, FAIL, "attr_info_by_idx_check");
        ENDDO

        ! /* Verify state of object */
!!$            ret = H5O_num_attrs_test(my_dataset, &nattrs);
!!$            CHECK(ret, FAIL, "H5O_num_attrs_test");
!!$            VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
!!$            is_empty = H5O_is_attr_empty_test(my_dataset);
!!$            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
!!$            is_dense = H5O_is_attr_dense_test(my_dataset);
!!$            VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");

!!$            if(new_format) {
!!$                /* Retrieve & verify # of records in the name & creation order indices */
!!$                ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
!!$                CHECK(ret, FAIL, "H5O_attr_dense_info_test");
!!$                if(use_index)
!!$                    VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
!!$                VERIFY(name_count, (max_compact * 2), "H5O_attr_dense_info_test");
!!$            } /* end if */

!!$            /* Test opening attributes stored compactly */
!!$            ret = attr_open_check(fid, dsetname, my_dataset, u);
!!$            CHECK(ret, FAIL, "attr_open_check");

     ENDDO

     ! /* Close Datasets */
     CALL h5dclose_f(dset1, error)
     CALL check("h5dclose_f",error,total_error)
     CALL h5dclose_f(dset2, error)
     CALL check("h5dclose_f",error,total_error)
     CALL h5dclose_f(dset3, error)
     CALL check("h5dclose_f",error,total_error)


     ! /* Close file */
     CALL h5fclose_f(fid, error)
     CALL check("h5fclose_f",error,total_error)
  ENDDO

  ! /* Close property list */
  CALL h5pclose_f(dcpl, error)
  CALL check("h5pclose_f",error,total_error)

  ! /* Close dataspace */
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f",error,total_error)

END SUBROUTINE test_attr_create_by_name


SUBROUTINE test_attr_info_by_idx(new_format, fcpl, fapl, total_error)

!/****************************************************************
!**
!**  test_attr_info_by_idx(): Test basic H5A (attribute) code.
!**      Tests querying attribute info by index
!**
!****************************************************************/

  USE HDF5

  IMPLICIT NONE

  LOGICAL :: new_format
  INTEGER(HID_T), INTENT(IN) :: fcpl
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error
  CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid
  INTEGER(HID_T) :: dcpl
  INTEGER(HID_T) :: sid

  CHARACTER(LEN=8) :: DSET1_NAME = "Dataset1"
  CHARACTER(LEN=8) :: DSET2_NAME = "Dataset2"
  CHARACTER(LEN=8) :: DSET3_NAME = "Dataset3"
  INTEGER, PARAMETER :: NUM_DSETS = 3

  INTEGER :: curr_dset

  INTEGER(HID_T) :: dset1, dset2, dset3
  INTEGER(HID_T) :: my_dataset
  INTEGER :: error

  INTEGER(HID_T) :: attr        !String Attribute identifier
  INTEGER(HSIZE_T), DIMENSION(7) :: data_dims

  LOGICAL :: f_corder_valid ! Indicates whether the the creation order data is valid for this attribute
  INTEGER :: corder ! Is a positive integer containing the creation order of the attribute
  INTEGER :: cset ! Indicates the character set used for the attribute’s name
  INTEGER(HSIZE_T) :: data_size   ! indicates the size, in the number of characters
  INTEGER(HSIZE_T) :: n
  LOGICAL, DIMENSION(1:2) :: use_index = (/.FALSE.,.TRUE./)

  INTEGER :: max_compact ! Maximum # of links to store in group compactly
  INTEGER :: min_dense   ! Minimum # of links to store in group "densely"

  CHARACTER(LEN=2) :: chr2

  INTEGER :: i, j

  INTEGER, DIMENSION(1) ::  attr_integer_data
  CHARACTER(LEN=7) :: attrname

  INTEGER(SIZE_T) :: size
  CHARACTER(LEN=80) :: tmpname

  INTEGER :: Input1
  INTEGER(HSIZE_T) :: hzero = 0_HSIZE_T
  INTEGER :: minusone = -1
  INTEGER(HSIZE_T) :: htmp

  data_dims = 0

  ! /* Create dataspace for dataset & attributes */

  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)


  ! /* Create dataset creation property list */

  CALL H5Pcreate_f(H5P_DATASET_CREATE_F,dcpl,error)
  CALL check("h5Pcreate_f",error,total_error)


  ! /* Query the attribute creation properties */
  CALL H5Pget_attr_phase_change_f(dcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f",error,total_error)

  !  /* Loop over using index for creation order value */

  DO i = 1, 2

     ! /* Output message about test being performed */
!!$     IF(use_index(i))THEN
!!$        WRITE(*,'(A72)') "       - Testing Querying Attribute Info By Index w/Creation Order Index"
!!$     ELSE
!!$        WRITE(*,'(A74)') "       - Testing Querying Attribute Info By Index w/o Creation Order Index"
!!$     ENDIF

     ! /* Create file */
     CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, fcpl, fapl)
     CALL check("h5fcreate_f",error,total_error)

     ! /* Set attribute creation order tracking & indexing for object */
     IF(new_format)THEN
        IF(use_index(i))THEN
           Input1 = H5P_CRT_ORDER_INDEXED_F
        ELSE
           Input1 = 0
        ENDIF
        CALL H5Pset_attr_creation_order_f(dcpl, IOR(H5P_CRT_ORDER_TRACKED_F, Input1), error)
        CALL check("H5Pset_attr_creation_order",error,total_error)
     ENDIF

     ! /* Create datasets */

     CALL h5dcreate_f(fid, DSET1_NAME, H5T_NATIVE_CHARACTER, sid, dset1, error )
     CALL check("h5dcreate_f",error,total_error)

     CALL h5dcreate_f(fid, DSET2_NAME, H5T_NATIVE_CHARACTER, sid, dset2, error )
     CALL check("h5dcreate_f",error,total_error)

     CALL h5dcreate_f(fid, DSET3_NAME, H5T_NATIVE_CHARACTER, sid, dset3, error )
     CALL check("h5dcreate_f",error,total_error)

     ! /* Work on all the datasets */

     DO curr_dset = 0,NUM_DSETS-1

        SELECT CASE (curr_dset)
        CASE (0)
           my_dataset = dset1
        CASE (1)
           my_dataset = dset2
        CASE (2)
           my_dataset = dset3
           !     CASE DEFAULT
           !        CALL HDassert(0.AND."Toomanydatasets!")
        END SELECT

        !/* Check on dataset's attribute storage status */
!!$            is_empty = H5O_is_attr_empty_test(my_dataset);
!!$            VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
!!$            is_dense = H5O_is_attr_dense_test(my_dataset);
!!$            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        ! /* Check for query on non-existant attribute */

        n = 0

        ! -- CHECK PASSING AN INTEGER CONSTANT IN DIFFERENT FORMS --

        ! 1) call by passing an integer with the _hsize_t declaration

        CALL h5aget_info_by_idx_f(my_dataset, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, 0_hsize_t, &
             f_corder_valid, corder, cset, data_size, error, lapl_id=H5P_DEFAULT_F)
        CALL VERIFY("h5aget_info_by_idx_f",error,minusone,total_error)

        ! 2) call by passing an integer with the INT(,hsize_t) declaration

        CALL h5aget_info_by_idx_f(my_dataset, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, INT(0,hsize_t), &
             f_corder_valid, corder, cset, data_size, error, lapl_id=H5P_DEFAULT_F)
        CALL VERIFY("h5aget_info_by_idx_f",error,minusone,total_error)


        ! 3) call by passing a variable with the attribute hsize_t

        CALL h5aget_info_by_idx_f(my_dataset, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, hzero, &
             f_corder_valid, corder, cset, data_size, error, lapl_id=H5P_DEFAULT_F)
        CALL VERIFY("h5aget_info_by_idx_f",error,minusone,total_error)

        CALL h5aget_name_by_idx_f(my_dataset, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, &
             hzero, tmpname,  error, size, lapl_id=H5P_DEFAULT_F)
        CALL VERIFY("h5aget_name_by_idx_f",error,minusone,total_error)


        ! /* Create attributes, up to limit of compact form */

        DO j = 0, max_compact-1
           ! /* Create attribute */
           WRITE(chr2,'(I2.2)') j
           attrname = 'attr '//chr2

           ! attr = H5Acreate2(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
           ! check with the optional information create2 specs.
           CALL h5acreate_f(my_dataset, attrname, H5T_NATIVE_INTEGER, sid, attr, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
           CALL check("h5acreate_f",error,total_error)

           ! /* Write data into the attribute */

           attr_integer_data(1) = j
           data_dims(1) = 1
           CALL h5awrite_f(attr, H5T_NATIVE_INTEGER, attr_integer_data, data_dims, error)
           CALL check("h5awrite_f",error,total_error)

           ! /* Close attribute */

           CALL h5aclose_f(attr, error)
           CALL check("h5aclose_f",error,total_error)

           ! /* Verify information for new attribute */

!EP        CALL attr_info_by_idx_check(my_dataset, attrname, INT(j,HSIZE_T), use_index(i), total_error )
           htmp = j
           CALL attr_info_by_idx_check(my_dataset, attrname, htmp, use_index(i), total_error )

           !CHECK(ret, FAIL, "attr_info_by_idx_check");
        ENDDO

     ENDDO


     !  /* Close Datasets */
     CALL h5dclose_f(dset1, error)
     CALL check("h5dclose_f",error,total_error)
     CALL h5dclose_f(dset2, error)
     CALL check("h5dclose_f",error,total_error)
     CALL h5dclose_f(dset3, error)
     CALL check("h5dclose_f",error,total_error)

     !   /* Close file */
     CALL h5fclose_f(fid, error)
     CALL check("h5fclose_f",error,total_error)

  END DO

  ! /* Close property list */
  CALL h5pclose_f(dcpl,error)
  CALL check("h5pclose_f", error, total_error)

  ! /* Close dataspace */
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f",error,total_error)

END SUBROUTINE test_attr_info_by_idx


SUBROUTINE attr_info_by_idx_check(obj_id, attrname, n, use_index, total_error )

  USE HDF5

  IMPLICIT NONE

  INTEGER :: error, total_error

  INTEGER(HID_T) :: obj_id
  CHARACTER(LEN=*) :: attrname
  INTEGER(HSIZE_T) :: n
  LOGICAL :: use_index
  LOGICAL :: f_corder_valid ! Indicates whether the the creation order data is valid for this attribute
  INTEGER :: corder ! Is a positive integer containing the creation order of the attribute
  INTEGER :: cset ! Indicates the character set used for the attribute’s name
  INTEGER(HSIZE_T) :: data_size   ! indicates the size, in the number of characters

  INTEGER(SIZE_T) :: NAME_BUF_SIZE = 7
  CHARACTER(LEN=7) :: tmpname
  INTEGER(HSIZE_T) :: hzero = 0_HSIZE_T


  ! /* Verify the information for first attribute, in increasing creation order */
  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, hzero, &
       f_corder_valid, corder, cset, data_size, error)

  CALL check("h5aget_info_by_idx_f",error,total_error)
  CALL verify("h5aget_info_by_idx_f",corder,0,total_error)
  ! /* Verify the information for new attribute, in increasing creation order */

  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, n, &
       f_corder_valid, corder, cset, data_size, error)

  CALL check("h5aget_info_by_idx_f",error,total_error)
  CALL VERIFY("h5aget_info_by_idx_f",corder,INT(n),total_error)

  ! /* Verify the name for new link, in increasing creation order */

  ! Try with the correct buffer size

  CALL h5aget_name_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, &
       n, tmpname, error, NAME_BUF_SIZE)
  CALL check("h5aget_name_by_idx_f",error,total_error)
  CALL VERIFY("h5aget_name_by_idx_f", INT(NAME_BUF_SIZE), 7, error)

  IF(attrname.NE.tmpname)THEN
     error = -1
  ENDIF
  CALL VERIFY("h5aget_name_by_idx_f",error,0,total_error)

  !  /* Don't test "native" order if there is no creation order index, since
  !   *  there's not a good way to easily predict the attribute's order in the name
  !   *  index.
  !   */
  IF (use_index) THEN
     ! /* Verify the information for first attribute, in native creation order */
     CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_NATIVE_F, hzero, &
          f_corder_valid, corder, cset, data_size, error)
     CALL check("h5aget_info_by_idx_f",error,total_error)
     CALL VERIFY("h5aget_info_by_idx_f",corder,0,total_error)

     ! /* Verify the information for new attribute, in native creation order */
     CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_NATIVE_F, n, &
          f_corder_valid, corder, cset, data_size, error)
     CALL check("h5aget_info_by_idx_f",error,total_error)
     CALL VERIFY("h5aget_info_by_idx_f",corder,INT(n),total_error)

   ! /* Verify the name for new link, in increasing native order */
     CALL h5aget_name_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_NATIVE_F, &
          n, tmpname, error) ! check with no optional parameters
     CALL check("h5aget_name_by_idx_f",error,total_error)
     IF(TRIM(attrname).NE.TRIM(tmpname))THEN
        WRITE(*,*) "ERROR: attribute name size wrong!"
        error = -1
     ENDIF
     CALL VERIFY("h5aget_name_by_idx_f",error,0,total_error)
  END IF


  ! CALL HDmemset(ainfo, 0, SIZEOF(ainfo)
  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_DEC_F, n, &
       f_corder_valid, corder, cset, data_size, error)
  CALL check("h5aget_info_by_idx_f",error,total_error)
  CALL VERIFY("h5aget_info_by_idx_f",corder,0,total_error)

!EP  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_DEC_F, 0_HSIZE_T, &

  ! -- CHECK PASSING AN INTEGER CONSTANT IN DIFFERENT FORMS --

  ! 1) call by passing an integer with the _hsize_t declaration

  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_DEC_F, 0_HSIZE_T, &
       f_corder_valid, corder, cset, data_size, error)
  CALL check("h5aget_info_by_idx_f",error,total_error)
  CALL VERIFY("h5aget_info_by_idx_f",corder,INT(n),total_error)

  ! 2) call by passing an integer with the INT(,hsize_t) declaration

  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_DEC_F, INT(0,HSIZE_T), &
       f_corder_valid, corder, cset, data_size, error)
  CALL check("h5aget_info_by_idx_f",error,total_error)
  CALL VERIFY("h5aget_info_by_idx_f",corder,INT(n),total_error)

  ! 3) call by passing a variable with the attribute hsize_t

  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_DEC_F, hzero, &
       f_corder_valid, corder, cset, data_size, error)
  CALL check("h5aget_info_by_idx_f",error,total_error)
  CALL VERIFY("h5aget_info_by_idx_f",corder,INT(n),total_error)

!EP  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_NAME_F, H5_ITER_INC_F, 0_HSIZE_T, &
  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_NAME_F, H5_ITER_INC_F, hzero, &
       f_corder_valid, corder, cset, data_size, error)
  CALL check("h5aget_info_by_idx_f",error,total_error)
  CALL VERIFY("h5aget_info_by_idx_f",corder,0,total_error)
  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_NAME_F, H5_ITER_INC_F, n, &
       f_corder_valid, corder, cset, data_size, error)
  CALL check("h5aget_info_by_idx_f",error,total_error)
  CALL VERIFY("h5aget_info_by_idx_f",corder,INT(n),total_error)
!!$    ret = H5Aget_name_by_idx(obj_id, ".", H5_INDEX_NAME, H5_ITER_INC, n, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT)
!!$    CALL CHECK(ret, FAIL, "H5Aget_name_by_idx")
!!$    IF (HDstrcmp(attrname, tmpname)) CALL TestErrPrintf("Line %d: attribute name size wrong!\n"C, __LINE__)
  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_NAME_F, H5_ITER_DEC_F, n, &
       f_corder_valid, corder, cset, data_size, error)
  CALL check("h5aget_info_by_idx_f",error,total_error)
  CALL VERIFY("h5aget_info_by_idx_f",corder,0,total_error)
!EP  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_NAME_F, H5_ITER_DEC_F, 0_HSIZE_T, &
  CALL h5aget_info_by_idx_f(obj_id, ".", H5_INDEX_NAME_F, H5_ITER_DEC_F, hzero, &
       f_corder_valid, corder, cset, data_size, error)
  CALL check("h5aget_info_by_idx_f",error,total_error)
  CALL VERIFY("h5aget_info_by_idx_f",corder,INT(n),total_error)
!!$    ret = H5Aget_name_by_idx(obj_id, ".", H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE, H5P_DEFAULT)
!!$    CALL CHECK(ret, FAIL, "H5Aget_name_by_idx")
!!$    IF (HDstrcmp(attrname, tmpname)) CALL TestErrPrintf("Line %d: attribute name size wrong!\n"C, __LINE__)

END SUBROUTINE attr_info_by_idx_check


SUBROUTINE test_attr_shared_rename( fcpl, fapl, total_error)

!/****************************************************************
!**
!**  test_attr_shared_rename(): Test basic H5A (attribute) code.
!**      Tests renaming shared attributes in "compact" & "dense" storage
!**
!****************************************************************/

  USE HDF5

  IMPLICIT NONE

  INTEGER(HID_T), INTENT(IN) :: fcpl
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error

    CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid
  INTEGER(HID_T) :: dcpl
  INTEGER(HID_T) :: sid, big_sid

  CHARACTER(LEN=8) :: DSET1_NAME = "Dataset1"
  CHARACTER(LEN=8) :: DSET2_NAME = "Dataset2"
  INTEGER, PARAMETER :: NUM_DSETS = 3


  INTEGER(HID_T) :: dataset, dataset2

  INTEGER :: error

  INTEGER(HID_T) :: attr        !String Attribute identifier
  INTEGER(HID_T) :: attr_tid
  INTEGER(HSIZE_T), DIMENSION(7) :: data_dims


  INTEGER :: max_compact ! Maximum # of links to store in group compactly
  INTEGER :: min_dense   ! Minimum # of links to store in group "densely"

  CHARACTER(LEN=2) :: chr2


  INTEGER, DIMENSION(1) ::  attr_integer_data
  CHARACTER(LEN=7) :: attrname
  CHARACTER(LEN=11) :: attrname2

  CHARACTER(LEN=1), PARAMETER :: chr1 = '.'

  INTEGER :: u
  INTEGER, PARAMETER :: SPACE1_RANK = 3
  INTEGER, PARAMETER :: NX = 20
  INTEGER, PARAMETER :: NY = 5
  INTEGER, PARAMETER :: NZ = 10
  INTEGER(HID_T) :: my_fcpl

  CHARACTER(LEN=5), PARAMETER :: TYPE1_NAME = "/Type"

  INTEGER, PARAMETER :: SPACE1_DIM1 = 4
  INTEGER, PARAMETER :: SPACE1_DIM2 = 8
  INTEGER, PARAMETER :: SPACE1_DIM3 = 10


  INTEGER :: test_shared
  INTEGER(HSIZE_T), DIMENSION(1) :: adims2 = (/1/) ! Attribute dimension
  INTEGER     ::   arank = 1                      ! Attribure rank

  ! /* Output message about test being performed */
!  WRITE(*,*) "     - Testing Renaming Shared & Unshared Attributes in Compact & Dense Storage"
!!$ /* Initialize "big" attribute data */

  ! /* Create dataspace for dataset */
  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)

  ! /* Create "big" dataspace for "large" attributes */

  CALL h5screate_simple_f(arank, adims2, big_sid, error)
  CALL check("h5screate_simple_f",error,total_error)

  ! /* Loop over type of shared components */
  DO test_shared = 0, 2
     ! /* Make copy of file creation property list */
     CALL H5Pcopy_f(fcpl, my_fcpl, error)
     CALL check("H5Pcopy",error,total_error)

     ! /* Set up datatype for attributes */

     CALL H5Tcopy_f(H5T_NATIVE_INTEGER, attr_tid, error)
     CALL check("H5Tcopy",error,total_error)

     ! /* Special setup for each type of shared components */

     IF( test_shared .EQ. 0) THEN
        ! /* Make attributes > 500 bytes shared */
        CALL H5Pset_shared_mesg_nindexes_f(my_fcpl,1,error)
        CALL check("H5Pset_shared_mesg_nindexes_f",error, total_error)
        CALL H5Pset_shared_mesg_index_f(my_fcpl, 0, H5O_SHMESG_ATTR_FLAG_F, 500,error)
        CALL check(" H5Pset_shared_mesg_index_f",error, total_error)

     ELSE
        ! /* Set up copy of file creation property list */
        CALL H5Pset_shared_mesg_nindexes_f(my_fcpl,3,error)

        ! /* Make attributes > 500 bytes shared */
        CALL H5Pset_shared_mesg_index_f(my_fcpl, 0, H5O_SHMESG_ATTR_FLAG_F, 500,error)
        ! /* Make datatypes & dataspaces > 1 byte shared (i.e. all of them :-) */
        CALL H5Pset_shared_mesg_index_f(my_fcpl, 1, H5O_SHMESG_DTYPE_FLAG_F, 1,error)
        CALL H5Pset_shared_mesg_index_f(my_fcpl, 2,  H5O_SHMESG_SDSPACE_FLAG_F, 1,error)
     ENDIF

     ! /* Create file */
     CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, my_fcpl, fapl)
     CALL check("h5fcreate_f",error,total_error)

     ! /* Close FCPL copy */
     CALL h5pclose_f(my_fcpl, error)
     CALL check("h5pclose_f", error, total_error)
     ! /* Close file */
     CALL h5fclose_f(fid, error)
     CALL check("h5fclose_f",error,total_error)

     ! /* Re-open file */
     CALL h5fopen_f(FileName, H5F_ACC_RDWR_F, fid, error,fapl)
     CALL check("h5open_f",error,total_error)

     ! /* Commit datatype to file */
     IF(test_shared.EQ.2) THEN
        CALL H5Tcommit_f(fid, TYPE1_NAME, attr_tid, error, H5P_DEFAULT_F, H5P_DEFAULT_F, H5P_DEFAULT_F)
        CALL check("H5Tcommit",error,total_error)
     ENDIF

     ! /* Set up to query the object creation properties */
     CALL H5Pcreate_f(H5P_DATASET_CREATE_F,dcpl,error)
     CALL check("h5Pcreate_f",error,total_error)

     ! /* Create datasets */
     CALL h5dcreate_f(fid, DSET1_NAME, H5T_NATIVE_CHARACTER, sid, dataset, error, dcpl_id=dcpl )
     CALL check("h5dcreate_f",error,total_error)
     CALL h5dcreate_f(fid, DSET2_NAME, H5T_NATIVE_CHARACTER, sid, dataset2, error, dcpl_id=dcpl )
     CALL check("h5dcreate_f",error,total_error)

     ! /* Check on dataset's message storage status */
!!$        if(test_shared != 0) {
!!$            /* Datasets' datatypes can be shared */
!!$            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
!!$            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
!!$
!!$            /* Datasets' dataspace can be shared */
!!$            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
!!$            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
!!$        } /* end if */

     ! /* Retrieve limits for compact/dense attribute storage */
     CALL H5Pget_attr_phase_change_f(dcpl, max_compact, min_dense, error)
     CALL check("H5Pget_attr_phase_change_f",error,total_error)

     ! /* Close property list */
     CALL h5pclose_f(dcpl,error)
     CALL check("h5pclose_f", error, total_error)
!!$
!!$
!!$        /* Check on datasets' attribute storage status */
!!$        is_dense = H5O_is_attr_dense_test(dataset);
!!$        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
!!$        is_dense = H5O_is_attr_dense_test(dataset2);
!!$        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
     ! /* Add attributes to each dataset, until after converting to dense storage */


     DO u = 0, (max_compact * 2) - 1

        ! /* Create attribute name */
        WRITE(chr2,'(I2.2)') u
        attrname = 'attr '//chr2

        ! /* Alternate between creating "small" & "big" attributes */

        IF(MOD(u+1,2).EQ.0)THEN
           ! /* Create "small" attribute on first dataset */

           CALL h5acreate_f(dataset, attrname, attr_tid, sid, attr, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
           CALL check("h5acreate_f",error,total_error)

!!$                /* Check that attribute is not shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

           ! /* Write data into the attribute */
           attr_integer_data(1) = u + 1
           data_dims(1) = 1
           CALL h5awrite_f(attr, attr_tid, attr_integer_data, data_dims, error)
           CALL check("h5awrite_f",error,total_error)
        ELSE
           !  Create "big" attribute on first dataset */

           CALL h5acreate_f(dataset, attrname, attr_tid, big_sid, attr, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
           CALL check("h5acreate_f",error,total_error)

           !  Check that attribute is shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

           !  Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
!!$
           !  Write data into the attribute */

           data_dims(1) = 1
           attr_integer_data(1) = u + 1
           CALL h5awrite_f(attr, attr_tid, attr_integer_data, data_dims, error)
           CALL check("h5awrite_f",error,total_error)

           !  Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
        ENDIF

        ! /* Close attribute */
        CALL h5aclose_f(attr, error)
        CALL check("h5aclose_f",error,total_error)

        ! /* Check on dataset's attribute storage status */
!!$            is_dense = H5O_is_attr_dense_test(dataset);
!!$            if(u < max_compact)
!!$                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
!!$            else
!!$                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");
!!$
!!$
        ! /* Alternate between creating "small" & "big" attributes */
        IF(MOD(u+1,2).EQ.0)THEN

           !  /* Create "small" attribute on second dataset */

           CALL h5acreate_f(dataset2, attrname, attr_tid, sid, attr, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
           CALL check("h5acreate_f",error,total_error)

           !  /* Check that attribute is not shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
!!$
           ! /* Write data into the attribute */

           attr_integer_data(1) = u + 1
           data_dims(1) = 1
           CALL h5awrite_f(attr, attr_tid, attr_integer_data, data_dims, error)
           CALL check("h5awrite_f",error,total_error)
        ELSE

           ! /* Create "big" attribute on second dataset */

           CALL h5acreate_f(dataset2, attrname, attr_tid, big_sid, attr, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
           CALL check("h5acreate_f",error,total_error)

! /* Check that attribute is shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, TRUE, "H5A_is_shared_test");
!!$
! /* Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
!!$
! /* Write data into the attribute */


           attr_integer_data(1) = u + 1
           data_dims(1) = 1
!           CALL h5awrite_f(attr,  attr_tid, attr_integer_data, data_dims, error)
!           CALL check("h5awrite_f",error,total_error)


! /* Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");

        ENDIF
        ! /* Close attribute */
        CALL h5aclose_f(attr, error)
        CALL check("h5aclose_f",error,total_error)

        !    /* Check on dataset's attribute storage status */
!!$            is_dense = H5O_is_attr_dense_test(dataset2);
!!$            if(u < max_compact)
!!$                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
!!$            else
!!$                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


        ! /* Create new attribute name */

        WRITE(chr2,'(I2.2)') u
        attrname2 = 'new attr '//chr2


        ! /* Change second dataset's attribute's name */

        CALL H5Arename_by_name_f(fid, DSET2_NAME, attrname, attrname2, error, lapl_id=H5P_DEFAULT_F)
        CALL check("H5Arename_by_name_f",error,total_error)

        ! /* Check refcount on attributes now */

        ! /* Check refcount on renamed attribute */

        CALL H5Aopen_f(dataset2, attrname2, attr, error, aapl_id=H5P_DEFAULT_F)
        CALL check("H5Aopen_f",error,total_error)

!!$
!!$        IF(MOD(u+1,2).EQ.0)THEN
!!$           ! /* Check that attribute is not shared */
!!$           is_shared = H5A_is_shared_test(attr);
!!$           CALL VERIFY("H5A_is_shared_test", error, minusone)
!!$        ELSE
!!$                ! /* Check that attribute is shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, TRUE, "H5A_is_shared_test");
!!$
!!$                /* Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test")
!!$             ENDIF

        ! /* Close attribute */
        CALL h5aclose_f(attr, error)
        CALL check("h5aclose_f",error,total_error)

        ! /* Check refcount on original attribute */
        CALL H5Aopen_f(dataset, attrname, attr, error)
        CALL check("H5Aopen",error,total_error)

!!$            if(u % 2) {
!!$                /* Check that attribute is not shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
!!$            } /* end if */
!!$            else {
!!$                /* Check that attribute is shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, TRUE, "H5A_is_shared_test");
!!$
!!$                /* Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
!!$            } /* end else */

        ! /* Close attribute */
        CALL h5aclose_f(attr, error)
        CALL check("h5aclose_f",error,total_error)


        ! /* Change second dataset's attribute's name back to original */

        CALL H5Arename_by_name_f(fid, DSET2_NAME, attrname2, attrname, error)
        CALL check("H5Arename_by_name_f",error,total_error)

        ! /* Check refcount on attributes now */

        ! /* Check refcount on renamed attribute */
        CALL H5Aopen_f(dataset2, attrname, attr, error)
        CALL check("H5Aopen",error,total_error)
!!$
!!$            if(u % 2) {
!!$                /* Check that attribute is not shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
!!$            } /* end if */
!!$            else {
!!$                /* Check that attribute is shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, TRUE, "H5A_is_shared_test");
!!$
!!$                /* Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
!!$            } /* end else */

        ! /* Close attribute */
        CALL h5aclose_f(attr, error)
        CALL check("h5aclose_f",error,total_error)

        ! /* Check refcount on original attribute */

        ! /* Check refcount on renamed attribute */
        CALL H5Aopen_f(dataset, attrname, attr, error)
        CALL check("H5Aopen",error,total_error)

!!$            if(u % 2) {
!!$                /* Check that attribute is not shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
!!$            } /* end if */
!!$            else {
!!$                /* Check that attribute is shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, TRUE, "H5A_is_shared_test");
!!$
!!$                /* Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
!!$            } /* end else */

        ! /* Close attribute */
        CALL h5aclose_f(attr, error)
        CALL check("h5aclose_f",error,total_error)

     ENDDO

     ! /* Close attribute's datatype */
     CALL h5tclose_f(attr_tid, error)
     CALL check("h5tclose_f",error,total_error)

     ! /* Close attribute's datatype */
     CALL h5dclose_f(dataset, error)
     CALL check("h5dclose_f",error,total_error)
     CALL h5dclose_f(dataset2, error)
     CALL check("h5dclose_f",error,total_error)

!!$        /* Check on shared message status now */
!!$        if(test_shared != 0) {
!!$            if(test_shared == 1) {
!!$                /* Check on datatype storage status */
!!$                ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
!!$                CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$                VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
!!$            } /* end if */
!!$
!!$            /* Check on dataspace storage status */
!!$            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
!!$            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$            VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
!!$        } /* end if */

     ! /* Unlink datasets with attributes */
     CALL H5Ldelete_f(fid, DSET1_NAME, error, H5P_DEFAULT_F)
     CALL check("HLdelete",error,total_error)
     CALL H5Ldelete_f(fid, DSET2_NAME, error)
     CALL check("HLdelete",error,total_error)

     !/* Unlink committed datatype */
     IF(test_shared == 2)THEN
        CALL H5Ldelete_f(fid, TYPE1_NAME, error)
        CALL check("HLdelete_f",error,total_error)
     ENDIF

     ! /* Check on attribute storage status */
!!$        ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
!!$        CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$        VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
!!$
!!$        if(test_shared != 0) {
!!$            /* Check on datatype storage status */
!!$            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
!!$            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
!!$
!!$            /* Check on dataspace storage status */
!!$            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
!!$            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
!!$        } /* end if */

     ! /* Close file */
     CALL h5fclose_f(fid, error)
     CALL check("h5fclose_f",error,total_error)

     ! /* Check size of file */
     !filesize = h5_get_file_size(FILENAME);
     !VERIFY(filesize, empty_filesize, "h5_get_file_size");
  ENDDO

  ! /* Close dataspaces */
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5sclose_f(big_sid, error)
  CALL check("h5sclose_f",error,total_error)

END SUBROUTINE test_attr_shared_rename


SUBROUTINE test_attr_delete_by_idx(new_format, fcpl, fapl, total_error)

!/****************************************************************
!**
!**  test_attr_delete_by_idx(): Test basic H5A (attribute) code.
!**      Tests deleting attribute by index
!**
!****************************************************************/

  USE HDF5

  IMPLICIT NONE

  LOGICAL, INTENT(IN) :: new_format
  INTEGER(HID_T), INTENT(IN) :: fcpl
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error
  CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid ! /* HDF5 File ID			*/
  INTEGER(HID_T) :: dcpl ! /* Dataset creation property list ID */
  INTEGER(HID_T) :: sid ! /* Dataspace ID			*/

  CHARACTER(LEN=8) :: DSET1_NAME = "Dataset1"
  CHARACTER(LEN=8) :: DSET2_NAME = "Dataset2"
  CHARACTER(LEN=8) :: DSET3_NAME = "Dataset3"
  INTEGER, PARAMETER :: NUM_DSETS = 3

  INTEGER :: curr_dset

  INTEGER(HID_T) :: dset1, dset2, dset3
  INTEGER(HID_T) :: my_dataset

  INTEGER :: error

  INTEGER(HID_T) :: attr        !String Attribute identifier
  INTEGER(HSIZE_T), DIMENSION(7) :: data_dims

  LOGICAL :: f_corder_valid ! Indicates whether the the creation order data is valid for this attribute
  INTEGER :: corder ! Is a positive integer containing the creation order of the attribute
  INTEGER :: cset ! Indicates the character set used for the attribute’s name
  INTEGER(HSIZE_T) :: data_size   ! indicates the size, in the number of characters
  LOGICAL, DIMENSION(1:2) :: use_index = (/.FALSE.,.TRUE./)

  INTEGER :: max_compact ! Maximum # of links to store in group compactly
  INTEGER :: min_dense   ! Minimum # of links to store in group "densely"

  CHARACTER(LEN=2) :: chr2

  INTEGER :: i

  INTEGER, DIMENSION(1) ::  attr_integer_data
  CHARACTER(LEN=7) :: attrname

  INTEGER(SIZE_T) :: size
  CHARACTER(LEN=8) :: tmpname
  CHARACTER(LEN=1), PARAMETER :: chr1 = '.'

  INTEGER :: idx_type
  INTEGER :: order
  INTEGER :: u     ! /* Local index variable */
  INTEGER :: Input1
  INTEGER(HSIZE_T) :: hzero = 0_HSIZE_T
  INTEGER :: minusone = -1

  data_dims = 0

  ! /* Create dataspace for dataset & attributes */
  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)

  !    /* Create dataset creation property list */
  CALL H5Pcreate_f(H5P_DATASET_CREATE_F,dcpl,error)
  CALL check("h5Pcreate_f",error,total_error)

  ! /* Query the attribute creation properties */
  CALL H5Pget_attr_phase_change_f(dcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f",error,total_error)


  !/* Loop over operating on different indices on link fields */
  DO idx_type = H5_INDEX_NAME_F, H5_INDEX_CRT_ORDER_F

     ! /* Loop over operating in different orders */
     DO order = H5_ITER_INC_F, H5_ITER_DEC_F

        ! /* Loop over using index for creation order value */
        DO i = 1, 2

           ! /* Print appropriate test message */
!!$           IF(idx_type .EQ. H5_INDEX_CRT_ORDER_F)THEN
!!$              IF(order .EQ. H5_ITER_INC_F) THEN
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(A102)') &
!!$                         "       - Testing Deleting Attribute By Creation Order Index in Increasing Order w/Creation Order Index"
!!$                 ELSE
!!$                    WRITE(*,'(A104)') &
!!$                         "       - Testing Deleting Attribute By Creation Order Index in Increasing Order w/o Creation Order Index"
!!$                 ENDIF
!!$              ELSE
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(A102)') &
!!$                         "       - Testing Deleting Attribute By Creation Order Index in Decreasing Order w/Creation Order Index"
!!$                 ELSE
!!$                    WRITE(*,'(A104)') &
!!$                         "       - Testing Deleting Attribute By Creation Order Index in Decreasing Order w/o Creation Order Index"
!!$                 ENDIF
!!$              ENDIF
!!$           ELSE
!!$              IF(order .EQ. H5_ITER_INC_F)THEN
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(7X,A86)')"- Testing Deleting Attribute By Name Index in Increasing Order w/Creation Order Index"
!!$                 ELSE
!!$                    WRITE(*,'(7X,A88)')"- Testing Deleting Attribute By Name Index in Increasing Order w/o Creation Order Index"
!!$                 ENDIF
!!$              ELSE
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(7X,A86)') "- Testing Deleting Attribute By Name Index in Decreasing Order w/Creation Order Index"
!!$                 ELSE
!!$                    WRITE(*,'(7X,A88)') "- Testing Deleting Attribute By Name Index in Decreasing Order w/o Creation Order Index"
!!$                 ENDIF
!!$              ENDIF
!!$           ENDIF

           ! /* Create file */
           CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, fcpl, fapl)
           CALL check("h5fcreate_f",error,total_error)

           !  /* Set attribute creation order tracking & indexing for object */
           IF(new_format)THEN

              IF(use_index(i))THEN
                 Input1 = H5P_CRT_ORDER_INDEXED_F
              ELSE
                 Input1 = 0
              ENDIF

              CALL H5Pset_attr_creation_order_f(dcpl, IOR(H5P_CRT_ORDER_TRACKED_F, Input1), error)
              CALL check("H5Pset_attr_creation_order",error,total_error)

           ENDIF

           ! /* Create datasets */

           CALL h5dcreate_f(fid, DSET1_NAME, H5T_NATIVE_CHARACTER, sid, dset1, error, dcpl )
           CALL check("h5dcreate_f2",error,total_error)

           CALL h5dcreate_f(fid, DSET2_NAME, H5T_NATIVE_CHARACTER, sid, dset2, error, dcpl )
           CALL check("h5dcreate_f3",error,total_error)

           CALL h5dcreate_f(fid, DSET3_NAME, H5T_NATIVE_CHARACTER, sid, dset3, error, dcpl )
           CALL check("h5dcreate_f4",error,total_error)

           !   /* Work on all the datasets */

           DO curr_dset = 0,NUM_DSETS-1
              SELECT CASE (curr_dset)
              CASE (0)
                 my_dataset = dset1
              CASE (1)
                 my_dataset = dset2
              CASE (2)
                 my_dataset = dset3
                 !     CASE DEFAULT
                 !        CALL HDassert(0.AND."Toomanydatasets!")
              END SELECT

              ! /* Check on dataset's attribute storage status */
!!$                is_empty = H5O_is_attr_empty_test(my_dataset);
!!$                VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
!!$                is_dense = H5O_is_attr_dense_test(my_dataset);
!!$                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

              ! /* Check for deleting non-existant attribute */
!EP              CALL H5Adelete_by_idx_f(my_dataset, '.', idx_type, order, 0_HSIZE_T,error, lapl_id=H5P_DEFAULT_F)
              CALL H5Adelete_by_idx_f(my_dataset, '.', idx_type, order, hzero,error, lapl_id=H5P_DEFAULT_F)
              CALL VERIFY("H5Adelete_by_idx_f",error,minusone,total_error)

              !    /* Create attributes, up to limit of compact form */
              DO u = 0, max_compact - 1
                 ! /* Create attribute */
                 WRITE(chr2,'(I2.2)') u
                 attrname = 'attr '//chr2

                 CALL h5acreate_f(my_dataset, attrname, H5T_NATIVE_INTEGER, sid, attr, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
                 CALL check("h5acreate_f",error,total_error)

                 ! /* Write data into the attribute */
                 attr_integer_data(1) = u
                 data_dims(1) = 1
                 CALL h5awrite_f(attr, H5T_NATIVE_INTEGER, attr_integer_data, data_dims, error)
                 CALL check("h5awrite_f",error,total_error)

                 ! /* Close attribute */
                 CALL h5aclose_f(attr, error)
                 CALL check("h5aclose_f",error,total_error)

                 ! /* Verify information for new attribute */
                 CALL attr_info_by_idx_check(my_dataset, attrname, INT(u,HSIZE_T), use_index(i), total_error )

              ENDDO



              !  /* Verify state of object */

!!$                ret = H5O_num_attrs_test(my_dataset, &nattrs);
!!$                CHECK(ret, FAIL, "H5O_num_attrs_test");
!!$                VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
!!$                is_empty = H5O_is_attr_empty_test(my_dataset);
!!$                VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
!!$                is_dense = H5O_is_attr_dense_test(my_dataset);
!!$                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

              !/* Check for out of bound deletions */
              CALL H5Adelete_by_idx_f(my_dataset, ".", idx_type, order, INT(u,HSIZE_T), error, lapl_id=H5P_DEFAULT_F)
              CALL VERIFY("H5Adelete_by_idx_f",error,minusone,total_error)

           ENDDO


           DO curr_dset = 0, NUM_DSETS-1
              SELECT CASE (curr_dset)
              CASE (0)
                 my_dataset = dset1
              CASE (1)
                 my_dataset = dset2
              CASE (2)
                 my_dataset = dset3
                 !     CASE DEFAULT
                 !        CALL HDassert(0.AND."Toomanydatasets!")
              END SELECT

              ! /* Delete attributes from compact storage */

              DO u = 0, max_compact - 2

                 ! /* Delete first attribute in appropriate order */


!EP                 CALL H5Adelete_by_idx_f(my_dataset, ".", idx_type, order, 0_HSIZE_T, error)
                 CALL H5Adelete_by_idx_f(my_dataset, ".", idx_type, order, hzero, error)
                 CALL check("H5Adelete_by_idx_f",error,total_error)


                 ! /* Verify the attribute information for first attribute in appropriate order */
                 ! HDmemset(&ainfo, 0, sizeof(ainfo));

!EP                 CALL h5aget_info_by_idx_f(my_dataset, ".", idx_type, order, 0_HSIZE_T, &
                 CALL h5aget_info_by_idx_f(my_dataset, ".", idx_type, order, hzero, &
                      f_corder_valid, corder, cset, data_size, error)

                 IF(new_format)THEN
                    IF(order.EQ.H5_ITER_INC_F)THEN
                       CALL VERIFY("H5Aget_info_by_idx_f",corder,u + 1,total_error)
                    ENDIF
                 ELSE
                    CALL VERIFY("H5Aget_info_by_idx_f",corder, max_compact-(u + 2),total_error)
                 ENDIF

                   ! /* Verify the name for first attribute in appropriate order */
                   ! HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);

                 size = 7 ! *CHECK* IF NOT THE SAME SIZE
                 CALL h5aget_name_by_idx_f(my_dataset, ".", idx_type, order,INT(0,hsize_t), &
                      tmpname, error, lapl_id=H5P_DEFAULT_F, size=size)
                 CALL check('h5aget_name_by_idx_f',error,total_error)
                 IF(order .EQ. H5_ITER_INC_F)THEN
                    WRITE(chr2,'(I2.2)') u + 1
                    attrname = 'attr '//chr2
                 ELSE
                    WRITE(chr2,'(I2.2)') max_compact - (u + 2)
                    attrname = 'attr '//chr2
                 ENDIF
                 IF(TRIM(attrname).NE.TRIM(tmpname)) error = -1
                 CALL VERIFY("h5aget_name_by_idx_f",error,0,total_error)
              ENDDO

              ! /* Delete last attribute */

!EP              CALL H5Adelete_by_idx_f(my_dataset, ".", idx_type, order, 0_HSIZE_T, error)
              CALL H5Adelete_by_idx_f(my_dataset, ".", idx_type, order, hzero, error)
              CALL check("H5Adelete_by_idx_f",error,total_error)


              ! /* Verify state of attribute storage (empty) */
!!$                is_empty = H5O_is_attr_empty_test(my_dataset);
!!$                VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
           ENDDO

!   /* Work on all the datasets */

           DO curr_dset = 0,NUM_DSETS-1
              SELECT CASE (curr_dset)
              CASE (0)
                 my_dataset = dset1
              CASE (1)
                 my_dataset = dset2
              CASE (2)
                 my_dataset = dset3
                 !     CASE DEFAULT
                 !        CALL HDassert(0.AND."Toomanydatasets!")
              END SELECT

              ! /* Create more attributes, to push into dense form */

              DO u = 0, (max_compact * 2) - 1

                 ! /* Create attribute */
                 WRITE(chr2,'(I2.2)') u
                 attrname = 'attr '//chr2

                 CALL h5acreate_f(my_dataset, attrname, H5T_NATIVE_INTEGER, sid, attr, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
                 CALL check("h5acreate_f",error,total_error)


                 ! /* Write data into the attribute */
                 attr_integer_data(1) = u
                 data_dims(1) = 1
                 CALL h5awrite_f(attr, H5T_NATIVE_INTEGER, attr_integer_data, data_dims, error)
                 CALL check("h5awrite_f",error,total_error)

                 ! /* Close attribute */
                 CALL h5aclose_f(attr, error)
                 CALL check("h5aclose_f",error,total_error)

                 ! /* Verify state of object */
                 IF(u .GE. max_compact)THEN
!!$                  is_dense = H5O_is_attr_dense_test(my_dataset);
!!$                  VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");
                 ENDIF

                 ! /* Verify information for new attribute */
!!$              CALL check("attr_info_by_idx_check",error,total_error)
              ENDDO

              ! /* Verify state of object */
!!$                    ret = H5O_num_attrs_test(my_dataset, &nattrs);
!!$                    CHECK(ret, FAIL, "H5O_num_attrs_test");
!!$                    VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
!!$                    is_empty = H5O_is_attr_empty_test(my_dataset);
!!$                    VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
!!$                    is_dense = H5O_is_attr_dense_test(my_dataset);
!!$                    VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");
!!$
              IF(new_format)THEN
!!$                 ! /* Retrieve & verify # of records in the name & creation order indices */
!!$                 ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
!!$                 CHECK(ret, FAIL, "H5O_attr_dense_info_test");
!!$                 IF(use_index)
!!$                 VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
!!$                 VERIFY(name_count, (max_compact * 2), "H5O_attr_dense_info_test");
              ENDIF

              ! /* Check for out of bound deletion */
              CALL H5Adelete_by_idx_f(my_dataset, ".", idx_type, order, INT(u,HSIZE_T), error)
              CALL VERIFY("H5Adelete_by_idx_f",error,minusone,total_error)
           ENDDO

           ! /* Work on all the datasets */

           DO curr_dset = 0,NUM_DSETS-1
              SELECT CASE (curr_dset)
              CASE (0)
                 my_dataset = dset1
              CASE (1)
                 my_dataset = dset2
              CASE (2)
                 my_dataset = dset3
                 !     CASE DEFAULT
                 !        CALL HDassert(0.AND."Toomanydatasets!")
              END SELECT

              ! /* Delete attributes from dense storage */

              DO u = 0, (max_compact * 2) - 1 - 1

                 ! /* Delete first attribute in appropriate order */

                 CALL H5Adelete_by_idx_f(my_dataset, ".", idx_type, order, INT(0,HSIZE_T), error)
                 CALL check("H5Adelete_by_idx_f",error,total_error)
                 ! /* Verify the attribute information for first attribute in appropriate order */

                 CALL h5aget_info_by_idx_f(my_dataset, ".", idx_type, order, INT(0,HSIZE_T), &
                      f_corder_valid, corder, cset, data_size, error)
                 IF(new_format)THEN
                    IF(order.EQ.H5_ITER_INC_F)THEN
                       CALL VERIFY("H5Aget_info_by_idx_f",corder,u + 1,total_error)
                    ENDIF
                 ELSE
                    CALL VERIFY("H5Aget_info_by_idx_f",corder, ((max_compact * 2) - (u + 2)), total_error)
                 ENDIF

                 ! /* Verify the name for first attribute in appropriate order */
                 ! HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);

                 size = 7 ! *CHECK* if not the correct size
                 CALL h5aget_name_by_idx_f(my_dataset, ".", idx_type, order,INT(0,hsize_t), &
                      tmpname, error, size)

                 IF(order .EQ. H5_ITER_INC_F)THEN
                    WRITE(chr2,'(I2.2)') u + 1
                    attrname = 'attr '//chr2
                 ELSE
                    WRITE(chr2,'(I2.2)') max_compact * 2 - (u + 2)
                    attrname = 'attr '//chr2
                 ENDIF
                 IF(TRIM(attrname).NE.TRIM(tmpname)) error = -1
                 CALL VERIFY("h5aget_name_by_idx_f",error,0,total_error)


              ENDDO
              ! /* Delete last attribute */

              CALL H5Adelete_by_idx_f(my_dataset, ".", idx_type, order, INT(0,HSIZE_T), error, lapl_id=H5P_DEFAULT_F)
              CALL check("H5Adelete_by_idx_f",error,total_error)
              ! /* Verify state of attribute storage (empty) */
!!$                    is_empty = H5O_is_attr_empty_test(my_dataset);
!!$                    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");

              !/* Check for deletion on empty attribute storage again */
              CALL H5Adelete_by_idx_f(my_dataset, ".", idx_type, order, INT(0,HSIZE_T), error)
              CALL VERIFY("H5Adelete_by_idx_f",error,minusone,total_error)
           ENDDO

           !  /* Close Datasets */
           CALL h5dclose_f(dset1, error)
           CALL check("h5dclose_f",error,total_error)
           CALL h5dclose_f(dset2, error)
           CALL check("h5dclose_f",error,total_error)
           CALL h5dclose_f(dset3, error)
           CALL check("h5dclose_f",error,total_error)

           !   /* Close file */
           CALL h5fclose_f(fid, error)
           CALL check("h5fclose_f",error,total_error)
        ENDDO
     ENDDO
  ENDDO

  ! /* Close property list */
  CALL h5pclose_f(dcpl,error)
  CALL check("h5pclose_f", error, total_error)

  ! /* Close dataspace */
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f",error,total_error)

END SUBROUTINE test_attr_delete_by_idx

SUBROUTINE test_attr_shared_delete(fcpl, fapl, total_error)

!/****************************************************************
!**
!**  test_attr_shared_delete(): Test basic H5A (attribute) code.
!**      Tests deleting shared attributes in "compact" & "dense" storage
!**
!****************************************************************/

  USE HDF5

  IMPLICIT NONE

  INTEGER(HID_T), INTENT(IN) :: fcpl
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error
  CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid
  INTEGER(HID_T) :: dcpl
  INTEGER(HID_T) :: sid, big_sid

  CHARACTER(LEN=8) :: DSET1_NAME = "Dataset1"
  CHARACTER(LEN=8) :: DSET2_NAME = "Dataset2"
  INTEGER, PARAMETER :: NUM_DSETS = 3


  INTEGER(HID_T) :: dataset, dataset2

  INTEGER :: error

  INTEGER(HID_T) :: attr        !String Attribute identifier
  INTEGER(HID_T) :: attr_tid
  INTEGER(HSIZE_T), DIMENSION(7) :: data_dims


  INTEGER :: max_compact ! Maximum # of links to store in group compactly
  INTEGER :: min_dense   ! Minimum # of links to store in group "densely"

  CHARACTER(LEN=2) :: chr2

  INTEGER, DIMENSION(1) ::  attr_integer_data
  CHARACTER(LEN=7) :: attrname

  CHARACTER(LEN=1), PARAMETER :: chr1 = '.'

  INTEGER :: u
  INTEGER, PARAMETER :: SPACE1_RANK = 3
  INTEGER, PARAMETER :: NX = 20
  INTEGER, PARAMETER :: NY = 5
  INTEGER, PARAMETER :: NZ = 10
  INTEGER(HID_T) :: my_fcpl

  CHARACTER(LEN=5), PARAMETER :: TYPE1_NAME = "/Type"

  INTEGER :: test_shared
  INTEGER(HSIZE_T), DIMENSION(1) :: adims2 = (/1/) ! Attribute dimension
  INTEGER     ::   arank = 1                      ! Attribure rank

  ! /* Output message about test being performed */
!  WRITE(*,*) "     - Testing Deleting Shared & Unshared Attributes in Compact & Dense Storage"

  ! /* Initialize "big" attribute DATA */
  !    /* Create dataspace for dataset */
  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)

  !/* Create "big" dataspace for "large" attributes */

  CALL h5screate_simple_f(arank, adims2, big_sid, error)
  CALL check("h5screate_simple_f",error,total_error)

  ! /* Loop over type of shared components */

  DO test_shared = 0, 2

     ! /* Make copy of file creation property list */

     CALL H5Pcopy_f(fcpl, my_fcpl, error)
     CALL check("H5Pcopy",error,total_error)

     ! /* Set up datatype for attributes */

     CALL H5Tcopy_f(H5T_NATIVE_INTEGER, attr_tid, error)
     CALL check("H5Tcopy",error,total_error)

     ! /* Special setup for each type of shared components */
     IF( test_shared .EQ. 0) THEN
        ! /* Make attributes > 500 bytes shared */
        CALL H5Pset_shared_mesg_nindexes_f(my_fcpl,1,error)
        CALL check("H5Pset_shared_mesg_nindexes_f",error, total_error)
!!$     CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
        CALL H5Pset_shared_mesg_index_f(my_fcpl, 0, H5O_SHMESG_ATTR_FLAG_F, 500,error)
        CALL check(" H5Pset_shared_mesg_index_f",error, total_error)

!!$     CHECK_I(ret, "H5Pset_shared_mesg_index");
     ELSE
        ! /* Set up copy of file creation property list */
        CALL H5Pset_shared_mesg_nindexes_f(my_fcpl,3,error)
        ! /* Make attributes > 500 bytes shared */
        CALL H5Pset_shared_mesg_index_f(my_fcpl, 0, H5O_SHMESG_ATTR_FLAG_F, 500,error)
        ! /* Make datatypes & dataspaces > 1 byte shared (i.e. all of them :-) */
        CALL H5Pset_shared_mesg_index_f(my_fcpl, 1, H5O_SHMESG_DTYPE_FLAG_F, 1,error)
        CALL H5Pset_shared_mesg_index_f(my_fcpl, 2,  H5O_SHMESG_SDSPACE_FLAG_F, 1,error)
!!$            CHECK_I(ret, "H5Pset_shared_mesg_index");
     ENDIF

     ! /* Create file */
     CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, my_fcpl, fapl)
     CALL check("h5fcreate_f",error,total_error)

     ! /* Close FCPL copy */
     CALL h5pclose_f(my_fcpl, error)
     CALL check("h5pclose_f", error, total_error)
     ! /* Close file */
     CALL h5fclose_f(fid, error)
     CALL check("h5fclose_f",error,total_error)

     ! /* Re-open file */
     CALL h5fopen_f(FileName, H5F_ACC_RDWR_F, fid, error,fapl)
     CALL check("h5open_f",error,total_error)

     ! /* Commit datatype to file */

     IF(test_shared.EQ.2) THEN
        CALL H5Tcommit_f(fid, TYPE1_NAME, attr_tid, error, H5P_DEFAULT_F, H5P_DEFAULT_F, H5P_DEFAULT_F)
        CALL check("H5Tcommit",error,total_error)
     ENDIF

     ! /* Set up to query the object creation properties */
     CALL H5Pcreate_f(H5P_DATASET_CREATE_F,dcpl,error)
     CALL check("h5Pcreate_f",error,total_error)

     ! /* Create datasets */

     CALL h5dcreate_f(fid, DSET1_NAME, H5T_NATIVE_CHARACTER, sid, dataset, error, dcpl_id=dcpl )
     CALL check("h5dcreate_f",error,total_error)

     CALL h5dcreate_f(fid, DSET2_NAME, H5T_NATIVE_CHARACTER, sid, dataset2, error, dcpl_id=dcpl )
     CALL check("h5dcreate_f",error,total_error)

     ! /* Check on dataset's message storage status */
!!$        if(test_shared != 0) {
!!$            /* Datasets' datatypes can be shared */
!!$            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
!!$            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
!!$
!!$            /* Datasets' dataspace can be shared */
!!$            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
!!$            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
!!$        } /* end if */
!!$
     ! /* Retrieve limits for compact/dense attribute storage */
     CALL H5Pget_attr_phase_change_f(dcpl, max_compact, min_dense, error)
     CALL check("H5Pget_attr_phase_change_f",error,total_error)

     ! /* Close property list */
     CALL h5pclose_f(dcpl,error)
     CALL check("h5pclose_f", error, total_error)
!!$
!!$        /* Check on datasets' attribute storage status */
!!$        is_dense = H5O_is_attr_dense_test(dataset);
!!$        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
!!$        is_dense = H5O_is_attr_dense_test(dataset2);
!!$        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
!!$
     ! /* Add attributes to each dataset, until after converting to dense storage */

     DO u = 0, (max_compact * 2) - 1

        ! /* Create attribute name */
        WRITE(chr2,'(I2.2)') u
        attrname = 'attr '//chr2

        ! /* Alternate between creating "small" & "big" attributes */

        IF(MOD(u+1,2).EQ.0)THEN
           ! /* Create "small" attribute on first dataset */

           CALL h5acreate_f(dataset, attrname, attr_tid, sid, attr, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
           CALL check("h5acreate_f",error,total_error)

!!$                /* Check that attribute is not shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

           ! /* Write data into the attribute */
           attr_integer_data(1) = u + 1
           data_dims(1) = 1
           CALL h5awrite_f(attr, attr_tid, attr_integer_data, data_dims, error)
           CALL check("h5awrite_f",error,total_error)
        ELSE
           !  Create "big" attribute on first dataset */

           CALL h5acreate_f(dataset, attrname, attr_tid, big_sid, attr, error)
           CALL check("h5acreate_f",error,total_error)
!!$
           !  Check that attribute is shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

           !  Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
!!$
           !  Write data into the attribute */

           attr_integer_data(1) = u + 1
           data_dims(1) = 1
           CALL h5awrite_f(attr,  attr_tid, attr_integer_data, data_dims, error)
           CALL check("h5awrite_f",error,total_error)

           !  Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
        ENDIF

        ! /* Close attribute */
        CALL h5aclose_f(attr, error)
        CALL check("h5aclose_f",error,total_error)

        ! /* Check on dataset's attribute storage status */
!!$            is_dense = H5O_is_attr_dense_test(dataset);
!!$            if(u < max_compact)
!!$                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
!!$            else
!!$                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");
!!$
!!$
        ! /* Alternate between creating "small" & "big" attributes */
        IF(MOD(u+1,2).EQ.0)THEN

           !  /* Create "small" attribute on second dataset */

           CALL h5acreate_f(dataset2, attrname, attr_tid, sid, attr, error)
           CALL check("h5acreate_f",error,total_error)

           !  /* Check that attribute is not shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
!!$
           ! /* Write data into the attribute */
           attr_integer_data(1) = u + 1
           data_dims(1) = 1
           CALL h5awrite_f(attr, attr_tid, attr_integer_data, data_dims, error)
           CALL check("h5awrite_f",error,total_error)
        ELSE

           ! /* Create "big" attribute on second dataset */

           CALL h5acreate_f(dataset2, attrname, attr_tid, big_sid, attr, error, acpl_id=H5P_DEFAULT_F, aapl_id=H5P_DEFAULT_F)
           CALL check("h5acreate_f",error,total_error)

! /* Check that attribute is shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, TRUE, "H5A_is_shared_test");
!!$
! /* Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
!!$
! /* Write data into the attribute */


           attr_integer_data(1) = u + 1
           data_dims(1) = 1
           CALL h5awrite_f(attr,  attr_tid, attr_integer_data, data_dims, error)
           CALL check("h5awrite_f",error,total_error)


! /* Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");

        ENDIF
        ! /* Close attribute */
        CALL h5aclose_f(attr, error)
        CALL check("h5aclose_f",error,total_error)

        ! /* Check on dataset's attribute storage status */
!!$            is_dense = H5O_is_attr_dense_test(dataset2);
!!$            if(u < max_compact)
!!$                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
!!$            else
!!$                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");
     ENDDO

     ! /* Delete attributes from second dataset */

     DO u = 0, max_compact*2-1

        ! /* Create attribute name */
        WRITE(chr2,'(I2.2)') u
        attrname = 'attr '//chr2

        ! /* Delete second dataset's attribute */
        CALL H5Adelete_by_name_f(fid, DSET2_NAME, attrname,error,lapl_id=H5P_DEFAULT_F)
        CALL check("H5Adelete_by_name", error, total_error)

!!$            /* Check refcount on attributes now */
!!$
!!$            /* Check refcount on first dataset's attribute */

        CALL h5aopen_f(dataset, attrname, attr, error, aapl_id=H5P_DEFAULT_F)
        CALL check("h5aopen_f",error,total_error)

!!$
!!$            if(u % 2) {
! /* Check that attribute is not shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
!!$            } /* end if */
!!$            else {
!/*  Check that attribute is shared */
!!$                is_shared = H5A_is_shared_test(attr);
!!$                VERIFY(is_shared, TRUE, "H5A_is_shared_test");
!!$
!/*  Check refcount for attribute */
!!$                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
!!$                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
!!$                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
!!$            } /* end else */

        ! /* Close attribute */
        CALL h5aclose_f(attr, error)
        CALL check("h5aclose_f",error,total_error)
     ENDDO

     ! /* Close attribute's datatype */

     CALL h5tclose_f(attr_tid, error)
     CALL check("h5tclose_f",error,total_error)

     ! /* Close Datasets */

     CALL h5dclose_f(dataset, error)
     CALL check("h5dclose_f",error,total_error)
     CALL h5dclose_f(dataset2, error)
     CALL check("h5dclose_f",error,total_error)

     ! /* Check on shared message status now */
!!$        if(test_shared != 0) {
!!$            if(test_shared == 1) {
     ! /*  Check on datatype storage status */
!!$                ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
!!$                CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$                VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
!!$            } /* end if */
!!$
!!$            /* Check on dataspace storage status */
!!$            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
!!$            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$            VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
!!$        } /* end if */
!!$
     ! /* Unlink datasets WITH attributes */

     CALL h5ldelete_f(fid, DSET1_NAME, error, H5P_DEFAULT_F)
     CALL check("H5Ldelete_f", error, total_error)
     CALL h5ldelete_f(fid, DSET2_NAME, error)
     CALL check("H5Ldelete_f", error, total_error)

     ! /* Unlink committed datatype */

     IF( test_shared == 2) THEN
        CALL h5ldelete_f(fid, TYPE1_NAME, error)
        CALL check("H5Ldelete_f", error, total_error)
     ENDIF

     ! /* Check on attribute storage status */
!!$        ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
!!$        CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$        VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
!!$
!!$        if(test_shared != 0) {
!!$            /* Check on datatype storage status */
!!$            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
!!$            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
!!$
!!$            /* Check on dataspace storage status */
!!$            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
!!$            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
!!$            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
!!$        } /* end if */
!!$

     ! /* Close file */
     CALL h5fclose_f(fid, error)
     CALL check("h5fclose_f",error,total_error)
!!$
!!$        /* Check size of file */
!!$        filesize = h5_get_file_size(FILENAME);
!!$        VERIFY(filesize, empty_filesize, "h5_get_file_size");
  ENDDO

  ! /* Close dataspaces */
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5sclose_f(big_sid, error)
  CALL check("h5sclose_f",error,total_error)

END SUBROUTINE test_attr_shared_delete



SUBROUTINE test_attr_dense_open( fcpl, fapl, total_error)

!/****************************************************************
!**
!**  test_attr_dense_open(): Test basic H5A (attribute) code.
!**      Tests opening attributes in "dense" storage
!**
!****************************************************************/

  USE HDF5

  IMPLICIT NONE

  INTEGER(HID_T), INTENT(IN) :: fcpl
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error
  CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid
  INTEGER(HID_T) :: dcpl
  INTEGER(HID_T) :: sid

  CHARACTER(LEN=8) :: DSET1_NAME = "Dataset1"

  INTEGER :: error
  INTEGER(HID_T) :: attr        !String Attribute identifier
  INTEGER(HSIZE_T), DIMENSION(7) :: data_dims


  INTEGER :: max_compact ! Maximum # of links to store in group compactly
  INTEGER :: min_dense   ! Minimum # of links to store in group "densely"

  CHARACTER(LEN=2) :: chr2


  CHARACTER(LEN=7) :: attrname

  INTEGER(HID_T) :: dataset
  INTEGER :: u

  data_dims = 0

  ! /* Output message about test being performed */
!  WRITE(*,*) "     - Testing Opening Attributes in Dense Storage"

  ! /* Create file */

  CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, fcpl, fapl)
  CALL check("h5fcreate_f",error,total_error)

  ! /* Close file */
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)



  ! /* Re-open file */
  CALL h5fopen_f(FileName, H5F_ACC_RDWR_F, fid, error, fapl)
  CALL check("h5open_f",error,total_error)

  ! /* Create dataspace for dataset */
  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)

  ! /* Query the group creation properties */
  CALL H5Pcreate_f(H5P_DATASET_CREATE_F,dcpl,error)
  CALL check("h5Pcreate_f",error,total_error)

  ! /* Enable creation order tracking on attributes, so creation order tests work */
  CALL H5Pset_attr_creation_order_f(dcpl, H5P_CRT_ORDER_TRACKED_F, error)
  CALL check("H5Pset_attr_creation_order",error,total_error)

  ! /* Create a dataset */

  CALL h5dcreate_f(fid, DSET1_NAME, H5T_NATIVE_CHARACTER, sid, dataset, error, &
       lcpl_id=H5P_DEFAULT_F, dcpl_id=dcpl, dapl_id=H5P_DEFAULT_F)
  CALL check("h5dcreate_f",error,total_error)

  ! /* Retrieve limits for compact/dense attribute storage */
  CALL H5Pget_attr_phase_change_f(dcpl, max_compact, min_dense, error)
  CALL check("H5Pget_attr_phase_change_f",error,total_error)

  ! /* Close property list */
  CALL h5pclose_f(dcpl, error)
  CALL check("h5pclose_f",error,total_error)

  ! /* Check on dataset's attribute storage status */
  !  is_dense = H5O_is_attr_dense_test(dataset);
  !  VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

  ! /* Add attributes, until just before converting to dense storage */

  DO u = 0, max_compact - 1
     ! /* Create attribute */
     WRITE(chr2,'(I2.2)') u
     attrname = 'attr '//chr2

     CALL h5acreate_f(dataset, attrname, H5T_NATIVE_INTEGER, sid, attr, error, aapl_id=H5P_DEFAULT_F)
     CALL check("h5acreate_f",error,total_error)

     ! /* Write data into the attribute */

     data_dims(1) = 1
     CALL h5awrite_f(attr, H5T_NATIVE_INTEGER, u, data_dims, error)
     CALL check("h5awrite_f",error,total_error)

     ! /* Close attribute */
     CALL h5aclose_f(attr, error)
     CALL check("h5aclose_f",error,total_error)

     ! /* Verify attributes written so far */
     CALL test_attr_dense_verify(dataset, u, total_error)
     ! CHECK(ret, FAIL, "test_attr_dense_verify");
  ENDDO

  ! /* Check on dataset's attribute storage status */
!!$    is_dense = H5O_is_attr_dense_test(dataset);
!!$    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

!    /* Add one more attribute, to push into "dense" storage */
!    /* Create attribute */

  WRITE(chr2,'(I2.2)') u
  attrname = 'attr '//chr2

  CALL h5acreate_f(dataset, attrname, H5T_NATIVE_INTEGER, sid, attr, error, aapl_id=H5P_DEFAULT_F)
  CALL check("h5acreate_f",error,total_error)

  ! /* Check on dataset's attribute storage status */
!!$    is_dense = H5O_is_attr_dense_test(dataset);
!!$    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


  ! /* Write data into the attribute */
  data_dims(1) = 1
  CALL h5awrite_f(attr, H5T_NATIVE_INTEGER, u, data_dims, error)
  CALL check("h5awrite_f",error,total_error)

  ! /* Close attribute */
  CALL h5aclose_f(attr, error)
  CALL check("h5aclose_f",error,total_error)


  ! /* Close dataspace */
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f",error,total_error)

  ! /* Verify all the attributes written */
  !  ret = test_attr_dense_verify(dataset, (u + 1));
  !  CHECK(ret, FAIL, "test_attr_dense_verify");

  ! /* CLOSE Dataset */
  CALL h5dclose_f(dataset, error)
  CALL check("h5dclose_f",error,total_error)

  ! /* Unlink dataset with attributes */
  CALL h5ldelete_f(fid, DSET1_NAME, error, H5P_DEFAULT_F)
  CALL check("H5Ldelete_f", error, total_error)

  ! /* Close file */
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)

  ! /* Check size of file */
  !  filesize = h5_get_file_size(FILENAME);
  !  VERIFY(filesize, empty_filesize, "h5_get_file_size")

END SUBROUTINE test_attr_dense_open

!/****************************************************************
!**
!**  test_attr_dense_verify(): Test basic H5A (attribute) code.
!**      Verify attributes on object
!**
!****************************************************************/

SUBROUTINE test_attr_dense_verify(loc_id, max_attr, total_error)

  USE HDF5

  IMPLICIT NONE

  INTEGER(HID_T), INTENT(IN) :: loc_id
  INTEGER, INTENT(IN) :: max_attr
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(SIZE_T), PARAMETER :: ATTR_NAME_LEN = 8 ! FIX, why if 7 does not work?

  INTEGER :: u
  CHARACTER(LEN=2) :: chr2
  CHARACTER(LEN=ATTR_NAME_LEN) :: attrname
  CHARACTER(LEN=ATTR_NAME_LEN) :: check_name
  INTEGER(HSIZE_T), DIMENSION(7) :: data_dims

  INTEGER(HID_T) :: attr        !String Attribute identifier
  INTEGER :: error
  INTEGER :: value

  data_dims = 0


    ! /* Retrieve the current # of reported errors */
    ! old_nerrs = GetTestNumErrs();

    ! /* Re-open all the attributes by name and verify the data */

  DO u = 0, max_attr -1

     ! /* Open attribute */
     WRITE(chr2,'(I2.2)') u
     attrname = 'attr '//chr2

     CALL h5aopen_f(loc_id, attrname, attr, error)
     CALL check("h5aopen_f",error,total_error)

     ! /* Read data from the attribute */

!     value = 103
     data_dims(1) = 1
     CALL h5aread_f(attr, H5T_NATIVE_INTEGER, value, data_dims, error)

     CALL CHECK("H5Aread_F", error, total_error)
     CALL VERIFY("H5Aread_F", value, u, total_error)

     ! /* Close attribute */
     CALL h5aclose_f(attr, error)
     CALL check("h5aclose_f",error,total_error)
  ENDDO

  ! /* Re-open all the attributes by index and verify the data */

  DO u=0, max_attr-1


     ! /* Open attribute */

     CALL H5Aopen_by_idx_f(loc_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, INT(u,HSIZE_T), &
          attr, error, aapl_id=H5P_DEFAULT_F)

     ! /* Verify Name */

     WRITE(chr2,'(I2.2)') u
     attrname = 'attr '//chr2

     CALL H5Aget_name_f(attr, ATTR_NAME_LEN, check_name, error)
     CALL check('H5Aget_name',error,total_error)
     IF(check_name.NE.attrname) THEN
        WRITE(*,*) 'ERROR: attribute name different: attr_name = ',check_name, ', should be ', attrname
        total_error = total_error + 1
     ENDIF
     ! /* Read data from the attribute */
     data_dims(1) = 1
     CALL h5aread_f(attr, H5T_NATIVE_INTEGER, value, data_dims, error)
     CALL CHECK("H5Aread_f", error, total_error)
     CALL VERIFY("H5Aread_f", value, u, total_error)


     ! /* Close attribute */
     CALL h5aclose_f(attr, error)
     CALL check("h5aclose_f",error,total_error)
  ENDDO

END SUBROUTINE test_attr_dense_verify

!/****************************************************************
!**
!**  test_attr_corder_create_empty(): Test basic H5A (attribute) code.
!**      Tests basic code to create objects with attribute creation order info
!**
!****************************************************************/

SUBROUTINE test_attr_corder_create_basic( fcpl, fapl, total_error )

  USE HDF5

  IMPLICIT NONE

  INTEGER(HID_T), INTENT(IN) :: fcpl
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error
  CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid
  INTEGER(HID_T) :: dcpl
  INTEGER(HID_T) :: sid

  CHARACTER(LEN=8) :: DSET1_NAME = "Dataset1"

  INTEGER(HID_T) :: dataset

  INTEGER :: error

  INTEGER :: crt_order_flags
  INTEGER :: minusone = -1

  ! /* Output message about test being performed */
!  WRITE(*,*) "     - Testing Basic Code for Attributes with Creation Order Info"

  ! /* Create file */
  CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, fcpl, fapl)
  CALL check("h5fcreate_f",error,total_error)

  ! /* Create dataset creation property list */
  CALL H5Pcreate_f(H5P_DATASET_CREATE_F,dcpl,error)
  CALL check("h5Pcreate_f",error,total_error)

  ! /* Get creation order indexing on object */
  CALL H5Pget_attr_creation_order_f(dcpl, crt_order_flags, error)
  CALL check("H5Pget_attr_creation_order_f",error,total_error)
  CALL VERIFY("H5Pget_attr_creation_order_f",crt_order_flags , 0, total_error)

  ! /* Setting invalid combination of a attribute order creation order indexing on should fail */
  CALL H5Pset_attr_creation_order_f(dcpl, H5P_CRT_ORDER_INDEXED_F, error)
  CALL VERIFY("H5Pset_attr_creation_order_f",error , minusone, total_error)
  CALL H5Pget_attr_creation_order_f(dcpl, crt_order_flags, error)
  CALL check("H5Pget_attr_creation_order_f",error,total_error)
  CALL VERIFY("H5Pget_attr_creation_order_f",crt_order_flags , 0, total_error)

  ! /* Set attribute creation order tracking & indexing for object */
  CALL h5pset_attr_creation_order_f(dcpl, IOR(H5P_CRT_ORDER_TRACKED_F, H5P_CRT_ORDER_INDEXED_F), error)
  CALL check("H5Pset_attr_creation_order_f",error,total_error)

  CALL H5Pget_attr_creation_order_f(dcpl, crt_order_flags, error)
  CALL check("H5Pget_attr_creation_order_f",error,total_error)
  CALL VERIFY("H5Pget_attr_creation_order_f",crt_order_flags , &
       IOR(H5P_CRT_ORDER_TRACKED_F, H5P_CRT_ORDER_INDEXED_F), total_error)

  ! /* Create dataspace for dataset */
  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)

  ! /* Create a dataset */
  CALL h5dcreate_f(fid, DSET1_NAME, H5T_NATIVE_CHARACTER, sid, dataset, error, &
       lcpl_id=H5P_DEFAULT_F, dapl_id=H5P_DEFAULT_F, dcpl_id=dcpl)
  CALL check("h5dcreate_f",error,total_error)

  ! /* Close dataspace */
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f",error,total_error)


  ! /* Close Dataset */
  CALL h5dclose_f(dataset, error)
  CALL check("h5dclose_f",error,total_error)

  ! /* Close property list */
  CALL h5pclose_f(dcpl, error)
  CALL check("h5pclose_f",error,total_error)

  ! /* Close file */
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)

  ! /* Re-open file */
  CALL h5fopen_f(FileName, H5F_ACC_RDWR_F, fid, error, fapl)
  CALL check("h5open_f",error,total_error)

  ! /* Open dataset created */
  CALL h5dopen_f(fid, DSET1_NAME, dataset, error, H5P_DEFAULT_F )
  CALL check("h5dopen_f",error,total_error)


  ! /* Retrieve dataset creation property list for group */
  CALL H5Dget_create_plist_f(dataset, dcpl, error)
  CALL check("H5Dget_create_plist_f",error,total_error)

  ! /* Query the attribute creation properties */
  CALL H5Pget_attr_creation_order_f(dcpl, crt_order_flags, error)
  CALL check("H5Pget_attr_creation_order_f",error,total_error)
  CALL VERIFY("H5Pget_attr_creation_order_f",crt_order_flags , &
       IOR(H5P_CRT_ORDER_TRACKED_F, H5P_CRT_ORDER_INDEXED_F), total_error )

  ! /* Close property list */
  CALL h5pclose_f(dcpl, error)
  CALL check("h5pclose_f",error,total_error)

  ! /* Close Dataset */
  CALL h5dclose_f(dataset, error)
  CALL check("h5dclose_f",error,total_error)

  ! /* Close file */
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)


END SUBROUTINE test_attr_corder_create_basic

!/****************************************************************
!**
!**  test_attr_basic_write(): Test basic H5A (attribute) code.
!**      Tests integer attributes on both datasets and groups
!**
!****************************************************************/

SUBROUTINE test_attr_basic_write(fapl, total_error)

  USE HDF5

  IMPLICIT NONE

  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error
  CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid1
  INTEGER(HID_T) :: sid1, sid2

  CHARACTER(LEN=8) :: DSET1_NAME = "Dataset1"

  INTEGER(HID_T) :: dataset
  INTEGER :: i
  INTEGER :: error

  INTEGER(HID_T) :: attr,attr2        !String Attribute identifier
  INTEGER(HID_T) :: group

  CHARACTER(LEN=25) :: check_name
  CHARACTER(LEN=18) :: chr_exact_size

  INTEGER, PARAMETER :: SPACE1_RANK = 2

  CHARACTER(LEN=5), PARAMETER ::  ATTR1_NAME="Attr1"
  INTEGER, PARAMETER :: ATTR1_RANK = 1
  INTEGER, PARAMETER ::  ATTR1_DIM1 = 3
  CHARACTER(LEN=7), PARAMETER :: ATTR1A_NAME ="Attr1_a"
  CHARACTER(LEN=18), PARAMETER :: ATTR_TMP_NAME = "Attr1_a-1234567890"
  INTEGER, DIMENSION(ATTR1_DIM1) :: attr_data1
  INTEGER, DIMENSION(ATTR1_DIM1) :: attr_data1a
  INTEGER, DIMENSION(ATTR1_DIM1) :: read_data1
  INTEGER(HSIZE_T) :: attr_size   ! attributes storage requirements .MSB.
  INTEGER(HSIZE_T), DIMENSION(1) :: dimsa = (/3/) ! Dataset dimensions

  INTEGER     ::   rank1 = 2               ! Dataspace1 rank
  INTEGER(HSIZE_T), DIMENSION(2) :: dims1 = (/4,6/) ! Dataset dimensions
  INTEGER(HSIZE_T), DIMENSION(2) :: maxdims1 = (/4,6/) ! maximum dimensions

  INTEGER(SIZE_T) :: size

!! Initialize attribute data
  attr_data1(1) = 258
  attr_data1(2) = 9987
  attr_data1(3) = -99890

  attr_data1a(1) = 258
  attr_data1a(2) = 1087
  attr_data1a(3) = -99890

  ! /* Output message about test being performed */
!  WRITE(*,*) "     - Testing Basic Scalar Attribute Writing Functions"

  ! /* Create file */
  CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid1, error, H5P_DEFAULT_F, fapl)
  CALL check("h5fcreate_f",error,total_error)

  ! /* Create dataspace for dataset */
  CALL h5screate_simple_f(rank1, dims1, sid1, error, maxdims1)
  CALL check("h5screate_simple_f",error,total_error)

  ! /* Create a dataset */
  CALL h5dcreate_f(fid1, DSET1_NAME, H5T_NATIVE_CHARACTER, sid1, dataset, error, H5P_DEFAULT_F, H5P_DEFAULT_F, H5P_DEFAULT_F )
  CALL check("h5dcreate_f",error,total_error)

  ! /* Create dataspace for attribute */
  CALL h5screate_simple_f(ATTR1_RANK, dimsa, sid2, error)
  CALL check("h5screate_simple_f",error,total_error)

  ! /* Try to create an attribute on the file (should create an attribute on root group) */
  CALL h5acreate_f(fid1, ATTR1_NAME, H5T_NATIVE_INTEGER, sid2, attr, error, aapl_id=H5P_DEFAULT_F, acpl_id=H5P_DEFAULT_F)
  CALL check("h5acreate_f",error,total_error)

  !  /* Close attribute */
  CALL h5aclose_f(attr, error)
  CALL check("h5aclose_f",error,total_error)

  ! /* Open the root group */
  CALL H5Gopen_f(fid1, "/", group, error, H5P_DEFAULT_F)
  CALL check("H5Gopen_f",error,total_error)

  ! /* Open attribute again */
  CALL h5aopen_f(group,  ATTR1_NAME, attr, error)
  CALL check("h5aopen_f",error,total_error)

  ! /* Close attribute */
  CALL h5aclose_f(attr, error)
  CALL check("h5aclose_f",error,total_error)

  ! /* Close root group */
  CALL  H5Gclose_f(group, error)
  CALL check("h5gclose_f",error,total_error)

  ! /* Create an attribute for the dataset */
  CALL h5acreate_f(dataset, ATTR1_NAME, H5T_NATIVE_INTEGER, sid2, attr, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
  CALL check("h5acreate_f",error,total_error)

  ! /* Write attribute information */

  CALL h5awrite_f(attr, H5T_NATIVE_INTEGER, attr_data1, dimsa, error)
  CALL check("h5awrite_f",error,total_error)

  ! /* Create an another attribute for the dataset */
  CALL h5acreate_f(dataset, ATTR1A_NAME, H5T_NATIVE_INTEGER, sid2, attr2, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
  CALL check("h5acreate_f",error,total_error)

  ! /* Write attribute information */
  CALL h5awrite_f(attr2, H5T_NATIVE_INTEGER, attr_data1a, dimsa, error)
  CALL check("h5awrite_f",error,total_error)

  ! /* Check storage size for attribute */

  CALL h5aget_storage_size_f(attr, attr_size, error)
  CALL check("h5aget_storage_size_f",error,total_error)
!EP  CALL VERIFY("h5aget_storage_size_f", INT(attr_size), 2*HSIZE_T, total_error)

!  attr_size = H5Aget_storage_size(attr);
!  VERIFY(attr_size, (ATTR1_DIM1 * sizeof(int)), "H5A_get_storage_size");

  ! /* Read attribute information immediately, without closing attribute */
  CALL h5aread_f(attr, H5T_NATIVE_INTEGER, read_data1, dimsa, error)
  CALL check("h5aread_f",error,total_error)

  ! /* Verify values read in */
  DO i = 1, ATTR1_DIM1
     CALL VERIFY('h5aread_f',attr_data1(i),read_data1(i), total_error)
  ENDDO

  ! /* CLOSE attribute */
  CALL h5aclose_f(attr, error)
  CALL check("h5aclose_f",error,total_error)

  ! /* Close attribute */
  CALL h5aclose_f(attr2, error)
  CALL check("h5aclose_f",error,total_error)

  ! /* change attribute name */
  CALL H5Arename_f(dataset, ATTR1_NAME, ATTR_TMP_NAME, error)
  CALL check("H5Arename_f", error, total_error)

  ! /* Open attribute again */

  CALL h5aopen_f(dataset,  ATTR_TMP_NAME, attr, error)
  CALL check("h5aopen_f",error,total_error)

  ! /* Verify new attribute name */
  ! Set a deliberately small size

  check_name = '                         ' ! need to initialize or does not pass test

  size = 1
  CALL H5Aget_name_f(attr, size, check_name, error)
  CALL check('H5Aget_name',error,total_error)

  ! Now enter with the corrected size
  IF(error.NE.size)THEN
     size = error
     CALL H5Aget_name_f(attr, size, check_name, error)
     CALL check('H5Aget_name',error,total_error)
  ENDIF

  IF(TRIM(ADJUSTL(check_name)).NE.TRIM(ADJUSTL(ATTR_TMP_NAME))) THEN
     PRINT*,'.'//TRIM(check_name)//'.',LEN_TRIM(check_name)
     PRINT*,'.'//TRIM(ATTR_TMP_NAME)//'.',LEN_TRIM(ATTR_TMP_NAME)
     WRITE(*,*) 'ERROR: attribute name different: attr_name ='//TRIM(check_name)//'.'
     WRITE(*,*) '                                 should be ='//TRIM(ATTR_TMP_NAME)//'.'
     total_error = total_error + 1
     stop
  ENDIF

  ! Try with a string buffer that is exactly the correct size
  size = 18
  CALL H5Aget_name_f(attr, size, chr_exact_size, error)
  CALL check('H5Aget_name_f',error,total_error)
  CALL VerifyString('H5Aget_name_f',chr_exact_size,ATTR_TMP_NAME, total_error)

  ! /* Close attribute */
  CALL h5aclose_f(attr, error)
  CALL check("h5aclose_f",error,total_error)

  CALL h5sclose_f(sid1, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5sclose_f(sid2, error)
  CALL check("h5sclose_f",error,total_error)
  !/* Close Dataset */
  CALL h5dclose_f(dataset, error)
  CALL check("h5dclose_f",error,total_error)

  ! /* Close file */
  CALL h5fclose_f(fid1, error)
  CALL check("h5fclose_f",error,total_error)

END SUBROUTINE test_attr_basic_write

!/****************************************************************
!**
!**  test_attr_many(): Test basic H5A (attribute) code.
!**      Tests storing lots of attributes
!**
!****************************************************************/

SUBROUTINE test_attr_many(new_format, fcpl, fapl, total_error)

  USE HDF5

  IMPLICIT NONE

  LOGICAL, INTENT(IN) :: new_format
  INTEGER(HID_T), INTENT(IN) :: fcpl
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error
  CHARACTER(LEN=8) :: FileName = "tattr.h5"
  INTEGER(HID_T) :: fid
  INTEGER(HID_T) :: sid
  INTEGER(HID_T) :: gid
  INTEGER(HID_T) :: aid



  INTEGER :: error

  INTEGER(HSIZE_T), DIMENSION(7) :: data_dims
  CHARACTER(LEN=5) :: chr5


  CHARACTER(LEN=11) :: attrname
  CHARACTER(LEN=8), PARAMETER :: GROUP1_NAME="/Group1"

  INTEGER :: u
  INTEGER :: nattr
  LOGICAL :: exists
  INTEGER, DIMENSION(1) ::  attr_data1

  data_dims = 0

  ! /* Output message about test being performed */
!  WRITE(*,*) "     - Testing Storing Many Attributes"

  !/* Create file */
  CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, fcpl, fapl)
  CALL check("h5fcreate_f",error,total_error)

  ! /* Create dataspace for attribute */
  CALL h5screate_f(H5S_SCALAR_F, sid, error)
  CALL check("h5screate_f",error,total_error)

  ! /* Create group for attributes */

  CALL H5Gcreate_f(fid, GROUP1_NAME, gid, error)
  CALL check("H5Gcreate_f", error, total_error)

  ! /* Create many attributes */

  IF(new_format)THEN
     nattr = 250
  ELSE
     nattr = 2
  ENDIF

  DO u = 0, nattr - 1

     WRITE(chr5,'(I5.5)') u
     attrname = 'attr '//chr5
     CALL H5Aexists_f( gid, attrname, exists, error)
     CALL VerifyLogical("H5Aexists",exists,.FALSE.,total_error )

     CALL H5Aexists_by_name_f(fid, GROUP1_NAME, attrname,  exists, error, lapl_id = H5P_DEFAULT_F)
     CALL VerifyLogical("H5Aexists_by_name_f",exists,.FALSE.,total_error )

     CALL h5acreate_f(gid, attrname, H5T_NATIVE_INTEGER, sid, aid, error, H5P_DEFAULT_F, H5P_DEFAULT_F)
     CALL check("h5acreate_f",error,total_error)

     CALL H5Aexists_f(gid, attrname, exists, error)
     CALL VerifyLogical("H5Aexists",exists,.TRUE.,total_error )

     CALL H5Aexists_by_name_f(fid, GROUP1_NAME, attrname, exists, error)
     CALL VerifyLogical("H5Aexists_by_name_f",exists,.TRUE.,total_error )

     attr_data1(1) = u
     data_dims(1) = 1

     CALL h5awrite_f(aid, H5T_NATIVE_INTEGER, attr_data1, data_dims, error)
     CALL check("h5awrite_f",error,total_error)

     CALL h5aclose_f(aid, error)
     CALL check("h5aclose_f",error,total_error)

     CALL H5Aexists_f(gid, attrname, exists, error)
     CALL VerifyLogical("H5Aexists",exists,.TRUE.,total_error )

     CALL H5Aexists_by_name_f(fid, GROUP1_NAME, attrname, exists, error)
     CALL VerifyLogical("H5Aexists_by_name_f",exists,.TRUE.,total_error )

  ENDDO

  ! /* Close group */
  CALL  H5Gclose_f(gid, error)
  CALL check("h5gclose_f",error,total_error)

  ! /* Close file */
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)

  ! /* Close dataspaces */
  CALL h5sclose_f(sid, error)
  CALL check("h5sclose_f",error,total_error)

END SUBROUTINE test_attr_many

!/*-------------------------------------------------------------------------
! * Function:    attr_open_check
! *
! * Purpose:     Check opening attribute on an object
! *
! * Return:      Success:        0
! *              Failure:        -1
! *
! * Programmer:  Fortran version (M.S. Breitenfeld)
! *              March 21, 2008
! *
! *-------------------------------------------------------------------------
! */

SUBROUTINE attr_open_check(fid, dsetname, obj_id, max_attrs, total_error )

  USE HDF5

  IMPLICIT NONE
  INTEGER(HID_T), INTENT(IN) :: fid
  CHARACTER(LEN=*), INTENT(IN) :: dsetname
  INTEGER(HID_T), INTENT(IN) :: obj_id
  INTEGER, INTENT(IN) :: max_attrs
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER :: u
  CHARACTER (LEN=8) :: attrname
  INTEGER, PARAMETER :: NUM_DSETS = 3
  INTEGER :: error
  LOGICAL :: f_corder_valid ! Indicates whether the the creation order data is valid for this attribute
  INTEGER :: corder ! Is a positive integer containing the creation order of the attribute
  INTEGER :: cset ! Indicates the character set used for the attribute’s name
  INTEGER(HSIZE_T) :: data_size   ! indicates the size, in the number of characters

  INTEGER(HSIZE_T) :: storage_size   ! attributes storage requirements
  CHARACTER(LEN=2) :: chr2
  INTEGER(HID_T) attr_id
  ! /* Open each attribute on object by index and check that it's the correct one */

  DO u = 0, max_attrs-1
     ! /* Open the attribute */

     WRITE(chr2,'(I2.2)') u
     attrname = 'attr '//chr2


     CALL h5aopen_f(obj_id, attrname, attr_id, error)
     CALL check("h5aopen_f",error,total_error)


     ! /* Get the attribute's information */

     CALL h5aget_info_f(attr_id, f_corder_valid, corder, cset, data_size,  error)
     CALL check("h5aget_info_f",error,total_error)

     ! /* Check that the object's attributes are correct */
     CALL VERIFY("h5aget_info_f.corder",corder,u,total_error)
     CALL Verifylogical("h5aget_info_f.corder_valid",f_corder_valid,.TRUE.,total_error)
     CALL VERIFY("h5aget_info_f.cset", cset, H5T_CSET_ASCII_F, total_error)
     CALL h5aget_storage_size_f(attr_id, storage_size, error)
     CALL check("h5aget_storage_size_f",error,total_error)

     CALL VERIFY("h5aget_info_f.data_size", INT(data_size), INT(storage_size), total_error)


     ! /* Close attribute */
     CALL h5aclose_f(attr_id, error)
     CALL check("h5aclose_f",error,total_error)

     ! /* Open the attribute */

     CALL H5Aopen_by_name_f(obj_id, ".", attrname, attr_id, error, lapl_id=H5P_DEFAULT_F, aapl_id=H5P_DEFAULT_F)
     CALL check("H5Aopen_by_name_f", error, total_error)

     CALL h5aget_info_f(attr_id, f_corder_valid, corder, cset, data_size,  error)
     CALL check("h5aget_info_f",error,total_error)
     ! /* Check the attribute's information */
     CALL VERIFY("h5aget_info_f",corder,u,total_error)
     CALL Verifylogical("h5aget_info_f",f_corder_valid,.TRUE.,total_error)
     CALL VERIFY("h5aget_info_f", cset, H5T_CSET_ASCII_F, total_error)
     CALL h5aget_storage_size_f(attr_id, storage_size, error)
     CALL check("h5aget_storage_size_f",error,total_error)
     CALL VERIFY("h5aget_info_f", INT(data_size), INT(storage_size), total_error)

     ! /* Close attribute */
     CALL h5aclose_f(attr_id, error)
     CALL check("h5aclose_f",error,total_error)


     ! /* Open the attribute */
     CALL H5Aopen_by_name_f(fid, dsetname, attrname, attr_id, error)
     CALL check("H5Aopen_by_name_f", error, total_error)


     ! /* Get the attribute's information */
     CALL h5aget_info_f(attr_id, f_corder_valid, corder, cset, data_size,  error)
     CALL check("h5aget_info_f",error,total_error)

     ! /* Check the attribute's information */
     CALL VERIFY("h5aget_info_f",corder,u,total_error)
     CALL Verifylogical("h5aget_info_f",f_corder_valid,.TRUE.,total_error)
     CALL VERIFY("h5aget_info_f", cset, H5T_CSET_ASCII_F, total_error)
     CALL h5aget_storage_size_f(attr_id, storage_size, error)
     CALL check("h5aget_storage_size_f",error,total_error)
     CALL VERIFY("h5aget_info_f", INT(data_size), INT(storage_size), total_error)

     ! /* Close attribute */
     CALL h5aclose_f(attr_id, error)
     CALL check("h5aclose_f",error,total_error)
  ENDDO

END SUBROUTINE attr_open_check
