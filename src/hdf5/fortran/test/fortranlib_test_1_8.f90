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
!
!    Testing Fortran wrappers introduced in 1.8 release.
!
PROGRAM fortranlibtest

  USE HDF5

  IMPLICIT NONE
  INTEGER :: total_error = 0
  INTEGER :: error
  INTEGER :: ret_total_error
  INTEGER :: majnum, minnum, relnum
  LOGICAL :: cleanup, status

  CALL h5open_f(error)

  cleanup = .TRUE.
  CALL h5_env_nocleanup_f(status)
  IF(status) cleanup=.FALSE.

  WRITE(*,*) '                       ==========================                            '
  WRITE(*,*) '                              FORTRAN 1.8 tests '
  WRITE(*,*) '                       ==========================                            '
  CALL h5get_libversion_f(majnum, minnum, relnum, total_error)
  IF(total_error .EQ. 0) THEN
     WRITE(*, '(" FORTRANLIB_TEST is linked with HDF5 Library version ")', advance="NO")
     WRITE(*, '(I1)', advance="NO") majnum
     WRITE(*, '(".")', advance="NO")
     WRITE(*, '(I1)', advance="NO") minnum
     WRITE(*, '(" release ")', advance="NO")
     WRITE(*, '(I3)') relnum
  ELSE
     total_error = total_error + 1
  ENDIF
  WRITE(*,*)

  ret_total_error = 0
  CALL file_space("file_space_1_8",cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing file free space', &
       total_error)

  ret_total_error = 0
  CALL attribute_test_1_8(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing attributes', &
       total_error)

  ret_total_error = 0
  CALL group_test(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing groups', &
       total_error)

  ret_total_error = 0
  CALL test_h5o(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing object interface', &
       total_error)

  ret_total_error = 0
  CALL dtransform(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing data transform', &
       total_error)

  ret_total_error = 0
  CALL test_genprop_basic_class(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing basic generic properties', &
       total_error)

  ret_total_error = 0
  CALL test_h5s_encode(cleanup, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing dataspace encoding and decoding', &
       total_error)



!  CALL test_hard_query(group_total_error)

  WRITE(*,*)

  WRITE(*,*) '                  ============================================  '
  WRITE(*, fmt = '(19x, 27a)', advance='NO') ' FORTRAN tests completed with '
  WRITE(*, fmt = '(i4)', advance='NO') total_error
  WRITE(*, fmt = '(12a)' ) ' error(s) ! '
  WRITE(*,*) '                  ============================================  '

  CALL h5close_f(error)

  ! if errors detected, exit with non-zero code.
  IF (total_error .NE. 0) CALL h5_exit_f (1)

END PROGRAM fortranlibtest

SUBROUTINE dtransform(cleanup, total_error)
  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(HID_T) :: dxpl_id_c_to_f
  INTEGER(HID_T) :: file_id

  CHARACTER(LEN=15), PARAMETER :: c_to_f = "(9/5.0)*x + 123"
  INTEGER :: error
  CHARACTER(LEN=15) :: ptrgetTest
  CHARACTER(LEN=7) :: ptrgetTest_small
  CHARACTER(LEN=30) :: ptrgetTest_big

  INTEGER(SIZE_T) :: size


  CALL H5Fcreate_f("dtransform.h5", H5F_ACC_TRUNC_F, file_id, error)
  CALL check("dtransform.H5Fcreate_f", error, total_error)

  CALL H5Pcreate_f(H5P_DATASET_XFER_F, dxpl_id_c_to_f, error)
  CALL check("dtransform.H5Pcreate_f", error, total_error)

  CALL H5Pset_data_transform_f(dxpl_id_c_to_f, c_to_f, error)
  CALL check("dtransform.H5Pset_data_transform_f", error, total_error)

  CALL H5Pget_data_transform_f(dxpl_id_c_to_f, ptrgetTest, error, size=size)
  CALL check("dtransform.H5Pget_data_transform_f",  error, total_error)
  CALL VerifyString("dtransform.H5Pget_data_transform_f", c_to_f, ptrgetTest, total_error)
  CALL VERIFY("dtransform.H5Pget_data_transform_f", INT(size),15, total_error)

! check case when receiving buffer to small

  CALL H5Pget_data_transform_f(dxpl_id_c_to_f, ptrgetTest_small, error, size=size)
  CALL check("dtransform.H5Pget_data_transform_f",  error, total_error)
  CALL VerifyString("dtransform.H5Pget_data_transform_f", c_to_f(1:7), ptrgetTest_small, total_error)
  CALL VERIFY("dtransform.H5Pget_data_transform_f", INT(size),15, total_error)

! check case when receiving buffer to big

  CALL H5Pget_data_transform_f(dxpl_id_c_to_f, ptrgetTest_big, error, size=size)
  CALL check("dtransform.H5Pget_data_transform_f",  error, total_error)
  CALL VerifyString("dtransform.H5Pget_data_transform_f", c_to_f(1:15), ptrgetTest_big(1:15), total_error)
  CALL VERIFY("dtransform.H5Pget_data_transform_f", INT(size), 15, total_error)

  CALL H5Fclose_f(file_id, error)
  CALL check("H5Fclose_f", error, total_error)

  IF(cleanup) CALL h5_cleanup_f("dtransform", H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)


END SUBROUTINE dtransform


!/****************************************************************
!**
!**  test_genprop_basic_class(): Test basic generic property list code.
!**      Tests creating new generic classes.
!**
!****************************************************************/

SUBROUTINE test_genprop_basic_class(cleanup, total_error)

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(HID_T) :: cid1		!/* Generic Property class ID */
  INTEGER(HID_T) :: cid2		!/* Generic Property class ID */

  CHARACTER(LEN=7) :: CLASS1_NAME = "Class 1"
  CHARACTER(LEN=7)  :: name              ! /* Name of class */
  CHARACTER(LEN=10) :: name_big          ! /* Name of class bigger buffer */
  CHARACTER(LEN=4)  :: name_small        ! /* Name of class smaller buffer*/
  INTEGER :: error
  INTEGER :: size
  LOGICAL :: flag

  !/* Output message about test being performed */

  !WRITE(*,*) "Testing Basic Generic Property List Class Creation Functionality"

  ! /* Create a new generic class, derived from the root of the class hierarchy */
  CALL H5Pcreate_class_f(H5P_ROOT_F, CLASS1_NAME, cid1, error)
  CALL check("H5Pcreate_class", error, total_error)

  ! /* Check class name */
  CALL H5Pget_class_name_f(cid1, name, size, error)
  CALL check("H5Pget_class_name", error, total_error)
  CALL VERIFY("H5Pget_class_name", size,7,error)
  CALL verifystring("H5Pget_class_name", name, CLASS1_NAME, error)
  IF(error.NE.0)THEN
     WRITE(*,*) 'Class names do not match! name=',name, 'CLASS1_NAME=',CLASS1_NAME
     total_error = total_error + 1
  ENDIF

  ! /* Check class name smaller buffer*/
  CALL H5Pget_class_name_f(cid1, name_small, size, error)
  CALL check("H5Pget_class_name", error, total_error)
  CALL VERIFY("H5Pget_class_name", size,7,error)
  CALL verifystring("H5Pget_class_name", name_small(1:4), CLASS1_NAME(1:4), error)
  IF(error.NE.0)THEN
     WRITE(*,*) 'Class names do not match! name=',name_small(1:4), 'CLASS1_NAME=',CLASS1_NAME(1:4)
     total_error = total_error + 1
  ENDIF

  ! /* Check class name bigger buffer*/
  CALL H5Pget_class_name_f(cid1, name_big, size, error)
  CALL check("H5Pget_class_name", error, total_error)
  CALL VERIFY("H5Pget_class_name", size,7,error)
  CALL verifystring("H5Pget_class_name", TRIM(name_big), TRIM(CLASS1_NAME), error)
  IF(error.NE.0)THEN
     WRITE(*,*) 'Class names do not match! name=',TRIM(name_small), 'CLASS1_NAME=',TRIM(CLASS1_NAME)
     total_error = total_error + 1
  ENDIF

  ! /* Check class parent */
  CALL H5Pget_class_parent_f(cid1, cid2, error)
  CALL check("H5Pget_class_parent_f", error, total_error)

  ! /* Verify class parent correct */
  CALL H5Pequal_f(cid2, H5P_ROOT_F, flag, error)
  CALL check("H5Pequal_f", error, total_error)
  CALL verifylogical("H5Pequal_f", flag, .TRUE., total_error)


  ! /* Make certain false postives aren't being returned */
  CALL H5Pequal_f(cid2, H5P_FILE_CREATE_F, flag, error)
  CALL check("H5Pequal_f", error, total_error)
  CALL verifylogical("H5Pequal_f", flag, .FALSE., total_error)

  !/* Close parent class */
  CALL H5Pclose_class_f(cid2, error)
  CALL check("H5Pclose_class_f", error, total_error)


  !/* Close class */
  CALL H5Pclose_class_f(cid1, error)
  CALL check("H5Pclose_class_f", error, total_error)

END SUBROUTINE test_genprop_basic_class

SUBROUTINE test_h5s_encode(cleanup, total_error)

!/****************************************************************
!**
!**  test_h5s_encode(): Test H5S (dataspace) encoding and decoding.
!**
!****************************************************************/

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER(hid_t) :: sid1, sid3!	/* Dataspace ID		*/
  INTEGER(hid_t) :: decoded_sid1, decoded_sid3
  INTEGER :: rank	!/* LOGICAL rank of dataspace	*/
  INTEGER(size_t) :: sbuf_size=0, scalar_size=0

! Make sure the size is large, need variable length in fortran 2003
  CHARACTER(LEN=288) :: sbuf
  CHARACTER(LEN=288) :: scalar_buf
! F2003  CHARACTER(LEN=:), ALLOCATABLE :: sbuf

!    unsigned char       *sbuf=NULL, *null_sbuf=NULL, *scalar_buf=NULL;
!    hsize_t		tdims[4];	/* Dimension array to test with */
  INTEGER(hsize_t) :: n ! /* Number of dataspace elements */

  INTEGER(hsize_t), DIMENSION(1:3) :: start = (/0, 0, 0/)
  INTEGER(hsize_t), DIMENSION(1:3) :: stride = (/2, 5, 3/)
  INTEGER(hsize_t), DIMENSION(1:3) :: count = (/2, 2, 2/)
  INTEGER(hsize_t), DIMENSION(1:3) :: BLOCK = (/1, 3, 1/)

  INTEGER :: space_type

!    H5S_sel_type        sel_type;
!    hssize_t            nblocks;
  !
  !Dataset dimensions
  !
  INTEGER, PARAMETER :: SPACE1_DIM1= 3,  SPACE1_DIM2=15, SPACE1_DIM3=13

  INTEGER(HSIZE_T), DIMENSION(1:3) :: dims1 = (/SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3/)
  INTEGER :: SPACE1_RANK = 3
  INTEGER :: error

  !/* Output message about test being performed */
  !WRITE(*,*) "Testing Dataspace Encoding and Decoding"

  !/*-------------------------------------------------------------------------
  ! * Test encoding and decoding of simple dataspace and hyperslab selection.
  ! *-------------------------------------------------------------------------
  ! */

  CALL H5Screate_simple_f(SPACE1_RANK, dims1, sid1, error)
  CALL check("H5Screate_simple", error, total_error)

  CALL h5sselect_hyperslab_f(sid1, H5S_SELECT_SET_F, &
       start, count, error, stride=stride, BLOCK=BLOCK)
  CALL check("h5sselect_hyperslab_f", error, total_error)


  !/* Encode simple data space in a buffer */

  !         First find the buffer size
  CALL H5Sencode_f(sid1, sbuf, sbuf_size, error)
  CALL check("H5Sencode", error, total_error)

  ! In fortran 2003 we can allocate the needed character size here

  ! /* Try decoding bogus buffer */

  CALL H5Sdecode_f(sbuf, decoded_sid1, error)
  CALL VERIFY("H5Sdecode", error, -1, total_error)

  CALL H5Sencode_f(sid1, sbuf, sbuf_size, error)
  CALL check("H5Sencode", error, total_error)

  ! /* Decode from the dataspace buffer and return an object handle */
  CALL H5Sdecode_f(sbuf, decoded_sid1, error)
  CALL check("H5Sdecode", error, total_error)


  ! /* Verify the decoded dataspace */
  CALL h5sget_simple_extent_npoints_f(decoded_sid1, n, error)
  CALL check("h5sget_simple_extent_npoints_f", error, total_error)
  CALL VERIFY("h5sget_simple_extent_npoints_f", INT(n), SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3, &
       total_error)

!!$
!!$    rank = H5Sget_simple_extent_ndims(decoded_sid1);
!!$    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
!!$    VERIFY(rank, SPACE1_RANK, "H5Sget_simple_extent_ndims");
!!$
!!$    rank = H5Sget_simple_extent_dims(decoded_sid1, tdims, NULL);
!!$    CHECK(rank, FAIL, "H5Sget_simple_extent_dims");
!!$    VERIFY(HDmemcmp(tdims, dims1, SPACE1_RANK * sizeof(hsize_t)), 0,
!!$	   "H5Sget_simple_extent_dims");
!!$
!!$    /* Verify hyperslabe selection */
!!$    sel_type = H5Sget_select_type(decoded_sid1);
!!$    VERIFY(sel_type, H5S_SEL_HYPERSLABS, "H5Sget_select_type");
!!$
!!$    nblocks = H5Sget_select_hyper_nblocks(decoded_sid1);
!!$    VERIFY(nblocks, 2*2*2, "H5Sget_select_hyper_nblocks");
!!$
  !
  !Close the dataspace for the dataset.
  !
  CALL h5sclose_f(sid1, error)
  CALL check("h5sclose_f", error, total_error)

  CALL h5sclose_f(decoded_sid1, error)
  CALL check("h5sclose_f", error, total_error)

  ! /*-------------------------------------------------------------------------
  !  * Test encoding and decoding of scalar dataspace.
  !  *-------------------------------------------------------------------------
  !  */
  ! /* Create scalar dataspace */

  CALL H5Screate_f(H5S_SCALAR_F, sid3, error)
  CALL check("H5Screate_f",error, total_error)

  ! /* Encode scalar data space in a buffer */

  !        First find the buffer size
  CALL H5Sencode_f(sid3, scalar_buf, scalar_size, error)
  CALL check("H5Sencode_f", error, total_error)

  ! encode

  CALL H5Sencode_f(sid3, scalar_buf, scalar_size, error)
  CALL check("H5Sencode_f", error, total_error)


  ! /* Decode from the dataspace buffer and return an object handle */

  CALL H5Sdecode_f(scalar_buf, decoded_sid3, error)
  CALL check("H5Sdecode_f", error, total_error)


  ! /* Verify extent type */

  CALL H5Sget_simple_extent_type_f(decoded_sid3, space_type, error)
  CALL check("H5Sget_simple_extent_type_f", error, total_error)
  CALL VERIFY("H5Sget_simple_extent_type_f", space_type, H5S_SCALAR_F, total_error)

  ! /* Verify decoded dataspace */
  CALL h5sget_simple_extent_npoints_f(decoded_sid3, n, error)
  CALL check("h5sget_simple_extent_npoints_f", error, total_error)
  CALL VERIFY("h5sget_simple_extent_npoints_f", INT(n), 1, total_error)

  CALL H5Sget_simple_extent_ndims_f(decoded_sid3, rank, error)
  CALL CHECK("H5Sget_simple_extent_ndims_f", error, total_error)
  CALL VERIFY("H5Sget_simple_extent_ndims_f", rank, 0, total_error )

  CALL h5sclose_f(sid3, error)
  CALL check("h5sclose_f", error, total_error)

  CALL h5sclose_f(decoded_sid3, error)
  CALL check("h5sclose_f", error, total_error)

END SUBROUTINE test_h5s_encode

