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
!    Testing Reference Interface functionality.
!
!    The following subroutine tests h5rcreate_f, h5rdereference_f, h5rget_name_f
!    and H5Rget_object_type functions
!
SUBROUTINE refobjtest(cleanup, total_error)
  USE HDF5 ! This module contains all necessary modules
  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(OUT) :: total_error

  CHARACTER(LEN=9), PARAMETER :: filename = "reference"
  CHARACTER(LEN=80) :: fix_filename
  CHARACTER(LEN=8), PARAMETER :: dsetnamei = "INTEGERS"
  CHARACTER(LEN=17), PARAMETER :: dsetnamer = "OBJECT_REFERENCES"
  CHARACTER(LEN=6), PARAMETER :: groupname1 = "GROUP1"
  CHARACTER(LEN=6), PARAMETER :: groupname2 = "GROUP2"

  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: grp1_id       ! Group identifier
  INTEGER(HID_T) :: grp2_id       ! Group identifier
  INTEGER(HID_T) :: dset1_id      ! Dataset identifier
  INTEGER(HID_T) :: dsetr_id      ! Dataset identifier
  INTEGER(HID_T) :: type_id       ! Type identifier
  INTEGER(HID_T) :: space_id      ! Dataspace identifier
  INTEGER(HID_T) :: spacer_id     ! Dataspace identifier
  INTEGER     ::   error, obj_type
  INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/5/)
  INTEGER(HSIZE_T), DIMENSION(1) :: dimsr= (/4/)
  INTEGER(HSIZE_T), DIMENSION(1) :: my_maxdims = (/5/)
  INTEGER :: rank = 1
  INTEGER :: rankr = 1
  TYPE(hobj_ref_t_f), DIMENSION(4) ::  ref
  TYPE(hobj_ref_t_f), DIMENSION(4) ::  ref_out
  INTEGER(HSIZE_T), DIMENSION(1) :: ref_dim
  INTEGER, DIMENSION(5) :: DATA = (/1, 2, 3, 4, 5/)
  INTEGER(HSIZE_T), DIMENSION(2) :: data_dims

  CHARACTER(LEN=7) :: buf        ! buffer to hold the region name
  CHARACTER(LEN=16) :: buf_big    ! buffer bigger then needed
  CHARACTER(LEN=4) :: buf_small  ! buffer smaller then needed
  INTEGER(SIZE_T) :: buf_size     ! returned size of the region buffer name

  !
  !Create a new file with Default file access and
  !file creation properties .
  !
  CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
  IF (error .NE. 0) THEN
     WRITE(*,*) "Cannot modify filename"
     STOP
  ENDIF
  CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
  CALL check("h5fcreate_f",error,total_error)


  !
  ! Create a group inside the file
  !
  CALL h5gcreate_f(file_id, groupname1, grp1_id, error)
  CALL check("h5gcreate_f",error,total_error)

  !
  ! Create a group inside the group GROUP1
  !
  CALL h5gcreate_f(grp1_id, groupname2, grp2_id, error)
  CALL check("h5gcreate_f",error,total_error)

  !
  ! Create dataspaces for datasets
  !
  CALL h5screate_simple_f(rank, dims, space_id, error, maxdims=my_maxdims)
  CALL check("h5screate_simple_f",error,total_error)
  CALL h5screate_simple_f(rankr, dimsr, spacer_id, error)
  CALL check("h5screate_simple_f",error,total_error)

  !
  ! Create integer dataset
  !
  CALL h5dcreate_f(file_id, dsetnamei, H5T_NATIVE_INTEGER, space_id, &
       dset1_id, error)
  CALL check("h5dcreate_f",error,total_error)
  !
  ! Create dataset to store references to the objects
  !
  CALL h5dcreate_f(file_id, dsetnamer, H5T_STD_REF_OBJ, spacer_id, &
       dsetr_id, error)
  CALL check("h5dcreate_f",error,total_error)
  !
  ! Create a datatype and store in the file
  !
  CALL h5tcopy_f(H5T_NATIVE_REAL, type_id, error)
  CALL check("h5tcopy_f",error,total_error)
  CALL h5tcommit_f(file_id, "MyType", type_id, error)
  CALL check("h5tcommit_f",error,total_error)

  !
  !  Close dataspaces, groups and integer dataset
  !
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5sclose_f(spacer_id, error)
  CALL check("h5sclose_f",error,total_error)
  CALL h5dclose_f(dset1_id, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("h5tclose_f",error,total_error)
  CALL h5gclose_f(grp1_id, error)
  CALL check("h5gclose_f",error,total_error)
  CALL h5gclose_f(grp2_id, error)
  CALL check("h5gclose_f",error,total_error)

  !
  ! Craete references to two groups, integer dataset and shared datatype
  ! and write it to the dataset in the file
  !
  CALL h5rcreate_f(file_id, groupname1, ref(1), error)
  CALL check("h5rcreate_f",error,total_error)
  CALL h5rcreate_f(file_id, "/GROUP1/GROUP2", ref(2), error)
  CALL check("h5rcreate_f",error,total_error)
  CALL h5rcreate_f(file_id, dsetnamei, ref(3), error)
  CALL check("h5rcreate_f",error,total_error)
  CALL h5rcreate_f(file_id, "MyType", ref(4), error)
  CALL check("h5rcreate_f",error,total_error)
  ref_dim(1) = SIZE(ref)
  CALL h5dwrite_f(dsetr_id, H5T_STD_REF_OBJ, ref, ref_dim, error)
  CALL check("h5dwrite_f",error,total_error)

  ! getting path to normal dataset in root group

  CALL H5Rget_name_f(dsetr_id, ref(1), buf, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)


  CALL VERIFY("H5Rget_name_f", INT(buf_size),7, total_error)
  CALL VerifyString("H5Rget_name_f", buf, "/GROUP1", total_error)

  ! with buffer bigger then needed

  CALL H5Rget_name_f(dsetr_id, ref(1), buf_big, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL VERIFY("H5Rget_name_f", INT(buf_size),7,total_error)
  CALL VerifyString("H5Rget_name_f", TRIM(buf_big), "/GROUP1", total_error)

  ! getting path to dataset in /Group1

  CALL H5Rget_name_f(dsetr_id, ref(2), buf_big, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL VERIFY("H5Rget_name_f", INT(buf_size),14,total_error)
  CALL VerifyString("H5Rget_name_f", TRIM(buf_big), "/GROUP1/GROUP2", total_error)

  !
  !Close the dataset
  !
  CALL h5dclose_f(dsetr_id, error)
  CALL check("h5dclose_f",error,total_error)

  !
  ! Reopen the dataset with object references
  !
  CALL h5dopen_f(file_id, dsetnamer,dsetr_id,error)
  CALL check("h5dopen_f",error,total_error)
  ref_dim(1) = SIZE(ref_out)
  CALL h5dread_f(dsetr_id, H5T_STD_REF_OBJ, ref_out, ref_dim, error)
  CALL check("h5dread_f",error,total_error)

  !
  !get the third reference's type and Dereference it
  !
  CALL h5rget_object_type_f(dsetr_id, ref(3), obj_type, error)
  CALL check("h5rget_object_type_f",error,total_error)
  IF (obj_type == H5G_DATASET_F) THEN
     CALL h5rdereference_f(dsetr_id, ref(3), dset1_id, error)
     CALL check("h5rdereference_f",error,total_error)

     data_dims(1) = 5
     CALL h5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, DATA, data_dims, error)
     CALL check("h5dwrite_f",error,total_error)
  END IF

  !
  !get the fourth reference's type and Dereference it
  !
  CALL h5rget_object_type_f(dsetr_id, ref(4), obj_type, error)
  CALL check("h5rget_object_type_f",error,total_error)
  IF (obj_type == H5G_TYPE_F) THEN
     CALL h5rdereference_f(dsetr_id, ref(4), type_id, error)
     CALL check("h5rdereference_f",error,total_error)
  END IF

  !
  ! Close all objects.
  !
  CALL h5dclose_f(dset1_id, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("h5tclose_f",error,total_error)

  CALL h5dclose_f(dsetr_id, error)
  CALL check("h5dclose_f",error,total_error)
  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f",error,total_error)


  IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)
  RETURN

END SUBROUTINE refobjtest
!
!   The following subroutine tests h5rget_region_f, h5rcreate_f, h5rget_name_f,
!   and h5rdereference_f functionalities
!
SUBROUTINE refregtest(cleanup, total_error)
  USE HDF5 ! This module contains all necessary modules
  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(OUT) :: total_error

  CHARACTER(LEN=6), PARAMETER :: filename = "Refreg"
  CHARACTER(LEN=80) :: fix_filename
  CHARACTER(LEN=6), PARAMETER :: dsetnamev = "MATRIX"
  CHARACTER(LEN=17), PARAMETER :: dsetnamer = "REGION_REFERENCES"

  CHARACTER(LEN=7) :: buf        ! buffer to hold the region name
  CHARACTER(LEN=11) :: buf_big    ! buffer bigger then needed
  CHARACTER(LEN=4) :: buf_small  ! buffer smaller then needed
  INTEGER(SIZE_T) :: buf_size     ! returned size of the region buffer name
  INTEGER(HID_T) :: file_id       ! File identifier
  INTEGER(HID_T) :: space_id      ! Dataspace identifier
  INTEGER(HID_T) :: spacer_id     ! Dataspace identifier
  INTEGER(HID_T) :: dsetv_id      ! Dataset identifier
  INTEGER(HID_T) :: dsetr_id      ! Dataset identifier
  INTEGER     ::   error
  TYPE(hdset_reg_ref_t_f) , DIMENSION(2) :: ref     ! Buffers to store references
  TYPE(hdset_reg_ref_t_f) , DIMENSION(2) :: ref_out !
  INTEGER(HSIZE_T), DIMENSION(2) :: ref_dim
  INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
  INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/2,9/)  ! Datasets dimensions
  INTEGER(HSIZE_T), DIMENSION(1) :: dimsr = (/2/)   !
  INTEGER(HSIZE_T), DIMENSION(2) :: start
  INTEGER(HSIZE_T), DIMENSION(2) :: count
  INTEGER :: rankr = 1
  INTEGER :: rank = 2
  INTEGER , DIMENSION(2,9) ::  DATA
  INTEGER , DIMENSION(2,9) ::  data_out = 0
  INTEGER(HSIZE_T) , DIMENSION(2,3) :: coord
  INTEGER(SIZE_T) ::num_points = 3  ! Number of selected points
  coord = RESHAPE((/1,1,2,7,1,9/), (/2,3/))   ! Coordinates of selected points
  DATA = RESHAPE ((/1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6/), (/2,9/))

  !
  !  Initialize FORTRAN predefined datatypes.
  !
  !          CALL h5init_types_f(error)
  !              CALL check("h5init_types_f", error, total_error)
  !
  !  Create a new file.
  !
  CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
  IF (error .NE. 0) THEN
     WRITE(*,*) "Cannot modify filename"
     STOP
  ENDIF
  CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
  ! Default file access and file creation
  ! properties are used.
  CALL check("h5fcreate_f", error, total_error)
  !
  ! Create  dataspaces:
  !
  ! for dataset with references to dataset regions
  !
  CALL h5screate_simple_f(rankr, dimsr, spacer_id, error)
  CALL check("h5screate_simple_f", error, total_error)
  !
  ! for integer dataset
  !
  CALL h5screate_simple_f(rank, dims, space_id, error)
  CALL check("h5screate_simple_f", error, total_error)
  !
  ! Create  and write datasets:
  !
  ! Integer dataset
  !
  CALL h5dcreate_f(file_id, dsetnamev, H5T_NATIVE_INTEGER, space_id, &
       dsetv_id, error)
  CALL check("h5dcreate_f", error, total_error)
  data_dims(1) = 2
  data_dims(2) = 9
  CALL h5dwrite_f(dsetv_id, H5T_NATIVE_INTEGER, DATA, data_dims, error)
  CALL check("h5dwrite_f", error, total_error)

  CALL h5dclose_f(dsetv_id, error)
  CALL check("h5dclose_f", error, total_error)
  !
  ! Dataset with references
  !
  CALL h5dcreate_f(file_id, dsetnamer, H5T_STD_REF_DSETREG, spacer_id, &
       dsetr_id, error)
  CALL check("h5dcreate_f", error, total_error)
  !
  ! Create a reference to the hyperslab selection.
  !
  start(1) = 0
  start(2) = 3
  COUNT(1) = 2
  COUNT(2) = 3
  CALL h5sselect_hyperslab_f(space_id, H5S_SELECT_SET_F, &
       start, count, error)
  CALL check("h5sselect_hyperslab_f", error, total_error)
  CALL h5rcreate_f(file_id, dsetnamev, space_id, ref(1), error)
  CALL check("h5rcreate_f", error, total_error)
  !
  ! Create a reference to elements selection.
  !
  CALL h5sselect_none_f(space_id, error)
  CALL check("h5sselect_none_f", error, total_error)
  CALL h5sselect_elements_f(space_id, H5S_SELECT_SET_F, rank, num_points,&
       coord, error)
  CALL check("h5sselect_elements_f", error, total_error)
  CALL h5rcreate_f(file_id, dsetnamev, space_id, ref(2), error)
  CALL check("h5rcreate_f", error, total_error)
  !
  ! Write dataset with the references.
  !
  ref_dim(1) = SIZE(ref)
  CALL h5dwrite_f(dsetr_id, H5T_STD_REF_DSETREG, ref, ref_dim, error)
  CALL check("h5dwrite_f", error, total_error)
  !
  ! Close all objects.
  !
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5sclose_f(spacer_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5dclose_f(dsetr_id, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f", error, total_error)
  !
  ! Reopen the file to test selections.
  !
  CALL h5fopen_f (fix_filename, H5F_ACC_RDWR_F, file_id, error)
  CALL check("h5fopen_f", error, total_error)
  CALL h5dopen_f(file_id, dsetnamer, dsetr_id, error)
  CALL check("h5dopen_f", error, total_error)
  !
  ! Read references to the dataset regions.
  !
  ref_dim(1) = SIZE(ref_out)
  CALL h5dread_f(dsetr_id, H5T_STD_REF_DSETREG, ref_out, ref_dim, error)
  CALL check("h5dread_f", error, total_error)


  ! Get name of the dataset the first region reference points to using H5Rget_name_f
  CALL H5Rget_name_f(dsetr_id, ref_out(1), buf, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL VERIFY("H5Rget_name_f", INT(buf_size),7,total_error)
  CALL VerifyString("H5Rget_name_f", buf, "/MATRIX", total_error)

  ! Get name of the dataset the first region reference points to using H5Rget_name_f
  ! buffer bigger then needed
  CALL H5Rget_name_f(dsetr_id, ref_out(1), buf_big, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL VERIFY("H5Rget_name_f", INT(buf_size),7,total_error)
  CALL VerifyString("H5Rget_name_f", TRIM(buf_big), "/MATRIX", total_error)


  ! Get name of the dataset the first region reference points to using H5Rget_name_f
  ! buffer smaller then needed
  CALL H5Rget_name_f(dsetr_id, ref_out(1), buf_small, error, buf_size )
  CALL check("H5Rget_name_f", error, total_error)
  CALL VERIFY("H5Rget_name_f", INT(buf_size),7,total_error)
  CALL VerifyString("H5Rget_name_f", TRIM(buf_small), "/MAT", total_error)

  !
  ! Dereference the first reference.
  !
  CALL H5rdereference_f(dsetr_id, ref_out(1), dsetv_id, error)
  CALL check("h5rdereference_f", error, total_error)
  CALL H5rget_region_f(dsetr_id, ref_out(1), space_id, error)
  CALL check("h5rget_region_f", error, total_error)

  ! Get name of the dataset the second region reference points to using H5Rget_name_f
  CALL H5Rget_name_f(dsetr_id, ref_out(2), buf, error) ! no optional size
  CALL check("H5Rget_name_f", error, total_error)
  CALL VerifyString("H5Rget_name_f", buf, "/MATRIX", total_error)


  !
  ! Read selected data from the dataset.
  !
  data_dims(1) = 2
  data_dims(2) = 9
  CALL h5dread_f(dsetv_id, H5T_NATIVE_INTEGER, data_out, data_dims, error, &
       mem_space_id = space_id, file_space_id = space_id)
  CALL check("h5dread_f", error, total_error)
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5dclose_f(dsetv_id, error)
  CALL check("h5dclose_f", error, total_error)
  data_out = 0
  !
  ! Dereference the second reference.
  !
  CALL H5rdereference_f(dsetr_id, ref_out(2), dsetv_id, error)
  CALL check("h5rdereference_f", error, total_error)

  CALL H5rget_region_f(dsetr_id, ref_out(2), space_id, error)
  CALL check("h5rget_region_f", error, total_error)
  !
  ! Read selected data from the dataset.
  !
  CALL h5dread_f(dsetv_id, H5T_NATIVE_INTEGER, data_out, data_dims, error, &
       mem_space_id = space_id, file_space_id = space_id)
  CALL check("h5dread_f", error, total_error)
  !
  ! Close all objects
  !
  CALL h5sclose_f(space_id, error)
  CALL check("h5sclose_f", error, total_error)
  CALL h5dclose_f(dsetv_id, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5dclose_f(dsetr_id, error)
  CALL check("h5dclose_f", error, total_error)
  CALL h5fclose_f(file_id, error)
  CALL check("h5fclose_f", error, total_error)


  IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)
  RETURN

END SUBROUTINE refregtest

