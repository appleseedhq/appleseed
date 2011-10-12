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
    SUBROUTINE identifier_test(cleanup, total_error)

!   This subroutine tests following functionalities: h5iget_type_f

   USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(OUT) :: total_error

     CHARACTER(LEN=6), PARAMETER :: filename = "itestf" ! File name
     CHARACTER(LEN=80) :: fix_filename
     CHARACTER(LEN=10), PARAMETER :: dsetname = "/itestdset" ! Dataset name
     CHARACTER(LEN=10), PARAMETER :: groupname = "itestgroup"! group name
     CHARACTER(LEN=10), PARAMETER :: aname = "itestattr"! group name



     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: new_file_id   ! File identifier
     INTEGER(HID_T) :: group_id      ! group identifier
     INTEGER(HID_T) :: dset_id       ! Dataset identifier
     INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
     INTEGER(HID_T) :: attr_id      ! Datatype attribute identifier
     INTEGER(HID_T) :: aspace_id     ! attribute data space identifier
     INTEGER(HID_T) :: atype_id     ! attribute data type identifier


     INTEGER, DIMENSION(1) :: dset_data = 0 ! Data value

     INTEGER(HSIZE_T), DIMENSION(1) :: dims = 1 ! Datasets dimensions
     INTEGER(HSIZE_T), DIMENSION(1) :: adims = 1 ! Attribute dimensions

     INTEGER, DIMENSION(1) ::  attr_data = 12
     INTEGER     ::   rank = 1 ! Datasets rank
     INTEGER     ::   arank = 1 ! Attribute rank

     INTEGER     ::   type !object identifier
     INTEGER     ::   error ! Error flag
     INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
     CHARACTER(LEN=80) name_buf
     CHARACTER(LEN=280) name_buf1
     INTEGER(SIZE_T)   buf_size
     INTEGER(SIZE_T)   name_size
     INTEGER    ::    ref_count ! Reference count for IDs


     INTEGER(hid_t) :: dtype  ! datatype id
     LOGICAL :: tri_ret       ! value

     !
     ! Tests the function H5Iis_valid_f
     !
     ! check that the ID is not valid
     dtype = -1
     CALL H5Iis_valid_f(dtype, tri_ret, error)
     CALL check("H5Iis_valid_f", error, total_error)
     CALL VerifyLogical("H5Iis_valid_f", tri_ret, .FALSE., total_error)

     ! Create a datatype id
     CALL H5Tcopy_f(H5T_NATIVE_INTEGER,dtype,error)
     CALL check("H5Tcopy_f", error, total_error)

     ! Check that the ID is valid
     CALL H5Iis_valid_f(dtype, tri_ret, error)
     CALL check("H5Iis_valid_f", error, total_error)
     CALL VerifyLogical("H5Tequal_f", tri_ret, .TRUE., total_error)

     CALL H5Tclose_f(dtype, error)
     CALL check("H5Tclose_f", error, total_error)

     !
     ! Create a new file using default properties.
     !
     CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
     endif
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
     CALL check("h5fcreate_f",error,total_error)

     !
     ! Create a group named "/MyGroup" in the file.
     !
     CALL h5gcreate_f(file_id, groupname, group_id, error)
     CALL check("h5gcreate_f",error,total_error)

     !
     !Create data space for the dataset.
     !
     CALL h5screate_simple_f(rank, dims, dspace_id, error)
     CALL check("h5screate_simple_f",error,total_error)

     !
     ! create dataset in the file.
     !
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dspace_id, &
               dset_id, error)
     CALL check("h5dcreate_f",error,total_error)
     !
     ! Get dataset name from dataset identifier
     !
     buf_size = 80
     CALL h5iget_name_f(dset_id, name_buf, buf_size, name_size, error)
     CALL check("h5iget_name_f",error,total_error)
      if (name_size .ne. len(dsetname)) then
          write(*,*) "h5iget_name returned wrong name size"
          total_error = total_error + 1
      else
          if (name_buf(1:name_size) .ne. dsetname) then
              write(*,*) "h5iget_name returned wrong name"
              total_error = total_error + 1
          endif
      endif

     !
     ! Get file identifier from dataset identifier and then get file name
     !
     CALL h5iget_file_id_f(dset_id, new_file_id, error)
     CALL check("h5iget_file_id_f",error,total_error)
     name_size = 280
     CALL h5fget_name_f(new_file_id, name_buf1, name_size, error)
     CALL check("h5fget_name_f",error,total_error)
          if (name_buf1(1:name_size) .ne. fix_filename(1:name_size)) then
              write(*,*) "h5fget_name returned wrong file name"
              total_error = total_error + 1
          endif

     !
     ! Write data_in to the dataset
     !
     data_dims(1) = 1
     CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, dset_data, data_dims, error)
     CALL check("h5dwrite_f",error,total_error)

     !
     ! Create scalar data space for dataset attribute.
     !
     CALL h5screate_simple_f(arank, adims, aspace_id, error)
     CALL check("h5screate_simple_f",error,total_error)

     !
     ! Create datatype for the Integer attribute.
     !
     CALL h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
     CALL check("h5tcopy_f",error,total_error)

     !
     ! Create dataset INTEGER attribute.
     !
     CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
     CALL check("h5acreate_f",error,total_error)

     !
     ! Write the Integer attribute data.
     !
     CALL h5awrite_f(attr_id, atype_id, attr_data, data_dims, error)
     CALL check("h5awrite_f",error,total_error)

     !
     !Get the file identifier
     !
     CALL h5iget_type_f(file_id, type, error)
     CALL check("h5iget_type_f",error,total_error)
     CALL verify("get file identifier wrong",type,H5I_FILE_F,total_error)

     !
     !Get the group identifier
     !
     CALL h5iget_type_f(group_id, type, error)
     CALL check("h5iget_type_f",error,total_error)
     CALL verify("get group identifier wrong",type,H5I_GROUP_F,total_error)

     !
     !Get the datatype identifier
     !
     CALL h5iget_type_f(atype_id, type, error)
     CALL check("h5iget_type_f",error,total_error)
     CALL verify("get datatype identifier wrong",type,H5I_DATATYPE_F,total_error)

     !
     !Get the dataspace identifier
     !
     CALL h5iget_type_f(aspace_id, type, error)
     CALL check("h5iget_type_f",error,total_error)
     CALL verify("get dataspace identifier wrong",type,H5I_DATASPACE_F,total_error)

     !
     !Get the dataset identifier
     !
     CALL h5iget_type_f(dset_id, type, error)
     CALL check("h5iget_type_f",error,total_error)
     CALL verify("get dataset identifier wrong",type,H5I_DATASET_F,total_error)

     !
     !Get the attribute identifier
     !
     CALL h5iget_type_f(attr_id, type, error)
     CALL check("h5iget_type_f",error,total_error)
     CALL verify("get attribute identifier wrong",type,H5I_ATTR_F,total_error)

     !
     ! Close the attribute.
     !
     CALL h5aclose_f(attr_id, error)
     CALL check("h5aclose_f",error,total_error)
     !
     ! Close the dataspace.
     !
     CALL h5sclose_f(aspace_id, error)
     CALL check("h5sclose_f",error,total_error)
     CALL h5sclose_f(dspace_id, error)
     CALL check("h5sclose_f",error,total_error)
     !
     ! Close the dataype.
     !
     CALL h5tclose_f(atype_id, error)
     CALL check("h5tclose_f",error,total_error)

     !
     ! Close the dataset.
     !
     CALL h5dclose_f(dset_id, error)
     CALL check("h5dclose_f",error,total_error)

     !
     ! Close the group.
     !
     CALL h5gclose_f(group_id, error)
     CALL check("h5gclose_f",error,total_error)

     !
     ! Close the file.
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f",error,total_error)
     CALL h5fclose_f(new_file_id, error)
     CALL check("h5fclose_f",error,total_error)

     !
     ! Basic Test of increment/decrement ID functions
     !

     ! Create a file
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
     CALL check("h5fcreate_f",error,total_error)

     ! Get the reference count for the file ID
     CALL h5iget_ref_f(file_id, ref_count, error)
     CALL check("h5iget_ref_f",error,total_error)
     CALL verify("get file ref count wrong",ref_count,1,total_error)

     ! Increment the reference count for the file ID
     CALL h5iinc_ref_f(file_id, ref_count, error)
     CALL check("h5iinc_ref_f",error,total_error)
     CALL verify("get file ref count wrong",ref_count,2,total_error)

     ! Close the file normally.
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f",error,total_error)

     ! Get the reference count for the file ID
     CALL h5iget_ref_f(file_id, ref_count, error)
     CALL check("h5iget_ref_f",error,total_error)
     CALL verify("get file ref count wrong",ref_count,1,total_error)

     ! Close the file by decrementing the reference count
     CALL h5idec_ref_f(file_id, ref_count, error)
     CALL check("h5idec_ref_f",error,total_error)
     CALL verify("get file ref count wrong",ref_count,0,total_error)
     ! Try closing the file again (should fail)
     CALL h5eset_auto_f(0, error)
     CALL h5fclose_f(file_id, error)
     CALL verify("file close should fail",error,-1,total_error)
     ! Clear the error stack from the file close failure
     CALL h5eset_auto_f(1, error)
     CALL h5eclear_f(error)

      if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)

     RETURN
     END SUBROUTINE identifier_test
