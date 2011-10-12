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
    SUBROUTINE group_test(cleanup, total_error)

!   This subroutine tests following functionalities:
!   h5gcreate_f, h5gopen_f, h5gclose_f, (?)h5gget_obj_info_idx_f,  h5gn_members_f
!   h5glink(2)_f, h5gunlink_f, h5gmove(2)_f,  h5gget_linkval_f, h5gset_comment_f,
!   h5gget_comment_f

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(OUT) :: total_error

     CHARACTER(LEN=5), PARAMETER :: filename = "gtest"    !File name
     CHARACTER(LEN=80) :: fix_filename
     CHARACTER(LEN=33), PARAMETER :: comment = "Testing the group functionalities"
                                              ! comment for this file
     CHARACTER(LEN=7), PARAMETER :: groupname1 = "MyGroup"  ! Group name
     CHARACTER(LEN=16), PARAMETER :: groupname2 = "/MyGroup/Group_A"
     CHARACTER(LEN=9), PARAMETER :: linkname1 = "hardlink1"
     CHARACTER(LEN=9), PARAMETER :: linkname2 = "hardlink2"
     CHARACTER(LEN=9), PARAMETER :: linkname3 = "softlink1"
     CHARACTER(LEN=9), PARAMETER :: linkname4 = "softlink2"
     CHARACTER(LEN=12), PARAMETER :: linkname5 = "newsoftlink2"

     CHARACTER(LEN=13), PARAMETER :: dsetname1 = "MyGroup/dset1" ! Dataset name
     CHARACTER(LEN=5),  PARAMETER :: dsetname2 = "dset2" ! dataset name

     INTEGER(HID_T) :: file_id       ! File identifier
     INTEGER(HID_T) :: group1_id      ! Group identifier
     INTEGER(HID_T) :: group2_id      ! Group identifier
     INTEGER(HID_T) :: dset1_id    ! Dataset identifier
     INTEGER(HID_T) :: dset2_id    ! Dataset identifier
     INTEGER(HID_T) :: dsetnew_id    ! Dataset identifier
     INTEGER(HID_T) :: dspace_id  ! Data space identifier

     INTEGER, DIMENSION(1) :: dset1_data = 34 ! Data value
     INTEGER, DIMENSION(1) :: dset2_data = 98 ! Data value
     INTEGER(HSIZE_T), DIMENSION(1) :: dims = 1 ! Datasets dimensions
     INTEGER     ::   rank = 1 ! Datasets rank
     INTEGER     ::   error ! Error flag
     INTEGER(SIZE_T)   ::   namesize = 100 !size for symbolic object
     CHARACTER(LEN=100) :: name !name to put symbolic object
     CHARACTER(LEN=100) :: commentout !comment to the file
     INTEGER     ::   nmembers
     INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
     !
     ! Create the file.
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
     CALL h5gcreate_f(file_id, groupname1, group1_id, error)
     CALL check("h5gcreate_f",error,total_error)

     !
     ! Create a group named "/MyGroup/Group_A" in the file.
     !
     CALL h5gcreate_f(file_id, groupname2, group2_id, error)
     CALL check("h5gcreate_f",error,total_error)
     !
     !Create data space for the dataset.
     !
     CALL h5screate_simple_f(rank, dims, dspace_id, error)
     CALL check("h5screate_simple_f",error,total_error)
     !
     ! create dataset in the file.
     !
     CALL h5dcreate_f(file_id, dsetname1, H5T_NATIVE_INTEGER, dspace_id, &
               dset1_id, error)
     CALL check("h5dcreate_f",error,total_error)

     !
     ! Write data_in to dataset1
     !
     data_dims(1) = 1
     CALL h5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, dset1_data, data_dims, error)
     CALL check("h5dwrite_f",error,total_error)

     !
     ! create dataset2 in the Group_A.
     !
     CALL h5dcreate_f(group2_id, dsetname2, H5T_NATIVE_INTEGER, dspace_id, &
               dset2_id, error)
     CALL check("h5dcreate_f",error,total_error)

     !
     ! Write data_in to dataset2
     !
     CALL h5dwrite_f(dset2_id, H5T_NATIVE_INTEGER, dset2_data, data_dims, error)
     CALL check("h5dwrite_f",error,total_error)

     !
     !Create a hard link to the group1
     !
     CALL h5glink_f(file_id, H5G_LINK_HARD_F, groupname1, linkname1, error)
     CALL check("h5glink_f",error,total_error)
     !
     !Create a hard link to the group2
     !
     CALL h5glink2_f(file_id, groupname2, H5G_LINK_HARD_F, file_id, linkname2, error)
     CALL check("h5glink2_f",error,total_error)
     !
     !Create a soft link to  dataset11
     !
     CALL h5glink_f(file_id, H5G_LINK_SOFT_F, dsetname1, linkname3, error)
     CALL check("h5glink_f",error,total_error)
     !
     !Create a soft link to  dataset2
     !
     CALL h5glink_f(file_id, H5G_LINK_SOFT_F, dsetname2, linkname4, error)
     CALL check("h5glink_f",error,total_error)
     !
     !close group1
     !
     CALL h5gclose_f(group1_id, error)
     CALL check("h5gclose_f", error, total_error)
     !
     !reopen group1
     !
     CALL h5gopen_f(file_id, groupname1, group1_id, error)
     CALL check("h5gopen_f", error, total_error)
     !
     !get obj info of group1
     !
!     CALL h5gget_obj_info_idx_f(file_id, linkname1, 2, name, obj_type, error)
!     CALL check("h5gget_obj_info_idx_f", error, total_error)
! XXX: Fix problems with H5G_LINK_F! - QAK
!     if (obj_type .ne. H5G_LINK_F) then
!         write(*,*)  "got object  ", name, " type error ", obj_type
!         total_error = total_error +1
!     end if
     !
     !Get number of members in the group
     !
     CALL  h5gn_members_f(file_id, groupname1, nmembers, error)
     CALL check("h5gn_members_f",error,total_error)
     if (nmembers .ne. 2) then
         write(*,*)  "got nmembers ", nmembers, " is wrong"
         total_error = total_error +1
     end if
     !
     !Get the name of a symbolic name
     !
     CALL h5gget_linkval_f(file_id, linkname3, namesize, name, error)
     CALL check("h5gget_linkval_f",error,total_error)
     if ( name(1:13) .ne. dsetname1) then
         write(*,*)  "got symbolic name  ", name, " is wrong"
         total_error = total_error +1
     end if
     !
     !move softlink2 to newsoftlink2
     !
     CALL h5gmove_f(file_id, linkname4, linkname5, error)
     CALL check("h5gmove_f",error,total_error)
     !
     !Get the name of the moved symbolic name
     !
     CALL h5gget_linkval_f(file_id, linkname5, namesize, name, error)
     CALL check("h5gget_linkval_f",error,total_error)
     if ( name(1:5) .ne. dsetname2) then
         write(*,*)  "got symbolic name  ", name, " is wrong"
         total_error = total_error +1
     end if

     !
     !Unlink the moved symbolic link
     !
     CALL h5gunlink_f(file_id, linkname5, error)
     CALL check("h5gunlink_f", error, total_error)


     !
     !set the comment of dataset1 to comment
     !
     CALL  h5gset_comment_f(file_id, dsetname1, comment, error)
     CALL check("h5gset_comment_f", error, total_error)
     !
     !get the comment of dataset1
     !
     CALL  h5gget_comment_f(file_id, dsetname1,namesize, commentout, error)
     CALL check("h5gget_comment_f", error, total_error)
     if ( commentout(1:33) .ne. comment) then
         write(*,*)  "got comment  ", commentout, " is wrong"
         total_error = total_error +1
     end if
     !
     ! Move dataset1 to gourp2_id location
     !
     CALL h5dclose_f(dset1_id, error)
     CALL check("h5dclose_f", error, total_error)

     CALL h5gmove2_f(file_id, dsetname1, group2_id, "dset1", error)
     CALL check("h5gmove2_f", error, total_error)
     !
     ! Open dataset from the new location
     !
     Call h5dopen_f(file_id, "/MyGroup/Group_A/dset1" , dsetnew_id, error)
     CALL check("h5dopen_f",error, total_error)
     !
     !release all the resources
     !
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f", error, total_error)
     CALL h5gclose_f(group1_id, error)
     CALL check("h5gclose_f", error, total_error)
     CALL h5gclose_f(group2_id, error)
     CALL check("h5gclose_f", error, total_error)
     CALL h5dclose_f(dset2_id, error)
     CALL check("h5dclose_f", error, total_error)
     CALL h5dclose_f(dsetnew_id, error)
     CALL check("h5dclose_f", error, total_error)
     CALL h5sclose_f(dspace_id, error)
     CALL check("h5sclose_f", error, total_error)

          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
  END SUBROUTINE group_test
