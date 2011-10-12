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
  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(OUT) :: total_error

  INTEGER(HID_T) :: fapl, fapl2, my_fapl ! /* File access property lists */

  INTEGER :: error, ret_total_error

!  WRITE(*,*) "TESTING GROUPS"
  CALL H5Pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
  CALL check("H5Pcreate_f",error, total_error)

  ! /* Copy the file access property list */
  CALL H5Pcopy_f(fapl, fapl2, error)
  CALL check("H5Pcopy_f",error, total_error)

  ! /* Set the "use the latest version of the format" bounds for creating objects in the file */
  CALL H5Pset_libver_bounds_f(fapl2, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)
  CALL check("H5Pset_libver_bounds_f",error, total_error)

  ! /* Check for FAPL to USE */
  my_fapl = fapl2

  ret_total_error = 0
  CALL mklinks(fapl2, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing building a file with assorted links', &
       total_error)

  ret_total_error = 0
  CALL cklinks(fapl2, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing links are correct and building assorted links', &
       total_error)

  ret_total_error = 0
  CALL group_info(cleanup, fapl2, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing create group with creation order indices, test querying group info', &
       total_error)

! CALL ud_hard_links(fapl2,total_error)
  ret_total_error = 0
  CALL timestamps(cleanup, fapl2, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing disabling tracking timestamps for an object', &
       total_error)

  ret_total_error = 0
  CALL test_move_preserves(fapl2, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing moving and renaming links preserves their properties', &
       total_error)

  ret_total_error = 0
  CALL delete_by_idx(cleanup,fapl2,ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing deleting links by index', &
       total_error)

  ret_total_error = 0
  CALL test_lcpl(cleanup, fapl, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing link creation property lists', &
       total_error)

  ret_total_error = 0
  CALL objcopy(fapl, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing object copy', &
       total_error)

  ret_total_error = 0
  CALL lifecycle(cleanup, fapl2, ret_total_error)
  CALL write_test_status(ret_total_error, &
       ' Testing adding links to a group follow proper "lifecycle"', &
       total_error)

  IF(cleanup) CALL h5_cleanup_f("TestLinks", H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)


END SUBROUTINE group_test

!/*-------------------------------------------------------------------------
! * Function:    group_info
! *
! * Purpose:     Create a group with creation order indices and test querying
! *              group info.
! *
! * Return:      Success:        0
! *              Failure:        -1
! *
! * Programmer:  Adapted from C test routines by
! *              M.S. Breitenfeld
! *              February 18, 2008
! *
! *-------------------------------------------------------------------------
! */

SUBROUTINE group_info(cleanup, fapl, total_error)

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: total_error
  INTEGER(HID_T), INTENT(IN) :: fapl

  INTEGER(HID_T) :: gcpl_id ! /* Group creation property list ID */

  INTEGER :: max_compact ! /* Maximum # of links to store in group compactly */
  INTEGER :: min_dense ! /* Minimum # of links to store in group "densely" */

  INTEGER :: idx_type ! /* Type of index to operate on */
  INTEGER :: order, iorder   ! /* Order within in the index */
  LOGICAL, DIMENSION(1:2) :: use_index = (/.FALSE.,.TRUE./) ! /* Use index on creation order values */
  CHARACTER(LEN=6), PARAMETER :: prefix = 'links0'
  CHARACTER(LEN=9), PARAMETER :: filename = prefix//'.h5'  ! /* File name */
  INTEGER :: Input1
  INTEGER(HID_T) :: group_id ! /* Group ID */
  INTEGER(HID_T) :: soft_group_id ! /* Group ID for soft links */

  INTEGER :: i ! /* Local index variables */
  INTEGER :: storage_type ! Type of storage for links in group:
                                          ! H5G_STORAGE_TYPE_COMPACT: Compact storage
                                          ! H5G_STORAGE_TYPE_DENSE: Indexed storage
                                          ! H5G_STORAGE_TYPE_SYMBOL_TABLE: Symbol tables, the original HDF5 structure
  INTEGER :: nlinks ! Number of links in group
  INTEGER :: max_corder ! Current maximum creation order value for group

  INTEGER :: u,v  ! /* Local index variables */
  CHARACTER(LEN=2) :: chr2
  INTEGER(HID_T) :: group_id2, group_id3 ! /* Group IDs */
  CHARACTER(LEN=7) :: objname ! /* Object name */
  CHARACTER(LEN=7) :: objname2 ! /* Object name */
  CHARACTER(LEN=19) :: valname !  /* Link value */
  CHARACTER(LEN=12), PARAMETER :: CORDER_GROUP_NAME = "corder_group"
  CHARACTER(LEN=17), PARAMETER :: CORDER_SOFT_GROUP_NAME =  "corder_soft_group"
  INTEGER(HID_T) :: file_id ! /* File ID */
  INTEGER :: error ! /* Generic return value */
  LOGICAL :: mounted
  LOGICAL :: cleanup

  ! /* Create group creation property list */
  CALL H5Pcreate_f(H5P_GROUP_CREATE_F, gcpl_id, error )
  CALL check("H5Pcreate_f", error, total_error)

  ! /* Query the group creation properties */
  CALL H5Pget_link_phase_change_f(gcpl_id, max_compact, min_dense, error)
  CALL check("H5Pget_link_phase_change_f", error, total_error)

  ! /* Loop over operating on different indices on link fields */
  DO idx_type = H5_INDEX_NAME_F, H5_INDEX_CRT_ORDER_F
     ! /* Loop over operating in different orders */
     DO iorder = H5_ITER_INC_F,  H5_ITER_NATIVE_F
        ! /* Loop over using index for creation order value */
        DO i = 1, 2
           ! /* Print appropriate test message */
           IF(idx_type == H5_INDEX_CRT_ORDER_F)THEN
              IF(iorder == H5_ITER_INC_F)THEN
                 order = H5_ITER_INC_F
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in increasing order w/creation order index"
!!$                 ELSE
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in increasing order w/o creation order index"
!!$                 ENDIF
              ELSE IF (iorder == H5_ITER_DEC_F) THEN
                 order = H5_ITER_DEC_F
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in decreasing order w/creation order index"
!!$                 ELSE
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in decreasing order w/o creation order index"
!!$                 ENDIF
              ELSE
                 order = H5_ITER_NATIVE_F
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in native order w/creation order index"
!!$                 ELSE
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in native order w/o creation order index"
!!$                 ENDIF
              ENDIF
           ELSE
              IF(iorder == H5_ITER_INC_F)THEN
                 order = H5_ITER_INC_F
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in increasing order w/creation order index"
!!$                 ELSE
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in increasing order w/o creation order index"
!!$                 ENDIF
              ELSE IF (iorder == H5_ITER_DEC_F) THEN
                 order = H5_ITER_DEC_F
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in decreasing order w/creation order index"
!!$                 ELSE
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in decreasing order w/o creation order index"
!!$                 ENDIF
              ELSE
                 order = H5_ITER_NATIVE_F
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in native order w/creation order index"
!!$                 ELSE
!!$                    WRITE(*,'(5x,A)')"query group info by creation order index in native order w/o creation order index"
!!$                 ENDIF
              ENDIF
           END IF

           ! /* Create file */
           CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, H5P_DEFAULT_F, fapl)
           CALL check("H5Fcreate_f", error, total_error)

           ! /* Set creation order tracking & indexing on group */
           IF(use_index(i))THEN
              Input1 = H5P_CRT_ORDER_INDEXED_F
           ELSE
              Input1 = 0
           ENDIF
           CALL H5Pset_link_creation_order_f(gcpl_id, IOR(H5P_CRT_ORDER_TRACKED_F, Input1), error)
           CALL check("H5Pset_link_creation_order_f", error, total_error)

           ! /* Create group with creation order tracking on */
           CALL H5Gcreate_f(file_id, CORDER_GROUP_NAME, group_id, error, gcpl_id=gcpl_id)
           CALL check("H5Gcreate_f", error, total_error)

           ! /* Create group with creation order tracking on for soft links */
           CALL H5Gcreate_f(file_id, CORDER_SOFT_GROUP_NAME, soft_group_id, error, &
                OBJECT_NAMELEN_DEFAULT_F, H5P_DEFAULT_F, gcpl_id)
           CALL check("H5Gcreate_f", error, total_error)

           ! /* Check for out of bound query by index on empty group, should fail */
           CALL H5Gget_info_by_idx_f(group_id, ".", H5_INDEX_NAME_F, order, INT(0,HSIZE_T), &
                storage_type, nlinks, max_corder, error)
           CALL VERIFY("H5Gget_info_by_idx_f", error, -1, total_error)

           ! /* Create several links, up to limit of compact form */
           DO u = 0, max_compact-1

              ! /* Make name for link */
              WRITE(chr2,'(I2.2)') u
              objname = 'fill '//chr2

              ! /* Create hard link, with group object */
              CALL H5Gcreate_f(group_id, objname, group_id2, error, OBJECT_NAMELEN_DEFAULT_F, H5P_DEFAULT_F, gcpl_id)
              CALL check("H5Gcreate_f", error, total_error)

              ! /* Retrieve group's information */
              CALL H5Gget_info_f(group_id2, storage_type, nlinks, max_corder, error, mounted)
              CALL check("H5Gget_info_f", error, total_error)

              ! /* Check (new/empty) group's information */
              CALL VERIFY("H5Gget_info_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
              CALL VERIFY("H5Gget_info_f", max_corder, 0, total_error)
              CALL VERIFY("H5Gget_info_f", nlinks, 0, total_error)
              CALL verifyLogical("H5Gget_info_f.mounted", mounted,.FALSE.,total_error)

              ! /* Retrieve group's information */
              CALL H5Gget_info_by_name_f(group_id, objname, storage_type, nlinks, max_corder, error, mounted=mounted)
              CALL check("H5Gget_info_by_name_f", error, total_error)

              ! /* Check (new/empty) group's information */
              CALL VERIFY("H5Gget_info_by_name_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", max_corder, 0, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", nlinks, 0, total_error)
              CALL verifyLogical("H5Gget_info_by_name_f.mounted", mounted,.FALSE.,total_error)

              ! /* Retrieve group's information */
              CALL H5Gget_info_by_name_f(group_id2, ".", storage_type, nlinks, max_corder, error)
              CALL check("H5Gget_info_by_name", error, total_error)

              ! /* Check (new/empty) group's information */
              CALL VERIFY("H5Gget_info_by_name_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", max_corder, 0, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", nlinks, 0, total_error)

              ! /* Create objects in new group created */
              DO v = 0, u
                 ! /* Make name for link */
                 WRITE(chr2,'(I2.2)') v
                 objname2 = 'fill '//chr2

                 ! /* Create hard link, with group object */
                 CALL H5Gcreate_f(group_id2, objname2, group_id3, error )
                 CALL check("H5Gcreate_f", error, total_error)

                 ! /* Close group created */
                 CALL H5Gclose_f(group_id3, error)
                 CALL check("H5Gclose_f", error, total_error)
              ENDDO

              ! /* Retrieve group's information */
              CALL H5Gget_info_f(group_id2, storage_type, nlinks, max_corder, error)
              CALL check("H5Gget_info_f", error, total_error)

              ! /* Check (new) group's information */
              CALL VERIFY("H5Gget_info_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
              CALL VERIFY("H5Gget_info_f", max_corder, u+1, total_error)
              CALL VERIFY("H5Gget_info_f", nlinks, u+1, total_error)

              ! /* Retrieve group's information */
              CALL H5Gget_info_by_name_f(group_id, objname, storage_type, nlinks, max_corder, error)
              CALL check("H5Gget_info_by_name_f", error, total_error)

              ! /* Check (new) group's information */
              CALL VERIFY("H5Gget_info_by_name_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
              CALL VERIFY("H5Gget_info_by_name_f",max_corder, u+1, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", nlinks, u+1, total_error)

              ! /* Retrieve group's information */
              CALL H5Gget_info_by_name_f(group_id2, ".", storage_type, nlinks, max_corder, error)
              CALL check("H5Gget_info_by_name_f", error, total_error)

              ! /* Check (new) group's information */
              CALL VERIFY("H5Gget_info_by_name_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", max_corder, u+1, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", nlinks, u+1, total_error)

              ! /* Retrieve group's information */
              IF(order.NE.H5_ITER_NATIVE_F)THEN
                 IF(order.EQ.H5_ITER_INC_F) THEN
                    CALL H5Gget_info_by_idx_f(group_id, ".", idx_type, order, INT(u,HSIZE_T), &
                         storage_type, nlinks, max_corder, error,lapl_id=H5P_DEFAULT_F, mounted=mounted)
                    CALL check("H5Gget_info_by_idx_f", error, total_error)
                    CALL verifyLogical("H5Gget_info_by_idx_f", mounted,.FALSE.,total_error)
                 ELSE
                    CALL H5Gget_info_by_idx_f(group_id, ".", idx_type, order, INT(0,HSIZE_T), &
                         storage_type, nlinks, max_corder, error, mounted=mounted)
                    CALL verifyLogical("H5Gget_info_by_idx_f", mounted,.FALSE.,total_error)
                    CALL check("H5Gget_info_by_idx_f", error, total_error)
                 ENDIF
              ! /* Check (new) group's information */
                 CALL VERIFY("H5Gget_info_by_idx_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
                 CALL VERIFY("H5Gget_info_by_idx_f", max_corder, u+1, total_error)
                 CALL VERIFY("H5Gget_info_by_idx_f", nlinks, u+1, total_error)
              ENDIF
              ! /* Close group created */
              CALL H5Gclose_f(group_id2, error)
              CALL check("H5Gclose_f", error, total_error)

              ! /* Retrieve main group's information */
              CALL H5Gget_info_f(group_id, storage_type, nlinks, max_corder, error)
              CALL check("H5Gget_info_f", error, total_error)

              ! /* Check main group's information */
              CALL VERIFY("H5Gget_info_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
              CALL VERIFY("H5Gget_info_f", max_corder, u+1, total_error)
              CALL VERIFY("H5Gget_info_f", nlinks, u+1, total_error)

              ! /* Retrieve main group's information, by name */
              CALL H5Gget_info_by_name_f(file_id, CORDER_GROUP_NAME, storage_type, nlinks, max_corder, error)
              CALL check("H5Gget_info_by_name_f", error, total_error)

              ! /* Check main group's information */
              CALL VERIFY("H5Gget_info_by_name_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", max_corder, u+1, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", nlinks, u+1, total_error)

              ! /* Retrieve main group's information, by name */
              CALL H5Gget_info_by_name_f(group_id, ".", storage_type, nlinks, max_corder, error, H5P_DEFAULT_F)
              CALL check("H5Gget_info_by_name_f", error, total_error)

              ! /* Check main group's information */
              CALL VERIFY("H5Gget_info_by_name_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", max_corder, u+1, total_error)
              CALL VERIFY("H5Gget_info_by_name_f", nlinks, u+1, total_error)

              ! /* Create soft link in another group, to objects in main group */
              valname = CORDER_GROUP_NAME//objname

              CALL H5Lcreate_soft_f(valname, soft_group_id, objname, error, H5P_DEFAULT_F, H5P_DEFAULT_F)

              ! /* Retrieve soft link group's information, by name */
              CALL H5Gget_info_f(soft_group_id, storage_type, nlinks, max_corder, error)
              CALL check("H5Gget_info_f", error, total_error)

              ! /* Check soft link group's information */
              CALL VERIFY("H5Gget_info_f", storage_type, H5G_STORAGE_TYPE_COMPACT_F, total_error)
              CALL VERIFY("H5Gget_info_f", max_corder, u+1, total_error)
              CALL VERIFY("H5Gget_info_f", nlinks, u+1, total_error)
           ENDDO

           ! /* Close the groups */

              CALL H5Gclose_f(group_id, error)
              CALL check("H5Gclose_f", error, total_error)
              CALL H5Gclose_f(soft_group_id, error)
              CALL check("H5Gclose_f", error, total_error)

              ! /* Close the file */
              CALL H5Fclose_f(file_id, error)
              CALL check("H5Fclose_f", error, total_error)
           ENDDO
        ENDDO
     ENDDO

     ! /* Free resources */
     CALL H5Pclose_f(gcpl_id, error)
     CALL check("H5Pclose_f", error, total_error)

     IF(cleanup) CALL h5_cleanup_f(prefix, H5P_DEFAULT_F, error)
     CALL check("h5_cleanup_f", error, total_error)


   END SUBROUTINE group_info

!/*-------------------------------------------------------------------------
! * Function:    timestamps
! *
! * Purpose:     Verify that disabling tracking timestamps for an object
! *              works correctly
! *
! *
! * Programmer:  M.S. Breitenfeld
! *              February 20, 2008
! *
! *-------------------------------------------------------------------------
! */

   SUBROUTINE timestamps(cleanup, fapl, total_error)

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE
     INTEGER, INTENT(OUT) :: total_error
     INTEGER(HID_T), INTENT(IN) :: fapl

     INTEGER(HID_T) :: file_id !/* File ID */
     INTEGER(HID_T) :: group_id !/* Group ID */
     INTEGER(HID_T) :: group_id2 !/* Group ID */
     INTEGER(HID_T) :: gcpl_id !/* Group creation property list ID */
     INTEGER(HID_T) :: gcpl_id2 !/* Group creation property list ID */

     CHARACTER(LEN=6), PARAMETER :: prefix = 'links9'
     CHARACTER(LEN=9), PARAMETER :: filename = prefix//'.h5'  ! /* File name */
     ! /* Timestamp macros */
     CHARACTER(LEN=10), PARAMETER :: TIMESTAMP_GROUP_1="timestamp1"
     CHARACTER(LEN=10), PARAMETER :: TIMESTAMP_GROUP_2="timestamp2"
     LOGICAL :: track_times
     LOGICAL :: cleanup

     INTEGER :: error

     ! /* Print test message */
!     WRITE(*,*) "timestamps on objects"

     ! /* Create group creation property list */
     CALL H5Pcreate_f(H5P_GROUP_CREATE_F, gcpl_id, error )
     CALL check("H5Pcreate_f", error, total_error)

     ! /* Query the object timestamp setting */
     CALL H5Pget_obj_track_times_f(gcpl_id, track_times, error)
     CALL check("H5Pget_obj_track_times_f", error, total_error)

     !/* Check default timestamp information */
     CALL VerifyLogical("H5Pget_obj_track_times",track_times,.TRUE.,total_error)

     ! /* Set a non-default object timestamp setting */
     CALL H5Pset_obj_track_times_f(gcpl_id, .FALSE., error)
     CALL check("H5Pset_obj_track_times_f", error, total_error)

     ! /* Query the object timestamp setting */
     CALL H5Pget_obj_track_times_f(gcpl_id, track_times, error)
     CALL check("H5Pget_obj_track_times_f", error, total_error)

     ! /* Check default timestamp information */
     CALL VerifyLogical("H5Pget_obj_track_times",track_times,.FALSE.,total_error)

     ! /* Create file */
     !h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

     CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, H5P_DEFAULT_F, fapl)
     CALL check("h5fcreate_f",error,total_error)

    ! /* Create group with non-default object timestamp setting */
     CALL h5gcreate_f(file_id, TIMESTAMP_GROUP_1, group_id, error, &
          OBJECT_NAMELEN_DEFAULT_F, H5P_DEFAULT_F, gcpl_id, H5P_DEFAULT_F)
     CALL check("h5fcreate_f",error,total_error)

    ! /* Close the group creation property list */
     CALL H5Pclose_f(gcpl_id, error)
     CALL check("H5Pclose_f", error, total_error)

    ! /* Create group with default object timestamp setting */
     CALL h5gcreate_f(file_id, TIMESTAMP_GROUP_2, group_id2, error, &
          OBJECT_NAMELEN_DEFAULT_F, H5P_DEFAULT_F, H5P_DEFAULT_F, H5P_DEFAULT_F)
     CALL check("h5fcreate_f",error,total_error)

    ! /* Retrieve the new groups' creation properties */
     CALL H5Gget_create_plist_f(group_id, gcpl_id, error)
     CALL check("H5Gget_create_plist", error, total_error)
     CALL H5Gget_create_plist_f(group_id2, gcpl_id2, error)
     CALL check("H5Gget_create_plist", error, total_error)

    ! /* Query & verify the object timestamp settings */
     CALL H5Pget_obj_track_times_f(gcpl_id, track_times, error)
     CALL check("H5Pget_obj_track_times_f", error, total_error)
     CALL VerifyLogical("H5Pget_obj_track_times1",track_times,.FALSE.,total_error)
     CALL H5Pget_obj_track_times_f(gcpl_id2, track_times, error)
     CALL check("H5Pget_obj_track_times_f", error, total_error)
     CALL VerifyLogical("H5Pget_obj_track_times2",track_times,.TRUE.,total_error)

!    /* Query the object information for each group */
!    if(H5Oget_info(group_id, &oinfo) < 0) TEST_ERROR
!    if(H5Oget_info(group_id2, &oinfo2) < 0) TEST_ERROR

!!$    /* Sanity check object information for each group */
!!$    if(oinfo.atime != 0) TEST_ERROR
!!$    if(oinfo.mtime != 0) TEST_ERROR
!!$    if(oinfo.ctime != 0) TEST_ERROR
!!$    if(oinfo.btime != 0) TEST_ERROR
!!$    if(oinfo.atime == oinfo2.atime) TEST_ERROR
!!$    if(oinfo.mtime == oinfo2.mtime) TEST_ERROR
!!$    if(oinfo.ctime == oinfo2.ctime) TEST_ERROR
!!$    if(oinfo.btime == oinfo2.btime) TEST_ERROR
!!$    if((oinfo.hdr.flags & H5O_HDR_STORE_TIMES) != 0) TEST_ERROR
!!$    if((oinfo2.hdr.flags & H5O_HDR_STORE_TIMES) == 0) TEST_ERROR
!!$    if(oinfo.hdr.space.total >= oinfo2.hdr.space.total) TEST_ERROR
!!$    if(oinfo.hdr.space.meta >= oinfo2.hdr.space.meta) TEST_ERROR

     ! /* Close the property lists */
     CALL H5Pclose_f(gcpl_id, error)
     CALL check("H5Pclose_f", error, total_error)
     CALL H5Pclose_f(gcpl_id2, error)
     CALL check("H5Pclose_f", error, total_error)

     ! /* Close the groups */
     CALL H5Gclose_f(group_id, error)
     CALL check("H5Gclose_f", error, total_error)
     CALL H5Gclose_f(group_id2, error)
     CALL check("H5Gclose_f", error, total_error)

     !/* Close the file */
     CALL H5Fclose_f(file_id, error)
     CALL check("H5Fclose_f", error, total_error)

     !/* Re-open the file */

     CALL h5fopen_f(FileName, H5F_ACC_RDONLY_F, file_id, error, H5P_DEFAULT_F)
     CALL check("h5fopen_f",error,total_error)

     !/* Open groups */
     CALL H5Gopen_f(file_id, TIMESTAMP_GROUP_1, group_id, error) ! with no optional param.
     CALL check("H5Gopen_f", error, total_error)
     CALL H5Gopen_f(file_id, TIMESTAMP_GROUP_2, group_id2, error, H5P_DEFAULT_F) ! with optional param.
     CALL check("H5Gopen_f", error, total_error)

    ! /* Retrieve the new groups' creation properties */
     CALL H5Gget_create_plist_f(group_id, gcpl_id, error)
     CALL check("H5Gget_create_plist", error, total_error)
     CALL H5Gget_create_plist_f(group_id2, gcpl_id2, error)
     CALL check("H5Gget_create_plist", error, total_error)

    ! /* Query & verify the object timestamp settings */

     CALL H5Pget_obj_track_times_f(gcpl_id, track_times, error)
     CALL check("H5Pget_obj_track_times_f", error, total_error)
     CALL VerifyLogical("H5Pget_obj_track_times1",track_times,.FALSE.,total_error)
     CALL H5Pget_obj_track_times_f(gcpl_id2, track_times, error)
     CALL check("H5Pget_obj_track_times_f", error, total_error)
     CALL VerifyLogical("H5Pget_obj_track_times2",track_times,.TRUE.,total_error)
!!$
!!$    /* Query the object information for each group */
!!$    if(H5Oget_info(group_id, &oinfo) < 0) TEST_ERROR
!!$    if(H5Oget_info(group_id2, &oinfo2) < 0) TEST_ERROR
!!$
!!$    /* Sanity check object information for each group */
!!$    if(oinfo.atime != 0) TEST_ERROR
!!$    if(oinfo.mtime != 0) TEST_ERROR
!!$    if(oinfo.ctime != 0) TEST_ERROR
!!$    if(oinfo.btime != 0) TEST_ERROR
!!$    if(oinfo.atime == oinfo2.atime) TEST_ERROR
!!$    if(oinfo.mtime == oinfo2.mtime) TEST_ERROR
!!$    if(oinfo.ctime == oinfo2.ctime) TEST_ERROR
!!$    if(oinfo.btime == oinfo2.btime) TEST_ERROR
!!$    if((oinfo.hdr.flags & H5O_HDR_STORE_TIMES) != 0) TEST_ERROR
!!$    if((oinfo2.hdr.flags & H5O_HDR_STORE_TIMES) == 0) TEST_ERROR
!!$    if(oinfo.hdr.space.total >= oinfo2.hdr.space.total) TEST_ERROR
!!$    if(oinfo.hdr.space.meta >= oinfo2.hdr.space.meta) TEST_ERROR

     ! /* Close the property lists */
     CALL H5Pclose_f(gcpl_id, error)
     CALL check("H5Pclose_f", error, total_error)
     CALL H5Pclose_f(gcpl_id2, error)
     CALL check("H5Pclose_f", error, total_error)

     ! /* Close the groups */
     CALL H5Gclose_f(group_id, error)
     CALL check("H5Gclose_f", error, total_error)
     CALL H5Gclose_f(group_id2, error)
     CALL check("H5Gclose_f", error, total_error)

     !/* Close the file */
     CALL H5Fclose_f(file_id, error)
     CALL check("H5Fclose_f", error, total_error)

     IF(cleanup) CALL h5_cleanup_f(prefix, H5P_DEFAULT_F, error)
     CALL check("h5_cleanup_f", error, total_error)

   END SUBROUTINE timestamps

!/*-------------------------------------------------------------------------
! * Function:	mklinks
! *
! * Purpose:	Build a file with assorted links.
! *
! *
! * Programmer:	Adapted from C test by:
! *             M.S. Breitenfeld
! *
! * Modifications:
! *
! *-------------------------------------------------------------------------
! */

   SUBROUTINE mklinks(fapl, total_error)

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE
     INTEGER, INTENT(OUT) :: total_error
     INTEGER(HID_T), INTENT(IN) :: fapl

     INTEGER(HID_T) :: file, scalar, grp, d1
     CHARACTER(LEN=12), PARAMETER :: filename ='TestLinks.h5'
     INTEGER(HSIZE_T), DIMENSION(1) :: adims2 = (/1/) ! Attribute dimension
     INTEGER ::   arank = 1                      ! Attribure rank
     INTEGER :: error

     INTEGER :: cset ! Indicates the character set used for the link’s name.
     INTEGER :: corder ! Specifies the link’s creation order position.
     LOGICAL :: f_corder_valid ! Indicates whether the value in corder is valid.
     INTEGER :: link_type ! Specifies the link class:
     	                              !  H5L_TYPE_HARD_F      - Hard link
     	                              !  H5L_TYPE_SOFT_F      - Soft link
     	                              !  H5L_TYPE_EXTERNAL_F  - External link
     	                              !  H5L_TYPE_ERROR _F    - Error
     INTEGER(HADDR_T) :: address  ! If the link is a hard link, address specifies the file address that the link points to
     INTEGER(SIZE_T) :: val_size ! If the link is a symbolic link, val_size will be the length of the link value


!     WRITE(*,*) "link creation (w/new group format)"

     ! /* Create a file */
     CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, file, error, H5P_DEFAULT_F, fapl)
     CALL check("mklinks.h5fcreate_f",error,total_error)
     CALL h5screate_simple_f(arank, adims2, scalar, error)
     CALL check("mklinks.h5screate_simple_f",error,total_error)

     !/* Create a group */
     CALL H5Gcreate_f(file, "grp1", grp, error)
     CALL check("H5Gcreate_f", error, total_error)
     CALL H5Gclose_f(grp, error)
     CALL check("h5gclose_f",error,total_error)

     !/* Create a dataset */
     CALL h5dcreate_f(file, "d1", H5T_NATIVE_INTEGER, scalar, d1, error)
     CALL check("h5dcreate_f",error,total_error)
     CALL h5dclose_f(d1, error)
     CALL check("h5dclose_f",error,total_error)

     !/* Create a hard link */
     CALL H5Lcreate_hard_f(file, "d1", INT(H5L_SAME_LOC_F,HID_T), "grp1/hard", error)
     CALL check("H5Lcreate_hard_f", error, total_error)

     !/* Create a symbolic link */
     CALL H5Lcreate_soft_f("/d1", file, "grp1/soft",error)
     CALL check("H5Lcreate_soft_f", error, total_error)

     CALL H5Lget_info_f(file, "grp1/soft", &
          cset, corder, f_corder_valid, link_type, address, val_size, &
          error, H5P_DEFAULT_F)
     CALL check("H5Lget_info_f",error,total_error)

!     CALL VerifyLogical("H5Lget_info_by_idx_f11", f_corder_valid, .TRUE., total_error)

     CALL VERIFY("H5Lget_info_by_idx_f", H5L_TYPE_SOFT_F, link_type, total_error)
     CALL VERIFY("H5Lget_info_by_idx_f", cset, H5T_CSET_ASCII_F, total_error)
     ! should be '/d1' + NULL character = 4
     CALL VERIFY("H5Lget_info_by_idx_f", INT(val_size), 4, total_error)

    !/* Create a symbolic link to something that doesn't exist */

     CALL H5Lcreate_soft_f("foobar", file, "grp1/dangle",error)

    !/* Create a recursive symbolic link */
     CALL H5Lcreate_soft_f("/grp1/recursive", file, "/grp1/recursive",error)

    !/* Close */
     CALL h5sclose_f(scalar, error)
     CALL check("h5sclose_f",error,total_error)
     CALL h5fclose_f(file, error)
     CALL check("h5fclose_f",error,total_error)

  END SUBROUTINE mklinks

!/*-------------------------------------------------------------------------
! * Function:    test_move_preserves
! *
! * Purpose:     Tests that moving and renaming links preserves their
! *              properties.
! *
! * Programmer:  M.S. Breitenfeld
! *              March 3, 2008
! *
! * Modifications:
! *
! *-------------------------------------------------------------------------
! */

  SUBROUTINE test_move_preserves(fapl_id, total_error)

    USE HDF5 ! This module contains all necessary modules

    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: total_error
    INTEGER(HID_T), INTENT(IN) :: fapl_id

    INTEGER(HID_T):: file_id
    INTEGER(HID_T):: group_id
    INTEGER(HID_T):: fcpl_id ! /* Group creation property list ID */
    INTEGER(HID_T):: lcpl_id
    !H5O_info_t oinfo;
    !H5L_info_t linfo;
    INTEGER :: old_cset
    INTEGER :: old_corder
    !H5T_cset_t old_cset;
    !int64_t old_corder;         /* Creation order value of link */
    !time_t old_modification_time;
    !time_t curr_time;
    !unsigned crt_order_flags;   /* Status of creation order info for GCPL */
    !char filename[1024];

    INTEGER :: crt_order_flags ! /* Status of creation order info for GCPL */
    CHARACTER(LEN=12), PARAMETER :: filename = 'TestLinks.h5'

    INTEGER :: cset ! Indicates the character set used for the link’s name.
    INTEGER :: corder ! Specifies the link’s creation order position.
    LOGICAL :: f_corder_valid ! Indicates whether the value in corder is valid.
    INTEGER :: link_type ! Specifies the link class:
     	                              !  H5L_TYPE_HARD_F      - Hard link
     	                              !  H5L_TYPE_SOFT_F      - Soft link
     	                              !  H5L_TYPE_EXTERNAL_F  - External link
     	                              !  H5L_TYPE_ERROR _F    - Error
    INTEGER(HADDR_T) :: address  ! If the link is a hard link, address specifies the file address that the link points to
    INTEGER(SIZE_T) :: val_size ! If the link is a symbolic link, val_size will be the length of the link value

    INTEGER :: error

!    WRITE(*,*) "moving and copying links preserves their properties (w/new group format)"

    !/* Create a file creation property list with creation order stored for links
    ! * in the root group
    ! */

    CALL H5Pcreate_f(H5P_FILE_CREATE_F, fcpl_id, error)
    CALL check("H5Pcreate_f",error, total_error)

    CALL H5Pget_link_creation_order_f(fcpl_id, crt_order_flags, error)
    CALL check("H5Pget_link_creation_order_f",error, total_error)
    CALL VERIFY("H5Pget_link_creation_order_f",crt_order_flags,0, total_error)

    CALL H5Pset_link_creation_order_f(fcpl_id, H5P_CRT_ORDER_TRACKED_F, error)
    CALL check("H5Pset_link_creation_order_f", error, total_error)

    CALL H5Pget_link_creation_order_f(fcpl_id, crt_order_flags, error)
    CALL check("H5Pget_link_creation_order_f",error, total_error)
    CALL VERIFY("H5Pget_link_creation_order_f",crt_order_flags, H5P_CRT_ORDER_TRACKED_F, total_error)

    !/* Create file */
    !/* (with creation order tracking for the root group) */

    CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, file_id, error, fcpl_id, fapl_id)
    CALL check("h5fcreate_f",error,total_error)

    !/* Create a link creation property list with the UTF-8 character encoding */
    CALL H5Pcreate_f(H5P_LINK_CREATE_F, lcpl_id, error)
    CALL check("H5Pcreate_f",error, total_error)

    CALL H5Pset_char_encoding_f(lcpl_id, H5T_CSET_UTF8_F, error)
    CALL check("H5Pset_char_encoding_f",error, total_error)

    !/* Create a group with that lcpl */
    CALL H5Gcreate_f(file_id, "group", group_id, error,lcpl_id=lcpl_id, gcpl_id=H5P_DEFAULT_F, gapl_id=H5P_DEFAULT_F)
    CALL check("H5Gcreate_f", error, total_error)
    CALL H5Gclose_f(group_id, error)
    CALL check("H5Gclose_f", error, total_error)

    ! /* Get the group's link's information */
    CALL H5Lget_info_f(file_id, "group", &
         cset, corder, f_corder_valid, link_type, address, val_size, &
         error, H5P_DEFAULT_F)
    CALL check("H5Lget_info_f",error,total_error)

!    if(H5Oget_info_by_name(file_id, "group", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR

    old_cset = cset
    CALL VERIFY("H5Lget_info_f",old_cset,H5T_CSET_UTF8_F,total_error)
    CALL VerifyLogical("H5Lget_info_f",f_corder_valid,.TRUE.,total_error)
    old_corder = corder;
    CALL VERIFY("H5Lget_info_f",old_corder,0,total_error)

!    old_modification_time = oinfo.mtime;

!    /* If this test happens too quickly, the times will all be the same.  Make sure the time changes. */
!    curr_time = HDtime(NULL);
!    while(HDtime(NULL) <= curr_time)
!        ;

!    /* Close the file and reopen it */
    CALL H5Fclose_f(file_id, error)
    CALL check("H5Fclose_f", error, total_error)

!!$    if((file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0) TEST_ERROR
!!$
!!$    /* Get the link's character set & modification time .  They should be unchanged */
!!$    if(H5Lget_info(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(H5Oget_info_by_name(file_id, "group", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(old_modification_time != oinfo.mtime) TEST_ERROR
!!$    if(old_cset != linfo.cset) TEST_ERROR
!!$    if(linfo.corder_valid != TRUE) TEST_ERROR
!!$    if(old_corder != linfo.corder) TEST_ERROR
!!$
!!$    /* Create a new link to the group.  It should have a different creation order value but the same modification time */
!!$    if(H5Lcreate_hard(file_id, "group", file_id, "group2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(H5Oget_info_by_name(file_id, "group2", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(old_modification_time != oinfo.mtime) TEST_ERROR
!!$    if(H5Lget_info(file_id, "group2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(old_corder == linfo.corder) TEST_ERROR
!!$    if(linfo.corder_valid != TRUE) TEST_ERROR
!!$    if(linfo.corder != 1) TEST_ERROR
!!$    if(linfo.cset != H5T_CSET_ASCII) TEST_ERROR
!!$
!!$    /* Copy the first link to a UTF-8 name.
!!$     *  Its creation order value should be different, but modification time
!!$     * should not change.
!!$     */
!!$    if(H5Lcopy(file_id, "group", file_id, "group_copied", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(H5Oget_info_by_name(file_id, "group_copied", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(old_modification_time != oinfo.mtime) TEST_ERROR
!!$    if(H5Lget_info(file_id, "group_copied", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(linfo.corder_valid != TRUE) TEST_ERROR
!!$    if(linfo.corder != 2) TEST_ERROR
!!$
!!$    /* Check that its character encoding is UTF-8 */
!!$    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR
!!$
!!$    /* Move the link with the default property list. */
!!$    if(H5Lmove(file_id, "group_copied", file_id, "group_copied2", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(H5Oget_info_by_name(file_id, "group_copied2", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(old_modification_time != oinfo.mtime) TEST_ERROR
!!$    if(H5Lget_info(file_id, "group_copied2", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(linfo.corder_valid != TRUE) TEST_ERROR
!!$    if(linfo.corder != 3) TEST_ERROR
!!$
!!$    /* Check that its character encoding is not UTF-8 */
!!$    if(linfo.cset == H5T_CSET_UTF8) TEST_ERROR
!!$
!!$    /* Check that the original link is unchanged */
!!$    if(H5Oget_info_by_name(file_id, "group", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(old_modification_time != oinfo.mtime) TEST_ERROR
!!$    if(H5Lget_info(file_id, "group", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(linfo.corder_valid != TRUE) TEST_ERROR
!!$    if(old_corder != linfo.corder) TEST_ERROR
!!$    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR
!!$
!!$    /* Move the first link to a UTF-8 name.
!!$     *  Its creation order value will change, but modification time should not
!!$     *  change. */
!!$    if(H5Lmove(file_id, "group", file_id, "group_moved", lcpl_id, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(H5Oget_info_by_name(file_id, "group_moved", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(old_modification_time != oinfo.mtime) TEST_ERROR
!!$    if(H5Lget_info(file_id, "group_moved", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(linfo.corder_valid != TRUE) TEST_ERROR
!!$    if(linfo.corder != 4) TEST_ERROR
!!$
!!$    /* Check that its character encoding is UTF-8 */
!!$    if(linfo.cset != H5T_CSET_UTF8) TEST_ERROR
!!$
!!$    /* Move the link again using the default property list. */
!!$    if(H5Lmove(file_id, "group_moved", file_id, "group_moved_again", H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(H5Oget_info_by_name(file_id, "group_moved_again", &oinfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(old_modification_time != oinfo.mtime) TEST_ERROR
!!$    if(H5Lget_info(file_id, "group_moved_again", &linfo, H5P_DEFAULT) < 0) TEST_ERROR
!!$    if(linfo.corder_valid != TRUE) TEST_ERROR
!!$    if(linfo.corder != 5) TEST_ERROR
!!$
!!$    /* Check that its character encoding is not UTF-8 */
!!$    if(linfo.cset == H5T_CSET_UTF8) TEST_ERROR

    ! /* Close open IDs */
     CALL H5Pclose_f(fcpl_id, error)
     CALL check("H5Pclose_f", error, total_error)
     CALL H5Pclose_f(lcpl_id, error)
     CALL check("H5Pclose_f", error, total_error)

    ! if(H5Fclose(file_id) < 0) TEST_ERROR

   END SUBROUTINE test_move_preserves

!/*-------------------------------------------------------------------------
! * Function:    lifecycle
! *
! * Purpose:     Test that adding links to a group follow proper "lifecycle"
! *              of empty->compact->symbol table->compact->empty.  (As group
! *              is created, links are added, then links removed)
! *
! * Return:      Success:        0
! *
! *              Failure:        -1
! *
! * Programmer:  Quincey Koziol
! *              Monday, October 17, 2005
! *
! *-------------------------------------------------------------------------
! */
SUBROUTINE lifecycle(cleanup, fapl2, total_error)


  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: total_error
  INTEGER(HID_T), INTENT(IN) :: fapl2
  INTEGER :: error

  INTEGER, PARAMETER :: NAME_BUF_SIZE =7

  INTEGER(HID_T) :: fid            !/* File ID */
  INTEGER(HID_T) :: gid            !/* Group ID */
  INTEGER(HID_T) :: gcpl           !/* Group creation property list ID */
  INTEGER(size_t) :: lheap_size_hint !/* Local heap size hint */
  INTEGER :: max_compact            !/* Maximum # of links to store in group compactly */
  INTEGER :: min_dense              !/* Minimum # of links to store in group "densely" */
  INTEGER :: est_num_entries	!/* Estimated # of entries in group */
  INTEGER :: est_name_len		!/* Estimated length of entry name */
  CHARACTER(LEN=NAME_BUF_SIZE) :: filename = 'fixx.h5'
  INTEGER(SIZE_T) :: LIFECYCLE_LOCAL_HEAP_SIZE_HINT = 256
  INTEGER :: LIFECYCLE_MAX_COMPACT = 4
  INTEGER :: LIFECYCLE_MIN_DENSE = 3
  INTEGER :: LIFECYCLE_EST_NUM_ENTRIES = 4
  INTEGER :: LIFECYCLE_EST_NAME_LEN=8
  CHARACTER(LEN=3) :: LIFECYCLE_TOP_GROUP="top"
! These value are taken from H5Gprivate.h
  INTEGER :: H5G_CRT_GINFO_MAX_COMPACT = 8
  INTEGER :: H5G_CRT_GINFO_MIN_DENSE = 6
  INTEGER :: H5G_CRT_GINFO_EST_NUM_ENTRIES = 4
  INTEGER :: H5G_CRT_GINFO_EST_NAME_LEN = 8
  logical :: cleanup

!  WRITE(*,*) 'group lifecycle'

  ! /* Create file */
  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, fid, error, access_prp=fapl2)
  CALL check("H5Fcreate_f",error,total_error)

  !/* Close file */
  CALL H5Fclose_f(fid,error)
  CALL check("H5Fclose_f",error,total_error)

  ! /* Get size of file as empty */
  ! if((empty_size = h5_get_file_size(filename)) < 0) TEST_ERROR

  ! /* Re-open file */

  CALL H5Fopen_f(filename, H5F_ACC_RDWR_F, fid, error,access_prp=fapl2)
  CALL check("H5Fopen_f",error,total_error)


  ! /* Set up group creation property list */
  CALL H5Pcreate_f(H5P_GROUP_CREATE_F,gcpl,error)
  CALL check("H5Pcreate_f",error,total_error)


  ! /* Query default group creation property settings */
  CALL H5Pget_local_heap_size_hint_f(gcpl, lheap_size_hint, error)
  CALL check("H5Pget_local_heap_size_hint_f",error,total_error)
  CALL verify("H5Pget_local_heap_size_hint_f", INT(lheap_size_hint),0,total_error)

  CALL H5Pget_link_phase_change_f(gcpl, max_compact, min_dense, error)
  CALL check("H5Pget_link_phase_change_f", error, total_error)
  CALL verify("H5Pget_link_phase_change_f", max_compact, H5G_CRT_GINFO_MAX_COMPACT,total_error)
  CALL verify("H5Pget_link_phase_change_f", min_dense, H5G_CRT_GINFO_MIN_DENSE,total_error)


  CALL H5Pget_est_link_info_f(gcpl, est_num_entries, est_name_len, error)
  CALL check("H5Pget_est_link_info_f", error, total_error)
  CALL verify("H5Pget_est_link_info_f", est_num_entries, H5G_CRT_GINFO_EST_NUM_ENTRIES,total_error)
  CALL verify("H5Pget_est_link_info_f", est_name_len, H5G_CRT_GINFO_EST_NAME_LEN,total_error)


  !/* Set GCPL parameters */

  CALL H5Pset_local_heap_size_hint_f(gcpl, LIFECYCLE_LOCAL_HEAP_SIZE_HINT, error)
  CALL check("H5Pset_local_heap_size_hint_f", error, total_error)
  CALL H5Pset_link_phase_change_f(gcpl, LIFECYCLE_MAX_COMPACT, LIFECYCLE_MIN_DENSE, error)
  CALL check("H5Pset_link_phase_change_f", error, total_error)
  CALL H5Pset_est_link_info_f(gcpl, LIFECYCLE_EST_NUM_ENTRIES, LIFECYCLE_EST_NAME_LEN, error)
  CALL check("H5Pset_est_link_info_f", error, total_error)

  ! /* Create group for testing lifecycle */

  CALL H5Gcreate_f(fid, LIFECYCLE_TOP_GROUP, gid, error, gcpl_id=gcpl)
  CALL check("H5Gcreate_f", error, total_error)

  ! /* Query group creation property settings */

  CALL H5Pget_local_heap_size_hint_f(gcpl, lheap_size_hint, error)
  CALL check("H5Pget_local_heap_size_hint_f",error,total_error)
  CALL verify("H5Pget_local_heap_size_hint_f", INT(lheap_size_hint),INT(LIFECYCLE_LOCAL_HEAP_SIZE_HINT),total_error)

  CALL H5Pget_link_phase_change_f(gcpl, max_compact, min_dense, error)
  CALL check("H5Pget_link_phase_change_f", error, total_error)
  CALL verify("H5Pget_link_phase_change_f", max_compact, LIFECYCLE_MAX_COMPACT,total_error)
  CALL verify("H5Pget_link_phase_change_f", min_dense, LIFECYCLE_MIN_DENSE,total_error)

  CALL H5Pget_est_link_info_f(gcpl, est_num_entries, est_name_len, error)
  CALL check("H5Pget_est_link_info_f", error, total_error)
  CALL verify("H5Pget_est_link_info_f", est_num_entries, LIFECYCLE_EST_NUM_ENTRIES,total_error)
  CALL verify("H5Pget_est_link_info_f", est_name_len, LIFECYCLE_EST_NAME_LEN,total_error)



    !/* Close top group */
    CALL H5Gclose_f(gid, error)
    CALL check("H5Gclose_f", error, total_error)

    !/* Unlink top group */

    CALL H5Ldelete_f(fid, LIFECYCLE_TOP_GROUP, error)
    CALL check("H5Ldelete_f", error, total_error)

    ! /* Close GCPL */
    CALL H5Pclose_f(gcpl, error)
    CALL check("H5Pclose_f", error, total_error)

    ! /* Close file */
    CALL H5Fclose_f(fid,error)
    CALL check("H5Fclose_f",error,total_error)

    IF(cleanup) CALL h5_cleanup_f("fixx", H5P_DEFAULT_F, error)
    CALL check("h5_cleanup_f", error, total_error)

  END SUBROUTINE lifecycle
!/*-------------------------------------------------------------------------
! * Function:	cklinks
! *
! * Purpose:	Open the file created in the first step and check that the
! *		links look correct.
! *
! * Return:	Success:	0
! *
! *		Failure:	-1
! *
! * Programmer:	M.S. Breitenfeld
! *             April 14, 2008
! *
! * Modifications: Modified Original C code
! *
! *-------------------------------------------------------------------------
! */


  SUBROUTINE cklinks(fapl, total_error)

!    USE ISO_C_BINDING
  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: total_error
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER :: error

  INTEGER(HID_T) :: file
!    H5O_info_t		oinfo1, oinfo2;
!    H5L_info_t		linfo2;

  CHARACTER(LEN=12), PARAMETER :: filename ='TestLinks.h5'

!  TYPE(C_PTR) :: linkval

  LOGICAL :: Lexists

  ! /* Open the file */
  CALL H5Fopen_f(filename, H5F_ACC_RDONLY_F, file, error,access_prp=fapl)
  CALL check("H5Fopen_f",error,total_error)


  ! /* Hard link */
!!$  IF(H5Oget_info_by_name(file, "d1", &oinfo1, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
!!$  IF(H5Oget_info_by_name(file, "grp1/hard", &oinfo2, H5P_DEFAULT) < 0) FAIL_STACK_ERROR
!!$  IF(H5O_TYPE_DATASET != oinfo2.type) {
!!$	H5_FAILED();
!!$	printf("    %d: Unexpected object type should have been a dataset\n", __LINE__);
!!$	TEST_ERROR
!!$    } /* end if */
!!$    if(H5F_addr_ne(oinfo1.addr, oinfo2.addr)) {
!!$	H5_FAILED();
!!$	puts("    Hard link test failed. Link seems not to point to the ");
!!$	puts("    expected file location.");
!!$	TEST_ERROR
!!$    } /* end if */


  CALL H5Lexists_f(file,"d1",Lexists, error)
  CALL verifylogical("test_lcpl.H5Lexists", Lexists,.TRUE.,total_error)

  CALL H5Lexists_f(file,"grp1/hard",Lexists, error)
  CALL verifylogical("test_lcpl.H5Lexists", Lexists,.TRUE.,total_error)

  ! /* Cleanup */
  CALL H5Fclose_f(file,error)
  CALL check("H5Fclose_f",error,total_error)

END SUBROUTINE cklinks


!/*-------------------------------------------------------------------------
! * Function:    delete_by_idx
! *
! * Purpose:     Create a group with creation order indices and test deleting
! *              links by index.
! *
! * Return:      Total error
! *
! * C Programmer:  Quincey Koziol
! *                Tuesday, November 14, 2006
! *
! * Adapted to FORTRAN: M.S. Breitenfeld
! *                     March 3, 2008
! *
! *-------------------------------------------------------------------------
! */
SUBROUTINE delete_by_idx(cleanup, fapl, total_error)

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: total_error
  INTEGER(HID_T), INTENT(IN) :: fapl

  INTEGER(HID_T) :: file_id  ! /* File ID */
  INTEGER(HID_T) :: group_id ! /* Group ID */
  INTEGER(HID_T) :: gcpl_id  ! /* Group creation property list ID */

  INTEGER :: idx_type        ! /* Type of index to operate on */
  LOGICAL, DIMENSION(1:2) :: use_index = (/.FALSE.,.TRUE./)
                             ! /* Use index on creation order values */
  INTEGER :: max_compact     ! /* Maximum # of links to store in group compactly */
  INTEGER :: min_dense       ! /* Minimum # of links to store in group "densely" */

  CHARACTER(LEN=7) :: objname   ! /* Object name */
  CHARACTER(LEN=8) :: filename = 'file0.h5' ! /* File name */
  CHARACTER(LEN=12), PARAMETER :: CORDER_GROUP_NAME = "corder_group"

  LOGICAL :: f_corder_valid ! Indicates whether the creation order data is valid for this attribute
  INTEGER :: corder ! Is a positive integer containing the creation order of the attribute
  INTEGER :: cset ! Indicates the character set used for the attribute’s name
  INTEGER(SIZE_T) :: val_size
  INTEGER :: link_type
  INTEGER(HADDR_T) :: address

  INTEGER :: u ! /* Local index variable */
  INTEGER :: Input1, i
  INTEGER(HID_T) :: group_id2
  INTEGER(HID_T) :: grp
  INTEGER :: iorder ! /* Order within in the index */
  CHARACTER(LEN=2) :: chr2
  INTEGER :: error
  INTEGER :: id_type
  !
  !
  !
  CHARACTER(LEN=80) :: fix_filename1
  CHARACTER(LEN=80) :: fix_filename2
  INTEGER(HSIZE_T) :: htmp

  LOGICAL :: cleanup

  DO i = 1, 80
     fix_filename1(i:i) = " "
     fix_filename2(i:i) = " "
  ENDDO

  ! /* Loop over operating on different indices on link fields */
  DO idx_type = H5_INDEX_NAME_F, H5_INDEX_CRT_ORDER_F
     ! /* Loop over operating in different orders */
     DO iorder = H5_ITER_INC_F,  H5_ITER_DEC_F
        ! /* Loop over using index for creation order value */
        DO i = 1, 2
           ! /* Print appropriate test message */
!!$           IF(idx_type == H5_INDEX_CRT_ORDER_F)THEN
!!$              IF(iorder == H5_ITER_INC_F)THEN
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(5x,A)')"deleting links by creation order index in increasing order w/creation order index"
!!$                 ELSE
!!$                    WRITE(*,'(5x,A)')"deleting links by creation order index in increasing order w/o creation order index"
!!$                 ENDIF
!!$              ELSE
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(5x,A)')"deleting links by creation order index in decreasing order w/creation order index"
!!$                 ELSE
!!$                    WRITE(*,'(5x,A)')"deleting links by creation order index in decreasing order w/o creation order index"
!!$                 ENDIF
!!$              ENDIF
!!$           ELSE
!!$              IF(iorder == H5_ITER_INC_F)THEN
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(5x,A)')"deleting links by name index in increasing order w/creation order index"
!!$                 ELSE
!!$                    WRITE(*,'(5x,A)')"deleting links by name index in increasing order w/o creation order index"
!!$                 ENDIF
!!$              ELSE
!!$                 IF(use_index(i))THEN
!!$                    WRITE(*,'(5x,A)')"deleting links by name index in decreasing order w/creation order index"
!!$                 ELSE
!!$                    WRITE(*,'(5x,A)')"deleting links by name index in decreasing order w/o creation order index"
!!$                 ENDIF
!!$              ENDIF
!!$           ENDIF

           ! /* Create file */
           CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp=fapl)
           CALL check("delete_by_idx.H5Fcreate_f", error, total_error)

           ! /* Create group creation property list */
           CALL H5Pcreate_f(H5P_GROUP_CREATE_F, gcpl_id, error )
           CALL check("delete_by_idx.H5Pcreate_f", error, total_error)

           ! /* Set creation order tracking & indexing on group */
           IF(use_index(i))THEN
              Input1 = H5P_CRT_ORDER_INDEXED_F
           ELSE
              Input1 = 0
           ENDIF

           CALL H5Pset_link_creation_order_f(gcpl_id, IOR(H5P_CRT_ORDER_TRACKED_F, Input1), error)
           CALL check("delete_by_idx.H5Pset_link_creation_order_f", error, total_error)

           ! /* Create group with creation order tracking on */
           CALL H5Gcreate_f(file_id, CORDER_GROUP_NAME, group_id, error, gcpl_id=gcpl_id)
           CALL check("delete_by_idx.H5Gcreate_f", error, total_error)

           ! /* Query the group creation properties */
           CALL H5Pget_link_phase_change_f(gcpl_id, max_compact, min_dense, error)
           CALL check("delete_by_idx.H5Pget_link_phase_change_f", error, total_error)


           ! /* Delete links from one end */

           ! /* Check for deletion on empty group */
           CALL H5Ldelete_by_idx_f(group_id, ".", idx_type, iorder, INT(0,HSIZE_T), error)
           CALL VERIFY("delete_by_idx.H5Ldelete_by_idx_f", error, -1, total_error) ! test should fail (error = -1)
           ! /* Create several links, up to limit of compact form */
           DO u = 0, max_compact-1
              ! /* Make name for link */
              WRITE(chr2,'(I2.2)') u
              objname = 'fill '//chr2

              ! /* Create hard link, with group object */
              CALL H5Gcreate_f(group_id, objname, group_id2, error)
              CALL check("delete_by_idx.H5Gcreate_f", error, total_error)
              CALL H5Gclose_f(group_id2, error)
              CALL check("delete_by_idx.H5Gclose_f", error, total_error)

              ! /* Verify link information for new link */
              CALL link_info_by_idx_check(group_id, objname, u, &
                   .TRUE., use_index(i), total_error)
           ENDDO

           ! /* Verify state of group (compact) */
           ! IF(H5G_has_links_test(group_id, NULL) != TRUE) TEST_ERROR

           ! /* Check for out of bound deletion */
           htmp =9
!EP           CALL H5Ldelete_by_idx_f(group_id, ".", idx_type, iorder, INT(u,HSIZE_T), error)
           CALL H5Ldelete_by_idx_f(group_id, ".", idx_type, iorder, htmp, error)
           CALL VERIFY("H5Ldelete_by_idx_f", error, -1, total_error) ! test should fail (error = -1)


           ! /* Delete links from compact group */

           DO u = 0, (max_compact - 1) -1
              ! /* Delete first link in appropriate order */
              CALL H5Ldelete_by_idx_f(group_id, ".", idx_type, iorder, INT(0,HSIZE_T), error)
              CALL check("H5Ldelete_by_idx_f", error, total_error)
              ! /* Verify the link information for first link in appropriate order */
              ! HDmemset(&linfo, 0, sizeof(linfo));

              CALL H5Lget_info_by_idx_f(group_id, ".", idx_type, iorder, INT(0,HSIZE_T), &
                   link_type, f_corder_valid, corder, cset, address, val_size, error)

              CALL H5Oopen_by_addr_f(group_id, address, grp, error)
              CALL check("H5Oopen_by_addr_f", error, total_error)

              CALL H5Iget_type_f(grp, id_type, error)
              CALL check("H5Iget_type_f", error, total_error)

              CALL VERIFY("H5Iget_type_f", id_type, H5I_GROUP_F, total_error)

              CALL H5Gclose_f(grp, error)
              CALL check("H5Gclose_f", error, total_error)

              CALL VerifyLogical("H5Lget_info_by_idx_f", f_corder_valid, .TRUE., total_error)

              CALL VERIFY("H5Lget_info_by_idx_f", H5L_TYPE_HARD_F, link_type, total_error)
              IF(iorder.EQ.H5_ITER_INC_F)THEN
                 CALL VERIFY("H5Lget_info_by_idx_f", corder, u+1, total_error)
              ELSE
                 CALL VERIFY("H5Lget_info_by_idx_f", corder, (max_compact - (u + 2)), total_error)
              ENDIF

              CALL VERIFY("H5Lget_info_by_idx_f",cset, H5T_CSET_ASCII_F, total_error)



              ! /* Verify the name for first link in appropriate order */
              ! HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
!!$              size_tmp = 20
!!$              CALL H5Lget_name_by_idx_f(group_id, ".", idx_type, order, INT(0,HSIZE_T), size_tmp, tmpname, error)
!!$              CALL check("delete_by_idx.H5Lget_name_by_idx_f", error, total_error)
!!$
!!$              IF(order .EQ. H5_ITER_INC_F)THEN
!!$                 WRITE(chr2,'(I2.2)') u + 1
!!$              ELSE
!!$                 WRITE(chr2,'(I2.2)') (max_compact - (u + 2))
!!$              ENDIF
!!$              objname = 'fill '//chr2
!!$              PRINT*,objname, tmpname
!!$              CALL verifyString("delete_by_idx.H5Lget_name_by_idx_f", objname, tmpname,  total_error)
           ENDDO

           ! /* Close the group */
           CALL H5Gclose_f(group_id, error)
           CALL check("delete_by_idx.H5Gclose_f", error, total_error)

           !/* Close the group creation property list */
           CALL H5Pclose_f(gcpl_id, error)
           CALL check("delete_by_idx.H5Gclose_f", error, total_error)

           !/* Close the file */
           CALL H5Fclose_f(file_id, error)
           CALL check("delete_by_idx.H5Gclose_f", error, total_error)

           IF(cleanup) CALL h5_cleanup_f("file0", H5P_DEFAULT_F, error)
           CALL check("h5_cleanup_f", error, total_error)

        ENDDO
     ENDDO
  ENDDO


END SUBROUTINE delete_by_idx



!/*-------------------------------------------------------------------------
! * Function:    link_info_by_idx_check
! *
! * Purpose:     Support routine for link_info_by_idx, to verify the link
! *              info is correct for a link
! *
! * Note:	This routine assumes that the links have been inserted in the
! *              group in alphabetical order.
! *
! * Return:      Success:        0
! *              Failure:        -1
! *
! * Programmer:  Quincey Koziol
! *              Tuesday, November  7, 2006
! *
! *-------------------------------------------------------------------------
! */
SUBROUTINE link_info_by_idx_check(group_id, linkname, n, &
    hard_link, use_index, total_error)

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: total_error
  INTEGER(HID_T), INTENT(IN) :: group_id
  CHARACTER(LEN=*), INTENT(IN) :: linkname
  INTEGER, INTENT(IN) :: n
  LOGICAL, INTENT(IN) :: hard_link
  LOGICAL, INTENT(IN) :: use_index

  LOGICAL :: f_corder_valid ! Indicates whether the creation order data is valid for this attribute
  INTEGER :: corder ! Is a positive integer containing the creation order of the attribute
  INTEGER :: cset ! Indicates the character set used for the attribute’s name
  INTEGER :: link_type
  INTEGER(HADDR_T) :: address
  INTEGER(SIZE_T) :: val_size   ! Indicates the size, in the number of characters, of the attribute

  CHARACTER(LEN=7) :: tmpname     !/* Temporary link name */
  CHARACTER(LEN=3) :: tmpname_small !/* to small temporary link name */
  CHARACTER(LEN=10) :: tmpname_big !/* to big temporary link name */

  CHARACTER(LEN=7) :: valname     !/* Link value name */
  CHARACTER(LEN=2) :: chr2
  INTEGER(SIZE_T) :: size_tmp
  INTEGER :: error

  ! /* Make link value for increasing/native order queries */

  WRITE(chr2,'(I2.2)') n
  valname = 'valn.'//chr2

  ! /* Verify the link information for first link, in increasing creation order */
  !  HDmemset(&linfo, 0, sizeof(linfo));
  CALL H5Lget_info_by_idx_f(group_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, INT(0,HSIZE_T), &
       link_type, f_corder_valid, corder, cset, address, val_size, error)
  CALL check("H5Lget_info_by_idx_f", error, total_error)
  CALL VERIFY("H5Lget_info_by_idx_f", corder, 0, total_error)

  ! /* Verify the link information for new link, in increasing creation order */
  ! HDmemset(&linfo, 0, sizeof(linfo));
  CALL H5Lget_info_by_idx_f(group_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, INT(n,HSIZE_T), &
       link_type, f_corder_valid, corder, cset, address, val_size, error)
  CALL check("H5Lget_info_by_idx_f", error, total_error)
  CALL VERIFY("H5Lget_info_by_idx_f", corder, n, total_error)

  ! /* Verify value for new soft link, in increasing creation order */
!!$  IF(hard_link)THEN
!!$     ! HDmemset(tmpval, 0, (size_t)NAME_BUF_SIZE);
!!$
!!$     CALL H5Lget_val_by_idx_f(group_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, n, tmpval, INT(7,SIZE_T),error)
!!$     CALL check("H5Lget_val_by_idx",error,total_error)
!!$
!!$!     IF(HDstrcmp(valname, tmpval)) TEST_ERROR
!!$  ENDIF

  ! /* Verify the name for new link, in increasing creation order */
  !  HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);

  ! The actual size of tmpname should be 7

  CALL H5Lget_name_by_idx_f(group_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, INT(n,HSIZE_T), tmpname_small, error, size_tmp)
  CALL check("link_info_by_idx_check.H5Lget_name_by_idx_f", error, total_error)
  CALL verifyString("link_info_by_idx_check.H5Lget_name_by_idx_f", &
       linkname(1:LEN(tmpname_small)), tmpname_small(1:LEN(tmpname_small)),  total_error)
  CALL VERIFY("link_info_by_idx_check.H5Lget_name_by_idx_f", INT(size_tmp), 7, total_error)
  ! try it with the correct size
  CALL H5Lget_name_by_idx_f(group_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, INT(n,HSIZE_T), tmpname, error, size=size_tmp)
  CALL check("link_info_by_idx_check.H5Lget_name_by_idx_f", error, total_error)
  CALL verifyString("link_info_by_idx_check.H5Lget_name_by_idx_f", &
       linkname(1:LEN(tmpname)), tmpname(1:LEN(tmpname)),  total_error)
  CALL VERIFY("link_info_by_idx_check.H5Lget_name_by_idx_f", INT(size_tmp), 7, total_error)

  CALL H5Lget_name_by_idx_f(group_id, ".", H5_INDEX_CRT_ORDER_F, H5_ITER_INC_F, INT(n,HSIZE_T), tmpname_big, error, size_tmp)
  CALL check("link_info_by_idx_check.H5Lget_name_by_idx_f", error, total_error)
  CALL verifyString("link_info_by_idx_check.H5Lget_name_by_idx_f", &
       linkname(1:7), tmpname_big(1:7),  total_error)
  CALL VERIFY("link_info_by_idx_check.H5Lget_name_by_idx_f", INT(size_tmp), 7, total_error)

  ! Try with a buffer set to small


  END SUBROUTINE link_info_by_idx_check


!/*-------------------------------------------------------------------------
! * Function:    test_lcpl
! *
! * Purpose:     Tests Link Creation Property Lists
! *
! * Return:      Success:        0
! *              Failure:        number of errors
! *
! * Programmer:  M.S. Breitenfeld
! *              Modified C routine
! *              March 12, 2008
! *
! * Modifications:
! *
! *-------------------------------------------------------------------------
! */


  SUBROUTINE test_lcpl(cleanup, fapl, total_error)

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: total_error
  INTEGER(HID_T), INTENT(IN) :: fapl
  LOGICAL :: cleanup

  INTEGER(HID_T) :: file_id
  INTEGER(HID_T) :: group_id
  INTEGER(HID_T) :: space_id, data_space
  INTEGER(HID_T) :: dset_id
  INTEGER(HID_T) :: type_id
  INTEGER(HID_T) :: lcpl_id

  INTEGER :: cset ! Indicates the character set used for the link’s name.
  INTEGER :: corder ! Specifies the link’s creation order position.
  LOGICAL :: f_corder_valid ! Indicates whether the value in corder is valid.
  INTEGER :: link_type ! Specifies the link class:
     	                              !  H5L_TYPE_HARD_F      - Hard link
     	                              !  H5L_TYPE_SOFT_F      - Soft link
     	                              !  H5L_TYPE_EXTERNAL_F  - External link
     	                              !  H5L_TYPE_ERROR _F    - Error
  INTEGER(HADDR_T) :: address  ! If the link is a hard link, address specifies the file address that the link points to
  INTEGER(SIZE_T) :: val_size ! If the link is a symbolic link, val_size will be the length of the link value

  CHARACTER(LEN=1024) :: filename = 'tempfile.h5'
  INTEGER, PARAMETER :: TEST6_DIM1 = 8, TEST6_DIM2 = 7
  INTEGER(HSIZE_T), DIMENSION(1:2), PARAMETER :: dims = (/TEST6_DIM1,TEST6_DIM2/)

  INTEGER :: encoding
  INTEGER :: error
  LOGICAL :: Lexists
  INTEGER(HSIZE_T), DIMENSION(1:2), PARAMETER :: extend_dim = (/TEST6_DIM1-2,TEST6_DIM2-3/)
  INTEGER(HSIZE_T), DIMENSION(1:2) :: dimsout, maxdimsout ! dimensions

  INTEGER :: i
  INTEGER :: tmp1, tmp2
  INTEGER(HID_T) :: crp_list

!  WRITE(*,*) "link creation property lists (w/new group format)"


  !/* Actually, intermediate group creation is tested elsewhere (tmisc).
  ! * Here we only need to test the character encoding property */

  !/* Create file */
  !  h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

  CALL H5Fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, H5P_DEFAULT_F, fapl)
  CALL check("test_lcpl.H5Fcreate_f", error, total_error)


  ! /* Create and link a group with the default LCPL */

  CALL H5Gcreate_f(file_id, "/group", group_id, error)
  CALL check("test_lcpl.H5Gcreate_f", error, total_error)


  ! /* Check that its character encoding is the default */

  CALL H5Lget_info_f(file_id, "group", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error, H5P_DEFAULT_F)

!/* File-wide default character encoding can not yet be set via the file
! * creation property list and is always ASCII. */
!#define H5F_DEFAULT_CSET H5T_CSET_ASCII  -- FROM H5Fprivate.h --

  CALL VERIFY("test_lcpl.H5Lget_info_f",cset, H5T_CSET_ASCII_F,total_error)

  ! /* Create and commit a datatype with the default LCPL */
  CALL h5tcopy_f(H5T_NATIVE_INTEGER, type_id, error)
  CALL check("test_lcpl.h5tcopy_f",error,total_error)
  CALL h5tcommit_f(file_id, "/type", type_id, error)
  CALL check("test_lcpl.h5tcommit_f", error, total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("test_lcpl.h5tclose_f", error, total_error)


  ! /* Check that its character encoding is the default */
  CALL H5Lget_info_f(file_id, "type", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.h5tclose_f", error, total_error)

!/* File-wide default character encoding can not yet be set via the file
! * creation property list and is always ASCII. */
!#define H5F_DEFAULT_CSET H5T_CSET_ASCII  -- FROM H5Fprivate.h --

  CALL verify("test_lcpl.H5Lget_info_f",cset, H5T_CSET_ASCII_F,total_error)

  !/* Create a dataspace */
  CALL h5screate_simple_f(2, dims, space_id, error)
  CALL check("test_lcpl.h5screate_simple_f",error,total_error)
  CALL h5pcreate_f(H5P_DATASET_CREATE_F, crp_list, error)
  CALL h5pset_chunk_f(crp_list, 2, dims, error)

  ! /* Create a dataset using the default LCPL */
  CALL h5dcreate_f(file_id, "/dataset", H5T_NATIVE_INTEGER, space_id, dset_id, error, crp_list)
  CALL check("test_lcpl.h5dcreate_f", error, total_error)
  CALL h5dclose_f(dset_id, error)
  CALL check("test_lcpl.h5dclose_f", error, total_error)

  ! Reopen

  CALL H5Dopen_f(file_id, "/dataset", dset_id, error)
  CALL check("test_lcpl.h5dopen_f", error, total_error)

  !  /* Extend the  dataset */
  CALL H5Dset_extent_f(dset_id, extend_dim, error)
  CALL check("test_lcpl.H5Dset_extent_f", error, total_error)
  !  /* Verify the dataspaces */
        !
          !Get dataset's dataspace handle.
          !
  CALL h5dget_space_f(dset_id, data_space, error)
  CALL check("h5dget_space_f",error,total_error)

  CALL h5sget_simple_extent_dims_f(data_space, dimsout, maxdimsout, error)
  CALL check("test_lcpl.h5sget_simple_extent_dims_f",error, total_error)

  DO i = 1, 2
     tmp1 = dimsout(i)
     tmp2 = extend_dim(i)
!EP     CALL VERIFY("H5Sget_simple_extent_dims", dimsout(i), extend_dim(i), total_error)
     CALL VERIFY("H5Sget_simple_extent_dims", tmp1, tmp2, total_error)
!EP     CALL VERIFY("H5Sget_simple_extent_dims", maxdimsout(i), dims(i), total_error)
     tmp1 = maxdimsout(i)
     tmp2 = dims(i)
     CALL VERIFY("H5Sget_simple_extent_dims", tmp1, tmp2, total_error)
  ENDDO

  ! /* close data set */

  CALL h5dclose_f(dset_id, error)
  CALL check("test_lcpl.h5dclose_f", error, total_error)

  ! /* Check that its character encoding is the default */
  CALL H5Lget_info_f(file_id, "dataset", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.H5Lget_info_f", error, total_error)

!/* File-wide default character encoding can not yet be set via the file
! * creation property list and is always ASCII. */
!#define H5F_DEFAULT_CSET H5T_CSET_ASCII  -- FROM H5Fprivate.h --

  CALL verify("test_lcpl.h5tclose_f",cset, H5T_CSET_ASCII_F,total_error)

  !/* Create a link creation property list with the UTF-8 character encoding */
  CALL H5Pcreate_f(H5P_LINK_CREATE_F,lcpl_id,error)
  CALL check("test_lcpl.h5Pcreate_f",error,total_error)
  CALL H5Pset_char_encoding_f(lcpl_id, H5T_CSET_UTF8_F, error)
  CALL check("test_lcpl.H5Pset_char_encoding_f",error, total_error)

  ! /* Create and link a group with the new LCPL */
  CALL H5Gcreate_f(file_id, "/group2", group_id, error,lcpl_id=lcpl_id)
  CALL check("test_lcpl.test_lcpl.H5Gcreate_f", error, total_error)
  CALL H5Gclose_f(group_id, error)
  CALL check("test_lcpl.test_lcpl.H5Gclose_f", error, total_error)


  !/* Check that its character encoding is UTF-8 */
  CALL H5Lget_info_f(file_id, "group2", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.H5Lget_info_f", error, total_error)
  CALL verify("test_lcpl.H5Lget_info_f",cset, H5T_CSET_UTF8_F,total_error)


  ! /* Create and commit a datatype with the new LCPL */

  CALL h5tcopy_f(H5T_NATIVE_INTEGER, type_id, error)
  CALL check("test_lcpl.h5tcopy_f",error,total_error)
  CALL h5tcommit_f(file_id, "/type2", type_id, error, lcpl_id=lcpl_id)
  CALL check("test_lcpl.h5tcommit_f", error, total_error)
  CALL h5tclose_f(type_id, error)
  CALL check("test_lcpl.h5tclose_f", error, total_error)


  !/* Check that its character encoding is UTF-8 */
  CALL H5Lget_info_f(file_id, "type2", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.H5Lget_info_f", error, total_error)
  CALL verify("test_lcpl.H5Lget_info_f",cset, H5T_CSET_UTF8_F,total_error)

  ! /* Create a dataset using the new LCPL */
  CALL h5dcreate_f(file_id, "/dataset2", H5T_NATIVE_INTEGER, space_id, dset_id, error,lcpl_id=lcpl_id)
  CALL check("test_lcpl.h5dcreate_f", error, total_error)

  CALL h5dclose_f(dset_id, error)
  CALL check("test_lcpl.h5dclose_f", error, total_error)

  CALL H5Pget_char_encoding_f(lcpl_id, encoding, error)
  CALL check("test_lcpl.H5Pget_char_encoding_f", error, total_error)
  CALL VERIFY("test_lcpl.H5Pget_char_encoding_f", encoding, H5T_CSET_UTF8_F, total_error)

  ! /* Check that its character encoding is UTF-8 */
  CALL H5Lget_info_f(file_id, "dataset2", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.H5Lget_info_f", error, total_error)
  CALL verify("test_lcpl.H5Lget_info_f2",cset, H5T_CSET_UTF8_F,total_error)

  ! /* Create a new link to the dataset with a different character encoding. */
  CALL H5Pclose_f(lcpl_id, error)
  CALL check("test_lcpl.H5Pclose_f", error, total_error)

  CALL H5Pcreate_f(H5P_LINK_CREATE_F,lcpl_id,error)
  CALL check("test_lcpl.h5Pcreate_f",error,total_error)
  CALL H5Pset_char_encoding_f(lcpl_id, H5T_CSET_ASCII_F, error)
  CALL check("test_lcpl.H5Pset_char_encoding_f",error, total_error)
  CALL H5Lcreate_hard_f(file_id, "/dataset2", file_id, "/dataset2_link", error, lcpl_id)
  CALL check("test_lcpl.H5Lcreate_hard_f",error, total_error)

  CALL H5Lexists_f(file_id,"/dataset2_link",Lexists, error)
  CALL check("test_lcpl.H5Lexists",error, total_error)
  CALL verifylogical("test_lcpl.H5Lexists", Lexists,.TRUE.,total_error)

  ! /* Check that its character encoding is ASCII */
  CALL H5Lget_info_f(file_id, "/dataset2_link", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.H5Lget_info_f", error, total_error)
  CALL verify("test_lcpl.H5Lget_info_f",cset, H5T_CSET_ASCII_F,total_error)

  ! /* Check that the first link's encoding hasn't changed */

  CALL H5Lget_info_f(file_id, "/dataset2", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.H5Lget_info_f", error, total_error)
  CALL verify("test_lcpl.H5Lget_info_f3",cset, H5T_CSET_UTF8_F,total_error)


  !/* Make sure that LCPLs work properly for other API calls: */
  !/* H5Lcreate_soft */

  CALL H5Pset_char_encoding_f(lcpl_id, H5T_CSET_UTF8_F, error)
  CALL check("test_lcpl.H5Pset_char_encoding_f",error, total_error)
  CALL H5Lcreate_soft_f("dataset2", file_id, "slink_to_dset2",error,lcpl_id)
  CALL check("H5Lcreate_soft_f", error, total_error)

  CALL H5Lget_info_f(file_id, "slink_to_dset2", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.H5Lget_info_f", error, total_error)
  CALL verify("test_lcpl.H5Lget_info_f",cset, H5T_CSET_UTF8_F,total_error)


  ! /* H5Lmove */
  CALL H5Pset_char_encoding_f(lcpl_id, H5T_CSET_ASCII_F, error)
  CALL check("test_lcpl.H5Pset_char_encoding_f",error, total_error)

  CALL H5Lmove_f(file_id, "slink_to_dset2", file_id, "moved_slink", error, lcpl_id, H5P_DEFAULT_F)
  CALL check("test_lcpl.H5Lmove_f",error, total_error)

  CALL H5Lget_info_f(file_id, "moved_slink", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.H5Lget_info_f", error, total_error)
  CALL verify("test_lcpl.H5Lget_info_f",cset, H5T_CSET_ASCII_F,total_error)


  ! /* H5Lcopy */

  CALL H5Pset_char_encoding_f(lcpl_id, H5T_CSET_UTF8_F, error)
  CALL check("test_lcpl.H5Pset_char_encoding_f",error, total_error)

  CALL H5Lcopy_f(file_id, "moved_slink", file_id, "copied_slink", error, lcpl_id)

  CALL H5Lget_info_f(file_id, "copied_slink", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.H5Lget_info_f", error, total_error)
  CALL verify("test_lcpl.H5Lget_info_f",cset, H5T_CSET_UTF8_F,total_error)


  ! /* H5Lcreate_external */

  CALL H5Lcreate_external_f("test_lcpl.filename", "path", file_id, "extlink", error, lcpl_id)
  CALL check("test_lcpl.H5Lcreate_external_f", error, total_error)

  CALL H5Lget_info_f(file_id, "extlink", &
       cset, corder, f_corder_valid, link_type, address, val_size, &
       error)
  CALL check("test_lcpl.H5Lget_info_f", error, total_error)
  CALL verify("test_lcpl.H5Lget_info_f",cset, H5T_CSET_UTF8_F,total_error)


  ! /* Close open IDs */

  CALL H5Pclose_f(lcpl_id, error)
  CALL check("test_lcpl.H5Pclose_f", error, total_error)
  CALL H5Sclose_f(space_id, error)
  CALL check("test_lcpl.h5Sclose_f",error,total_error)
  CALL H5Fclose_f(file_id, error)
  CALL check("test_lcpl.H5Fclose_f", error, total_error)

  IF(cleanup) CALL h5_cleanup_f("tempfile", H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)


END SUBROUTINE test_lcpl

SUBROUTINE objcopy(fapl, total_error)

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: total_error
  INTEGER(HID_T), INTENT(IN) :: fapl

  INTEGER(HID_T) :: fapl2, pid

  INTEGER :: flag, cpy_flags

  INTEGER :: error

  flag = H5O_COPY_SHALLOW_HIERARCHY_F

!/* Copy the file access property list */
  CALL H5Pcopy_f(fapl, fapl2, error)
  CALL check("H5Pcopy_f", error, total_error)

!/* Set the "use the latest version of the format" bounds for creating objects in the file */
  CALL H5Pset_libver_bounds_f(fapl2, H5F_LIBVER_LATEST_F, H5F_LIBVER_LATEST_F, error)

  ! /* create property to pass copy options */
  CALL h5pcreate_f(H5P_OBJECT_COPY_F, pid, error)
  CALL check("h5pcreate_f",error, total_error)

  ! /* set options for object copy */
  CALL H5Pset_copy_object_f(pid, flag, error)
  CALL check("H5Pset_copy_object_f",error, total_error)

  ! /* Verify object copy flags */
  CALL H5Pget_copy_object_f(pid, cpy_flags, error)
  CALL check("H5Pget_copy_object_f",error, total_error)
  CALL VERIFY("H5Pget_copy_object_f", cpy_flags, flag, total_error)

!!$
!!$  CALL test_copy_option(fcpl_src, fcpl_dst, my_fapl, H5O_COPY_WITHOUT_ATTR_FLAG,
!!$                       FALSE, "H5Ocopy(): without attributes");

  CALL lapl_nlinks(fapl2, total_error)

END SUBROUTINE objcopy


!/*-------------------------------------------------------------------------
! * Function:    lapl_nlinks
! *
! * Purpose:     Check that the maximum number of soft links can be adjusted
! *              by the user using the Link Access Property List.
! *
! * Return:      Success:        0
! *
! *              Failure:        -1
! *
! * Programmer:  James Laird
! *              Tuesday, June 6, 2006
! *
! * Modifications:
! *
! *-------------------------------------------------------------------------
! */

SUBROUTINE lapl_nlinks( fapl, total_error)

  USE HDF5

  IMPLICIT NONE
  INTEGER(HID_T), INTENT(IN) :: fapl
  INTEGER, INTENT(INOUT) :: total_error

  INTEGER :: error

  INTEGER(HID_T) :: fid = (-1) !/* File ID */
  INTEGER(HID_T) :: gid = (-1), gid2 = (-1) !/* Group IDs */
  INTEGER(HID_T) :: plist = (-1) ! /* lapl ID */
  INTEGER(HID_T) :: tid = (-1) ! /* Other IDs */
  INTEGER(HID_T) :: gapl = (-1), dapl = (-1), tapl = (-1) ! /* Other property lists */

  CHARACTER(LEN=7) :: objname ! /* Object name */
  INTEGER(size_t) :: name_len ! /* Length of object name */
  CHARACTER(LEN=12) :: filename = 'TestLinks.h5'
  INTEGER(size_t) ::              nlinks ! /* nlinks for H5Pset_nlinks */
  INTEGER(size_t) :: buf_size = 7

!  WRITE(*,*) "adjusting nlinks with LAPL (w/new group format)"


  ! /* Create file */
  CALL h5fcreate_f(FileName, H5F_ACC_TRUNC_F, fid, error, access_prp=fapl)
  CALL check(" lapl_nlinks.h5fcreate_f",error,total_error)

  ! /* Create group with short name in file (used as target for links) */
  CALL H5Gcreate_f(fid, "final", gid, error)
  CALL check(" lapl_nlinks.H5Gcreate_f", error, total_error)

  !/* Create chain of soft links to existing object (limited) */
  CALL H5Lcreate_soft_f("final", fid, "soft1", error)
  CALL H5Lcreate_soft_f("soft1", fid, "soft2", error)
  CALL H5Lcreate_soft_f("soft2", fid, "soft3", error)
  CALL H5Lcreate_soft_f("soft3", fid, "soft4", error)
  CALL H5Lcreate_soft_f("soft4", fid, "soft5", error)
  CALL H5Lcreate_soft_f("soft5", fid, "soft6", error)
  CALL H5Lcreate_soft_f("soft6", fid, "soft7", error)
  CALL H5Lcreate_soft_f("soft7", fid, "soft8", error)
  CALL H5Lcreate_soft_f("soft8", fid, "soft9", error)
  CALL H5Lcreate_soft_f("soft9", fid, "soft10", error)
  CALL H5Lcreate_soft_f("soft10", fid, "soft11", error)
  CALL H5Lcreate_soft_f("soft11", fid, "soft12", error)
  CALL H5Lcreate_soft_f("soft12", fid, "soft13", error)
  CALL H5Lcreate_soft_f("soft13", fid, "soft14", error)
  CALL H5Lcreate_soft_f("soft14", fid, "soft15", error)
  CALL H5Lcreate_soft_f("soft15", fid, "soft16", error)
  CALL H5Lcreate_soft_f("soft16", fid, "soft17", error)

  !/* Close objects */
  CALL H5Gclose_f(gid, error)
  CALL check("h5gclose_f",error,total_error)
  CALL h5fclose_f(fid, error)
  CALL check("h5fclose_f",error,total_error)

  !/* Open file */

  CALL h5fopen_f(FileName, H5F_ACC_RDWR_F, fid, error, fapl)
  CALL check("h5open_f",error,total_error)

  !/* Create LAPL with higher-than-usual nlinks value */
  !/* Create a non-default lapl with udata set to point to the first group */

  CALL H5Pcreate_f(H5P_LINK_ACCESS_F,plist,error)
  CALL check("h5Pcreate_f",error,total_error)
  nlinks = 20
  CALL H5Pset_nlinks_f(plist, nlinks, error)
  CALL check("H5Pset_nlinks_f",error,total_error)
  !/* Ensure that nlinks was set successfully */
  nlinks = 0
  CALL H5Pget_nlinks_f(plist, nlinks, error)
  CALL check("H5Pset_nlinks_f",error,total_error)
  CALL VERIFY("H5Pset_nlinks_f",INT(nlinks), 20, total_error)


  !/* Open object through what is normally too many soft links using
  ! * new property list */

  CALL H5Oopen_f(fid,"soft17",gid,error,plist)
  CALL check("H5Oopen_f",error,total_error)

  !/* Check name */
  CALL h5iget_name_f(gid, objname, buf_size, name_len, error)
  CALL check("h5iget_name_f",error,total_error)
  CALL VerifyString("h5iget_name_f", TRIM(objname),"/soft17", total_error)
  !/* Create group using soft link */
  CALL H5Gcreate_f(gid, "new_soft", gid2, error)
  CALL check("H5Gcreate_f", error, total_error)

  ! /* Close groups */
  CALL H5Gclose_f(gid2, error)
  CALL check("H5Gclose_f", error, total_error)
  CALL H5Gclose_f(gid, error)
  CALL check("H5Gclose_f", error, total_error)


  !/* Set nlinks to a smaller number */
  nlinks = 4
  CALL H5Pset_nlinks_f(plist, nlinks, error)
  CALL check("H5Pset_nlinks_f", error, total_error)

  !/* Ensure that nlinks was set successfully */
  nlinks = 0

  CALL H5Pget_nlinks_f(plist, nlinks, error)
  CALL check("H5Pget_nlinks_f",error,total_error)
  CALL VERIFY("H5Pget_nlinks_f", INT(nlinks), 4, total_error)

  ! /* Try opening through what is now too many soft links */

  CALL H5Oopen_f(fid,"soft5",gid,error,plist)
  CALL VERIFY("H5Oopen_f", error, -1, total_error) ! should fail

  ! /* Open object through lesser soft link */
  CALL H5Oopen_f(fid,"soft4",gid,error,plist)
  CALL check("H5Oopen_",error,total_error)

  ! /* Check name */
  CALL h5iget_name_f(gid, objname, buf_size, name_len, error)
  CALL check("h5iget_name_f",error,total_error)
  CALL VerifyString("h5iget_name_f", TRIM(objname),"/soft4", total_error)

  ! /* Test other functions that should use a LAPL */
  nlinks = 20
  CALL H5Pset_nlinks_f(plist, nlinks, error)
  CALL check("H5Pset_nlinks_f", error, total_error)

  !/* Try copying and moving when both src and dst contain many soft links
  ! * using a non-default LAPL
  ! */
  CALL H5Lcopy_f(fid, "soft17", fid, "soft17/newer_soft", error, H5P_DEFAULT_F, plist)
  CALL check("H5Lcopy_f",error,total_error)

  CALL H5Lmove_f(fid, "soft17/newer_soft", fid, "soft17/newest_soft", error, lapl_id=plist)
  CALL check("H5Lmove_f",error, total_error)

  ! /* H5Olink */
  CALL H5Olink_f(gid, fid, "soft17/link_to_group", error, H5P_DEFAULT_F, plist)
  CALL check("H5Olink_f", error, total_error)

  ! /* H5Lcreate_hard and H5Lcreate_soft */
  CALL H5Lcreate_hard_f(fid, "soft17", fid, "soft17/link2_to_group", error, H5P_DEFAULT_F, plist)
  CALL check("H5Lcreate_hard_f", error, total_error)


  CALL H5Lcreate_soft_f("/soft4", fid, "soft17/soft_link",error, H5P_DEFAULT_F, plist)
  CALL check("H5Lcreate_soft_f", error, total_error)

  ! /* H5Ldelete */
  CALL h5ldelete_f(fid, "soft17/soft_link", error, plist)
  CALL check("H5Ldelete_f", error, total_error)

!!$    /* H5Lget_val and H5Lget_info */
!!$    if(H5Lget_val(fid, "soft17", NULL, (size_t)0, plist) < 0) TEST_ERROR
!!$    if(H5Lget_info(fid, "soft17", NULL, plist) < 0) TEST_ERROR
!!$

  ! /* H5Lcreate_external and H5Lcreate_ud */
  CALL H5Lcreate_external_f("filename", "path", fid, "soft17/extlink", error, H5P_DEFAULT_F, plist)
  CALL check("H5Lcreate_external_f", error, total_error)

!!$    if(H5Lregister(UD_rereg_class) < 0) TEST_ERROR
!!$    if(H5Lcreate_ud(fid, "soft17/udlink", UD_HARD_TYPE, NULL, (size_t)0, H5P_DEFAULT, plist) < 0) TEST_ERROR
!!$
    ! /* Close plist */
  CALL h5pclose_f(plist, error)
  CALL check("h5pclose_f", error, total_error)

    ! /* Create a datatype and dataset as targets inside the group */
  CALL h5tcopy_f(H5T_NATIVE_INTEGER, tid, error)
  CALL check("h5tcopy_f",error,total_error)
  CALL h5tcommit_f(gid, "datatype", tid, error)
  CALL check("h5tcommit_f", error, total_error)
  CALL h5tclose_f(tid, error)
  CALL check("h5tclose_f", error, total_error)

!!$
!!$    dims[0] = 2;
!!$    dims[1] = 2;
!!$    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR
!!$    if((did = H5Dcreate2(gid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR
!!$    if(H5Dclose(did) < 0) TEST_ERROR
!!$
  !/* Close group */
  CALL h5gclose_f(gid, error)
  CALL check("h5gclose_f",error,total_error)

!!$
!!$    /* Try to open the objects using too many symlinks with default *APLs */
!!$    H5E_BEGIN_TRY {
!!$        if((gid = H5Gopen2(fid, "soft17", H5P_DEFAULT)) >= 0)
!!$            FAIL_PUTS_ERROR("    Should have failed for too many nested links.")
!!$        if((tid = H5Topen2(fid, "soft17/datatype", H5P_DEFAULT)) >= 0)
!!$            FAIL_PUTS_ERROR("    Should have failed for too many nested links.")
!!$        if((did = H5Dopen2(fid, "soft17/dataset", H5P_DEFAULT)) >= 0)
!!$            FAIL_PUTS_ERROR("    Should have failed for too many nested links.")
!!$    } H5E_END_TRY
!!$
    ! /* Create property lists with nlinks set */

  CALL H5Pcreate_f(H5P_GROUP_ACCESS_F,gapl,error)
  CALL check("h5Pcreate_f",error,total_error)
  CALL H5Pcreate_f(H5P_DATATYPE_ACCESS_F,tapl,error)
  CALL check("h5Pcreate_f",error,total_error)
  CALL H5Pcreate_f(H5P_DATASET_ACCESS_F,dapl,error)
  CALL check("h5Pcreate_f",error,total_error)


  nlinks = 20
  CALL H5Pset_nlinks_f(gapl, nlinks, error)
  CALL check("H5Pset_nlinks_f", error, total_error)
  CALL H5Pset_nlinks_f(tapl, nlinks, error)
  CALL check("H5Pset_nlinks_f", error, total_error)
  CALL H5Pset_nlinks_f(dapl, nlinks, error)
  CALL check("H5Pset_nlinks_f", error, total_error)

  !/* We should now be able to use these property lists to open each kind
  ! * of object.
  ! */

  CALL H5Gopen_f(fid, "soft17", gid, error, gapl)
  CALL check("H5Gopen_f",error,total_error)

  CALL H5Topen_f(fid, "soft17/datatype", tid, error, tapl)
  CALL check("H5Gopen_f",error,total_error)

!!$    if((did = H5Dopen2(fid, "soft17/dataset", dapl)) < 0) TEST_ERROR

  ! /* Close objects */

  CALL h5gclose_f(gid, error)
  CALL check("h5gclose_f",error,total_error)
  CALL h5tclose_f(tid, error)
  CALL check("h5tclose_f", error, total_error)

!!$    if(H5Dclose(did) < 0) TEST_ERROR
!!$
  ! /* Close plists */

  CALL h5pclose_f(gapl, error)
  CALL check("h5pclose_f", error, total_error)
  CALL h5pclose_f(tapl, error)
  CALL check("h5pclose_f", error, total_error)

!!$    if(H5Pclose(dapl) < 0) TEST_ERROR
!!$
!!$    /* Unregister UD hard link class */
!!$    if(H5Lunregister(UD_HARD_TYPE) < 0) TEST_ERROR
!!$

  ! /* Close file */
  CALL H5Fclose_f(fid, error)
  CALL check("H5Fclose_f", error, total_error)

END SUBROUTINE lapl_nlinks
