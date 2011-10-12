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
    SUBROUTINE external_test(cleanup, total_error)

!   This subroutine tests following functionalities:
!   h5pset_external_f,  h5pget_external_count_f,
!   h5pget_external_f

     USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(OUT) :: total_error

     CHARACTER(LEN=8), PARAMETER :: filename = "external"
     CHARACTER(LEN=80) :: fix_filename
     INTEGER(HID_T) :: file_id
     INTEGER(HID_T) :: plist_id
     INTEGER(HID_T) :: space_id
     INTEGER(HID_T) :: dataset_id
     INTEGER(HSIZE_T), DIMENSION(1) :: cur_size !data space current size
     INTEGER(HSIZE_T), DIMENSION(1) :: max_size !data space maximum size
     CHARACTER(LEN=256) :: name !external file name
     INTEGER :: file_offset !external file offset
     INTEGER(HSIZE_T) :: file_size   !sizeof external file segment
     INTEGER :: error !error code
     INTEGER(SIZE_T) :: int_size !size of integer
     INTEGER(HSIZE_T) :: file_bytes !Number of bytes reserved
                                   !in the file for the data
     INTEGER :: RANK = 1 !dataset rank
     INTEGER :: count !number of external files for the
                      !specified dataset
     INTEGER(SIZE_T) :: namesize
     INTEGER(HSIZE_T) :: size, buf_size
     INTEGER :: idx

     buf_size = 4*1024*1024

     !
     !Create file "external.h5" using default properties.
     !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
     CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
     CALL check("h5fcreate_f",error,total_error)


     CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
         CALL check("h5pcreate_f", error, total_error)
     CALL h5pset_buffer_f(plist_id, buf_size, error)
         CALL check("h5pset_buffer_f", error, total_error)
     CALL h5pget_buffer_f(plist_id, size, error)
         CALL check("h5pget_buffer_f", error, total_error)
     if (size .ne.buf_size) then
         total_error = total_error + 1
         write(*,*) "h5pget_buffer_f returned wrong size, error"
      endif
     CALL h5pclose_f(plist_id, error)
         CALL check("h5pclose_f", error, total_error)

     CALL h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, error)
     CALL check("h5pcreate_f",error,total_error)
     cur_size(1) =100
     max_size(1) = 100;
     call h5tget_size_f(H5T_NATIVE_INTEGER, int_size, error)
     CALL check("h5tget_size_f",error,total_error)
     file_size = int_size * max_size(1);
     CALL h5pset_external_f(plist_id, "ext1.data", 0, file_size, error)
     CALL check("h5pset_external_f",error,total_error)
     CALL h5screate_simple_f(RANK, cur_size, space_id, error, max_size)
     CALL check("h5screate_simple_f", error, total_error)
     CALL h5dcreate_f(file_id, "dset1", H5T_NATIVE_INTEGER, space_id, &
                           dataset_id, error, plist_id)
     CALL check("h5dcreate_f", error, total_error)

     CALL h5dclose_f(dataset_id, error)
     CALL check("h5dclose_f", error, total_error)
     CALL h5pclose_f(plist_id, error)
     CALL check("h5pclose_f", error, total_error)
     CALL h5sclose_f(space_id, error)
     CALL check("h5sclose_f", error, total_error)
     CALL h5fclose_f(file_id, error)

     CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, file_id, error)
     CALL h5dopen_f(file_id, "dset1", dataset_id, error)
     CALL check("h5dopen_f",error,total_error)

     ! Read dataset creation information
     CALL h5dget_create_plist_f(dataset_id, plist_id, error)
     CALL check("h5dget_create_plist_f",error,total_error)
     CALL h5pget_external_count_f(plist_id, count, error)
     CALL check("h5pget_external_count_f",error,total_error)
     if(count .ne. 1 ) then
         write (*,*) "got external_count is not correct"
         total_error = total_error + 1
     end if
     namesize = 10
     idx = 0
     CALL h5pget_external_f(plist_id, idx, namesize, name, file_offset, &
                            file_bytes, error)
     CALL check("h5pget_external_f",error,total_error)
     if(file_offset .ne. 0 ) then
         write (*,*) "got external file offset is not correct"
         total_error = total_error + 1
     end if
     if(file_bytes .ne. file_size ) then
         write (*,*) "got external file size is not correct"
         total_error = total_error + 1
     end if

     CALL h5dclose_f(dataset_id, error)
     CALL check("h5dclose_f", error, total_error)
     CALL h5pclose_f(plist_id, error)
     CALL check("h5pclose_f", error, total_error)
     CALL h5fclose_f(file_id, error)
     CALL check("h5fclose_f", error, total_error)


          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
     RETURN
     END SUBROUTINE external_test

        SUBROUTINE multi_file_test(cleanup, total_error)
        USE HDF5 ! This module contains all necessary modules

          IMPLICIT NONE
          LOGICAL, INTENT(IN) :: cleanup
          INTEGER, INTENT(OUT) :: total_error

          CHARACTER(LEN=9), PARAMETER :: filename = "multidset" ! File name
          CHARACTER(LEN=80) :: fix_filename
          CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"     ! Dataset name

          INTEGER(HID_T) :: file_id       ! File identifier
          INTEGER(HID_T) :: dset_id       ! Dataset identifier
          INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
          INTEGER(HID_T) :: dtype_id      ! Datatype identifier
          INTEGER(HID_T) :: fapl, fapl_1  ! File access property list identifier
          INTEGER, DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: memb_map, memb_map_out
          INTEGER(HID_T), DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: memb_fapl, memb_fapl_out
          CHARACTER(LEN=20), DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: memb_name, memb_name_out
          REAL, DIMENSION(0:H5FD_MEM_NTYPES_F-1) :: memb_addr, memb_addr_out
          !INTEGER(HADDR_T), DIMENSION(0:H5FD_MEM_NTYPES_F) :: memb_addr
          LOGICAL :: relax  = .TRUE.
          LOGICAL :: relax_out = .TRUE.

          INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/4,6/) ! Dataset dimensions
          INTEGER     ::   rank = 2                        ! Dataset rank

          INTEGER, DIMENSION(4,6) :: dset_data, data_out ! Data buffers
          INTEGER     ::   error ! Error flag
          INTEGER(HID_T) :: driver
          INTEGER     :: i, j    !general purpose integers
          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
          INTEGER :: mdc_nelmts
          INTEGER(SIZE_T) :: rdcc_nelmts
          INTEGER(SIZE_T) :: rdcc_nbytes
          REAL :: rdcc_w0
          memb_fapl = H5P_DEFAULT_F
          memb_map = H5FD_MEM_SUPER_F
          memb_addr = 0.
          memb_map(H5FD_MEM_SUPER_F) = H5FD_MEM_SUPER_F
          memb_addr(H5FD_MEM_SUPER_F) = 0.
          memb_map(H5FD_MEM_BTREE_F) = H5FD_MEM_BTREE_F
          memb_addr(H5FD_MEM_BTREE_F) = 0.1
          memb_map(H5FD_MEM_DRAW_F)  = H5FD_MEM_DRAW_F
          memb_addr(H5FD_MEM_DRAW_F) = 0.5
          memb_map(H5FD_MEM_GHEAP_F) = H5FD_MEM_GHEAP_F
          memb_addr(H5FD_MEM_GHEAP_F) = 0.2
          memb_map(H5FD_MEM_LHEAP_F) = H5FD_MEM_LHEAP_F
          memb_addr(H5FD_MEM_LHEAP_F) = 0.3
          memb_map(H5FD_MEM_OHDR_F)  = H5FD_MEM_OHDR_F
          memb_addr(H5FD_MEM_OHDR_F) = 0.4

          memb_name = ' '
          memb_name(H5FD_MEM_SUPER_F) = '%s-s.h5'
          memb_name(H5FD_MEM_BTREE_F) = '%s-b.h5'
          memb_name(H5FD_MEM_DRAW_F)  = '%s-r.h5'
          memb_name(H5FD_MEM_GHEAP_F) = '%s-g.h5'
          memb_name(H5FD_MEM_LHEAP_F) = '%s-l.h5'
          memb_name(H5FD_MEM_OHDR_F)  = '%s-o.h5'

          !
          ! Initialize the dset_data array.
          !
          do i = 1, 4
             do j = 1, 6
                dset_data(i,j) = (i-1)*6 + j;
             end do
          end do

          !
          ! Create a new file using default properties.
          !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
          CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
              CALL check("h5pcreate_f", error, total_error)
          CALL h5pset_fapl_multi_f(fapl, memb_map, memb_fapl, memb_name, memb_addr, relax, error)
              CALL check("h5pset_fapl_multi_f", error, total_error)
          CALL h5pget_fapl_multi_f(fapl, memb_map_out, memb_fapl_out, memb_name_out, &
                                   memb_addr_out, relax_out, error)
              CALL check("h5pget_fapl_multi_f", error, total_error)
          CALL h5pget_driver_f(fapl, driver, error)
              CALL check("h5pget_driver_f",error, total_error)
          if(driver .ne. H5FD_MULTI_F) then
             write(*,*) "Wrong value for driver"
          endif
          !
          ! Let's check h5pget(set)cache_f APIs here for now
          !
          CALL h5pget_cache_f(fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, &
                              rdcc_w0, error)
               CALL check("h5pget_cache_f", error, total_error)

          ! Set cache to some number
          !
          rdcc_nbytes = 1024*1024
          CALL h5pset_cache_f(fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, &
                              rdcc_w0, error)
               CALL check("h5pset_cache_f", error, total_error)

          CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = fapl)
              CALL check("h5fcreate_f", error, total_error)
          if(error .ne. 0) then
             write(*,*) "Cannot create file using multi-file driver... Exiting...."
             total_error = 1
             call h5pclose_f(fapl, error)
             return
          endif


          !
          ! Create the dataspace.
          !
          CALL h5screate_simple_f(rank, dims, dspace_id, error)
              CALL check("h5screate_simple_f", error, total_error)


          !
          ! Create the dataset with default properties.
          !
          CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dspace_id, &
                           dset_id, error)
              CALL check("h5dcreate_f", error, total_error)

          !
          ! Write the dataset.
          !
          data_dims(1) = 4
          data_dims(2) = 6
          CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, dset_data, data_dims, error)
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
          ! Close the file.
          !
          CALL h5fclose_f(file_id, error)
              CALL check("h5fclose_f", error, total_error)
          CALL h5pclose_f(fapl, error)
              CALL check("h5pclose_f", error, total_error)
         !
          ! Open the existing file.
          !
          CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
              CALL check("h5pcreate_f", error, total_error)
          CALL h5pset_fapl_multi_f(fapl, relax, error)
              CALL check("h5pset_fapl_multi_f", error, total_error)
          CALL h5fopen_f (fix_filename, H5F_ACC_RDWR_F, file_id, error, access_prp = fapl)
              CALL check("h5fopen_f", error, total_error)
          !
          CALL h5fget_access_plist_f(file_id, fapl_1, error)
              CALL check("h5fget_access_plist_f", error, total_error)
          !It doesn't work on Windows.
          !CALL h5pget_fapl_multi_f(fapl_1, memb_map_out, memb_fapl_out, memb_name_out, &
          !                         memb_addr_out, relax_out, error)
          ! write(*,*)  memb_map_out
          ! write(*,*)  memb_fapl_out
          ! write(*,*)  memb_name_out
          ! write(*,*)  memb_addr_out
          !    CALL check("h5pget_fapl_multi_f", error, total_error)

          !
          ! Open the existing dataset.
          !
          CALL h5dopen_f(file_id, dsetname, dset_id, error)
              CALL check("h5dopen_f", error, total_error)

          !
          ! Get the dataset type.
          !
          CALL h5dget_type_f(dset_id, dtype_id, error)
              CALL check("h5dget_type_f", error, total_error)

          !
          ! Get the data space.
          !
          CALL h5dget_space_f(dset_id, dspace_id, error)
              CALL check("h5dget_space_f", error, total_error)

          !
          ! Read the dataset.
          !
          CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error)
              CALL check("h5dread_f", error, total_error)

          !
          !Compare the data.
          !
          do i = 1, 4
              do j = 1, 6
                  IF (data_out(i,j) .NE. dset_data(i, j)) THEN
                      write(*, *) "dataset test error occured"
                      write(*,*) "data read is not the same as the data writen"
                  END IF
              end do
          end do

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
          ! Terminate access to the data type.
          !
          CALL h5tclose_f(dtype_id, error)
              CALL check("h5tclose_f", error, total_error)
          !
          ! Close the file.
          !
          CALL h5fclose_f(file_id, error)
          CALL check("h5fclose_f", error, total_error)
          CALL h5pclose_f(fapl, error)
          CALL check("h5pclose_f", error, total_error)
          CALL h5pclose_f(fapl_1, error)
          CALL check("h5pclose_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)

          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-b', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-g', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-l', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-o', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-r', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)
          IF(cleanup) CALL h5_cleanup_f(filename//'.h5-s', H5P_DEFAULT_F, error)
          CALL check("h5_cleanup_f", error, total_error)

          RETURN
        END SUBROUTINE multi_file_test

!-------------------------------------------------------------------------
! Function: test_chunk_cache
!
! Purpose: Tests APIs:
!            H5P_H5PSET_CHUNK_CACHE_F
!            H5P_H5PGET_CHUNK_CACHE_F
!            H5D_H5DGET_ACCESS_PLIST_F
!
! Return:      Success: 0
!              Failure: -1
!
! C Programmer:  Neil Fortner
!                Wednesday, October 29, 2008
!
! FORTRAN Programmer: M. Scot Breitenfeld
!                     April 16, 2009
!-------------------------------------------------------------------------
!
SUBROUTINE test_chunk_cache(cleanup, total_error)

  USE HDF5 ! This module contains all necessary modules

  IMPLICIT NONE
  LOGICAL, INTENT(IN)  :: cleanup
  INTEGER, INTENT(OUT) :: total_error

  CHARACTER(LEN=14), PARAMETER :: filename="chunk_cache"
  CHARACTER(LEN=80) :: fix_filename
  INTEGER(hid_t) :: fid = -1        ! /* File ID */
  INTEGER(hid_t) :: file
  INTEGER(hid_t) :: fapl_local = -1 ! /* Local fapl */
  INTEGER(hid_t) :: fapl_def = -1  ! /* Default fapl */
  INTEGER(hid_t) :: dcpl = -1      !/* Dataset creation property list ID */
  INTEGER(hid_t) :: dapl1 = -1     !/* Dataset access property list ID */
  INTEGER(hid_t) :: dapl2 = -1     !/* Dataset access property list ID */
  INTEGER(hid_t) :: sid = -1       !/* Dataspace ID */
  INTEGER(hid_t) :: dsid = -1      !/* Dataset ID */
  INTEGER(hsize_t), DIMENSION(1:1) :: chunk_dim, NDIM = (/100/) !/* Dataset and chunk dimensions */
  INTEGER(size_t) :: nslots_1, nslots_2, nslots_3, nslots_4 !/* rdcc number of elements */
  INTEGER(size_t) :: nbytes_1, nbytes_2, nbytes_3, nbytes_4 !/* rdcc number of bytes */
  INTEGER :: mdc_nelmts
  INTEGER(size_t) ::nlinks         !/* Number of link traversals */
  REAL :: w0_1, w0_2, w0_3, w0_4; !/* rdcc preemption policy */
  INTEGER :: error
  INTEGER(size_t) rdcc_nelmts
  INTEGER(size_t) rdcc_nbytes
  REAL :: rdcc_w0


  CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
  IF (error .NE. 0) THEN
     WRITE(*,*) "Cannot modify filename"
     STOP
  ENDIF

  !/* Create a default fapl and dapl */
  CALL H5Pcreate_f(H5P_FILE_ACCESS_F, fapl_def, error)
  CALL check("H5Pcreate_f", error, total_error)
  CALL H5Pcreate_f(H5P_DATASET_ACCESS_F, dapl1, error)
  CALL check("H5Pcreate_f", error, total_error)

  !  Verify that H5Pget_chunk_cache(dapl) returns the same values as are in
  !  the default fapl.
  !
  CALL H5Pget_cache_f(fapl_def, mdc_nelmts, nslots_1, nbytes_1, w0_1, error)
  CALL check("H5Pget_cache_f", error, total_error)
  CALL H5Pget_chunk_cache_f(dapl1, nslots_4, nbytes_4, w0_4, error)
  CALL check("H5Pget_chunk_cache_f", error, total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nslots_1), INT(nslots_4), total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nbytes_1), INT(nbytes_4), total_error)
  IF(w0_1.NE.w0_4)THEN
     CALL VERIFYlogical("H5Pget_chunk_cache_f", .TRUE., .FALSE., total_error)
  ENDIF

  ! /* Set a lapl property on dapl1 (to verify inheritance) */
  CALL H5Pset_nlinks_f(dapl1, 134_size_t , error)
  CALL check("H5Pset_nlinks_f", error, total_error)
  CALL H5Pget_nlinks_f(dapl1, nlinks, error)
  CALL check("H5Pget_nlinks_f", error, total_error)
  CALL VERIFY("H5Pget_nlinks_f", INT(nlinks), 134, total_error)


  CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_local, error)
  CALL check("h5pcreate_f", error, total_error)
  ! Turn off the chunk cache, so all the chunks are immediately written to disk
  CALL H5Pget_cache_f(fapl_local, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0, error)
  CALL check("H5Pget_cache_f", error, total_error)
  rdcc_nbytes = 0;
  CALL H5Pset_cache_f(fapl_local, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0, error)
  CALL check("H5Pset_cache_f", error, total_error)

  ! Set new rdcc settings on fapl!
  nslots_2 = nslots_1 * 2
  nbytes_2 = nbytes_1 * 2
  w0_2 = w0_1 / 2.

  CALL H5Pset_cache_f(fapl_local, 0, nslots_2, nbytes_2, w0_2, error)
  CALL check("H5Pset_cache_f", error, total_error)

  !/* Create file */
  CALL H5Fcreate_f(fix_filename, H5F_ACC_TRUNC_F, fid, error,  H5P_DEFAULT_F, fapl_local)
  CALL check("H5Fcreate_f", error, total_error)

  !/* Create dataset creation property list */
  CALL H5Pcreate_f(H5P_DATASET_CREATE_F, dcpl, error)
  CALL check("H5Pcreate_f", error, total_error)

  !/* Set chunking */
  chunk_dim(1) = 10;
  CALL H5Pset_chunk_f(dcpl, 1, chunk_dim, error)
  CALL check("H5Pset_chunk_f", error, total_error)

  !/* Create 1-D dataspace */
  ndim(1) = 100
  CALL H5Screate_simple_f(1, ndim, sid, error)
  CALL check("H5Pcreate_f", error, total_error)

  ! /* Create dataset with default dapl */
  CALL H5Dcreate_f(fid, "dset", H5T_NATIVE_INTEGER, sid, dsid, error,  dcpl, H5P_DEFAULT_F,  dapl1)
  CALL check("H5Pcreate_f", error, total_error)

  ! /* Retrieve dapl from dataset, verify cache values are the same as on fapl_local */
  CALL H5Dget_access_plist_f(dsid, dapl2, error)
  CALL check("H5Dget_access_plist_f", error, total_error)
  CALL H5Pget_chunk_cache_f(dapl2, nslots_4, nbytes_4, w0_4, error)
  CALL check("H5Pget_chunk_cache_f", error, total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nslots_2), INT(nslots_4), total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nbytes_2), INT(nbytes_4), total_error)
  IF(w0_2.NE.w0_4)THEN
     CALL VERIFYlogical("H5Pget_chunk_cache_f", .TRUE., .FALSE., total_error)
  ENDIF
  CALL H5Pclose_f(dapl2,error); CALL check("H5Pclose_f", error, total_error)

  ! Set new values on dapl1.  nbytes will be set to default, so the file
  ! property will override this setting

  nslots_3 = nslots_2 * 2
  nbytes_3 = H5D_CHUNK_CACHE_NBYTES_DFLT_F
  w0_3 = w0_2 / 2

  CALL H5Pset_chunk_cache_f(dapl1, nslots_3, nbytes_3, w0_3, error)
  CALL check("H5Pset_chunk_cache_f", error, total_error)

  ! Close dataset, reopen with dapl1.  Note the use of a dapl with H5Oopen */
  CALL H5Dclose_f(dsid, error)
  CALL H5Oopen_f(fid, "dset", dsid, error, dapl1)

  ! Retrieve dapl from dataset, verfiy cache values are the same as on dapl1
  !
  ! Note we rely on the knowledge that H5Pget_chunk_cache retrieves these
  ! values directly from the dataset structure, and not from a copy of the
  ! dapl used to open the dataset (which is not preserved).
  !
  CALL H5Dget_access_plist_f(dsid, dapl2, error)
  CALL check("H5Dget_access_plist_f", error, total_error)
  CALL H5Pget_chunk_cache_f(dapl2, nslots_4, nbytes_4, w0_4, error)
  CALL check("H5Pget_chunk_cache_f", error, total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nslots_3), INT(nslots_4), total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nbytes_2), INT(nbytes_4), total_error)
  IF(w0_3.NE.w0_4)THEN
     CALL VERIFYlogical("H5Pget_chunk_cache_f4", .TRUE., .FALSE., total_error)
  ENDIF
  CALL H5Pclose_f(dapl2,error); CALL check("H5Pclose_f", error, total_error)

  ! Close dataset, reopen with H5P_DEFAULT as dapl
  CALL H5Dclose_f(dsid, error); CALL check("H5Dclose_f", error, total_error)
  CALL H5Oopen_f(fid, "dset", dsid, error)
  CALL check("H5Oopen_f", error, total_error)

  ! Retrieve dapl from dataset, verfiy cache values are the same as on fapl_local

  CALL H5Dget_access_plist_f(dsid, dapl2, error)
  CALL check("H5Dget_access_plist_f", error, total_error)
  CALL H5Pget_chunk_cache_f(dapl2, nslots_4, nbytes_4, w0_4, error)
  CALL check("H5Pget_chunk_cache_f", error, total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nslots_2), INT(nslots_4), total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nbytes_2), INT(nbytes_4), total_error)
  IF(w0_2.NE.w0_4)THEN
     CALL VERIFYlogical("H5Pget_chunk_cache_f", .TRUE., .FALSE., total_error)
  ENDIF
  CALL H5Pclose_f(dapl2,error); CALL check("H5Pclose_f", error, total_error)

  ! Similary, test use of H5Dcreate2 with H5P_DEFAULT
  CALL H5Dclose_f(dsid, error); CALL check("H5Dclose_f", error, total_error)

  CALL H5Dcreate_f(fid, "dset2", H5T_NATIVE_INTEGER, sid, dsid, error,  dcpl, H5P_DEFAULT_F,  H5P_DEFAULT_F)
  CALL check("H5Pcreate_f", error, total_error)

  CALL H5Dget_access_plist_f(dsid, dapl2, error)
  CALL check("H5Dget_access_plist_f", error, total_error)

  CALL H5Pget_chunk_cache_f(dapl2, nslots_4, nbytes_4, w0_4, error)
  CALL check("H5Pget_chunk_cache_f", error, total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nslots_2), INT(nslots_4), total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nbytes_2), INT(nbytes_4), total_error)
  IF(w0_2.NE.w0_4)THEN
     CALL VERIFYlogical("H5Pget_chunk_cache_f", .TRUE., .FALSE., total_error)
  ENDIF
  ! Don't close dapl2, we will use it in the next section

  ! Modify cache values on fapl_local
  nbytes_3 = nbytes_2 * 2

  CALL H5Pset_cache_f(fapl_local, 0, nslots_3, nbytes_3, w0_3, error)
  CALL check("H5Pset_cache_f", error, total_error)

  !  Close and reopen file with new fapl_local

  CALL H5Dclose_f(dsid, error); CALL check("H5Dclose_f", error, total_error)
  CALL H5Fclose_f(fid,error); CALL check("h5fclose_f", error, total_error)

  CALL H5Fopen_f (fix_filename, H5F_ACC_RDWR_F, fid, error, fapl_local)
  CALL check("h5fopen_f", error, total_error)

  ! Verify that dapl2 retrieved earlier (using values from the old fapl)
  ! sets its values in the new file (test use of H5Dopen2 with a dapl)
  !

  CALL h5dopen_f (fid, "dset", dsid, error, dapl2)
  CALL check("h5dopen_f", error, total_error)

  CALL H5Pclose_f(dapl2,error); CALL check("H5Pclose_f", error, total_error) ! Close dapl2, to avoid id leak

  CALL H5Dget_access_plist_f(dsid, dapl2, error)
  CALL check("H5Dget_access_plist_f", error, total_error)
  CALL H5Pget_chunk_cache_f(dapl2, nslots_4, nbytes_4, w0_4, error)
  CALL check("H5Pget_chunk_cache_f", error, total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nslots_2), INT(nslots_4), total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nbytes_2), INT(nbytes_4), total_error)
  IF(w0_2.NE.w0_4)THEN
     CALL VERIFYlogical("H5Pget_chunk_cache_f", .TRUE., .FALSE., total_error)
  ENDIF

  ! Test H5D_CHUNK_CACHE_NSLOTS_DEFAULT and H5D_CHUNK_CACHE_W0_DEFAULT
  nslots_2 = H5D_CHUNK_CACHE_NSLOTS_DFLT_F
  w0_2 = H5D_CHUNK_CACHE_W0_DFLT_F

  CALL H5Pset_chunk_cache_f(dapl2, nslots_2, nbytes_2, w0_2, error)
  CALL check("H5Pset_chunk_cache_f", error, total_error)

  CALL H5Dclose_f(dsid, error); CALL check("H5Dclose_f", error, total_error)
  CALL h5dopen_f (fid, "dset", dsid, error, dapl2)
  CALL check("h5dopen_f", error, total_error)

  CALL H5Pclose_f(dapl2,error); CALL check("H5Pclose_f", error, total_error)

  CALL H5Dget_access_plist_f(dsid, dapl2, error)
  CALL check("H5Dget_access_plist_f", error, total_error)
  CALL H5Pget_chunk_cache_f(dapl2, nslots_4, nbytes_4, w0_4, error)
  CALL check("H5Pget_chunk_cache_f", error, total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nslots_3), INT(nslots_4), total_error)
  CALL VERIFY("H5Pget_chunk_cache_f", INT(nbytes_2), INT(nbytes_4), total_error)
  IF(w0_3.NE.w0_4)THEN
     CALL VERIFYlogical("H5Pget_chunk_cache_f", .TRUE., .FALSE., total_error)
  ENDIF

! Close

  CALL H5Dclose_f(dsid, error); CALL check("H5Dclose_f", error, total_error)
  CALL H5Sclose_f(sid,error); CALL check("H5Sclose_f", error, total_error)
  CALL H5Pclose_f(fapl_local,error); CALL check("H5Pclose_f", error, total_error)
  CALL H5Pclose_f(fapl_def,error); CALL check("H5Pclose_f", error, total_error)
  CALL H5Pclose_f(dapl1,error); CALL check("H5Pclose_f", error, total_error)
  CALL H5Pclose_f(dapl2,error); CALL check("H5Pclose_f", error, total_error)
  CALL H5Pclose_f(dcpl,error); CALL check("H5Pclose_f", error, total_error)
  CALL H5Fclose_f(fid,error); CALL check("H5Fclose_f", error, total_error)

  IF(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
  CALL check("h5_cleanup_f", error, total_error)

END SUBROUTINE test_chunk_cache

