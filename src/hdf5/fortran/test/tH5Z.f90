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
    SUBROUTINE filters_test(cleanup, total_error)

!   This subroutine tests following functionalities: h5zfilter_avail_f, h5zunregister_f

   USE HDF5 ! This module contains all necessary modules

     IMPLICIT NONE
     LOGICAL, INTENT(IN)  :: cleanup
     INTEGER, INTENT(OUT) :: total_error
     LOGICAL :: status
     INTEGER(HID_T)    :: crtpr_id, xfer_id
     INTEGER           :: nfilters
     INTEGER           :: error
     INTEGER(HSIZE_T)  :: ch_dims(2)
     INTEGER           :: RANK = 2
     INTEGER           :: dlevel = 6
     INTEGER           :: edc_flag

     ch_dims(1) = 10
     ch_dims(2) = 3
!
! Deflate filter
!
     CALL h5zfilter_avail_f(H5Z_FILTER_DEFLATE_F, status, error)
              CALL check("h5zfilter_avail_f", error, total_error)
     if(status) then
        CALL h5pcreate_f(H5P_DATASET_CREATE_F, crtpr_id, error)
              CALL check("h5pcreate_f", error, total_error)
        CALL h5pset_chunk_f(crtpr_id, RANK, ch_dims, error)
              CALL check("h5pset_chunk_f",error, total_error)
        CALL h5pset_deflate_f(crtpr_id, dlevel, error)
              CALL check("h5pset_deflate_f", error, total_error)
        CALL h5pclose_f(crtpr_id,error)
              CALL check("h5pclose_f", error, total_error)
     endif

!
! Shuffle filter
!
     CALL h5zfilter_avail_f(H5Z_FILTER_SHUFFLE_F, status, error)
              CALL check("h5zfilter_avail_f", error, total_error)
     if(status) then
        CALL h5pcreate_f(H5P_DATASET_CREATE_F, crtpr_id, error)
              CALL check("h5pcreate_f", error, total_error)
        CALL h5pset_chunk_f(crtpr_id, RANK, ch_dims, error)
              CALL check("h5pset_chunk_f",error, total_error)
        CALL h5pset_shuffle_f(crtpr_id, error)
              CALL check("h5pset_shuffle_f", error, total_error)
        CALL h5pclose_f(crtpr_id,error)
              CALL check("h5pclose_f", error, total_error)
     endif

!
! Checksum filter
!
     CALL h5zfilter_avail_f(H5Z_FILTER_FLETCHER32_F, status, error)
              CALL check("h5zfilter_avail_f", error, total_error)
     if(status) then
        CALL h5pcreate_f(H5P_DATASET_CREATE_F, crtpr_id, error)
              CALL check("h5pcreate_f", error, total_error)
        CALL h5pset_chunk_f(crtpr_id, RANK, ch_dims, error)
              CALL check("h5pset_chunk_f",error, total_error)
        CALL h5pset_fletcher32_f(crtpr_id, error)
              CALL check("h5pset_fletcher32_f", error, total_error)
        CALL h5pclose_f(crtpr_id,error)
              CALL check("h5pclose_f", error, total_error)
        CALL h5pcreate_f(H5P_DATASET_XFER_F, xfer_id, error)
              CALL check("h5pcreate_f", error, total_error)
        CALL h5pset_edc_check_f( xfer_id, H5Z_DISABLE_EDC_F, error)
              CALL check("h5pset_edc_check_f", error, total_error)
        CALL h5pget_edc_check_f( xfer_id, edc_flag, error)
              CALL check("h5pget_edc_check_f", error, total_error)
        if (edc_flag .ne. H5Z_DISABLE_EDC_F) then
              write(*,*) "EDC status is wrong"
              total_error = total_error + 1
        endif
        CALL h5pclose_f(xfer_id, error)
              CALL check("h5pclose_f", error, total_error)

     endif

!
! Verify h5premove_filter_f
!
     CALL h5zfilter_avail_f(H5Z_FILTER_FLETCHER32_F, status, error)
              CALL check("h5zfilter_avail_f", error, total_error)
     if(status) then
         CALL h5zfilter_avail_f(H5Z_FILTER_SHUFFLE_F, status, error)
                  CALL check("h5zfilter_avail_f", error, total_error)
         if(status) then
            CALL h5pcreate_f(H5P_DATASET_CREATE_F, crtpr_id, error)
                  CALL check("h5pcreate_f", error, total_error)
            CALL h5pset_fletcher32_f(crtpr_id, error)
                  CALL check("h5pset_fletcher32_f", error, total_error)
            CALL h5pset_shuffle_f(crtpr_id, error)
                  CALL check("h5pset_shuffle_f", error, total_error)
            CALL h5pget_nfilters_f(crtpr_id, nfilters, error)
                  CALL check("h5pget_nfilters_f", error, total_error)

            ! Verify the correct number of filters
            if (nfilters .ne. 2) then
                  write(*,*) "number of filters is wrong"
                  total_error = total_error + 1
            endif

            ! Delete a single filter
            CALL h5premove_filter_f(crtpr_id, H5Z_FILTER_SHUFFLE_F, error)
                  CALL check("h5pset_shuffle_f", error, total_error)

            ! Verify the correct number of filters now
            CALL h5pget_nfilters_f(crtpr_id, nfilters, error)
                  CALL check("h5pget_nfilters_f", error, total_error)
            if (nfilters .ne. 1) then
                  write(*,*) "number of filters is wrong"
                  total_error = total_error + 1
            endif

            ! Delete all filters
            CALL h5premove_filter_f(crtpr_id, H5Z_FILTER_ALL_F, error)
                  CALL check("h5premove_filter_f", error, total_error)

            ! Verify the correct number of filters now
            CALL h5pget_nfilters_f(crtpr_id, nfilters, error)
                  CALL check("h5pget_nfilters_f", error, total_error)
            if (nfilters .ne. 0) then
                  write(*,*) "number of filters is wrong"
                  total_error = total_error + 1
            endif
            CALL h5pclose_f(crtpr_id,error)
                  CALL check("h5pclose_f", error, total_error)
         endif
     endif

     RETURN
     END SUBROUTINE filters_test

        SUBROUTINE szip_test(szip_flag, cleanup, total_error)
        USE HDF5 ! This module contains all necessary modules

          IMPLICIT NONE
          LOGICAL, INTENT(OUT) :: szip_flag
          LOGICAL, INTENT(IN) :: cleanup
          INTEGER, INTENT(OUT) :: total_error


          CHARACTER(LEN=4), PARAMETER :: filename = "szip" ! File name
          CHARACTER(LEN=80) :: fix_filename
          CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"     ! Dataset name
          INTEGER, PARAMETER :: N = 1024
          INTEGER, PARAMETER :: NN = 64
          INTEGER, PARAMETER :: M = 512
          INTEGER, PARAMETER :: MM = 32

          INTEGER(HID_T) :: file_id       ! File identifier
          INTEGER(HID_T) :: dset_id       ! Dataset identifier
          INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
          INTEGER(HID_T) :: dtype_id      ! Datatype identifier


          INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/N,M/) ! Dataset dimensions
          INTEGER(HSIZE_T), DIMENSION(2) :: chunk_dims = (/NN, MM/)
          INTEGER     ::   rank = 2                        ! Dataset rank

          INTEGER, DIMENSION(N,M) :: dset_data, data_out ! Data buffers
          INTEGER     ::   error ! Error flag
          INTEGER     ::   num_errors = 0 ! Number of data errors

          INTEGER     :: i, j    !general purpose integers
          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
          INTEGER(HID_T) ::  crp_list
          INTEGER :: options_mask, pix_per_block
          LOGICAL :: flag
          CHARACTER(LEN=4) filter_name

          INTEGER :: filter_flag = -1
          INTEGER(SIZE_T) :: cd_nelemnts = 4
          INTEGER(SIZE_T) :: filter_name_len = 4
          INTEGER, DIMENSION(4) :: cd_values
          INTEGER     :: config_flag = 0   ! for h5zget_filter_info_f
          INTEGER     :: config_flag_both = 0   ! for h5zget_filter_info_f

          !
          ! Verify that SZIP exists and has an encoder
          !
          CALL h5zfilter_avail_f(H5Z_FILTER_SZIP_F, szip_flag, error)
              CALL check("h5zfilter_avail", error, total_error)

          ! Quit if failed
          if (error .ne. 0) return

          ! Skip if no SZIP available
          if (.NOT. szip_flag)then
              return

          else  !SZIP available

          ! Continue
          CALL h5zget_filter_info_f(H5Z_FILTER_SZIP_F, config_flag, error)
              CALL check("h5zget_filter_info_f", error, total_error)
          ! Quit if failed
          if (error .ne. 0) return
          !
          ! Make sure h5zget_filter_info_f returns the right flag
          !
          config_flag_both=IOR(H5Z_FILTER_ENCODE_ENABLED_F,H5Z_FILTER_DECODE_ENABLED_F)
          if( szip_flag ) then
              if (config_flag .NE. config_flag_both) then
                  if(config_flag .NE. H5Z_FILTER_DECODE_ENABLED_F)  then
                     error = -1
                     CALL check("h5zget_filter_info_f config_flag", error, total_error)
                  endif
              endif
          endif

          ! Continue only when encoder is available
          if ( IAND(config_flag,  H5Z_FILTER_ENCODE_ENABLED_F) .EQ. 0 ) return

          options_mask = H5_SZIP_NN_OM_F
          pix_per_block = 32
          !
          ! Initialize the dset_data array.
          !
          do i = 1, N
             do j = 1, M
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
          CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
              CALL check("h5fcreate_f", error, total_error)


          !
          ! Create the dataspace.
          !
          CALL h5screate_simple_f(rank, dims, dspace_id, error)
              CALL check("h5screate_simple_f", error, total_error)

          CALL h5pcreate_f(H5P_DATASET_CREATE_F, crp_list, error)
              CALL check("h5pcreat_f",error,total_error)

          CALL h5pset_chunk_f(crp_list, rank, chunk_dims, error)
              CALL check("h5pset_chunk_f",error,total_error)
          CALL h5pset_szip_f(crp_list, options_mask, pix_per_block, error)
              CALL check("h5pset_szip_f",error,total_error)
          CALL h5pall_filters_avail_f(crp_list, flag, error)
              CALL check("h5pall_filters_avail_f",error,total_error)
          if (.NOT. flag) then
             CALL h5pclose_f(crp_list, error)
             CALL h5sclose_f(dspace_id, error)
             CALL h5fclose_f(file_id, error)
             szip_flag = .FALSE.
             total_error = -1
             return
          endif

         CALL h5pget_filter_by_id_f(crp_list, H5Z_FILTER_SZIP_F, filter_flag, &

                                    cd_nelemnts, cd_values,&

                                    filter_name_len, filter_name, error)
               CALL check("h5pget_filter_by_id_f",error,total_error)
          !
          ! Create the dataset with default properties.
          !
          CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dspace_id, &
                           dset_id, error, crp_list)
              CALL check("h5dcreate_f", error, total_error)

          !
          ! Write the dataset.
          !
          data_dims(1) = N
          data_dims(2) =  M
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
             CALL h5pclose_f(crp_list, error)
          CALL h5fclose_f(file_id, error)
              CALL check("h5fclose_f", error, total_error)

          !
          ! Open the existing file.
          !
          CALL h5fopen_f (fix_filename, H5F_ACC_RDWR_F, file_id, error)
              CALL check("h5fopen_f", error, total_error)

          !
          ! Open the existing dataset.
          !
          CALL h5dopen_f(file_id, dsetname, dset_id, error)
              CALL check("h5dopen_f", error, total_error)
               CALL check("h5pget_filter_by_id_f",error,total_error)

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
          CALL h5dread_f (dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error)
              CALL check("h5dread_f", error, total_error)

          !
          !Compare the data.
          !
          do i = 1, N
              do j = 1, M
                  IF (data_out(i,j) .NE. dset_data(i, j)) THEN
                      write(*, *) "dataset test error occured"
                      write(*,*) "data read is not the same as the data written"
                      num_errors = num_errors + 1
                      IF (num_errors .GE. 512) THEN
                        write(*, *) "maximum data errors reached"
                        goto 100
                      END IF
                  END IF
              end do
          end do
100       IF (num_errors .GT. 0) THEN
            total_error=total_error + 1
          END IF

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
          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          endif ! SZIP available

          RETURN
        END SUBROUTINE szip_test
