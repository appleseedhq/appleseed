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
!    Testing Dataspace Interface functionality.
!
!
!   The following subroutine tests the following functionalities:
!   h5screate_f, h5scopy_f, h5screate_simple_f, h5sis_simple_f,
!   h5sget_simple_extent_dims_f,h5sget_simple_extent_ndims_f
!   h5sget_simple_extent_npoints_f, h5sget_simple_extent_type_f,
!   h5sextent_copy_f, h5sset_extent_simple_f, h5sset_extent_none_f
!
        SUBROUTINE dataspace_basic_test(cleanup, total_error)

        USE HDF5 ! This module contains all necessary modules

          IMPLICIT NONE
          LOGICAL, INTENT(IN)  :: cleanup
          INTEGER, INTENT(OUT) :: total_error

          CHARACTER(LEN=10), PARAMETER :: filename1 = "basicspace" ! File1 name
          CHARACTER(LEN=9), PARAMETER :: filename2 = "copyspace"  ! File2 name
          CHARACTER(LEN=80) :: fix_filename1
          CHARACTER(LEN=80) :: fix_filename2
          CHARACTER(LEN=9), PARAMETER :: dsetname = "basicdset"       ! Dataset name

          INTEGER(HID_T) :: file1_id, file2_id     ! File identifiers
          INTEGER(HID_T) :: dset1_id, dset2_id     ! Dataset identifiers
          INTEGER(HID_T) :: space1_id, space2_id   ! Dataspace identifiers

          INTEGER(HSIZE_T), DIMENSION(2) :: dims1 = (/4,6/) ! Dataset dimensions
          INTEGER(HSIZE_T), DIMENSION(2) :: maxdims1 = (/4,6/) ! maximum dimensions
          INTEGER(HSIZE_T), DIMENSION(2) :: dims2 = (/6,6/) ! Dataset dimensions
          INTEGER(HSIZE_T), DIMENSION(2) :: maxdims2 = (/6,6/) ! maximum dimensions
          INTEGER(HSIZE_T), DIMENSION(2) :: dimsout, maxdimsout ! dimensions
          INTEGER(HSIZE_T)   ::   npoints  !number of elements in the dataspace

          INTEGER     ::   rank1 = 2               ! Dataspace1 rank
          INTEGER     ::   rank2 = 2               ! Dataspace2 rank
          INTEGER     ::   classtype               ! Dataspace class type

          INTEGER, DIMENSION(4,6) :: data1_in, data1_out   ! Data input buffers
          INTEGER, DIMENSION(6,6) :: data2_in, data2_out  ! Data output buffers
          INTEGER     ::   error ! Error flag

          LOGICAL     ::   flag  !flag to test datyspace is simple or not
          INTEGER     :: i, j    !general purpose integers
          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims

          !
          ! Initialize the dset_data array.
          !
          do i = 1, 4
             do j = 1, 6
                data1_in(i,j) = (i-1)*6 + j;
             end do
          end do

          do i = 1, 6
             do j = 1, 6
                data2_in(i,j) = i*6 + j;
             end do
          end do

          !
          !  Initialize FORTRAN predefined datatypes.
          !
!          CALL h5init_types_f(error)
!              CALL check("h5init_types_f", error, total_error)

          !
          ! Create new files using default properties.
          !
          CALL h5_fixname_f(filename1, fix_filename1, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
          CALL h5fcreate_f(fix_filename1, H5F_ACC_TRUNC_F, file1_id, error)
              CALL check("h5fcreate_f", error, total_error)

          CALL h5_fixname_f(filename2, fix_filename2, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
          CALL h5fcreate_f(fix_filename2, H5F_ACC_TRUNC_F, file2_id, error)
              CALL check("h5fcreate_f", error, total_error)

          !
          ! Create dataspace for file1.
          !
          CALL h5screate_simple_f(rank1, dims1, space1_id, error, maxdims1)
              CALL check("h5screate_simple_f", error, total_error)
          !
          ! Copy space1_id to space2_id.
          !
          CALL h5scopy_f(space1_id, space2_id, error)
              CALL check("h5scopy_f", error, total_error)

          !
          !Check whether copied space is simple.
          !
          CALL h5sis_simple_f(space2_id, flag, error)
              CALL check("h5sissimple_f", error, total_error)
          IF (.NOT. flag) write(*,*) "dataspace is not simple type"

          !
          !set the copied space to none.
          !
          CALL h5sset_extent_none_f(space2_id, error)
              CALL check("h5sset_extent_none_f", error, total_error)

          !
          !copy the extent of space1_id to space2_id.
          !
          CALL h5sextent_copy_f(space2_id, space1_id, error)
              CALL check("h5sextent_copy_f", error, total_error)

          !
          !get the copied space's dimensions.
          !
          CALL h5sget_simple_extent_dims_f(space2_id, dimsout, maxdimsout, error)
              CALL check("h5sget_simple_extent_dims_f", error, total_error)
          IF ((dimsout(1) .NE. dims1(1)) .OR. (dimsout(2) .NE. dims1(2)) ) THEN
              write(*,*)"error occured, copied dims not same"
          END IF

          !
          !get the copied space's rank.
          !
          CALL h5sget_simple_extent_ndims_f(space2_id, rank2, error)
              CALL check("h5sget_simple_extent_ndims_f", error, total_error)
          IF (rank2 .NE. rank1) write(*,*)"error occured, copied ranks not same"

          !
          !get the copied space's number of elements.
          !
          CALL h5sget_simple_extent_npoints_f(space2_id, npoints, error)
              CALL check("h5sget_simple_extent_npoints_f", error, total_error)
          IF (npoints .NE. 24) write(*,*)"error occured, number of elements not correct"


          !
          !get the copied space's class type.
          !
          CALL h5sget_simple_extent_type_f(space2_id, classtype, error)
              CALL check("h5sget_simple_extent_type_f", error, total_error)
          IF (classtype .NE. 1) write(*,*)"class type not H5S_SIMPLE_f"

          !
          !set the copied space to none before extend the dimensions.
          !
          CALL h5sset_extent_none_f(space2_id, error)
              CALL check("h5sset_extent_none_f", error, total_error)

          !
          !set the copied space to dim2 size. 
          !
          CALL h5sset_extent_simple_f(space2_id, rank2, dims2, maxdims2, error)
              CALL check("h5sset_extent_simple_f", error, total_error)

          !
          !get the copied space's dimensions.
          !
          CALL h5sget_simple_extent_dims_f(space2_id, dimsout, maxdimsout, error)
              CALL check("h5sget_simple_extent_dims_f", error, total_error)
          IF ((dimsout(1) .NE. dims2(1)) .OR. (dimsout(2) .NE. dims2(2)) ) THEN
              write(*,*)"error occured, copied dims not same"
          END IF

          !
          ! Create the datasets with default properties in two files.
          !
          CALL h5dcreate_f(file1_id, dsetname, H5T_NATIVE_INTEGER, space1_id, &
                           dset1_id, error)
              CALL check("h5dcreate_f", error, total_error)

          CALL h5dcreate_f(file2_id, dsetname, H5T_NATIVE_INTEGER, space2_id, &
                           dset2_id, error)
              CALL check("h5dcreate_f", error, total_error)

          !
          ! Write the datasets.
          !
          data_dims(1) = 4
          data_dims(2) = 6
          CALL h5dwrite_f(dset1_id, H5T_NATIVE_INTEGER, data1_in, data_dims, error)
              CALL check("h5dwrite_f", error, total_error)

          data_dims(1) = 6
          data_dims(2) = 6
          CALL h5dwrite_f(dset2_id, H5T_NATIVE_INTEGER, data2_in, data_dims, error)
              CALL check("h5dwrite_f", error, total_error)

          !
          ! Read the first dataset.
          !
          data_dims(1) = 4
          data_dims(2) = 6
          CALL h5dread_f(dset1_id, H5T_NATIVE_INTEGER, data1_out, data_dims, error)
              CALL check("h5dread_f", error, total_error)

          !
          !Compare the data.
          !
          do i = 1, 4
              do j = 1, 6
                  IF (data1_out(i,j) .NE. data1_in(i, j)) THEN
                      write(*, *) "dataset test error occured"
                      write(*,*) "data read is not the same as the data writen"
                  END IF
              end do
          end do


          !
          ! Read the second dataset.
          !
          data_dims(1) = 6
          data_dims(2) = 6
          CALL h5dread_f(dset2_id, H5T_NATIVE_INTEGER, data2_out, data_dims, error)
              CALL check("h5dread_f", error, total_error)

          !
          !Compare the data.
          !
          do i = 1, 6
              do j = 1, 6
                  IF (data2_out(i,j) .NE. data2_in(i, j)) THEN
                      write(*, *) "dataset test error occured"
                      write(*,*) "data read is not the same as the data writen"
                  END IF
              end do
          end do

          !
          !Close the datasets.
          !
          CALL h5dclose_f(dset1_id, error)
              CALL check("h5dclose_f", error, total_error)
          CALL h5dclose_f(dset2_id, error)
              CALL check("h5dclose_f", error, total_error)

          !
          ! Terminate access to the data spaces.
          !
          CALL h5sclose_f(space1_id, error)
              CALL check("h5sclose_f", error, total_error)
          CALL h5sclose_f(space2_id, error)
              CALL check("h5sclose_f", error, total_error)
          !
          ! Close the files.
          !
          CALL h5fclose_f(file1_id, error)
              CALL check("h5fclose_f", error, total_error)
          CALL h5fclose_f(file2_id, error)
              CALL check("h5fclose_f", error, total_error)


          if(cleanup) CALL h5_cleanup_f(filename1, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          if(cleanup) CALL h5_cleanup_f(filename2, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          RETURN
        END SUBROUTINE dataspace_basic_test

