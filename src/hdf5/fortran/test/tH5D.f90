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
!    Testing Dataset Interface functionality.
!
!
!    The following subroutine tests the following functionalities:
!    h5dcreate_f, h5dopen_f, h5dclose_f, h5dget_space_f, h5dget_type_f,
!    h5dread_f, and h5dwrite_f
!
        SUBROUTINE datasettest(cleanup, total_error)
        USE HDF5 ! This module contains all necessary modules

          IMPLICIT NONE
          LOGICAL, INTENT(IN) :: cleanup
          INTEGER, INTENT(OUT) :: total_error

          CHARACTER(LEN=5), PARAMETER :: filename = "dsetf" ! File name
          CHARACTER(LEN=80) :: fix_filename
          CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"     ! Dataset name
          CHARACTER(LEN=9), PARAMETER :: null_dsetname = "null_dset"     ! Dataset name

          INTEGER(HID_T) :: file_id       ! File identifier
          INTEGER(HID_T) :: dset_id       ! Dataset identifier
          INTEGER(HID_T) :: null_dset     ! Null dataset identifier
          INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
          INTEGER(HID_T) :: null_dspace   ! Null dataspace identifier
          INTEGER(HID_T) :: dtype_id      ! Datatype identifier


          INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/4,6/) ! Dataset dimensions
          INTEGER     ::   rank = 2                        ! Dataset rank

          INTEGER, DIMENSION(4,6) :: dset_data, data_out ! Data buffers
          INTEGER     ::   error ! Error flag

          INTEGER     :: i, j    !general purpose integers
          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
          INTEGER(HSIZE_T), DIMENSION(1) :: null_data_dim
          INTEGER     ::   null_dset_data = 1              ! null data

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
          CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
              CALL check("h5fcreate_f", error, total_error)


          !
          ! Create the dataspace.
          !
          CALL h5screate_simple_f(rank, dims, dspace_id, error)
              CALL check("h5screate_simple_f", error, total_error)
          !
          ! Create null dataspace.
          !
          CALL h5screate_f(H5S_NULL_F, null_dspace, error)
              CALL check("h5screate_simple_f", error, total_error)


          !
          ! Create the dataset with default properties.
          !
          CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dspace_id, &
                           dset_id, error)
              CALL check("h5dcreate_f", error, total_error)
          !
          ! Create the null dataset.
          !
          CALL h5dcreate_f(file_id, null_dsetname, H5T_NATIVE_INTEGER, null_dspace, &
                           null_dset, error)
              CALL check("h5dcreate_f", error, total_error)

          !
          ! Write the dataset.
          !
          data_dims(1) = 4
          data_dims(2) = 6
          CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, dset_data, data_dims, error)
              CALL check("h5dwrite_f", error, total_error)
          !
          ! Write null dataset.  Nothing can be written.
          !
          null_data_dim(1) = 1
          CALL h5dwrite_f(null_dset, H5T_NATIVE_INTEGER, null_dset_data, null_data_dim, error)
              CALL check("h5dwrite_f", error, total_error)


          !
          ! End access to the dataset and release resources used by it.
          !
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f", error, total_error)
          CALL h5dclose_f(null_dset, error)
              CALL check("h5dclose_f", error, total_error)

          !
          ! Terminate access to the data space.
          !
          CALL h5sclose_f(dspace_id, error)
              CALL check("h5sclose_f", error, total_error)
          CALL h5sclose_f(null_dspace, error)
              CALL check("h5sclose_f", error, total_error)

          !
          ! Close the file.
          !
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
          CALL h5dopen_f(file_id, null_dsetname, null_dset, error)
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
          ! Read the null dataset.  Nothing should be read.
          !
          CALL h5dread_f(null_dset, H5T_NATIVE_INTEGER, null_dset_data, null_data_dim, error)
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
          ! Check if no change to null_dset_data
          !
          IF (null_dset_data .NE. 1) THEN
              write(*, *) "null dataset test error occured"
          END IF

          !
          ! End access to the dataset and release resources used by it.
          !
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f", error, total_error)
          CALL h5dclose_f(null_dset, error)
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

          RETURN
        END SUBROUTINE datasettest

!
!the following subroutine tests h5dextend_f functionality
!

        SUBROUTINE extenddsettest(cleanup, total_error)
        USE HDF5 ! This module contains all necessary modules

          IMPLICIT NONE
          LOGICAL, INTENT(IN)  :: cleanup
          INTEGER, INTENT(OUT) :: total_error

          !
          !the dataset is stored in file "extf.h5"
          !
          CHARACTER(LEN=4), PARAMETER :: filename = "extf"
          CHARACTER(LEN=80) :: fix_filename

          !
          !dataset name is "ExtendibleArray"
          !
          CHARACTER(LEN=15), PARAMETER :: dsetname = "ExtendibleArray"

          !
          !dataset rank is 2
          !
          INTEGER :: RANK = 2

          INTEGER(HID_T) :: file_id       ! File identifier
          INTEGER(HID_T) :: dset_id       ! Dataset identifier
          INTEGER(HID_T) :: dataspace     ! Dataspace identifier
          INTEGER(HID_T) :: memspace      ! memory Dataspace identifier
          INTEGER(HID_T) :: crp_list        ! dataset creatation property identifier

          !
          !dataset dimensions at creation time
          !
          INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/3,3/)

          !
          !data dimensions
          !
          INTEGER(HSIZE_T), DIMENSION(2) :: dims1 = (/10,3/)

          !
          !Maximum dimensions
          !
          INTEGER(HSIZE_T), DIMENSION(2) :: maxdims

          !
          !data arrays for reading and writing
          !
          INTEGER, DIMENSION(10,3) :: data_in, data_out

          !
          !Size of data in the file
          !
          INTEGER(HSIZE_T), DIMENSION(2) :: size

          !
          !general purpose integer
          !
          INTEGER :: i, j

          !
          !flag to check operation success
          !
          INTEGER :: error

          !
          !Variables used in reading data back
          !
          INTEGER(HSIZE_T), DIMENSION(2) :: dimsr, maxdimsr
          INTEGER :: rankr
          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims

          !
          !data initialization
          !
          do i = 1, 10
             do j = 1, 3
                data_in(i,j) = 2
             end do
          end do

          !
          !Initialize FORTRAN predifined datatypes
          !
!          CALL h5init_types_f(error)
!               CALL check("h5init_types_f",error,total_error)

          !
          !Create a new file using default properties.
          !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
          CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
               CALL check("h5fcreate_f",error,total_error)


          !
          !Create the data space with unlimited dimensions.
          !
          maxdims = (/H5S_UNLIMITED_F, H5S_UNLIMITED_F/)

          CALL h5screate_simple_f(RANK, dims, dataspace, error, maxdims)
               CALL check("h5screate_simple_f",error,total_error)

          !
          !Modify dataset creation properties, i.e. enable chunking
          !
          CALL h5pcreate_f(H5P_DATASET_CREATE_F, crp_list, error)
              CALL check("h5pcreat_f",error,total_error)

          CALL h5pset_chunk_f(crp_list, RANK, dims1, error)
              CALL check("h5pset_chunk_f",error,total_error)

          !
          !Create a dataset with 3X3 dimensions using cparms creation propertie .
          !
          CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
                           dset_id, error, crp_list )
              CALL check("h5dcreate_f",error,total_error)

          !
          !Extend the dataset. This call assures that dataset is 3 x 3.
          !
          size(1) = 3
          size(2) = 3
          CALL h5dextend_f(dset_id, size, error)
              CALL check("h5dextend_f",error,total_error)


          !
          !Extend the dataset. Dataset becomes 10 x 3.
          !
          size(1)   = 10;
          size(2)   = 3;
          CALL h5dextend_f(dset_id, size, error)
              CALL check("h5dextend_f",error,total_error)

          !
          !Write the data of size 10X3 to the extended dataset.
          !
          data_dims(1) = 10
          data_dims(2) = 3
          CALL H5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data_in, data_dims, error)
               CALL check("h5dwrite_f",error,total_error)

          !
          !Close the dataspace for the dataset.
          !
          CALL h5sclose_f(dataspace, error)
              CALL check("h5sclose_f",error,total_error)

          !
          !Close the property list.
          !
          CALL h5pclose_f(crp_list, error)
              CALL check("h5pclose_f",error,total_error)
          !
          !Close the dataset.
          !
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f",error,total_error)

          !
          !Close the file.
          !
          CALL h5fclose_f(file_id, error)
              CALL check("h5fclose_f",error,total_error)

          !
          !read the data back
          !
          !Open the file.
          !
          CALL h5fopen_f (fix_filename, H5F_ACC_RDONLY_F, file_id, error)
              CALL check("hfopen_f",error,total_error)

          !
          !Open the  dataset.
          !
          CALL h5dopen_f(file_id, dsetname, dset_id, error)
              CALL check("h5dopen_f",error,total_error)

          !
          !Get dataset's dataspace handle.
          !
          CALL h5dget_space_f(dset_id, dataspace, error)
              CALL check("h5dget_space_f",error,total_error)

          !
          !Get dataspace's rank.
          !
          CALL h5sget_simple_extent_ndims_f(dataspace, rankr, error)
              CALL check("h5sget_simple_extent_ndims_f",error,total_error)
          IF (rankr .NE. RANK) then
              write(*,*) "dataset rank error occured"
              stop
          END IF

          !
          !Get dataspace's dimensinons.
          !
          CALL h5sget_simple_extent_dims_f(dataspace, dimsr, maxdimsr, error)
              CALL check("h5sget_simple_extent_dims_f",error,total_error)
          IF ((dimsr(1) .NE. dims1(1)) .OR. (dimsr(2) .NE. dims1(2))) THEN
              write(*,*) "dataset dimensions error occured"
              stop
          END IF

          !
          !Get creation property list.
          !
          CALL h5dget_create_plist_f(dset_id, crp_list, error)
              CALL check("h5dget_create_plist_f",error,total_error)


          !
          !create memory dataspace.
          !
          CALL h5screate_simple_f(rankr, dimsr, memspace, error)
              CALL check("h5screate_simple_f",error,total_error)

          !
          !Read data
          !
          CALL H5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error, &
                    memspace, dataspace)
              CALL check("h5dread_f",error,total_error)


          !
          !Compare the data.
          !
          do i = 1, dims1(1)
              do j = 1, dims1(2)
                  IF (data_out(i,j) .NE. data_in(i, j)) THEN
                      write(*, *) "extend dataset test error occured"
                      write(*, *) "read value is not the same as the written values"
                  END IF
              end do
          end do

          !
          !Close the dataspace for the dataset.
          !
          CALL h5sclose_f(dataspace, error)
              CALL check("h5sclose_f",error,total_error)

          !
          !Close the memspace for the dataset.
          !
          CALL h5sclose_f(memspace, error)
              CALL check("h5sclose_f",error,total_error)

          !
          !Close the property list.
          !
          CALL h5pclose_f(crp_list, error)
              CALL check("h5pclose_f",error,total_error)

          !
          !Close the dataset.
          !
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f",error,total_error)

          !
          !Close the file.
          !
          CALL h5fclose_f(file_id, error)
              CALL check("h5fclose_f",error,total_error)
          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)

          RETURN
        END SUBROUTINE extenddsettest


