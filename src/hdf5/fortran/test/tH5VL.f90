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
!    Testing Variable_length datatypes
!
!
!
        SUBROUTINE vl_test_integer(cleanup, total_error)
        USE HDF5 ! This module contains all necessary modules

          IMPLICIT NONE
          LOGICAL, INTENT(IN) :: cleanup
          INTEGER, INTENT(OUT) :: total_error

          CHARACTER(LEN=7), PARAMETER :: filename = "VLtypes" ! File name
          CHARACTER(LEN=80) :: fix_filename
          CHARACTER(LEN=5), PARAMETER :: dsetname = "VLint"     ! Dataset name

          INTEGER(HID_T) :: file_id       ! File identifier
          INTEGER(HID_T) :: dset_id       ! Dataset identifier
          INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
          INTEGER(HID_T) :: vltype_id     ! Datatype identifier


          INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/6/) ! Dataset dimensions
          INTEGER(SIZE_T), DIMENSION(6) :: len           ! Elements lengths
          INTEGER(SIZE_T), DIMENSION(6) :: len_out
          INTEGER     ::   rank = 1                      ! Dataset rank

          INTEGER, DIMENSION(5,6) :: vl_int_data ! Data buffers
          INTEGER, DIMENSION(5,6) :: vl_int_data_out ! Data buffers
          INTEGER     ::   error ! Error flag

          INTEGER     :: i, j    !general purpose integers
          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims = (/5,6/)
          INTEGER(SIZE_T)  max_len

          !
          ! Initialize the vl_int_data array.
          !
          do i = 1, 6
             do j = 1, 5
                vl_int_data(j,i) = -100
             end do
          end do

          do i = 2, 6
             do j = 1,  i-1
                vl_int_data(j,i) = i-1
             end do
          end do

           do i = 1,6
              len(i) = i-1
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
          ! Create the dataset with default properties.
          !
          CALL h5tvlen_create_f(H5T_NATIVE_INTEGER, vltype_id, error)
              CALL check("h5dvlen_create_f", error, total_error)

          CALL h5dcreate_f(file_id, dsetname, vltype_id, dspace_id, &
                           dset_id, error)
              CALL check("h5dcreate_f", error, total_error)

          !
          ! Write the dataset.
          !
          CALL h5dwrite_vl_f(dset_id, vltype_id, vl_int_data, data_dims, len, error)
              CALL check("h5dwrite_int_f", error, total_error)


          !
          ! End access to the dataset and release resources used by it.
          !
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f", error, total_error)

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

          CALL h5dvlen_get_max_len_f(dset_id, vltype_id, dspace_id, max_len, error)
              CALL check("h5dvlen_get_max_len_f", error, total_error)
              if(max_len .ne. data_dims(1)) then
                      total_error = total_error + 1
                      write(*,*) "Wrong number of elemets returned by h5dvlen_get_max_len_f"
              endif
          !
          ! Read the dataset.
          !
          CALL h5dread_vl_f(dset_id, vltype_id, vl_int_data_out, data_dims, len_out, &
                            error, mem_space_id = dspace_id, file_space_id = dspace_id)
              CALL check("h5dread_int_f", error, total_error)
              do i = 1, data_dims(2)
              do j = 1, len_out(i)
              if(vl_int_data(j,i) .ne. vl_int_data_out(j,i))  then
                  total_error = total_error + 1
                  write(*,*) "h5dread_vl_f returned incorrect data"
              endif
              enddo
               if (len(i) .ne. len_out(i)) then
                  total_error = total_error + 1
                  write(*,*) "h5dread_vl_f returned incorrect data"
              endif
              enddo


          !
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f", error, total_error)

          CALL h5tclose_f(vltype_id, error)
              CALL check("h5tclose_f", error, total_error)

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
          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)

          RETURN
        END SUBROUTINE vl_test_integer

        SUBROUTINE vl_test_real(cleanup, total_error)
        USE HDF5 ! This module contains all necessary modules

          IMPLICIT NONE
          LOGICAL, INTENT(IN) :: cleanup
          INTEGER, INTENT(OUT) :: total_error

          CHARACTER(LEN=8), PARAMETER :: filename = "VLtypesR" ! File name
          CHARACTER(LEN=80) :: fix_filename
          CHARACTER(LEN=6), PARAMETER :: dsetname = "VLreal"     ! Dataset name

          INTEGER(HID_T) :: file_id       ! File identifier
          INTEGER(HID_T) :: dset_id       ! Dataset identifier
          INTEGER(HID_T) :: dspace_id     ! Dataspace identifier
          INTEGER(HID_T) :: vltype_id     ! Datatype identifier


          INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/6/) ! Dataset dimensions
          INTEGER(SIZE_T), DIMENSION(6) :: len           ! Elements lengths
          INTEGER(SIZE_T), DIMENSION(6) :: len_out
          INTEGER     ::   rank = 1                      ! Dataset rank

          REAL, DIMENSION(5,6) :: vl_real_data ! Data buffers
          REAL, DIMENSION(5,6) :: vl_real_data_out ! Data buffers
          INTEGER     ::   error ! Error flag

          INTEGER     :: i, j    !general purpose integers
          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims = (/5,6/)
          INTEGER(SIZE_T)  max_len
          INTEGER(HID_T) ::  vl_type_id
          LOGICAL        ::  vl_flag

          !
          ! Initialize the vl_int_data array.
          !
          do i = 1, 6
             do j = 1, 5
                vl_real_data(j,i) = -100.
             end do
          end do

          do i = 2, 6
             do j = 1,  i-1
                vl_real_data(j,i) = i-1
             end do
          end do

           do i = 1,6
              len(i) = i-1
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
          ! Create the dataset with default properties.
          !
          CALL h5tvlen_create_f(H5T_NATIVE_REAL, vltype_id, error)
              CALL check("h5dvlen_create_f", error, total_error)

          CALL h5dcreate_f(file_id, dsetname, vltype_id, dspace_id, &
                           dset_id, error)
              CALL check("h5dcreate_f", error, total_error)
          CALL h5dget_type_f(dset_id, vl_type_id, error)
              CALL check("h5dget_type_f", error, total_error)
          CALL h5tis_variable_str_f( vl_type_id, vl_flag, error)
              CALL check("h5tis_variable_str_f", error, total_error)
              if( vl_flag ) then
                 write(*,*) "type is wrong"
                 total_error = total_error + 1
              endif


          !
          ! Write the dataset.
          !
          CALL h5dwrite_vl_f(dset_id, vltype_id, vl_real_data, data_dims, len, error)
              CALL check("h5dwrite_vl_real_f", error, total_error)


          !
          ! End access to the dataset and release resources used by it.
          !
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f", error, total_error)

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

          CALL h5dvlen_get_max_len_f(dset_id, vltype_id, dspace_id, max_len, error)
              CALL check("h5dvlen_get_max_len_f", error, total_error)
              if(max_len .ne. data_dims(1)) then
                      total_error = total_error + 1
                      write(*,*) "Wrong number of elemets returned by h5dvlen_get_max_len_f"
              endif
          !
          ! Read the dataset.
          !
          CALL h5dread_vl_f(dset_id, vltype_id, vl_real_data_out, data_dims, len_out, &
                            error, mem_space_id = dspace_id, file_space_id = dspace_id)
              CALL check("h5dread_real_f", error, total_error)
              do i = 1, data_dims(2)
              do j = 1, len_out(i)
              if(vl_real_data(j,i) .ne. vl_real_data_out(j,i))  then
                  total_error = total_error + 1
                  write(*,*) "h5dread_vl_f returned incorrect data"
              endif
              enddo
               if (len(i) .ne. len_out(i)) then
                  total_error = total_error + 1
                  write(*,*) "h5dread_vl_f returned incorrect data"
              endif
              enddo


          !
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f", error, total_error)

          CALL h5tclose_f(vltype_id, error)
              CALL check("h5tclose_f", error, total_error)

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
          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)

          RETURN
        END SUBROUTINE vl_test_real

        SUBROUTINE vl_test_string(cleanup, total_error)
        USE HDF5 ! This module contains all necessary modules

          IMPLICIT NONE
          LOGICAL, INTENT(IN) :: cleanup
          INTEGER, INTENT(OUT) :: total_error

          CHARACTER(LEN=8), PARAMETER :: filename = "VLtypesS" ! File name
          CHARACTER(LEN=80) :: fix_filename
          CHARACTER(LEN=9), PARAMETER :: dsetname = "VLstrings"     ! Dataset name

          INTEGER(HID_T) :: file_id       ! File identifier
          INTEGER(HID_T) :: dset_id       ! Dataset identifier
          INTEGER(HID_T) :: dspace_id     ! Dataspace identifier


          INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/4/) ! Dataset dimensions
          INTEGER(SIZE_T), DIMENSION(4) :: str_len           ! Elements lengths
          INTEGER(SIZE_T), DIMENSION(4) :: str_len_out
          INTEGER     ::   rank = 1                      ! Dataset rank

          CHARACTER(LEN=10), DIMENSION(4) :: string_data ! Array of strings
          CHARACTER(LEN=10), DIMENSION(4) :: string_data_out     ! Data buffers
          INTEGER     ::   error ! Error flag

          INTEGER     :: i    !general purpose integers
          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims = (/10,4/)
          INTEGER(HID_T) :: vl_type_id
          LOGICAL        :: vl_flag

          !
          ! Initialize the string_data array.
          !
          string_data(1) = 'This is   '
          str_len(1) = 8
          string_data(2) = 'a fortran '
          str_len(2) = 10
          string_data(3) = 'strings   '
          str_len(3) = 8
          string_data(4) = 'test.     '
          str_len(4) = 5


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
          ! Create the dataset with default properties.
          !
          CALL h5dcreate_f(file_id, dsetname, H5T_STRING, dspace_id, &
                           dset_id, error)
              CALL check("h5dcreate_f", error, total_error)
          !
          ! Check that dataset has a string datatype
          !
          CALL h5dget_type_f(dset_id, vl_type_id, error)
              CALL check("h5dget_type_f", error, total_error)
          CALL h5tis_variable_str_f( vl_type_id, vl_flag, error)
              CALL check("h5tis_variable_str_f", error, total_error)
              if( .NOT. vl_flag ) then
                 write(*,*) "type is wrong"
                 total_error = total_error + 1
              endif

          !
          ! Write the dataset.
          !
          CALL h5dwrite_vl_f(dset_id, H5T_STRING, string_data, data_dims, str_len, error)
              CALL check("h5dwrite_string_f", error, total_error)


          !
          ! End access to the dataset and release resources used by it.
          !
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f", error, total_error)

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
          !
          !  Read the dataset.
          !
          CALL h5dread_vl_f(dset_id, H5T_STRING, string_data_out, data_dims,  &
                            str_len_out, error)
              CALL check("h5dread_string_f", error, total_error)
          do 100 i = 1, data_dims(2)
             if(str_len(i) .ne. str_len_out(i)) then
                total_error=total_error + 1
                write(*,*) 'Returned string length is incorrect'
                goto 100
             endif
             if(string_data(1)(1:str_len(i)) .ne. string_data_out(1)(1:str_len(i))) then
             write(*,*) ' Returned string is wrong'
             total_error = total_error + 1
             endif
100       continue

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
          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)

          RETURN
        END SUBROUTINE vl_test_string

