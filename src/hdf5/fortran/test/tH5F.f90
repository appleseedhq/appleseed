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
!    Testing File Interface functionality.
!
!    In the mountingtest subroutine we create one file with a group in it,
!    and another file with a dataset. Mounting is used to
!    access the dataset from the second file as a member of a group
!    in the first file.
!
        SUBROUTINE mountingtest(cleanup, total_error)
        USE HDF5  ! This module contains all necessary modules
          IMPLICIT NONE
          LOGICAL, INTENT(IN)  :: cleanup
          INTEGER, INTENT(OUT) :: total_error

          !
          !the respective filename is "mount1.h5" and "mount2.h5"
          !
          CHARACTER(LEN=6)  :: filename1
          CHARACTER(LEN=6)  :: filename2
          CHARACTER(LEN=80) :: fix_filename1
          CHARACTER(LEN=80) :: fix_filename2

          !
          !data space rank and dimensions
          !
          INTEGER, PARAMETER :: RANK = 2
          INTEGER, PARAMETER :: NX = 4
          INTEGER, PARAMETER :: NY = 5

          !
          ! File identifiers
          !
          INTEGER(HID_T) :: file1_id, file2_id

          !
          ! Group identifier
          !
          INTEGER(HID_T) :: gid

          !
          ! dataset identifier
          !
          INTEGER(HID_T) :: dset_id

          !
          ! data space identifier
          !
          INTEGER(HID_T) :: dataspace

          !
          ! data type identifier
          !
          INTEGER(HID_T) :: dtype_id

          !
          !The dimensions for the dataset.
          !
          INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/NX,NY/)

          !
          !return value for testing whether a file is in hdf5 format
          !
          LOGICAL     ::  status

          !
          !flag to check operation success
          !
          INTEGER     ::   error

          !
          !general purpose integer
          !
          INTEGER     ::   i, j

          !
          !data buffers
          !
          INTEGER, DIMENSION(NX,NY) :: data_in, data_out

          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
          filename1 = "mount1"
          filename2 = "mount2"

          do i = 1,80
             fix_filename1(i:i) = " "
             fix_filename2(i:i) = " "
          enddo
          !
          !Initialize data_in buffer
          !
          do j = 1, NY
             do i = 1, NX
                data_in(i,j) =  (i-1) + (j-1)
             end do
          end do

          !
          ! Fix names of the files
          !
          CALL h5_fixname_f(filename1, fix_filename1, H5P_DEFAULT_F, error)
          if(error .ne. 0) stop
          CALL h5_fixname_f(filename2, fix_filename2, H5P_DEFAULT_F, error)
          if(error .ne. 0) stop

          !
          !Create first file "mount1.h5" using default properties.
          !
          CALL h5fcreate_f(fix_filename1, H5F_ACC_TRUNC_F, file1_id, error)
               CALL check("h5fcreate_f",error,total_error)


          !
          !Create group "/G" inside file "mount1.h5".
          !
          CALL h5gcreate_f(file1_id, "/G", gid, error)
               CALL check("h5gcreate_f",error,total_error)

          !
          !close file and group identifiers.
          !
          CALL h5gclose_f(gid, error)
               CALL check("h5gclose_f",error,total_error)
          CALL h5fclose_f(file1_id, error)
               CALL check("h5fclose_f",error,total_error)

          !
          !Create second file "mount2.h5" using default properties.
          !
          CALL h5fcreate_f(fix_filename2, H5F_ACC_TRUNC_F, file2_id, error)
               CALL check("h5fcreate_f",error,total_error)

          !
          !Create data space for the dataset.
          !
          CALL h5screate_simple_f(RANK, dims, dataspace, error)
               CALL check("h5screate_simple_f",error,total_error)

          !
          !Create dataset "/D" inside file "mount2.h5".
          !
          CALL h5dcreate_f(file2_id, "/D", H5T_NATIVE_INTEGER, dataspace, &
               dset_id, error)
              CALL check("h5dcreate_f",error,total_error)

          !
          ! Write data_in to the dataset
          !
          data_dims(1) = NX
          data_dims(2) = NY
          CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, data_in, data_dims, error)
               CALL check("h5dwrite_f",error,total_error)

          !
          !close file, dataset and dataspace identifiers.
          !
          CALL h5sclose_f(dataspace, error)
              CALL check("h5sclose_f",error,total_error)
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f",error,total_error)
          CALL h5fclose_f(file2_id, error)
              CALL check("h5fclose_f",error,total_error)

          !
          !test whether files are in hdf5 format
          !
          CALL h5fis_hdf5_f(fix_filename1, status, error)
               CALL check("h5fis_hdf5_f",error,total_error)
          IF ( .NOT. status ) THEN
              write(*,*) "File ", fix_filename1, " is not in hdf5 format"
              stop
          END IF

          CALL h5fis_hdf5_f(fix_filename2, status, error)
               CALL check("h5fis_hdf5_f",error,total_error)
          IF ( .NOT. status ) THEN
              write(*,*) "File ", fix_filename2, " is not in hdf5 format"
              stop
          END IF

          !
          !reopen both files.
          !
          CALL h5fopen_f (fix_filename1, H5F_ACC_RDWR_F, file1_id, error)
              CALL check("hfopen_f",error,total_error)
          CALL h5fopen_f (fix_filename2, H5F_ACC_RDWR_F, file2_id, error)
              CALL check("h5fopen_f",error,total_error)

          !
          !mount the second file under the first file's "/G" group.
          !
          CALL h5fmount_f (file1_id, "/G", file2_id, error)
              CALL check("h5fmount_f",error,total_error)


          !
          !Access dataset D in the first file under /G/D name.
          !
          CALL h5dopen_f(file1_id, "/G/D", dset_id, error)
              CALL check("h5dopen_f",error,total_error)

          !
          !Get dataset's data type.
          !
          CALL h5dget_type_f(dset_id, dtype_id, error)
              CALL check("h5dget_type_f",error,total_error)

          !
          !Read the dataset.
          !
          CALL h5dread_f(dset_id, dtype_id, data_out, data_dims, error)
              CALL check("h5dread_f",error,total_error)

          !
          !Compare the data.
          !
          do i = 1, NX
              do j = 1, NY
                  IF (data_out(i,j) .NE. data_in(i, j)) THEN
                      write(*, *) "mounting test error occured"
                  END IF
              end do
          end do


          !
          !Close dset_id and dtype_id.
          !
          CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f",error,total_error)
          CALL h5tclose_f(dtype_id, error)
              CALL check("h5tclose_f",error,total_error)

          !
          !unmount the second file.
          !
          CALL h5funmount_f(file1_id, "/G", error);
              CALL check("h5funmount_f",error,total_error)

          !
          !Close both files.
          !
          CALL h5fclose_f(file1_id, error)
              CALL check("h5fclose_f",error,total_error)
          CALL h5fclose_f(file2_id, error)
              CALL check("h5fclose_f",error,total_error)

          if(cleanup) CALL h5_cleanup_f(filename1, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          if(cleanup) CALL h5_cleanup_f(filename2, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          RETURN
        END SUBROUTINE mountingtest

!
!    The following subroutine tests h5freopen_f.
!    It creates the file which has name "reopen.h5" and
!    the "/dset" dataset inside the file.
!    writes the data to the file, close the dataset.
!    Reopen the file based upon the file_id, open the
!    dataset use the reopen_id then reads the
!    dataset back to memory to test whether the data
!    read is identical to the data written
!

        SUBROUTINE reopentest(cleanup, total_error)
        USE HDF5  ! This module contains all necessary modules
          IMPLICIT NONE
          LOGICAL, INTENT(IN) :: cleanup
          INTEGER, INTENT(OUT) :: total_error

          !
          CHARACTER(LEN=6), PARAMETER :: filename = "reopen"
          CHARACTER(LEN=80)  :: fix_filename

          INTEGER(HID_T) :: file_id, reopen_id  ! File identifiers
          INTEGER(HID_T) :: dset_id             ! Dataset identifier

          !
          !dataset name is "dset"
          !
          CHARACTER(LEN=4), PARAMETER :: dsetname = "dset"

          !
          !data space rank and dimensions
          !
          INTEGER, PARAMETER :: RANK = 2
          INTEGER, PARAMETER :: NX = 4
          INTEGER, PARAMETER :: NY = 6

          !
          ! data space identifier
          !
          INTEGER(HID_T) :: dataspace

          !
          !The dimensions for the dataset.
          !
          INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/NX,NY/)

          !
          !flag to check operation success
          !
          INTEGER     ::   error

          !
          !general purpose integer
          !
          INTEGER     ::  i, j

          !
          !array to store data
          !
          INTEGER, DIMENSION(4,6) :: dset_data, data_out
          INTEGER(HSIZE_T), DIMENSION(2) :: data_dims
          INTEGER(HSIZE_T)  :: file_size
          CHARACTER(LEN=80) :: file_name
          INTEGER(SIZE_T) :: name_size

          !
          !initialize the dset_data array which will be written to the "/dset"
          !
          do j = 1, NY
               do i = 1, NX
                    dset_data(i,j) = (i-1)*6 + j;
               end do
          end do

          !
          !Initialize FORTRAN predifined datatypes
          !
!          CALL h5init_types_f(error)
!               CALL check("h5init_types_f",error,total_error)


          !
          !Create file "reopen.h5" using default properties.
          !
          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
          CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, file_id, error)
               CALL check("h5fcreate_f",error,total_error)

          !
          !Create data space for the dataset.
          !
          CALL h5screate_simple_f(RANK, dims, dataspace, error)
               CALL check("h5screate_simple_f",error,total_error)

          !
          !Create dataset "/dset" inside the file .
          !
          CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, dataspace, &
               dset_id, error)
              CALL check("h5dcreate_f",error,total_error)

         !
         !Write the dataset.
         !
         data_dims(1) = NX
         data_dims(2) = NY
         CALL h5dwrite_f(dset_id, H5T_NATIVE_INTEGER, dset_data, data_dims, error)
              CALL check("h5dwrite_f",error,total_error)

         !
         !close the dataset.
         !
         CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f",error,total_error)

         !
         !close the dataspace.
         !
         CALL h5sclose_f(dataspace, error)
              CALL check("h5sclose_f",error,total_error)

         !
         !Reopen file dsetf.h5.
         !
         CALL h5freopen_f(file_id, reopen_id, error)
              CALL check("h5freopen_f",error,total_error)
         !
         !Check file size
         !
         CALL h5fget_filesize_f(file_id, file_size, error)
              CALL check("h5fget_filesize_f",error,total_error)

         !
         !Open the dataset based on the reopen_id.
         !
         CALL h5dopen_f(reopen_id, dsetname, dset_id, error)
              CALL check("h5dopen_f",error,total_error)
         !
         !Get file name from the dataset identifier
         !
         CALL h5fget_name_f(dset_id, file_name, name_size, error)
              CALL check("h5fget_name_f",error,total_error)
              IF(file_name(1:name_size) .NE. fix_filename(1:name_size)) THEN
                 write(*,*) "file name obtained from the dataset id is incorrect"
              END IF

         !
         !Read the dataset.
         !
         CALL h5dread_f(dset_id, H5T_NATIVE_INTEGER, data_out, data_dims, error)
              CALL check("h5dread_f",error,total_error)

          !
          !Compare the data.
          !
          do i = 1, NX
              do j = 1, NY
                  IF (data_out(i,j) .NE. dset_data(i, j)) THEN
                      write(*, *) "reopen test error occured"
                  END IF
              end do
          end do


         !
         !Close the dataset.
         !
         CALL h5dclose_f(dset_id, error)
              CALL check("h5dclose_f",error,total_error)

         !
         !Close the file identifiers.
         !
         CALL h5fclose_f(file_id, error)
              CALL check("h5fclose_f",error,total_error)
         CALL h5fclose_f(reopen_id, error)
              CALL check("h5fclose_f",error,total_error)


          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          RETURN

        END SUBROUTINE reopentest

!
!    The following example demonstrates how to get creation property list,
!    and access property list.
!    We first create a file using the default creation and access property
!    list. Then, the file was closed and reopened. We then get the
!    creation and access property lists of the first file. The second file is
!    created using the got property lists

        SUBROUTINE plisttest(cleanup, total_error)
         USE HDF5  ! This module contains all necessary modules
          IMPLICIT NONE
          LOGICAL, INTENT(IN)  :: cleanup
          INTEGER, INTENT(OUT) :: total_error

          !
          !file names are "plist1.h5" and "plist2.h5"
          !
          CHARACTER(LEN=6), PARAMETER :: filename1 = "plist1"
          CHARACTER(LEN=80) :: fix_filename1
          CHARACTER(LEN=6), PARAMETER :: filename2 = "plist2"
          CHARACTER(LEN=80) :: fix_filename2

          INTEGER(HID_T) :: file1_id, file2_id   ! File identifiers
          INTEGER(HID_T) :: prop_id    ! File creation property list identifier
          INTEGER(HID_T) :: access_id  ! File Access property list identifier

          !flag to check operation success
          INTEGER     ::   error

          !
          !Create a file1 using default properties.
          !
          CALL h5_fixname_f(filename1, fix_filename1, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify file name"
              stop
          endif
          CALL h5fcreate_f(fix_filename1, H5F_ACC_TRUNC_F, file1_id, error)
              CALL check("h5fcreate_f",error,total_error)

          !
          !Terminate access to the file.
          !
          CALL h5fclose_f(file1_id, error)
              CALL check("h5fclose_f",error,total_error)

          !
          !Open an existing file.
          !
          CALL h5fopen_f (fix_filename1, H5F_ACC_RDWR_F, file1_id, error)
              CALL check("h5fopen_f",error,total_error)

          !
          !get the creation property list.
          !
          CALL h5fget_create_plist_f(file1_id, prop_id, error)
              CALL check("h5fget_create_plist_f",error,total_error)

          !
          !get the access property list.
          !
          CALL h5fget_access_plist_f(file1_id, access_id, error)
              CALL check("h5fget_access_plist_f",error,total_error)

          !
          !based on the creation property list id and access property list id
          !create a new file
          !
          CALL h5_fixname_f(filename2, fix_filename2, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify file name"
              stop
          endif
          CALL h5fcreate_f(fix_filename2, H5F_ACC_TRUNC_F, file2_id, error, &
               prop_id, access_id)
              CALL check("h5create_f",error,total_error)

          !
          !Close all the property lists.
          !
          CALL h5pclose_f(prop_id, error)
              CALL check("h5pclose_f",error,total_error)
          CALL h5pclose_f(access_id, error)
              CALL check("h5pclose_f",error,total_error)

          !
          !Terminate access to the files.
          !
          CALL h5fclose_f(file1_id, error)
              CALL check("h5fclose_f",error,total_error)

          CALL h5fclose_f(file2_id, error)
              CALL check("h5fclose_f",error,total_error)

          if(cleanup) CALL h5_cleanup_f(filename1, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          if(cleanup) CALL h5_cleanup_f(filename2, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          RETURN

        END SUBROUTINE plisttest


!
!    The following subroutine tests h5pget(set)_fclose_degree_f
!

        SUBROUTINE file_close(cleanup, total_error)
        USE HDF5  ! This module contains all necessary modules
          IMPLICIT NONE
          LOGICAL, INTENT(IN) :: cleanup
          INTEGER, INTENT(OUT) :: total_error
          INTEGER              :: error

          !
          CHARACTER(LEN=10), PARAMETER :: filename = "file_close"
          CHARACTER(LEN=80)  :: fix_filename

          INTEGER(HID_T) :: fid, fid_d, fid1, fid2, fid3  ! File identifiers
          INTEGER(HID_T) :: fapl, fapl1, fapl2, fapl3 ! File access identifiers
          INTEGER(HID_T) :: fid_d_fapl, fid1_fapl     ! File access identifiers
          LOGICAL        :: flag
          INTEGER(SIZE_T) :: obj_count, obj_countf
          INTEGER(HID_T), ALLOCATABLE, DIMENSION(:) :: obj_ids
          INTEGER        :: i

          CALL h5eset_auto_f(0, error)

          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
          CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, fid, error)
               CALL check("h5fcreate_f",error,total_error)

          CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl, error)
               CALL check("h5pcreate_f",error,total_error)
          CALL h5pset_fclose_degree_f(fapl, H5F_CLOSE_DEFAULT_F, error)
               CALL check("h5pset_fclose_degree_f",error,total_error)


          CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl1, error)
               CALL check("h5pcreate_f",error,total_error)
          CALL h5pset_fclose_degree_f(fapl1, H5F_CLOSE_WEAK_F, error)
               CALL check("h5pset_fclose_degree_f",error,total_error)


          CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl2, error)
               CALL check("h5pcreate_f",error,total_error)
          CALL h5pset_fclose_degree_f(fapl2, H5F_CLOSE_SEMI_F, error)
               CALL check("h5pset_fclose_degree_f",error,total_error)

          CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl3, error)
               CALL check("h5pcreate_f",error,total_error)
          CALL h5pset_fclose_degree_f(fapl3, H5F_CLOSE_STRONG_F, error)
               CALL check("h5pset_fclose_degree_f",error,total_error)

          CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, fid1, error, access_prp=fapl1)
               CALL check("h5fopen_f",error,total_error)
          CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, fid_d, error, access_prp=fapl)
               CALL check("h5fopen_f",error,total_error)
          CALL h5fget_access_plist_f(fid1, fid1_fapl, error)
               CALL check("h5fget_access_plist_f",error,total_error)
          CALL h5fget_access_plist_f(fid_d, fid_d_fapl, error)
               CALL check("h5fget_access_plist_f",error,total_error)

          CALL h5pequal_f(fid_d_fapl, fid1_fapl, flag, error)
               CALL check("h5pequal_f",error,total_error)
          if (.NOT. flag) then
               write(*,*) " File access lists should be equal, error "
               total_error=total_error + 1
          endif
          CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, fid2, error, access_prp=fapl2)
               if( error .ne. -1) then
                   total_error = total_error + 1
                   write(*,*) " Open with H5F_CLOSE_SEMI should fail "
               endif
          CALL h5fopen_f(fix_filename, H5F_ACC_RDWR_F, fid3, error, access_prp=fapl3)
               if( error .ne. -1) then
                   total_error = total_error + 1
                   write(*,*) " Open with H5F_CLOSE_STRONG should fail "
               endif

          CALL h5fget_obj_count_f(fid1, H5F_OBJ_ALL_F, obj_count, error)
               CALL check("h5fget_obj_count_f",error,total_error)
               if(error .eq.0 .and. obj_count .ne. 3) then
                 total_error = total_error + 1
                 write(*,*) "Wrong number of open objects reported, error"
               endif
          CALL h5fget_obj_count_f(fid1, H5F_OBJ_FILE_F, obj_countf, error)
               CALL check("h5fget_obj_count_f",error,total_error)
               if(error .eq.0 .and. obj_countf .ne. 3) then
                 total_error = total_error + 1
                 write(*,*) "Wrong number of open objects reported, error"
               endif
          allocate(obj_ids(obj_countf), stat = error)
          CALL h5fget_obj_ids_f(fid, H5F_OBJ_FILE_F, obj_countf, obj_ids, error)
               CALL check("h5fget_obj_ids_f",error,total_error)
          if(error .eq. 0) then
             do i = 1, obj_countf
                    CALL h5fclose_f(obj_ids(i), error)
                         CALL check("h5fclose_f",error,total_error)
             enddo
          endif

          CALL h5fclose_f(fid, error)
              if(error .eq. 0) then
                 total_error = total_error + 1
                 write(*,*) "File should be closed at this point, error"
              endif
          CALL h5fclose_f(fid1, error)
              if(error .eq. 0) then
                 total_error = total_error + 1
                 write(*,*) "File should be closed at this point, error"
              endif
          CALL h5fclose_f(fid_d, error)
              if(error .eq. 0) then
                 total_error = total_error + 1
                 write(*,*) "File should be closed at this point, error"
              endif

          if(cleanup) then
              CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          endif
          deallocate(obj_ids)
          RETURN

        END SUBROUTINE file_close

!
!    The following subroutine tests h5fget_freespace_f
!

        SUBROUTINE file_space(filename, cleanup, total_error)
        USE HDF5  ! This module contains all necessary modules
          IMPLICIT NONE
          CHARACTER(*), INTENT(IN) :: filename
          LOGICAL, INTENT(IN) :: cleanup
          INTEGER, INTENT(OUT) :: total_error
          INTEGER              :: error
          !
          CHARACTER(LEN=3), PARAMETER :: grpname = "grp"
          CHARACTER(LEN=80)  :: fix_filename

          INTEGER(HID_T) :: fid ! File identifiers
          INTEGER(HSSIZE_T) :: free_space
          INTEGER(HID_T) :: group_id      ! Group identifier

          CALL h5eset_auto_f(0, error)

          CALL h5_fixname_f(filename, fix_filename, H5P_DEFAULT_F, error)
          if (error .ne. 0) then
              write(*,*) "Cannot modify filename"
              stop
          endif
          CALL h5fcreate_f(fix_filename, H5F_ACC_TRUNC_F, fid, error)
               CALL check("h5fcreate_f",error,total_error)

          CALL h5fget_freespace_f(fid, free_space, error)
               CALL check("h5fget_freespace_f",error,total_error)
               if(error .eq.0 .and. free_space .ne. 0) then
                 total_error = total_error + 1
                 write(*,*) "1: Wrong amount of free space reported, ", free_space
               endif

          ! Create group in the file.
          CALL h5gcreate_f(fid, grpname, group_id, error)
          CALL check("h5gcreate_f",error,total_error)

          ! Close group
          CALL h5gclose_f(group_id, error)
          CALL check("h5gclose_f", error, total_error)

          ! Check the free space now
          CALL h5fget_freespace_f(fid, free_space, error)
               CALL check("h5fget_freespace_f",error,total_error)
               if(error .eq.0 .and. free_space .ne. 0) then
                 total_error = total_error + 1
                 write(*,*) "2: Wrong amount of free space reported, ", free_space
               endif

          !Unlink the group
          CALL h5gunlink_f(fid, grpname, error)
          CALL check("h5gunlink_f", error, total_error)

          ! Check the free space now
          CALL h5fget_freespace_f(fid, free_space, error)
               CALL check("h5fget_freespace_f",error,total_error)
               if(error .eq.0 .and. free_space .ne. 0) then
                 total_error = total_error + 1
                 write(*,*) "3: Wrong amount of free space reported, ", free_space
               endif

          CALL h5fclose_f(fid, error)
              CALL check("h5fclose_f",error,total_error)

          if(cleanup) CALL h5_cleanup_f(filename, H5P_DEFAULT_F, error)
              CALL check("h5_cleanup_f", error, total_error)
          RETURN

        END SUBROUTINE file_space



