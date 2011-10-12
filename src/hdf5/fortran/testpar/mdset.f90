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


!//////////////////////////////////////////////////////////
! writes/reads dataset by hyperslabs
!//////////////////////////////////////////////////////////

SUBROUTINE multiple_dset_write(length, do_collective, do_chunk, mpi_size, mpi_rank, nerrors)
  USE hdf5
  IMPLICIT NONE
  INCLUDE 'mpif.h'

  INTEGER, INTENT(in) :: length                     ! array length
  LOGICAL, INTENT(in) :: do_collective              ! use collective I/O
  LOGICAL, INTENT(in) :: do_chunk                   ! use chunking
  INTEGER, INTENT(in) :: mpi_size                   ! number of processes in the group of communicator
  INTEGER, INTENT(in) :: mpi_rank                   ! rank of the calling process in the communicator
  INTEGER, INTENT(inout) :: nerrors                 ! number of errors
  INTEGER :: mpierror                               ! MPI hdferror flag
  INTEGER :: hdferror                               ! HDF hdferror flag
  INTEGER(hsize_t), DIMENSION(1) :: dims            ! dataset dimensions
  INTEGER(hsize_t), DIMENSION(1) :: cdims           ! chunk dimensions
  INTEGER, ALLOCATABLE :: wbuf(:)                   ! write buffer
  INTEGER, ALLOCATABLE :: rbuf(:)                   ! read buffer
  INTEGER(hsize_t), DIMENSION(1) :: counti          ! hyperslab selection
  INTEGER(hsize_t), DIMENSION(1) :: start           ! hyperslab selection
  INTEGER(hid_t) :: fapl_id                         ! file access identifier
  INTEGER(hid_t) :: dxpl_id                         ! dataset transfer property list
  INTEGER(hid_t) :: dcpl_id                         ! dataset creation property list
  INTEGER(hid_t) :: file_id                         ! file identifier
  INTEGER(hid_t) :: dset_id                         ! dataset identifier
  INTEGER(hid_t) :: fspace_id                       ! file space identifier
  INTEGER(hid_t) :: mspace_id                       ! memory space identifier
  INTEGER(hid_t) :: driver_id                       ! low-level file driver identifier
  INTEGER        :: istart                          ! start position in array
  INTEGER        :: iend                            ! end position in array
  INTEGER        :: icount                          ! number of elements in array
  CHARACTER(len=80) :: filename                     ! filename
  CHARACTER(len=80) :: dsetname                     ! dataset name
  INTEGER        :: n, i

  !//////////////////////////////////////////////////////////
  ! initialize the array data between the processes (3)
  ! for the 12 size array we get
  ! p0 = 1,2,3,4
  ! p1 = 5,6,7,8
  ! p2 = 9,10,11,12
  !//////////////////////////////////////////////////////////

  ALLOCATE(wbuf(0:length-1),stat=hdferror)
  IF (hdferror /= 0) THEN
     WRITE(*,*) 'allocate error'
     RETURN
  ENDIF

  ALLOCATE(rbuf(0:length-1),stat=hdferror)
  IF (hdferror /= 0) THEN
     WRITE(*,*) 'allocate error'
     RETURN
  ENDIF

  icount  = length/mpi_size     ! divide the array by the number of processes
  istart  = mpi_rank*icount     ! start position
  iend    = istart + icount     ! end position

  !//////////////////////////////////////////////////////////
  ! HDF5 I/O
  !//////////////////////////////////////////////////////////

  dims(1)  = length
  cdims(1) = length/mpi_size     ! define chunks as the number of processes

  !//////////////////////////////////////////////////////////
  ! setup file access property list with parallel I/O access
  !//////////////////////////////////////////////////////////

  CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
  CALL check("h5pset_fapl_mpio_f", hdferror, nerrors)

  CALL h5pget_driver_f(fapl_id, driver_id, hdferror)
  CALL check("h5pget_driver_f", hdferror, nerrors)

  IF( driver_id /= H5FD_MPIO_F) THEN
     WRITE(*,*) "Wrong driver information returned"
     nerrors = nerrors + 1
  ENDIF

  !//////////////////////////////////////////////////////////
  ! create the file collectively
  !//////////////////////////////////////////////////////////

  CALL h5_fixname_f("parf2", filename, fapl_id, hdferror)

  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, hdferror, access_prp = fapl_id)
  CALL check("h5fcreate_f", hdferror, nerrors)

  CALL h5screate_simple_f(1, dims, fspace_id, hdferror)
  CALL check("h5screate_simple_f", hdferror, nerrors)

  CALL h5screate_simple_f(1, dims, mspace_id, hdferror)
  CALL check("h5screate_simple_f", hdferror, nerrors)

  !//////////////////////////////////////////////////////////
  ! modify dataset creation properties to enable chunking
  !//////////////////////////////////////////////////////////

  CALL h5pcreate_f(H5P_DATASET_CREATE_F, dcpl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  IF (do_chunk) THEN
     CALL h5pset_chunk_f(dcpl_id, 1, cdims, hdferror)
     CALL check("h5pset_chunk_f", hdferror, nerrors)
  ENDIF

  !//////////////////////////////////////////////////////////
  ! create a property list for collective dataset write
  !//////////////////////////////////////////////////////////

  CALL h5pcreate_f(H5P_DATASET_XFER_F, dxpl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  IF (do_collective) THEN
     CALL h5pset_dxpl_mpio_f(dxpl_id, H5FD_MPIO_COLLECTIVE_F, hdferror)
     CALL check("h5pset_dxpl_mpio_f", hdferror, nerrors)
  ENDIF

  !//////////////////////////////////////////////////////////
  ! define hyperslab
  !//////////////////////////////////////////////////////////

  counti(1) = icount
  start(1)  = istart

  !//////////////////////////////////////////////////////////
  ! select hyperslab in memory
  !//////////////////////////////////////////////////////////

  CALL h5sselect_hyperslab_f(mspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
  CALL check("h5sselect_hyperslab_f", hdferror, nerrors)

  !//////////////////////////////////////////////////////////
  ! select hyperslab in the file
  !//////////////////////////////////////////////////////////

  CALL h5sselect_hyperslab_f(fspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
  CALL check("h5sselect_hyperslab_f", hdferror, nerrors)

  !//////////////////////////////////////////////////////////
  ! create and write the datasets
  !//////////////////////////////////////////////////////////

  DO n = 1, 300

     ! direct the output of the write statement to unit "dsetname"
     WRITE(dsetname,*) 'dataset', n

     ! create this dataset
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_INTEGER, fspace_id, dset_id, hdferror, dcpl_id)
     CALL check("h5dcreate_f", hdferror, nerrors)

     DO i = istart, iend-1
        wbuf(i) = n + mpi_rank
     ENDDO

     ! write this dataset
     CALL h5dwrite_f(dset_id,H5T_NATIVE_INTEGER,wbuf,dims,hdferror,file_space_id=fspace_id,mem_space_id=mspace_id,xfer_prp=dxpl_id)
     CALL check("h5dwrite_f", hdferror, nerrors)

     ! close this dataset
     CALL h5dclose_f(dset_id, hdferror)
     CALL check("h5dclose_f", hdferror, nerrors)

  ENDDO

  !//////////////////////////////////////////////////////////
  ! close HDF5 I/O
  !//////////////////////////////////////////////////////////

  CALL h5pclose_f(fapl_id, hdferror)
  CALL check("h5pclose_f", hdferror, nerrors)

  CALL h5pclose_f(dcpl_id, hdferror)
  CALL check("h5pclose_f", hdferror, nerrors)

  CALL h5pclose_f(dxpl_id, hdferror)
  CALL check("h5pclose_f", hdferror, nerrors)

  CALL h5sclose_f(mspace_id, hdferror)
  CALL check("h5sclose_f", hdferror, nerrors)

  CALL h5sclose_f(fspace_id, hdferror)
  CALL check("h5sclose_f", hdferror, nerrors)

  CALL h5fclose_f(file_id, hdferror)
  CALL check("h5fclose_f", hdferror, nerrors)

  !//////////////////////////////////////////////////////////
  ! reopen file with read access
  !//////////////////////////////////////////////////////////

  CALL h5pcreate_f(H5P_FILE_ACCESS_F, fapl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5pset_fapl_mpio_f(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, hdferror, access_prp = fapl_id)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5screate_simple_f(1, dims, fspace_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5screate_simple_f(1, dims, mspace_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  !//////////////////////////////////////////////////////////
  ! select hyperslab in memory
  !//////////////////////////////////////////////////////////

  CALL h5sselect_hyperslab_f(mspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  !//////////////////////////////////////////////////////////
  ! select hyperslab in the file
  !//////////////////////////////////////////////////////////

  CALL h5sselect_hyperslab_f(fspace_id, H5S_SELECT_SET_F, start, counti, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  !//////////////////////////////////////////////////////////
  ! create a property list for collective dataset read
  !//////////////////////////////////////////////////////////

  CALL h5pcreate_f(H5P_DATASET_XFER_F, dxpl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  IF (do_collective) THEN
     CALL h5pset_dxpl_mpio_f(dxpl_id, H5FD_MPIO_COLLECTIVE_F, hdferror)
     CALL check("h5pcreate_f", hdferror, nerrors)
  ENDIF

  !//////////////////////////////////////////////////////////
  ! read dataset
  !//////////////////////////////////////////////////////////

  DO n = 1, 300

     ! direct the output of the write statement to unit "dsetname"
     WRITE(dsetname,*) 'dataset', n

     ! create this dataset
     CALL h5dopen_f(file_id, dsetname, dset_id, hdferror)
     CALL check("h5pcreate_f", hdferror, nerrors)

     ! read this dataset
     CALL h5dread_f(dset_id,H5T_NATIVE_INTEGER,rbuf,dims,hdferror,file_space_id=fspace_id,mem_space_id=mspace_id,xfer_prp=dxpl_id)
     CALL check("h5pcreate_f", hdferror, nerrors)

     ! close this dataset
     CALL h5dclose_f(dset_id, hdferror)
     CALL check("h5dclose_f", hdferror, nerrors)

     DO i = istart, iend-1
        wbuf(i) = n + mpi_rank
     ENDDO

     ! compare read and write data. each process compares a subset of the array
     DO i = istart, iend-1
        IF( wbuf(i) /= rbuf(i)) THEN
           WRITE(*,*) 'buffers differs at ', i, rbuf(i), wbuf(i)
           nerrors = nerrors + 1
        ENDIF
     ENDDO

  ENDDO


  !//////////////////////////////////////////////////////////
  ! close HDF5 I/O
  !//////////////////////////////////////////////////////////

  CALL h5pclose_f(fapl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5pclose_f(dxpl_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5sclose_f(fspace_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5sclose_f(mspace_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)

  CALL h5fclose_f(file_id, hdferror)
  CALL check("h5pcreate_f", hdferror, nerrors)


  DEALLOCATE(wbuf)
  DEALLOCATE(rbuf)

END SUBROUTINE multiple_dset_write

