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
! This file contains the FORTRAN90 tests for H5LT
!

PROGRAM lite_test

  CALL test_dataset1D()
  CALL test_dataset2D()
  CALL test_dataset3D()
  CALL test_datasetND(4)
  CALL test_datasetND(5)
  CALL test_datasetND(6)
  CALL test_datasetND(7)
  CALL test_datasets()
  CALL test_attributes()

END PROGRAM lite_test


!-------------------------------------------------------------------------
! test_dataset1D
!-------------------------------------------------------------------------

SUBROUTINE test_dataset1D()

USE H5LT ! module of H5LT
USE HDF5 ! module of HDF5 library

IMPLICIT NONE

INTEGER, PARAMETER :: DIM1 = 4;                      ! Dimension of array
CHARACTER(len=9), PARAMETER :: filename = "dsetf1.h5"! File name
CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"   ! Dataset name
CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"   ! Dataset name
CHARACTER(LEN=5), PARAMETER :: dsetname3 = "dset3"   ! Dataset name
INTEGER(HID_T) :: file_id                            ! File identifier
INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/DIM1/)    ! Dataset dimensions
INTEGER        :: rank = 1                           ! Dataset rank
INTEGER, DIMENSION(DIM1) :: buf1                     ! Data buffer
INTEGER, DIMENSION(DIM1) :: bufr1                    ! Data buffer
REAL, DIMENSION(DIM1)    :: buf2                     ! Data buffer
REAL, DIMENSION(DIM1)    :: bufr2                    ! Data buffer
DOUBLE PRECISION, DIMENSION(DIM1) :: buf3            ! Data buffer
DOUBLE PRECISION, DIMENSION(DIM1) :: bufr3           ! Data buffer
INTEGER        :: errcode                            ! Error flag
INTEGER        :: i                                  ! general purpose integer


CALL test_begin(' Make/Read datasets (1D)        ')


!
! Initialize the data array.
!
DO i = 1, DIM1
  buf1(i) = i;
  buf2(i) = i;
  buf3(i) = i;
END DO

!
! Initialize FORTRAN predefined datatypes.
!
CALL h5open_f(errcode)

!
! Create a new file using default properties.
!
CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

!-------------------------------------------------------------------------
! H5T_NATIVE_INTEGER
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname1, rank, dims, H5T_NATIVE_INTEGER, buf1, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname1, H5T_NATIVE_INTEGER, bufr1, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, DIM1
 IF ( buf1(i) .NE. bufr1(i) ) THEN
   PRINT *, 'read buffer differs from write buffer'
   PRINT *,  bufr1(i), ' and ',   buf1(i)
   STOP
  ENDIF
END DO

!-------------------------------------------------------------------------
! H5T_NATIVE_REAL
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims, H5T_NATIVE_REAL, buf2, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_REAL, bufr2, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, DIM1
 IF ( buf2(i) .NE. bufr2(i) ) THEN
   PRINT *, 'read buffer differs from write buffer'
   PRINT *,  bufr2(i), ' and ',   buf2(i)
   STOP
  ENDIF
END DO

!-------------------------------------------------------------------------
! H5T_NATIVE_DOUBLE
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_DOUBLE, buf3, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_DOUBLE, bufr3, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, DIM1
 IF ( buf3(i) .NE. bufr3(i) ) THEN
   PRINT *, 'read buffer differs from write buffer'
   PRINT *,  bufr3(i), ' and ',   buf3(i)
   STOP
  ENDIF
END DO

!
! Close the file.
!
CALL h5fclose_f(file_id, errcode)

!
! Close FORTRAN predefined datatypes.
!
CALL h5close_f(errcode)

CALL passed()
!
! end function.
!
END SUBROUTINE test_dataset1D

!-------------------------------------------------------------------------
! test_dataset2D
!-------------------------------------------------------------------------

SUBROUTINE test_dataset2D()

USE H5LT ! module of H5LT
USE HDF5 ! module of HDF5 library

IMPLICIT NONE


INTEGER, PARAMETER :: DIM1 = 4;                             ! columns
INTEGER, PARAMETER :: DIM2 = 6;                             ! rows
CHARACTER(len=9), PARAMETER :: filename = "dsetf2.h5"! File name
CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"   ! Dataset name
CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"   ! Dataset name
CHARACTER(LEN=5), PARAMETER :: dsetname3 = "dset3"   ! Dataset name
CHARACTER(LEN=5), PARAMETER :: dsetname4 = "dset4"   ! Dataset name
INTEGER(HID_T) :: file_id                            ! File identifier
INTEGER(HSIZE_T), DIMENSION(2) :: dims = (/4,6/)     ! Dataset dimensions
INTEGER        :: rank = 2                           ! Dataset rank
INTEGER, DIMENSION(DIM1*DIM2) :: buf                 ! Data buffer
INTEGER, DIMENSION(DIM1*DIM2) :: bufr                ! Data buffer
INTEGER, DIMENSION(DIM1,DIM2) :: buf2                ! Data buffer
INTEGER, DIMENSION(DIM1,DIM2) :: buf2r               ! Data buffer
REAL, DIMENSION(DIM1,DIM2)    :: buf3                ! Data buffer
REAL, DIMENSION(DIM1,DIM2)    :: buf3r               ! Data buffer
DOUBLE PRECISION, DIMENSION(DIM1,DIM2) :: buf4       ! Data buffer
DOUBLE PRECISION, DIMENSION(DIM1,DIM2) :: buf4r      ! Data buffer
INTEGER        :: errcode                            ! Error flag
INTEGER        :: i, j, n                            ! general purpose integers

CALL test_begin(' Make/Read datasets (2D)        ')


!
! Initialize the data arrays.
!
n=1
DO i = 1, DIM1*DIM2
   buf(i) = n;
   n = n + 1
END DO

DO i = 1, dims(1)
 DO j = 1, dims(2)
  buf2(i,j) = (i-1)*dims(2) + j;
  buf3(i,j) = (i-1)*dims(2) + j;
  buf4(i,j) = (i-1)*dims(2) + j;
 END DO
END DO


!
! Initialize FORTRAN predefined datatypes.
!
CALL h5open_f(errcode)

!
! Create a new file using default properties.
!
CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

!-------------------------------------------------------------------------
! H5T_NATIVE_INT 1D buffer
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname1, rank, dims, H5T_NATIVE_INTEGER, buf, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname1, H5T_NATIVE_INTEGER, bufr, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, DIM1*DIM2
  IF ( buf(i) .NE. bufr(i) ) THEN
    PRINT *, 'read buffer differs from write buffer'
    PRINT *,  bufr(i), ' and ',   buf(i)
    STOP
  ENDIF
END DO

!-------------------------------------------------------------------------
! H5T_NATIVE_INT 2D buffer
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims, H5T_NATIVE_INTEGER, buf2, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, buf2r, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, dims(1)
 DO j = 1, dims(2)
  IF ( buf2(i,j) .NE. buf2r(i,j) ) THEN
    PRINT *, 'read buffer differs from write buffer'
    PRINT *,  buf2r(i,j), ' and ',   buf2(i,j)
    STOP
  ENDIF
 END DO
END DO

!-------------------------------------------------------------------------
! H5T_NATIVE_REAL
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_REAL, buf3, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, buf3r, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, dims(1)
 DO j = 1, dims(2)
  IF ( buf3(i,j) .NE. buf3r(i,j) ) THEN
    PRINT *, 'read buffer differs from write buffer'
    PRINT *,  buf3r(i,j), ' and ',   buf3(i,j)
    STOP
  ENDIF
 END DO
END DO

!-------------------------------------------------------------------------
! H5T_NATIVE_DOUBLE
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims, H5T_NATIVE_DOUBLE, buf4, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, buf4r, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, dims(1)
 DO j = 1, dims(2)
  IF ( buf4(i,j) .NE. buf4r(i,j) ) THEN
    PRINT *, 'read buffer differs from write buffer'
    PRINT *,  buf4r(i,j), ' and ',   buf4(i,j)
    STOP
  ENDIF
 END DO
END DO

!
! Close the file.
!
CALL h5fclose_f(file_id, errcode)

!
! Close FORTRAN predefined datatypes.
!
CALL h5close_f(errcode)

CALL passed()
!
! end function.
!
END SUBROUTINE test_dataset2D


!-------------------------------------------------------------------------
! test_dataset3D
!-------------------------------------------------------------------------


SUBROUTINE test_dataset3D()

USE H5LT ! module of H5LT
USE HDF5 ! module of HDF5 library

IMPLICIT NONE

INTEGER, PARAMETER :: DIM1 = 6;                             ! columns
INTEGER, PARAMETER :: DIM2 = 4;                             ! rows
INTEGER, PARAMETER :: DIM3 = 2;                             ! layers
CHARACTER(len=9), PARAMETER :: filename = "dsetf3.h5"       ! File name
CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"          ! Dataset name
CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"          ! Dataset name
CHARACTER(LEN=5), PARAMETER :: dsetname3 = "dset3"          ! Dataset name
CHARACTER(LEN=5), PARAMETER :: dsetname4 = "dset4"          ! Dataset name
INTEGER(HID_T) :: file_id                                   ! File identifier
INTEGER(HSIZE_T), DIMENSION(3) :: dims = (/DIM1,DIM2,DIM3/) ! Dataset dimensions
INTEGER(HSIZE_T), DIMENSION(3) :: dimsr                     ! Dataset dimensions
INTEGER, DIMENSION(DIM1*DIM2*DIM3) :: buf                   ! Data buffer
INTEGER, DIMENSION(DIM1*DIM2*DIM3) :: bufr                  ! Data buffer
INTEGER, DIMENSION(DIM1,DIM2,DIM3) :: buf2                  ! Data buffer
INTEGER, DIMENSION(DIM1,DIM2,DIM3) :: buf2r                 ! Data buffer
REAL, DIMENSION(DIM1,DIM2,DIM3)    :: buf3                  ! Data buffer
REAL, DIMENSION(DIM1,DIM2,DIM3)    :: buf3r                 ! Data buffer
DOUBLE PRECISION, DIMENSION(DIM1,DIM2,DIM3) :: buf4         ! Data buffer
DOUBLE PRECISION, DIMENSION(DIM1,DIM2,DIM3) :: buf4r        ! Data buffer
INTEGER        :: rank = 3                                  ! Dataset rank
INTEGER        :: errcode                                   ! Error flag
INTEGER        :: i, j, k, n                                ! general purpose integers
INTEGER          :: type_class
INTEGER(SIZE_T)  :: type_size

CALL test_begin(' Make/Read datasets (3D)        ')


!
! Initialize the data array.
!
n=1
DO i = 1, DIM1*DIM2*DIM3
   buf(i) = n;
   n = n + 1
END DO

n = 1
DO i = 1, dims(1)
 DO j = 1, dims(2)
 DO k = 1, dims(3)
  buf2(i,j,k) = n;
  buf3(i,j,k) = n;
  buf4(i,j,k) = n;
  n = n + 1
 END DO
 END DO
END DO

!
! Initialize FORTRAN predefined datatypes.
!
CALL h5open_f(errcode)

!
! Create a new file using default properties.
!
CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

!-------------------------------------------------------------------------
! H5T_NATIVE_INT 1D buffer
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname1, rank, dims, H5T_NATIVE_INTEGER, buf, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname1, H5T_NATIVE_INTEGER, bufr, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, DIM1*DIM2*DIM3
  IF ( buf(i) .NE. bufr(i) ) THEN
    PRINT *, 'read buffer differs from write buffer'
    PRINT *,  bufr(i), ' and ',   buf(i)
    STOP
  ENDIF
END DO

!-------------------------------------------------------------------------
! H5T_NATIVE_INT 3D buffer
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims, H5T_NATIVE_INTEGER, buf2, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, buf2r, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, dims(1)
 DO j = 1, dims(2)
 DO k = 1, dims(3)
  IF ( buf2(i,j,k) .NE. buf2r(i,j,k) ) THEN
    PRINT *, 'read buffer differs from write buffer'
    PRINT *,  buf2r(i,j,k), ' and ',   buf2(i,j,k)
    STOP
  ENDIF
 END DO
 END DO
END DO

!-------------------------------------------------------------------------
! H5T_NATIVE_REAL
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims, H5T_NATIVE_REAL, buf3, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, buf3r, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, dims(1)
 DO j = 1, dims(2)
 DO k = 1, dims(3)
  IF ( buf3(i,j,k) .NE. buf3r(i,j,k) ) THEN
    PRINT *, 'read buffer differs from write buffer'
    PRINT *,  buf3r(i,j,k), ' and ',   buf3(i,j,k)
    STOP
  ENDIF
 END DO
 END DO
END DO

!-------------------------------------------------------------------------
! H5T_NATIVE_DOUBLE
!-------------------------------------------------------------------------

!
! write dataset.
!
CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims, H5T_NATIVE_DOUBLE, buf4, errcode)

!
! read dataset.
!
CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, buf4r, dims, errcode)

!
! compare read and write buffers.
!
DO i = 1, dims(1)
 DO j = 1, dims(2)
 DO k = 1, dims(3)
  IF ( buf4(i,j,k) .NE. buf4r(i,j,k) ) THEN
    PRINT *, 'read buffer differs from write buffer'
    PRINT *,  buf4r(i,j,k), ' and ',   buf4(i,j,k)
    STOP
  ENDIF
 END DO
 END DO
END DO

CALL h5ltget_dataset_info_f(file_id,dsetname4,dimsr,type_class,type_size,errcode )

!
! compare dimensions
!
DO i = 1, rank
 IF ( dimsr(i) .NE. dims(i) ) THEN
   PRINT *, 'dimensions differ '
   STOP
  ENDIF
END DO

!
! Close the file.
!
CALL h5fclose_f(file_id, errcode)

!
! Close FORTRAN predefined datatypes.
!
CALL h5close_f(errcode)

CALL passed()
!
! end function.
!
END SUBROUTINE test_dataset3D

!-------------------------------------------------------------------------
! test_datasetND
!-------------------------------------------------------------------------


SUBROUTINE test_datasetND(rank)

  USE H5LT ! module of H5LT
  USE HDF5 ! module of HDF5 library

  IMPLICIT NONE

  INTEGER            :: rank                                  ! Dataset rank

  INTEGER, PARAMETER :: DIM1 = 2                              ! columns
  INTEGER, PARAMETER :: DIM2 = 4                              ! rows
  INTEGER, PARAMETER :: DIM3 = 2                              ! layers
  INTEGER, PARAMETER :: DIM4 = 5                              ! columns
  INTEGER, PARAMETER :: DIM5 = 4                              ! rows
  INTEGER, PARAMETER :: DIM6 = 3                              ! layers
  INTEGER, PARAMETER :: DIM7 = 2                              ! layers
  CHARACTER(len=9), PARAMETER :: filename = "dsetf3.h5"       ! File name
  CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"          ! Dataset name
  CHARACTER(LEN=5), PARAMETER :: dsetname3 = "dset3"          ! Dataset name
  CHARACTER(LEN=5), PARAMETER :: dsetname4 = "dset4"          ! Dataset name
  INTEGER(HID_T) :: file_id                                   ! File identifier
  INTEGER(HSIZE_T), DIMENSION(7) :: dims
  INTEGER(HSIZE_T), DIMENSION(7) :: dimsr                     ! Dataset dimensions
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:) :: ibuf_4          ! Data buffer
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:) :: ibufr_4         ! Data buffer
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: ibuf_5        ! Data buffer
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: ibufr_5       ! Data buffer
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: ibuf_6      ! Data buffer
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: ibufr_6     ! Data buffer
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:) :: ibuf_7    ! Data buffer
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:) :: ibufr_7   ! Data buffer
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: rbuf_4                ! Data buffer
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: rbufr_4               ! Data buffer
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: rbuf_5              ! Data buffer
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: rbufr_5             ! Data buffer
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: rbuf_6            ! Data buffer
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: rbufr_6           ! Data buffer
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:) :: rbuf_7          ! Data buffer
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:) :: rbufr_7         ! Data buffer
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:) :: dbuf_4    ! Data buffer
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:) :: dbufr_4            ! Data buffer
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: dbuf_5           ! Data buffer
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:) :: dbufr_5          ! Data buffer
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: dbuf_6         ! Data buffer
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:,:) :: dbufr_6        ! Data buffer
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:) :: dbuf_7       ! Data buffer
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:) :: dbufr_7      ! Data buffer
  INTEGER        :: errcode                                   ! Error flag
  INTEGER        :: i, j, k, l, m, n, o, nn                   ! general purpose integers
  INTEGER          :: type_class
  INTEGER(SIZE_T)  :: type_size
  CHARACTER(LEN=1) :: ichr1

  WRITE(ichr1,'(I1.1)') rank
  CALL test_begin(' Make/Read datasets ('//ichr1//'D)        ')
!
! Initialize the data array.
!
  IF(rank.EQ.4)THEN
     
     ALLOCATE(ibuf_4 (1:DIM1,1:DIM2,1:DIM3,1:DIM4))
     ALLOCATE(ibufr_4(1:DIM1,1:DIM2,1:DIM3,1:DIM4))
     ALLOCATE(rbuf_4 (1:DIM1,1:DIM2,1:DIM3,1:DIM4))
     ALLOCATE(rbufr_4(1:DIM1,1:DIM2,1:DIM3,1:DIM4))
     ALLOCATE(dbuf_4 (1:DIM1,1:DIM2,1:DIM3,1:DIM4))
     ALLOCATE(dbufr_4(1:DIM1,1:DIM2,1:DIM3,1:DIM4))

     dims(1:7) = (/DIM1,DIM2,DIM3,DIM4,0,0,0/)
     
     nn = 1
     DO i = 1, DIM1
        DO j = 1, DIM2
           DO k = 1, DIM3
              DO l = 1, DIM4
                 ibuf_4(i,j,k,l) = nn
                 rbuf_4(i,j,k,l) = nn
                 dbuf_4(i,j,k,l) = nn
                 nn = nn + 1
              END DO
           END DO
        END DO
     ENDDO
     
  ELSE IF(rank.EQ.5)THEN

     ALLOCATE(ibuf_5 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
     ALLOCATE(ibufr_5(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
     ALLOCATE(rbuf_5 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
     ALLOCATE(rbufr_5(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
     ALLOCATE(dbuf_5 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))
     ALLOCATE(dbufr_5(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5))

     dims(1:7) = (/DIM1,DIM2,DIM3,DIM4,DIM5,0,0/)
     
     nn = 1
     DO i = 1, DIM1
        DO j = 1, DIM2
           DO k = 1, DIM3
              DO l = 1, DIM4
                 DO m = 1, DIM5
                    ibuf_5(i,j,k,l,m) = nn
                    rbuf_5(i,j,k,l,m) = nn
                    dbuf_5(i,j,k,l,m) = nn
                    nn = nn + 1
                 END DO
              END DO
           END DO
        ENDDO
     ENDDO

  ELSE IF(rank.EQ.6)THEN

     ALLOCATE(ibuf_6 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
     ALLOCATE(ibufr_6(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
     ALLOCATE(rbuf_6 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
     ALLOCATE(rbufr_6(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
     ALLOCATE(dbuf_6 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))
     ALLOCATE(dbufr_6(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6))

     dims(1:7) = (/DIM1,DIM2,DIM3,DIM4,DIM5,DIM6,0/)
     
     nn = 1
     DO i = 1, DIM1
        DO j = 1, DIM2
           DO k = 1, DIM3
              DO l = 1, DIM4
                 DO m = 1, DIM5
                    DO n = 1, DIM6
                       ibuf_6(i,j,k,l,m,n) = nn
                       rbuf_6(i,j,k,l,m,n) = nn
                       dbuf_6(i,j,k,l,m,n) = nn
                       nn = nn + 1
                    END DO
                 END DO
              END DO
           ENDDO
        ENDDO
     ENDDO
     
  ELSE IF(rank.EQ.7)THEN
     
     ALLOCATE(ibuf_7 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
     ALLOCATE(ibufr_7(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
     ALLOCATE(rbuf_7 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
     ALLOCATE(rbufr_7(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
     ALLOCATE(dbuf_7 (1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))
     ALLOCATE(dbufr_7(1:DIM1,1:DIM2,1:DIM3,1:DIM4,1:DIM5,1:DIM6,1:DIM7))

     dims(1:7) = (/DIM1,DIM2,DIM3,DIM4,DIM5,DIM6,DIM7/)
     
     nn = 1
     DO i = 1, DIM1
        DO j = 1, DIM2
           DO k = 1, DIM3
              DO l = 1, DIM4
                 DO m = 1, DIM5
                    DO n = 1, DIM6
                       DO o = 1, DIM7
                          ibuf_7(i,j,k,l,m,n,o) = nn
                          rbuf_7(i,j,k,l,m,n,o) = nn
                          dbuf_7(i,j,k,l,m,n,o) = nn
                          nn = nn + 1
                       END DO
                    END DO
                 END DO
              ENDDO
           ENDDO
        ENDDO
     ENDDO

  ENDIF

  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(errcode)

  !
  ! Create a new file using default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

  !-------------------------------------------------------------------------
  ! H5T_NATIVE_INT ND buffer
  !-------------------------------------------------------------------------

  !
  ! write dataset.
  !
  IF(rank.EQ.4)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims(1:rank), H5T_NATIVE_INTEGER, ibuf_4, errcode)
  ELSE IF(rank.EQ.5)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims(1:rank), H5T_NATIVE_INTEGER, ibuf_5, errcode)
  ELSE IF(rank.EQ.6)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims(1:rank), H5T_NATIVE_INTEGER, ibuf_6, errcode)
  ELSE IF(rank.EQ.7)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname2, rank, dims(1:rank), H5T_NATIVE_INTEGER, ibuf_7, errcode)
  ENDIF
     

  !
  ! read dataset.
  !
  IF(rank.EQ.4)THEN
     CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, ibufr_4, dims(1:rank), errcode)
  ELSE IF(rank.EQ.5)THEN
     CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, ibufr_5, dims(1:rank), errcode)
  ELSE IF(rank.EQ.6)THEN
     CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, ibufr_6, dims(1:rank), errcode)
  ELSE IF(rank.EQ.7)THEN
     CALL h5ltread_dataset_f(file_id, dsetname2, H5T_NATIVE_INTEGER, ibufr_7, dims(1:rank), errcode)
  ENDIF

  !
  ! compare read and write buffers.
  !
  DO i = 1, dims(1)
     DO j = 1, dims(2)
        DO k = 1, dims(3)
           DO l = 1, dims(4)
              IF(rank.EQ.4)THEN
                 IF ( ibuf_4(i,j,k,l) .NE. ibufr_4(i,j,k,l) ) THEN
                    PRINT *, 'read buffer differs from write buffer'
                    PRINT *,  ibuf_4(i,j,k,l), ' and ', ibufr_4(i,j,k,l)
                    STOP
                 ENDIF
              ENDIF
              DO m = 1, dims(5)
                 IF(rank.EQ.5)THEN
                    IF ( ibuf_5(i,j,k,l,m) .NE. ibufr_5(i,j,k,l,m) ) THEN
                       PRINT *, 'read buffer differs from write buffer'
                       PRINT *,  ibuf_5(i,j,k,l,m), ' and ', ibufr_5(i,j,k,l,m)
                       STOP
                    ENDIF
                 ENDIF
                 DO n = 1, dims(6)
                    IF(rank.EQ.6)THEN
                       IF ( ibuf_6(i,j,k,l,m,n) .NE. ibufr_6(i,j,k,l,m,n) ) THEN
                          PRINT *, 'read buffer differs from write buffer'
                          PRINT *,  ibuf_6(i,j,k,l,m,n), ' and ', ibufr_6(i,j,k,l,m,n)
                          STOP
                       ENDIF
                    ENDIF
                    DO o = 1, dims(7)
                       IF(rank.EQ.7)THEN
                          IF ( ibuf_7(i,j,k,l,m,n,o) .NE. ibufr_7(i,j,k,l,m,n,o) ) THEN
                             PRINT *, 'read buffer differs from write buffer'
                             PRINT *,  ibuf_7(i,j,k,l,m,n,o), ' and ', ibufr_7(i,j,k,l,m,n,o)
                             STOP
                          ENDIF
                       ENDIF
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ENDDO
  !-------------------------------------------------------------------------
  ! H5T_NATIVE_REAL
  !-------------------------------------------------------------------------

  !
  ! write dataset.
  !
  IF(rank.EQ.4)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, rbuf_4, errcode)
  ELSE IF(rank.EQ.5)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, rbuf_5, errcode)
  ELSE IF(rank.EQ.6)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, rbuf_6, errcode)
  ELSE IF(rank.EQ.7)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname3, rank, dims(1:rank), H5T_NATIVE_REAL, rbuf_7, errcode)
  ENDIF
     

  !
  ! read dataset.
  !
  IF(rank.EQ.4)THEN
     CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, rbufr_4, dims(1:rank), errcode)
  ELSE IF(rank.EQ.5)THEN
     CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, rbufr_5, dims(1:rank), errcode)
  ELSE IF(rank.EQ.6)THEN
     CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, rbufr_6, dims(1:rank), errcode)
  ELSE IF(rank.EQ.7)THEN
     CALL h5ltread_dataset_f(file_id, dsetname3, H5T_NATIVE_REAL, rbufr_7, dims(1:rank), errcode)
  ENDIF

  !
  ! compare read and write buffers.
  !
  DO i = 1, dims(1)
     DO j = 1, dims(2)
        DO k = 1, dims(3)
           DO l = 1, dims(4)
              IF(rank.EQ.4)THEN
                 IF ( rbuf_4(i,j,k,l) .NE. rbufr_4(i,j,k,l) ) THEN
                    PRINT *, 'read buffer differs from write buffer'
                    PRINT *,  rbuf_4(i,j,k,l), ' and ', rbufr_4(i,j,k,l)
                    STOP
                 ENDIF
              ENDIF
              DO m = 1, dims(5)
                 IF(rank.EQ.5)THEN
                    IF ( rbuf_5(i,j,k,l,m) .NE. rbufr_5(i,j,k,l,m) ) THEN
                       PRINT *, 'read buffer differs from write buffer'
                       PRINT *,  rbuf_5(i,j,k,l,m), ' and ', rbufr_5(i,j,k,l,m)
                       STOP
                    ENDIF
                 ENDIF
                 DO n = 1, dims(6)
                    IF(rank.EQ.6)THEN
                       IF ( rbuf_6(i,j,k,l,m,n) .NE. rbufr_6(i,j,k,l,m,n) ) THEN
                          PRINT *, 'read buffer differs from write buffer'
                          PRINT *,  rbuf_6(i,j,k,l,m,n), ' and ', rbufr_6(i,j,k,l,m,n)
                          STOP
                       ENDIF
                    ENDIF
                    DO o = 1, dims(7)
                       IF(rank.EQ.7)THEN
                          IF ( rbuf_7(i,j,k,l,m,n,o) .NE. rbufr_7(i,j,k,l,m,n,o) ) THEN
                             PRINT *, 'read buffer differs from write buffer'
                             PRINT *,  rbuf_7(i,j,k,l,m,n,o), ' and ', rbufr_7(i,j,k,l,m,n,o)
                             STOP
                          ENDIF
                       ENDIF
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ENDDO

  !-------------------------------------------------------------------------
  ! H5T_NATIVE_DOUBLE
  !-------------------------------------------------------------------------

  !
  ! write dataset.
  !
  IF(rank.EQ.4)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims(1:rank), H5T_NATIVE_DOUBLE, dbuf_4, errcode)
  ELSE IF(rank.EQ.5)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims(1:rank), H5T_NATIVE_DOUBLE, dbuf_5, errcode)
  ELSE IF(rank.EQ.6)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims(1:rank), H5T_NATIVE_DOUBLE, dbuf_6, errcode)
  ELSE IF(rank.EQ.7)THEN
     CALL h5ltmake_dataset_f(file_id, dsetname4, rank, dims(1:rank), H5T_NATIVE_DOUBLE, dbuf_7, errcode)
  ENDIF
     

  !
  ! read dataset.
  !
  IF(rank.EQ.4)THEN
     CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, dbufr_4, dims(1:rank), errcode)
  ELSE IF(rank.EQ.5)THEN
     CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, dbufr_5, dims(1:rank), errcode)
  ELSE IF(rank.EQ.6)THEN
     CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, dbufr_6, dims(1:rank), errcode)
  ELSE IF(rank.EQ.7)THEN
     CALL h5ltread_dataset_f(file_id, dsetname4, H5T_NATIVE_DOUBLE, dbufr_7, dims(1:rank), errcode)
  ENDIF

  !
  ! compare read and write buffers.
  !
  DO i = 1, dims(1)
     DO j = 1, dims(2)
        DO k = 1, dims(3)
           DO l = 1, dims(4)
              IF(rank.EQ.4)THEN
                 IF ( dbuf_4(i,j,k,l) .NE. dbufr_4(i,j,k,l) ) THEN
                    PRINT *, 'read buffer differs from write buffer'
                    PRINT *,  dbuf_4(i,j,k,l), ' and ', dbufr_4(i,j,k,l)
                    STOP
                 ENDIF
              ENDIF
              DO m = 1, dims(5)
                 IF(rank.EQ.5)THEN
                    IF ( dbuf_5(i,j,k,l,m) .NE. dbufr_5(i,j,k,l,m) ) THEN
                       PRINT *, 'read buffer differs from write buffer'
                       PRINT *,  dbuf_5(i,j,k,l,m), ' and ', dbufr_5(i,j,k,l,m)
                       STOP
                    ENDIF
                 ENDIF
                 DO n = 1, dims(6)
                    IF(rank.EQ.6)THEN
                       IF ( dbuf_6(i,j,k,l,m,n) .NE. dbufr_6(i,j,k,l,m,n) ) THEN
                          PRINT *, 'read buffer differs from write buffer'
                          PRINT *,  dbuf_6(i,j,k,l,m,n), ' and ', dbufr_6(i,j,k,l,m,n)
                          STOP
                       ENDIF
                    ENDIF
                    DO o = 1, dims(7)
                       IF(rank.EQ.7)THEN
                          IF ( dbuf_7(i,j,k,l,m,n,o) .NE. dbufr_7(i,j,k,l,m,n,o) ) THEN
                             PRINT *, 'read buffer differs from write buffer'
                             PRINT *,  dbuf_7(i,j,k,l,m,n,o), ' and ', dbufr_7(i,j,k,l,m,n,o)
                             STOP
                          ENDIF
                       ENDIF
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ENDDO

  CALL h5ltget_dataset_info_f(file_id,dsetname4,dimsr,type_class,type_size,errcode )

  !
  ! compare dimensions
  !
  DO i = 1, rank
     IF ( dimsr(i) .NE. dims(i) ) THEN
        PRINT *, 'dimensions differ '
        STOP
     ENDIF
  END DO

  !
  ! Close the file.
  !
  CALL h5fclose_f(file_id, errcode)

  !
  ! Close FORTRAN predefined datatypes.
  !
  CALL h5close_f(errcode)

  ! DEALLOCATE RESOURCES

  IF(rank.EQ.4)THEN
     DEALLOCATE(ibuf_4, ibufr_4, rbuf_4, rbufr_4, dbuf_4, dbufr_4)
  ELSE IF(rank.EQ.5)THEN
     DEALLOCATE(ibuf_5, ibufr_5, rbuf_5, rbufr_5, dbuf_5, dbufr_5)
  ELSE IF(rank.EQ.6)THEN
     DEALLOCATE(ibuf_6, ibufr_6, rbuf_6, rbufr_6, dbuf_6, dbufr_6)
  ELSE IF(rank.EQ.7)THEN
     DEALLOCATE(ibuf_7, ibufr_7, rbuf_7, rbufr_7, dbuf_7, dbufr_7)
  ENDIF

  CALL passed()
  !
  ! end function.
  !
END SUBROUTINE test_datasetND



!-------------------------------------------------------------------------
! test_datasets
!-------------------------------------------------------------------------

SUBROUTINE test_datasets()

  USE H5LT ! module of H5LT
  USE HDF5 ! module of HDF5 library

  IMPLICIT NONE

  CHARACTER(len=9), PARAMETER :: filename = "dsetf4.h5"! File name
  INTEGER(HID_T) :: file_id                            ! File identifier
  INTEGER        :: errcode                            ! Error flag
  INTEGER, PARAMETER :: DIM1 = 10;                     ! Dimension of array
  CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"   ! Dataset name
  CHARACTER(LEN=5), PARAMETER :: dsetname2 = "dset2"   ! Dataset name
  CHARACTER(LEN=5), PARAMETER :: dsetname3 = "dset3"   ! Dataset name
  CHARACTER(LEN=5), PARAMETER :: dsetname4 = "dset4"   ! Dataset name
  CHARACTER(LEN=5), PARAMETER :: dsetname5 = "dset5"   ! Dataset name
  INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/DIM1/)    ! Dataset dimensions
  INTEGER(HSIZE_T), DIMENSION(1) :: dimsr              ! Dataset dimensions
  INTEGER        :: rank = 1                           ! Dataset rank
  INTEGER        :: rankr                              ! Dataset rank
  CHARACTER(LEN=8), PARAMETER :: buf1 = "mystring"     ! Data buffer
  CHARACTER(LEN=8)            :: buf1r                 ! Data buffer
  INTEGER, DIMENSION(DIM1)          :: buf2            ! Data buffer
  INTEGER, DIMENSION(DIM1)          :: bufr2           ! Data buffer
  REAL, DIMENSION(DIM1)             :: buf3            ! Data buffer
  REAL, DIMENSION(DIM1)             :: bufr3           ! Data buffer
  DOUBLE PRECISION, DIMENSION(DIM1) :: buf4            ! Data buffer
  DOUBLE PRECISION, DIMENSION(DIM1) :: bufr4           ! Data buffer
  INTEGER          :: i, n                             ! general purpose integer
  INTEGER          :: has                              ! general purpose integer
  INTEGER          :: type_class
  INTEGER(SIZE_T)  :: type_size

  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(errcode)

  !
  ! Create a new file using default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)

  !
  ! Initialize the data array.
  !
  n = 1
  DO i = 1, DIM1
     buf2(i) = n;
     buf3(i) = n;
     buf4(i) = n;
     n = n + 1;
  END DO

  !-------------------------------------------------------------------------
  ! int
  !-------------------------------------------------------------------------

  CALL test_begin(' Make/Read datasets (integer)   ')

  !
  ! write dataset.
  !
  CALL h5ltmake_dataset_int_f(file_id, dsetname2, rank, dims, buf2, errcode)

  !
  ! read dataset.
  !
  CALL h5ltread_dataset_int_f(file_id, dsetname2, bufr2, dims, errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, DIM1
     IF ( buf2(i) .NE. bufr2(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufr2(i), ' and ',   buf2(i)
        STOP
     ENDIF
  END DO

  CALL passed()

  !-------------------------------------------------------------------------
  ! real
  !-------------------------------------------------------------------------

  CALL test_begin(' Make/Read datasets (float)     ')


  !
  ! write dataset.
  !
  CALL h5ltmake_dataset_float_f(file_id, dsetname3, rank, dims, buf3, errcode)

  !
  ! read dataset.
  !
  CALL h5ltread_dataset_float_f(file_id, dsetname3, bufr3, dims, errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, DIM1
     IF ( buf3(i) .NE. bufr3(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufr3(i), ' and ',   buf3(i)
        STOP
     ENDIF
  END DO

  CALL passed()

  !-------------------------------------------------------------------------
  ! double
  !-------------------------------------------------------------------------

  CALL test_begin(' Make/Read datasets (double)    ')


  !
  ! write dataset.
  !
  CALL h5ltmake_dataset_double_f(file_id, dsetname4, rank, dims, buf4, errcode)

  !
  ! read dataset.
  !
  CALL h5ltread_dataset_double_f(file_id, dsetname4, bufr4, dims, errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, DIM1
     IF ( buf4(i) .NE. bufr4(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufr4(i), ' and ',   buf4(i)
        STOP
     ENDIF
  END DO

  CALL passed()


  !-------------------------------------------------------------------------
  ! string
  !-------------------------------------------------------------------------

  CALL test_begin(' Make/Read datasets (string)    ')


  !
  ! write dataset.
  !
  CALL h5ltmake_dataset_string_f(file_id, dsetname5, buf1, errcode)

  !
  ! read dataset.
  !
  CALL h5ltread_dataset_string_f(file_id, dsetname5, buf1r, errcode)

  !
  ! compare read and write buffers.
  !
  IF ( buf1 .NE. buf1r ) THEN
     PRINT *, 'read buffer differs from write buffer'
     PRINT *,  buf1, ' and ',   buf1r
     STOP
  ENDIF

  CALL passed()






  CALL test_begin(' Get dataset dimensions/info    ')

  !-------------------------------------------------------------------------
  ! h5ltget_dataset_ndims_f
  !-------------------------------------------------------------------------

  CALL h5ltget_dataset_ndims_f(file_id, dsetname4, rankr, errcode)
  IF ( rankr .NE. rank ) THEN
     PRINT *, 'h5ltget_dataset_ndims_f return error'
     STOP
  ENDIF


  !-------------------------------------------------------------------------
  ! test h5ltfind_dataset_f function
  !-------------------------------------------------------------------------


  has = h5ltfind_dataset_f(file_id,dsetname4)
  IF ( has .NE. 1 ) THEN
     PRINT *, 'h5ltfind_dataset_f return error'
     STOP
  ENDIF

  !-------------------------------------------------------------------------
  ! test h5ltget_dataset_info_f function
  !-------------------------------------------------------------------------


  CALL h5ltget_dataset_info_f(file_id,dsetname4,dimsr,type_class,type_size,errcode )

  !
  ! compare dimensions
  !
  DO i = 1, rank
     IF ( dimsr(i) .NE. dims(i) ) THEN
        PRINT *, 'dimensions differ '
        STOP
     ENDIF
  END DO

  IF ( type_class .NE. 1 ) THEN ! H5T_FLOAT
     PRINT *, 'wrong type class '
     STOP
  ENDIF

  !
  ! Close the file.
  !
  CALL h5fclose_f(file_id, errcode)
  !
  ! Close FORTRAN predefined datatypes.
  !
  CALL h5close_f(errcode)

  CALL passed()
  !
  ! end function.
  !
END SUBROUTINE test_datasets



!-------------------------------------------------------------------------
! test_attributes
!-------------------------------------------------------------------------

SUBROUTINE test_attributes()

  USE H5LT ! module of H5LT
  USE HDF5 ! module of HDF5 library

  IMPLICIT NONE

  CHARACTER(len=9), PARAMETER :: filename = "dsetf5.h5"! File name
  INTEGER(HID_T) :: file_id                            ! File identifier
  INTEGER, PARAMETER :: DIM1 = 10;                     ! Dimension of array
  CHARACTER(LEN=5), PARAMETER :: attrname1 = "attr1"   ! Attribute name
  CHARACTER(LEN=5), PARAMETER :: attrname2 = "attr2"   ! Attribute name
  CHARACTER(LEN=5), PARAMETER :: attrname3 = "attr3"   ! Attribute name
  CHARACTER(LEN=5), PARAMETER :: attrname4 = "attr4"   ! Attribute name
  CHARACTER(LEN=5), PARAMETER :: attrname5 = "attr5"   ! Attribute name
  CHARACTER(LEN=8), PARAMETER :: buf1 = "mystring"     ! Data buffer
  CHARACTER(LEN=8)                  :: bufr1           ! Data buffer
  INTEGER, DIMENSION(DIM1)          :: buf2            ! Data buffer
  INTEGER, DIMENSION(DIM1)          :: bufr2           ! Data buffer
  REAL, DIMENSION(DIM1)             :: buf3            ! Data buffer
  REAL, DIMENSION(DIM1)             :: bufr3           ! Data buffer
  DOUBLE PRECISION, DIMENSION(DIM1) :: buf4            ! Data buffer
  DOUBLE PRECISION, DIMENSION(DIM1) :: bufr4           ! Data buffer
  INTEGER        :: errcode                            ! Error flag
  INTEGER        :: i, n                               ! general purpose integer
  INTEGER(SIZE_T) size                                 ! size of attribute array
  INTEGER        :: rankr                              ! rank
  INTEGER(HSIZE_T), DIMENSION(1) :: dimsr              ! attribute dimensions
  INTEGER          :: type_class
  INTEGER(SIZE_T)  :: type_size
  INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/DIM1/)    ! Dataset dimensions
  INTEGER        :: rank = 1                           ! Dataset rank
  CHARACTER(LEN=5), PARAMETER :: dsetname1 = "dset1"   ! Dataset name
  INTEGER, DIMENSION(DIM1)    :: buf                   ! Data buffer

  !
  ! Initialize FORTRAN predefined datatypes.
  !
  CALL h5open_f(errcode)
  !
  ! Create a new file using default properties.
  !
  CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, errcode)
  !
  ! make a dataset.
  !
  CALL h5ltmake_dataset_int_f(file_id, dsetname1, rank, dims, buf, errcode)

  !
  ! Initialize the data array.
  !
  size = DIM1
  n = 1
  DO i = 1, DIM1
     buf2(i) = n;
     buf3(i) = n;
     buf4(i) = n;
     n = n + 1;
  END DO


  !-------------------------------------------------------------------------
  ! int
  !-------------------------------------------------------------------------

  CALL test_begin(' Set/Get attributes int         ')


  !
  ! write attribute.
  !
  CALL h5ltset_attribute_int_f(file_id,dsetname1,attrname2,buf2,size,errcode)

  !
  ! read attribute.
  !
  CALL h5ltget_attribute_int_f(file_id,dsetname1,attrname2,bufr2,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, DIM1
     IF ( buf2(i) .NE. bufr2(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufr2(i), ' and ',   buf2(i)
        STOP
     ENDIF
  END DO

  CALL passed()

  !-------------------------------------------------------------------------
  ! float
  !-------------------------------------------------------------------------

  CALL test_begin(' Set/Get attributes float       ')


  !
  ! write attribute.
  !
  CALL h5ltset_attribute_float_f(file_id,dsetname1,attrname3,buf3,size,errcode)

  !
  ! read attribute.
  !
  CALL h5ltget_attribute_float_f(file_id,dsetname1,attrname3,bufr3,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, DIM1
     IF ( buf3(i) .NE. bufr3(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufr3(i), ' and ',   buf3(i)
        STOP
     ENDIF
  END DO


  CALL passed()

  !-------------------------------------------------------------------------
  ! double
  !-------------------------------------------------------------------------

  CALL test_begin(' Set/Get attributes double      ')


  !
  ! write attribute.
  !
  CALL h5ltset_attribute_double_f(file_id,dsetname1,attrname4,buf4,size,errcode)

  !
  ! read attribute.
  !
  CALL h5ltget_attribute_double_f(file_id,dsetname1,attrname4,bufr4,errcode)

  !
  ! compare read and write buffers.
  !
  DO i = 1, DIM1
     IF ( buf4(i) .NE. bufr4(i) ) THEN
        PRINT *, 'read buffer differs from write buffer'
        PRINT *,  bufr4(i), ' and ',   buf4(i)
        STOP
     ENDIF
  END DO

  CALL passed()



  !-------------------------------------------------------------------------
  ! string
  !-------------------------------------------------------------------------

  CALL test_begin(' Set/Get attributes string      ')


  !
  ! write attribute.
  !
  CALL h5ltset_attribute_string_f(file_id,dsetname1,attrname5,buf1,errcode)

  !
  ! read attribute.
  !
  CALL h5ltget_attribute_string_f(file_id,dsetname1,attrname5,bufr1,errcode)

  !
  ! compare read and write buffers.
  !

  IF ( buf1 .NE. bufr1 ) THEN
     PRINT *, 'read buffer differs from write buffer'
     PRINT *,  buf1, ' and ',   bufr1
     STOP
  ENDIF


  CALL passed()

  !-------------------------------------------------------------------------
  ! get attribute rank
  !-------------------------------------------------------------------------

  CALL test_begin(' Get attribute rank/info        ')


  CALL h5ltget_attribute_ndims_f(file_id,dsetname1,attrname2,rankr,errcode)

  IF ( rankr .NE. 1 ) THEN
     PRINT *, 'h5ltget_attribute_ndims_f return error'
     STOP
  ENDIF


  CALL h5ltget_attribute_info_f(file_id,dsetname1,attrname2,dimsr,type_class,type_size,errcode)

  !
  ! compare dimensions
  !
  DO i = 1, rank
     IF ( dimsr(i) .NE. dims(i) ) THEN
        PRINT *, 'dimensions differ '
        STOP
     ENDIF
  END DO


  !
  ! Close the file.
  !
  CALL h5fclose_f(file_id, errcode)
  !
  ! Close FORTRAN predefined datatypes.
  !
  CALL h5close_f(errcode)

  CALL passed()
  !
  ! end function.
  !
END SUBROUTINE test_attributes






!-------------------------------------------------------------------------
! test_begin
!-------------------------------------------------------------------------

SUBROUTINE test_begin(string)
  CHARACTER(LEN=*), INTENT(IN) :: string
  WRITE(*, fmt = '(14a)', advance = 'no') string
  WRITE(*, fmt = '(40x,a)', advance = 'no') ' '
END SUBROUTINE test_begin

!-------------------------------------------------------------------------
! passed
!-------------------------------------------------------------------------

SUBROUTINE passed()
  WRITE(*, fmt = '(6a)')  'PASSED'
END SUBROUTINE passed
