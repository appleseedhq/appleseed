!****h* fortran/src/H5_DBLE_InterfaceInclude.f90
!
! NAME
!   H5_DBLE_INTERFACE
!
! FUNCTION
!   This module is used for when the default REAL is not of the type DOUBLE PRECISION.
!   We only do not include the double precision interfaces if the defaut REAL is
!   DOUBLE PRECISION since this would lead to a non-unique conflict with the
!   generic interfaces declared as REAL. Otherwise it is okay to include the interfaces.
!
! NOTES
!   This module contains all the DOUBLE PRECISION interfaces and corresponding subroutines
!   from the HDF function catagory H5A, H5D and H5P.
!
! COPYRIGHT
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
! AUTHOR
!  M.S. Breitenfeld
!
!*****

MODULE H5_DBLE_INTERFACE

  USE H5GLOBAL

  !
  ! ----- H5A ----
  !
  INTERFACE h5awrite_f
     MODULE PROCEDURE h5awrite_double_scalar
     MODULE PROCEDURE h5awrite_double_1
     MODULE PROCEDURE h5awrite_double_2
     MODULE PROCEDURE h5awrite_double_3
     MODULE PROCEDURE h5awrite_double_4
     MODULE PROCEDURE h5awrite_double_5
     MODULE PROCEDURE h5awrite_double_6
     MODULE PROCEDURE h5awrite_double_7
  END INTERFACE

  INTERFACE h5aread_f
     MODULE PROCEDURE h5aread_double_scalar
     MODULE PROCEDURE h5aread_double_1
     MODULE PROCEDURE h5aread_double_2
     MODULE PROCEDURE h5aread_double_3
     MODULE PROCEDURE h5aread_double_4
     MODULE PROCEDURE h5aread_double_5
     MODULE PROCEDURE h5aread_double_6
     MODULE PROCEDURE h5aread_double_7
  END INTERFACE
  !
  ! ----- H5D ----
  !
  INTERFACE h5dwrite_f
     MODULE PROCEDURE h5dwrite_double_scalar
     MODULE PROCEDURE h5dwrite_double_1
     MODULE PROCEDURE h5dwrite_double_2
     MODULE PROCEDURE h5dwrite_double_3
     MODULE PROCEDURE h5dwrite_double_4
     MODULE PROCEDURE h5dwrite_double_5
     MODULE PROCEDURE h5dwrite_double_6
     MODULE PROCEDURE h5dwrite_double_7
  END INTERFACE

  INTERFACE h5dread_f
     MODULE PROCEDURE h5dread_double_scalar
     MODULE PROCEDURE h5dread_double_1
     MODULE PROCEDURE h5dread_double_2
     MODULE PROCEDURE h5dread_double_3
     MODULE PROCEDURE h5dread_double_4
     MODULE PROCEDURE h5dread_double_5
     MODULE PROCEDURE h5dread_double_6
     MODULE PROCEDURE h5dread_double_7
  END INTERFACE

  INTERFACE h5dfill_f
     MODULE PROCEDURE h5dfill_double
  END INTERFACE

  !
  ! ----- H5P ----
  !
  INTERFACE h5pset_fill_value_f
     MODULE PROCEDURE h5pset_fill_value_double
  END INTERFACE

  INTERFACE h5pget_fill_value_f
     MODULE PROCEDURE h5pget_fill_value_double
  END INTERFACE

  INTERFACE h5pset_f
     MODULE PROCEDURE h5pset_double
  END INTERFACE

  INTERFACE h5pget_f
     MODULE PROCEDURE h5pget_double
  END INTERFACE

  INTERFACE h5pregister_f
     MODULE PROCEDURE h5pregister_double
  END INTERFACE

  INTERFACE h5pinsert_f
     MODULE PROCEDURE h5pinsert_double
  END INTERFACE

CONTAINS

  !
  ! ----- H5A ----
  !
  SUBROUTINE h5awrite_double_scalar(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id    ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(IN) :: buf     ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_double_s_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_double_s_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_DOUBLE_S_C'::h5awrite_double_s_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(IN)::buf
       END FUNCTION h5awrite_double_s_c
    END INTERFACE

    hdferr = h5awrite_double_s_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_double_scalar

  SUBROUTINE h5awrite_double_1(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1)) :: buf ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_double_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_double_1_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_DOUBLE_1_C'::h5awrite_double_1_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5awrite_double_1_c
    END INTERFACE

    hdferr = h5awrite_double_1_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_double_1


  SUBROUTINE h5awrite_double_2(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_double_2_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_double_2_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_DOUBLE_2_C'::h5awrite_double_2_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5awrite_double_2_c
    END INTERFACE

    hdferr = h5awrite_double_2_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_double_2


  SUBROUTINE h5awrite_double_3(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_double_3_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_double_3_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_DOUBLE_3_C'::h5awrite_double_3_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5awrite_double_3_c
    END INTERFACE

    hdferr = h5awrite_double_3_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_double_3


  SUBROUTINE h5awrite_double_4(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_double_4_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_double_4_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_DOUBLE_4_C'::h5awrite_double_4_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5awrite_double_4_c
    END INTERFACE

    hdferr = h5awrite_double_4_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_double_4


  SUBROUTINE h5awrite_double_5(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_double_5_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_double_5_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_DOUBLE_5_C'::h5awrite_double_5_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5awrite_double_5_c
    END INTERFACE

    hdferr = h5awrite_double_5_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_double_5


  SUBROUTINE h5awrite_double_6(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_double_6_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_double_6_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_DOUBLE_6_C'::h5awrite_double_6_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5awrite_double_6_c
    END INTERFACE

    hdferr = h5awrite_double_6_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_double_6


  SUBROUTINE h5awrite_double_7(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5awrite_double_7_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5awrite_double_7_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_DOUBLE_7_C'::h5awrite_double_7_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5awrite_double_7_c
    END INTERFACE

    hdferr = h5awrite_double_7_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_double_7


  SUBROUTINE h5aread_double_scalar(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(INOUT) :: buf    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_double_s_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_double_s_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_DOUBLE_S_C'::h5aread_double_s_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(INOUT)::buf
       END FUNCTION h5aread_double_s_c
    END INTERFACE

    hdferr = h5aread_double_s_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5aread_double_scalar

  SUBROUTINE h5aread_double_1(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_double_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_double_1_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_DOUBLE_1_C'::h5aread_double_1_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5aread_double_1_c
    END INTERFACE

    hdferr = h5aread_double_1_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5aread_double_1


  SUBROUTINE h5aread_double_2(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_double_2_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_double_2_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_DOUBLE_2_C'::h5aread_double_2_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5aread_double_2_c
    END INTERFACE

    hdferr = h5aread_double_2_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5aread_double_2


  SUBROUTINE h5aread_double_3(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_double_3_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_double_3_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_DOUBLE_3_C'::h5aread_double_3_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5aread_double_3_c
    END INTERFACE

    hdferr = h5aread_double_3_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5aread_double_3


  SUBROUTINE h5aread_double_4(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_double_4_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_double_4_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_DOUBLE_4_C'::h5aread_double_4_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5aread_double_4_c
    END INTERFACE

    hdferr = h5aread_double_4_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5aread_double_4


  SUBROUTINE h5aread_double_5(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_double_5_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_double_5_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_DOUBLE_5_C'::h5aread_double_5_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5aread_double_5_c
    END INTERFACE

    hdferr = h5aread_double_5_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5aread_double_5


  SUBROUTINE h5aread_double_6(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_double_6_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_double_6_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_DOUBLE_6_C'::h5aread_double_6_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5aread_double_6_c
    END INTERFACE

    hdferr = h5aread_double_6_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5aread_double_6


  SUBROUTINE h5aread_double_7(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

    !            INTEGER, EXTERNAL :: h5aread_double_7_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5aread_double_7_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_DOUBLE_7_C'::h5aread_double_7_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5aread_double_7_c
    END INTERFACE

    hdferr = h5aread_double_7_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5aread_double_7


  !
  ! ----- H5D ----
  !
  SUBROUTINE h5dwrite_double_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dwrite_double_s_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dwrite_double_s_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_DOUBLE_S_C'::h5dwrite_double_s_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(IN) :: buf
       END FUNCTION h5dwrite_double_s_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_double_s_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_double_scalar

  SUBROUTINE h5dwrite_double_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dwrite_double_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dwrite_double_1_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_DOUBLE_1_C'::h5dwrite_double_1_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5dwrite_double_1_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_double_1_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_double_1

  SUBROUTINE h5dwrite_double_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dwrite_double_2_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dwrite_double_2_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_DOUBLE_2_C'::h5dwrite_double_2_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dwrite_double_2_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_double_2_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_double_2

  SUBROUTINE h5dwrite_double_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dwrite_double_3_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dwrite_double_3_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_DOUBLE_3_C'::h5dwrite_double_3_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5dwrite_double_3_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_double_3_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_double_3

  SUBROUTINE h5dwrite_double_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dwrite_double_4_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dwrite_double_4_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_DOUBLE_4_C'::h5dwrite_double_4_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5dwrite_double_4_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_double_4_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_double_4

  SUBROUTINE h5dwrite_double_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dwrite_double_5_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dwrite_double_5_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_DOUBLE_5_C'::h5dwrite_double_5_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5dwrite_double_5_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_double_5_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_double_5

  SUBROUTINE h5dwrite_double_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dwrite_double_6_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dwrite_double_6_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_DOUBLE_6_C'::h5dwrite_double_6_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5dwrite_double_6_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_double_6_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_double_6

  SUBROUTINE h5dwrite_double_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dwrite_double_7_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dwrite_double_7_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DWRITE_DOUBLE_7_C'::h5dwrite_double_7_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5dwrite_double_7_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dwrite_double_7_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dwrite_double_7

  SUBROUTINE h5dread_double_scalar(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT) :: buf ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dread_double_s_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_double_s_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_DOUBLE_S_C'::h5dread_double_s_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(OUT) :: buf
       END FUNCTION h5dread_double_s_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_double_s_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_double_scalar

  SUBROUTINE h5dread_double_1(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dread_double_1_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_double_1_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_DOUBLE_1_C'::h5dread_double_1_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5dread_double_1_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_double_1_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_double_1

  SUBROUTINE h5dread_double_2(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dread_double_2_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_double_2_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_DOUBLE_2_C'::h5dread_double_2_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5dread_double_2_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_double_2_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_double_2

  SUBROUTINE h5dread_double_3(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dread_double_3_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_double_3_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_DOUBLE_3_C'::h5dread_double_3_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5dread_double_3_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_double_3_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_double_3

  SUBROUTINE h5dread_double_4(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
    ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dread_double_4_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_double_4_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_DOUBLE_4_C'::h5dread_double_4_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5dread_double_4_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_double_4_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_double_4

  SUBROUTINE h5dread_double_5(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dread_double_5_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_double_5_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_DOUBLE_5_C'::h5dread_double_5_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5dread_double_5_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_double_5_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_double_5

  SUBROUTINE h5dread_double_6(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dread_double_6_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_double_6_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_DOUBLE_6_C'::h5dread_double_6_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5dread_double_6_c
    END INTERFACE


    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_double_6_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_double_6

  SUBROUTINE h5dread_double_7(dset_id, mem_type_id, buf, dims, hdferr, &
       mem_space_id, file_space_id, xfer_prp)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dset_id   ! Dataset identifier
    INTEGER(HID_T), INTENT(IN) :: mem_type_id ! Memory datatype identifier
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
    DOUBLE PRECISION, INTENT(INOUT), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    ! Data buffer
    INTEGER, INTENT(OUT) :: hdferr      ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: mem_space_id
    ! Memory dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: file_space_id
    ! File dataspace identfier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: xfer_prp
    ! Transfer property list identifier

    INTEGER(HID_T) :: xfer_prp_default
    INTEGER(HID_T) :: mem_space_id_default
    INTEGER(HID_T) :: file_space_id_default

    !            INTEGER, EXTERNAL :: h5dread_double_7_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dread_double_7_c(dset_id, mem_type_id, &
            mem_space_id_default, &
            file_space_id_default, &
            xfer_prp_default, buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DREAD_DOUBLE_7_C'::h5dread_double_7_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dset_id
         INTEGER(HID_T), INTENT(IN) :: mem_type_id
         INTEGER(HID_T)  :: mem_space_id_default
         INTEGER(HID_T) :: file_space_id_default
         INTEGER(HID_T) :: xfer_prp_default
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims
         DOUBLE PRECISION, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5dread_double_7_c
    END INTERFACE

    xfer_prp_default = H5P_DEFAULT_F
    mem_space_id_default = H5S_ALL_F
    file_space_id_default = H5S_ALL_F

    if (present(xfer_prp)) xfer_prp_default = xfer_prp
    if (present(mem_space_id))  mem_space_id_default = mem_space_id
    if (present(file_space_id)) file_space_id_default = file_space_id

    hdferr = h5dread_double_7_c(dset_id, mem_type_id, mem_space_id_default, &
         file_space_id_default, xfer_prp_default, buf, dims)

  END SUBROUTINE h5dread_double_7

  !----------------------------------------------------------------------
  ! Name:		h5dfill_double
  !
  ! Purpose:      Fills dataspace elements with a fill value in a memory buffer.
  !               Only INTEGER, CHARACTER, REAL and DOUBLE PRECISION datatypes
  !               of the fillvalues and buffers are supported. Buffer and fillvalue
  !               are assumed to have the same datatype.
  !               Only one-dimesional buffers are supported.
  !
  ! Inputs:
  !		fill_value	- fill value
  !		space_id	- memory space selection identifier
  !		buf		- data buffer iin memory ro apply selection to
  !				- of k-th dimension of the buf array
  ! Outputs:
  !		hdferr:		- error code
  !				 	Success:  0
  !				 	Failure: -1
  !
  ! Programmer:	Elena Pourmal
  !		March 12, 2003
  !
  !----------------------------------------------------------------------

  SUBROUTINE h5dfill_double(fill_value, space_id, buf,  hdferr)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: fill_value  ! Fill value
    INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
    DOUBLE PRECISION, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
    INTEGER, INTENT(OUT) :: hdferr      ! Error code

    INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
    INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier

    !            INTEGER, EXTERNAL :: h5dfill_double_c
    ! MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5dfill_double_c(fill_value, fill_type_id, space_id, &
            buf, mem_type_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5DFILL_DOUBLE_C'::h5dfill_double_c
         !DEC$ENDIF
         DOUBLE PRECISION, INTENT(IN) :: fill_value  ! Fill value
         INTEGER(HID_T) :: fill_type_id ! Fill value datatype identifier
         INTEGER(HID_T), INTENT(IN) :: space_id ! Memory dataspace selection identifier
         DOUBLE PRECISION, INTENT(IN), DIMENSION(*) :: buf ! Memory buffer to fill in
         INTEGER(HID_T) :: mem_type_id !  Buffer dadtype identifier
       END FUNCTION h5dfill_double_c
    END INTERFACE
    fill_type_id = H5T_NATIVE_DOUBLE
    mem_type_id  = H5T_NATIVE_DOUBLE

    hdferr = h5dfill_double_c(fill_value, fill_type_id, space_id, &
         buf, mem_type_id)


  END SUBROUTINE h5dfill_double

  !
  ! -- H5P ---
  !

  SUBROUTINE h5pset_fill_value_double(prp_id, type_id, fillvalue, &
       hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
    ! of fillvalue datatype
    ! (in memory)
    DOUBLE PRECISION, INTENT(IN) :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code

    !            INTEGER, EXTERNAL :: h5pset_fill_value_double_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5pset_fill_value_double_c(prp_id, type_id, fillvalue)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_FILL_VALUE_DOUBLE_C'::h5pset_fill_value_double_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         DOUBLE PRECISION, INTENT(IN) :: fillvalue
       END FUNCTION h5pset_fill_value_double_c
    END INTERFACE

    hdferr = h5pset_fill_value_double_c(prp_id, type_id, fillvalue)
  END SUBROUTINE h5pset_fill_value_double


  SUBROUTINE h5pget_fill_value_double(prp_id, type_id, fillvalue, &
       hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id ! Property list identifier
    INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier of
    ! of fillvalue datatype
    ! (in memory)
    DOUBLE PRECISION, INTENT(IN) :: fillvalue   ! Fillvalue
    INTEGER, INTENT(OUT) :: hdferr  ! Error code

    !            INTEGER, EXTERNAL :: h5pget_fill_value_double_c
    !  MS FORTRAN needs explicit interface for C functions called here.
    !
    INTERFACE
       INTEGER FUNCTION h5pget_fill_value_double_c(prp_id, type_id, fillvalue)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_FILL_VALUE_DOUBLE_C'::h5pget_fill_value_double_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: prp_id
         INTEGER(HID_T), INTENT(IN) :: type_id
         DOUBLE PRECISION :: fillvalue
       END FUNCTION h5pget_fill_value_double_c
    END INTERFACE

    hdferr = h5pget_fill_value_double_c(prp_id, type_id, fillvalue)
  END SUBROUTINE h5pget_fill_value_double


  !----------------------------------------------------------------------
  ! Name:		h5pset_double
  !
  ! Purpose: 	Sets a property list value
  !
  ! Inputs:
  !		prp_id		- iproperty list identifier to modify
  !		name 		- name of property to modify
  !		value		- value to set property to
  ! Outputs:
  !		hdferr:		- error code
  !				 	Success:  0
  !				 	Failure: -1
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 9, 2002
  !
  ! Modifications:
  !
  ! Comment:
  !----------------------------------------------------------------------

  SUBROUTINE h5pset_double(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    DOUBLE PRECISION,   INTENT(IN) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pset_double_c(prp_id, name, name_len, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PSET_DOUBLE_C'::h5pset_double_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         DOUBLE PRECISION, INTENT(IN) :: value
       END FUNCTION h5pset_double_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pset_double_c(prp_id, name , name_len, value)
  END SUBROUTINE h5pset_double


  !----------------------------------------------------------------------
  ! Name:		h5pget_double
  !
  ! Purpose: 	Gets a property list value
  !
  ! Inputs:
  !		prp_id		- iproperty list identifier to modify
  !		name 		- name of property to modify
  ! Outputs:
  !		value		- value of property
  !		hdferr:		- error code
  !				 	Success:  0
  !				 	Failure: -1
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 9, 2002
  !
  ! Modifications:
  !
  ! Comment:
  !----------------------------------------------------------------------

  SUBROUTINE h5pget_double(prp_id, name, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: prp_id  ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to modify
    DOUBLE PRECISION,   INTENT(OUT) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pget_double_c(prp_id, name, name_len, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PGET_DOUBLE_C'::h5pget_double_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: prp_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         DOUBLE PRECISION, INTENT(OUT) :: value
       END FUNCTION h5pget_double_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pget_double_c(prp_id, name , name_len, value)
  END SUBROUTINE h5pget_double


  !----------------------------------------------------------------------
  ! Name:		h5pregister_double
  !
  ! Purpose: 	Registers a permanent property with a property list class.
  !
  ! Inputs:
  !		class		- property list class to register
  !                                 permanent property within
  !		name 		- name of property to register
  !               size            - size of property in bytes
  !		value		- default value for property in newly
  !                                 created property lists
  ! Outputs:
  !		hdferr:		- error code
  !				 	Success:  0
  !				 	Failure: -1
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 10, 2002
  !
  ! Modifications:
  !
  ! Comment:
  !----------------------------------------------------------------------

  SUBROUTINE h5pregister_double(class, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: class   ! Property list class identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to register
    INTEGER(SIZE_T), INTENT(IN) :: size  ! size of the property value
    DOUBLE PRECISION,   INTENT(IN) :: value        ! Property value
    INTEGER, INTENT(OUT) :: hdferr  ! Error code
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pregister_double_c(class, name, name_len, size, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PREGISTER_DOUBLE_C'::h5pregister_double_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: class
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(SIZE_T), INTENT(IN) :: size
         DOUBLE PRECISION, INTENT(IN) :: value
       END FUNCTION h5pregister_double_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pregister_double_c(class, name , name_len, size, value)
  END SUBROUTINE h5pregister_double

  !----------------------------------------------------------------------
  ! Name:		h5pinsert_double
  !
  ! Purpose: 	Registers a temporary property with a property list class.
  !
  ! Inputs:
  !		plist		- property list identifier
  !                                 permanent property within
  !		name 		- name of property to insert
  !               size            - size of property in bytes
  !		value		- initial value for the property
  ! Outputs:
  !		hdferr:		- error code
  !				 	Success:  0
  !				 	Failure: -1
  ! Optional parameters:
  !				NONE
  !
  ! Programmer:	Elena Pourmal
  !	        October 10, 2002
  !
  ! Modifications:
  !
  ! Comment:
  !----------------------------------------------------------------------

  SUBROUTINE h5pinsert_double(plist, name, size, value, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: plist   ! Property list identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Name of property to insert
    INTEGER(SIZE_T), INTENT(IN) :: size   ! Size of the property value
    DOUBLE PRECISION, INTENT(IN) :: value ! Property value
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
    INTEGER :: name_len

    INTERFACE
       INTEGER FUNCTION h5pinsert_double_c(plist, name, name_len, size, value)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5PINSERT_DOUBLE_C'::h5pinsert_double_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: plist
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER, INTENT(IN)         :: name_len
         INTEGER(SIZE_T), INTENT(IN) :: size
         DOUBLE PRECISION, INTENT(IN) :: value
       END FUNCTION h5pinsert_double_c
    END INTERFACE

    name_len = LEN(name)
    hdferr = h5pinsert_double_c(plist, name , name_len, size, value)
  END SUBROUTINE h5pinsert_double

END MODULE H5_DBLE_INTERFACE
