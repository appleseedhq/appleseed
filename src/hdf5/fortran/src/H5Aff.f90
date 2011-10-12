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
! This file contains Fortran90 interfaces for H5A functions.
!
MODULE H5A

  USE H5GLOBAL
!
!On Windows there are no big (integer*8) integers, so overloading
!for bug #670 does not work. I have to use DEC compilation directives to make
!Windows DEC Visual Fortran and OSF compilers happy and do right things.
!						05/01/02 EP
!
  INTERFACE h5awrite_f

     MODULE PROCEDURE h5awrite_integer_scalar
     MODULE PROCEDURE h5awrite_integer_1
     MODULE PROCEDURE h5awrite_integer_2
     MODULE PROCEDURE h5awrite_integer_3
     MODULE PROCEDURE h5awrite_integer_4
     MODULE PROCEDURE h5awrite_integer_5
     MODULE PROCEDURE h5awrite_integer_6
     MODULE PROCEDURE h5awrite_integer_7
     MODULE PROCEDURE h5awrite_char_scalar
     MODULE PROCEDURE h5awrite_char_1
     MODULE PROCEDURE h5awrite_char_2
     MODULE PROCEDURE h5awrite_char_3
     MODULE PROCEDURE h5awrite_char_4
     MODULE PROCEDURE h5awrite_char_5
     MODULE PROCEDURE h5awrite_char_6
     MODULE PROCEDURE h5awrite_char_7
     MODULE PROCEDURE h5awrite_real_scalar
     MODULE PROCEDURE h5awrite_real_1
     MODULE PROCEDURE h5awrite_real_2
     MODULE PROCEDURE h5awrite_real_3
     MODULE PROCEDURE h5awrite_real_4
     MODULE PROCEDURE h5awrite_real_5
     MODULE PROCEDURE h5awrite_real_6
     MODULE PROCEDURE h5awrite_real_7

  END INTERFACE

  INTERFACE h5aread_f

     MODULE PROCEDURE h5aread_integer_scalar
     MODULE PROCEDURE h5aread_integer_1
     MODULE PROCEDURE h5aread_integer_2
     MODULE PROCEDURE h5aread_integer_3
     MODULE PROCEDURE h5aread_integer_4
     MODULE PROCEDURE h5aread_integer_5
     MODULE PROCEDURE h5aread_integer_6
     MODULE PROCEDURE h5aread_integer_7
     MODULE PROCEDURE h5aread_char_scalar
     MODULE PROCEDURE h5aread_char_1
     MODULE PROCEDURE h5aread_char_2
     MODULE PROCEDURE h5aread_char_3
     MODULE PROCEDURE h5aread_char_4
     MODULE PROCEDURE h5aread_char_5
     MODULE PROCEDURE h5aread_char_6
     MODULE PROCEDURE h5aread_char_7
     MODULE PROCEDURE h5aread_real_scalar
     MODULE PROCEDURE h5aread_real_1
     MODULE PROCEDURE h5aread_real_2
     MODULE PROCEDURE h5aread_real_3
     MODULE PROCEDURE h5aread_real_4
     MODULE PROCEDURE h5aread_real_5
     MODULE PROCEDURE h5aread_real_6
     MODULE PROCEDURE h5aread_real_7

  END INTERFACE

CONTAINS

!----------------------------------------------------------------------
! Name:		h5acreate_f
!
! Purpose: 	Creates a dataset as an attribute of a group, dataset,
!		or named datatype
!
! Inputs:
!		loc_id		- identifier of an object (group, dataset,
!				  or named datatype) attribute is attached to
!		name		- attribute name
!		type_id		- attribute datatype identifier
!		space_id	- attribute dataspace identifier
!
! Outputs:
!		attr_id		- attribute identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!		acpl_id	- Attribute creation property list identifier
!               appl_id - Attribute access property list identifier
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces are added for
!			called C functions (it is needed for Windows
!			port).  February 27, 2001
!
!----------------------------------------------------------------------
  SUBROUTINE h5acreate_f(loc_id, name, type_id, space_id, attr_id, &
                                 hdferr, acpl_id, aapl_id )
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
    INTEGER(HID_T), INTENT(IN) :: type_id
    ! Attribute datatype identifier
    INTEGER(HID_T), INTENT(IN) :: space_id
    ! Attribute dataspace identifier
    INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: acpl_id ! Attribute creation property list identifier
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: aapl_id ! Attribute access property list identifier

    INTEGER(HID_T) :: acpl_id_default
    INTEGER(HID_T) :: aapl_id_default
    INTEGER(SIZE_T) :: namelen
!            INTEGER, EXTERNAL :: h5acreate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5acreate_c(loc_id, name, namelen, type_id, &
            space_id, acpl_id_default, aapl_id_default, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ACREATE_C'::h5acreate_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER(SIZE_T) :: namelen
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HID_T) :: acpl_id_default
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION h5acreate_c
    END INTERFACE

    acpl_id_default = H5P_DEFAULT_F
    aapl_id_default = H5P_DEFAULT_F
    namelen = LEN(NAME)
    IF (PRESENT(acpl_id)) acpl_id_default = acpl_id
    IF (PRESENT(aapl_id)) aapl_id_default = aapl_id

    hdferr = h5acreate_c(loc_id, name, namelen, type_id, space_id, &
         acpl_id_default, aapl_id_default, attr_id)

  END SUBROUTINE h5acreate_f


!----------------------------------------------------------------------
! Name:		h5aopen_name_f
!
! Purpose:  	Opens an attribute specified by name.
!
! Inputs:
!		obj_id 		- identifier of a group, dataset, or named
!				  datatype atttribute to be attached to
!		name		- attribute name
! Outputs:
!		attr_id		- attribute identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces are added for
!			called C functions (it is needed for Windows
!			port).  February 27, 2001
!
!----------------------------------------------------------------------

  SUBROUTINE h5aopen_name_f(obj_id, name, attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
    INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    INTEGER(SIZE_T) :: namelen

!            INTEGER, EXTERNAL :: h5aopen_name_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aopen_name_c(obj_id, name, namelen, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AOPEN_NAME_C'::h5aopen_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER(SIZE_T) :: namelen
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION h5aopen_name_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5aopen_name_c(obj_id, name, namelen, attr_id)
  END SUBROUTINE h5aopen_name_f


!----------------------------------------------------------------------
! Name:		h5aopen_idx_f
!
! Purpose:  	Opens the attribute specified by its index.
!
! Inputs:
!		obj_id		- identifier of a group, dataset, or named
!				  datatype an attribute to be attached to
!		index		- index of the attribute to open (zero-based)
! Outputs:
!		attr_id		- attribute identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces are added for
!			called C functions (it is needed for Windows
!			port).  February 27, 2001
!
!----------------------------------------------------------------------

  SUBROUTINE h5aopen_idx_f(obj_id, index, attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier
    INTEGER, INTENT(IN) :: index            ! Attribute index
    INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aopen_idx_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aopen_idx_c(obj_id, index, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AOPEN_IDX_C'::h5aopen_idx_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER, INTENT(IN) :: index
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION h5aopen_idx_c
    END INTERFACE

    hdferr = h5aopen_idx_c(obj_id, index, attr_id)
  END SUBROUTINE h5aopen_idx_f


  SUBROUTINE h5awrite_integer_scalar(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
    ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    INTEGER, INTENT(IN) :: buf              ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_s_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_S_C'::h5awrite_integer_s_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN)::buf
       END FUNCTION h5awrite_integer_s_c
    END INTERFACE

    hdferr = h5awrite_integer_s_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_integer_scalar

  SUBROUTINE h5awrite_integer_1(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
    ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    INTEGER, INTENT(IN) , &
         DIMENSION(dims(1)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_1_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_1_C'::h5awrite_integer_1_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), DIMENSION(dims(1)) :: buf
       END FUNCTION h5awrite_integer_1_c
    END INTERFACE

    hdferr = h5awrite_integer_1_c(attr_id, memtype_id,  buf, dims)

  END SUBROUTINE h5awrite_integer_1


  SUBROUTINE h5awrite_integer_2(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
    ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    INTEGER, INTENT(IN) , &
         DIMENSION(dims(1),dims(2)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_2_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_2_C'::h5awrite_integer_2_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2)) :: buf
       END FUNCTION h5awrite_integer_2_c
    END INTERFACE

    hdferr = h5awrite_integer_2_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_integer_2


  SUBROUTINE h5awrite_integer_3(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
    ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_3_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_3_C'::h5awrite_integer_3_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3)) :: buf
       END FUNCTION h5awrite_integer_3_c
    END INTERFACE

    hdferr = h5awrite_integer_3_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_integer_3


  SUBROUTINE h5awrite_integer_4(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
    ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_4_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_4_C'::h5awrite_integer_4_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
       END FUNCTION h5awrite_integer_4_c
    END INTERFACE

    hdferr = h5awrite_integer_4_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_integer_4


  SUBROUTINE h5awrite_integer_5(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
    ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_5_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_5_C'::h5awrite_integer_5_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
       END FUNCTION h5awrite_integer_5_c
    END INTERFACE

    hdferr = h5awrite_integer_5_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_integer_5


  SUBROUTINE h5awrite_integer_6(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
    ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_6_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_6_C'::h5awrite_integer_6_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
       END FUNCTION h5awrite_integer_6_c
    END INTERFACE

    hdferr = h5awrite_integer_6_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_integer_6


  SUBROUTINE h5awrite_integer_7(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
    ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    INTEGER, INTENT(IN), &
         DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_integer_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5awrite_integer_7_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_INTEGER_7_C'::h5awrite_integer_7_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         INTEGER, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
       END FUNCTION h5awrite_integer_7_c
    END INTERFACE

    hdferr = h5awrite_integer_7_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_integer_7


  SUBROUTINE h5awrite_real_scalar(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
    ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    REAL, INTENT(IN) :: buf                 ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5awrite_real_s_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_S_C'::h5awrite_real_s_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(IN)::buf
       END FUNCTION h5awrite_real_s_c
    END INTERFACE

    hdferr = h5awrite_real_s_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_real_scalar

  SUBROUTINE h5awrite_real_1(attr_id, memtype_id,  buf, dims, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
    INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
    ! identifier  (in memory)
    INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
    REAL, INTENT(IN), &
         DIMENSION(dims(1)) :: buf
                                                    ! Attribute data
    INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5awrite_real_1_c(attr_id, memtype_id,  buf, dims)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_1_C'::h5awrite_real_1_c
         !DEC$ENDIF
         INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(IN) :: memtype_id
         REAL, INTENT(IN), &
              DIMENSION(dims(1)) :: buf
       END FUNCTION h5awrite_real_1_c
    END INTERFACE

    hdferr = h5awrite_real_1_c(attr_id, memtype_id,  buf, dims)
  END SUBROUTINE h5awrite_real_1


          SUBROUTINE h5awrite_real_2(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_2_C'::h5awrite_real_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5awrite_real_2_c
            END INTERFACE

            hdferr = h5awrite_real_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_2


          SUBROUTINE h5awrite_real_3(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_3_C'::h5awrite_real_3_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5awrite_real_3_c
            END INTERFACE

            hdferr = h5awrite_real_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_3


          SUBROUTINE h5awrite_real_4(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_4_C'::h5awrite_real_4_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5awrite_real_4_c
            END INTERFACE

            hdferr = h5awrite_real_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_4


          SUBROUTINE h5awrite_real_5(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_5_C'::h5awrite_real_5_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5awrite_real_5_c
            END INTERFACE

            hdferr = h5awrite_real_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_5


          SUBROUTINE h5awrite_real_6(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_6_C'::h5awrite_real_6_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5awrite_real_6_c
            END INTERFACE

            hdferr = h5awrite_real_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_6


          SUBROUTINE h5awrite_real_7(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awrite_real_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awrite_real_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITE_REAL_7_C'::h5awrite_real_7_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5awrite_real_7_c
            END INTERFACE

            hdferr = h5awrite_real_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_real_7


          SUBROUTINE h5awrite_char_scalar(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*),INTENT(IN) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_S_C'::h5awritec_s_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN)::buf
              END FUNCTION h5awritec_s_c
            END INTERFACE

            hdferr = h5awritec_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_scalar

          SUBROUTINE h5awrite_char_1(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_1_C'::h5awritec_1_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), DIMENSION(dims(1))::buf
              END FUNCTION h5awritec_1_c
            END INTERFACE

            hdferr = h5awritec_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_1


          SUBROUTINE h5awrite_char_2(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5awritec_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_2_C'::h5awritec_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              !DEC$ATTRIBUTES reference :: buf
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5awritec_2_c
            END INTERFACE

            hdferr = h5awritec_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_2


          SUBROUTINE h5awrite_char_3(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_3_C'::h5awritec_3_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5awritec_3_c
            END INTERFACE

            hdferr = h5awritec_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_3


          SUBROUTINE h5awrite_char_4(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_4_C'::h5awritec_4_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5awritec_4_c
            END INTERFACE

            hdferr = h5awritec_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_4


          SUBROUTINE h5awrite_char_5(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_5_C'::h5awritec_5_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5awritec_5_c
            END INTERFACE

            hdferr = h5awritec_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_5


          SUBROUTINE h5awrite_char_6(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_6_C'::h5awritec_6_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5awritec_6_c
            END INTERFACE

            hdferr = h5awritec_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_6


          SUBROUTINE h5awrite_char_7(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(IN), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
!            INTEGER, EXTERNAL :: h5awritec_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5awritec_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AWRITEC_7_C'::h5awritec_7_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(IN), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5awritec_7_c
            END INTERFACE

            hdferr = h5awritec_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5awrite_char_7

!----------------------------------------------------------------------
! Name:		h5aread_f
!
! Purpose:  	Reads an attribute.
!
! Inputs:
!		attr_id		- attribute identifier
!		memtype_id	- attribute memory type identifier
!		dims		- 1D array of size 7, stores sizes of the
!				- buf array dimensions.
! Outputs:
!		buf		- buffer to read attribute data in
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces are added for
!			called C functions (it is needed for Windows
!			port).  February 27, 2001
!
!			dims parameter was added to make code portable;
!			Aprile 4, 2001
!
!                       Changed buf intent to INOUT to be consistant
!                       with how the C functions handles it. The pg
!                       compiler will return 0 if a buf value is not set.
!                       February, 2008
!
! Comment:		This function is overloaded to write INTEGER,
!			REAL, DOUBLE PRECISION and CHARACTER buffers
!			up to 7 dimensions.
!----------------------------------------------------------------------

          SUBROUTINE h5aread_integer_scalar(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            INTEGER, INTENT(INOUT) :: buf             ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_S_C'::h5aread_integer_s_c
              !DEC$ENDIF
              INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(INOUT)::buf
              END FUNCTION h5aread_integer_s_c
            END INTERFACE
            hdferr = h5aread_integer_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_scalar

          SUBROUTINE h5aread_integer_1(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            INTEGER, INTENT(INOUT), DIMENSION(dims(1)) :: buf
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_1_C'::h5aread_integer_1_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(INOUT), DIMENSION(dims(1)) :: buf
              END FUNCTION h5aread_integer_1_c
            END INTERFACE

            hdferr = h5aread_integer_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_1


          SUBROUTINE h5aread_integer_2(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            INTEGER, INTENT(INOUT),DIMENSION(dims(1),dims(2)) :: buf
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_2_C'::h5aread_integer_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(INOUT), DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5aread_integer_2_c
            END INTERFACE

            hdferr = h5aread_integer_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_2


          SUBROUTINE h5aread_integer_3(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            INTEGER, INTENT(INOUT), &
                 DIMENSION(dims(1),dims(2),dims(3)) :: buf
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_3_C'::h5aread_integer_3_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5aread_integer_3_c
            END INTERFACE

            hdferr = h5aread_integer_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_3


          SUBROUTINE h5aread_integer_4(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_4_C'::h5aread_integer_4_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5aread_integer_4_c
            END INTERFACE

            hdferr = h5aread_integer_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_4


          SUBROUTINE h5aread_integer_5(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_5_C'::h5aread_integer_5_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5aread_integer_5_c
            END INTERFACE

            hdferr = h5aread_integer_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_5


          SUBROUTINE h5aread_integer_6(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_6_C'::h5aread_integer_6_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5aread_integer_6_c
            END INTERFACE

            hdferr = h5aread_integer_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_6


          SUBROUTINE h5aread_integer_7(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            INTEGER, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_integer_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_integer_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_INTEGER_7_C'::h5aread_integer_7_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              INTEGER, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5aread_integer_7_c
            END INTERFACE

            hdferr = h5aread_integer_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_integer_7


          SUBROUTINE h5aread_real_scalar(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(INOUT) :: buf                ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_S_C'::h5aread_real_s_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(INOUT)::buf
              END FUNCTION h5aread_real_s_c
            END INTERFACE

            hdferr = h5aread_real_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_scalar

          SUBROUTINE h5aread_real_1(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_1_C'::h5aread_real_1_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1)) :: buf
              END FUNCTION h5aread_real_1_c
            END INTERFACE

            hdferr = h5aread_real_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_1


          SUBROUTINE h5aread_real_2(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_2_C'::h5aread_real_2_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5aread_real_2_c
            END INTERFACE

            hdferr = h5aread_real_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_2


          SUBROUTINE h5aread_real_3(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_3_C'::h5aread_real_3_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5aread_real_3_c
            END INTERFACE

            hdferr = h5aread_real_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_3


          SUBROUTINE h5aread_real_4(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_4_C'::h5aread_real_4_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5aread_real_4_c
            END INTERFACE

            hdferr = h5aread_real_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_4


          SUBROUTINE h5aread_real_5(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_5_C'::h5aread_real_5_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5aread_real_5_c
            END INTERFACE

            hdferr = h5aread_real_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_5


          SUBROUTINE h5aread_real_6(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_6_C'::h5aread_real_6_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5aread_real_6_c
            END INTERFACE

            hdferr = h5aread_real_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_6


          SUBROUTINE h5aread_real_7(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            REAL, INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5aread_real_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aread_real_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREAD_REAL_7_C'::h5aread_real_7_c
              !DEC$ENDIF
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              REAL, INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5aread_real_7_c
            END INTERFACE

            hdferr = h5aread_real_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_real_7

          SUBROUTINE h5aread_char_scalar(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(INOUT) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_s_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_s_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_S_C'::h5areadc_s_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(INOUT) :: buf
              END FUNCTION h5areadc_s_c
            END INTERFACE

            hdferr = h5areadc_s_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_scalar

          SUBROUTINE h5aread_char_1(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_1_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_1_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_1_C'::h5areadc_1_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(INOUT), &
                   DIMENSION(dims(1)) :: buf
              END FUNCTION h5areadc_1_c
            END INTERFACE

            hdferr = h5areadc_1_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_1


          SUBROUTINE h5aread_char_2(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(INOUT), &
                 DIMENSION(dims(1),dims(2)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_2_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_2_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_2_C'::h5areadc_2_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2)) :: buf
              END FUNCTION h5areadc_2_c
            END INTERFACE

            hdferr = h5areadc_2_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_2


          SUBROUTINE h5aread_char_3(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_3_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_3_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_3_C'::h5areadc_3_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3)) :: buf
              END FUNCTION h5areadc_3_c
            END INTERFACE

            hdferr = h5areadc_3_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_3


          SUBROUTINE h5aread_char_4(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_4_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_4_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_4_C'::h5areadc_4_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4)) :: buf
              END FUNCTION h5areadc_4_c
            END INTERFACE

            hdferr = h5areadc_4_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_4


          SUBROUTINE h5aread_char_5(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_5_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_5_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_5_C'::h5areadc_5_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5)) :: buf
              END FUNCTION h5areadc_5_c
            END INTERFACE

            hdferr = h5areadc_5_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_5


          SUBROUTINE h5aread_char_6(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_6_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_6_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_6_C'::h5areadc_6_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)) :: buf
              END FUNCTION h5areadc_6_c
            END INTERFACE

            hdferr = h5areadc_6_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_6


          SUBROUTINE h5aread_char_7(attr_id, memtype_id,  buf, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id   ! Attribute identifier
            INTEGER(HID_T), INTENT(IN) :: memtype_id ! Attribute datatype
                                                     ! identifier  (in memory)
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
            CHARACTER(LEN=*), INTENT(INOUT), &
            DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
                                                    ! Attribute data
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5areadc_7_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5areadc_7_c(attr_id, memtype_id,  buf, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AREADC_7_C'::h5areadc_7_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: buf
            INTEGER(HSIZE_T), INTENT(IN), DIMENSION(*) :: dims       ! Array to story buf dimension sizes
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(IN) :: memtype_id
              CHARACTER(LEN=*), INTENT(INOUT), &
              DIMENSION(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)) :: buf
              END FUNCTION h5areadc_7_c
            END INTERFACE

            hdferr = h5areadc_7_c(attr_id, memtype_id,  buf, dims)
          END SUBROUTINE h5aread_char_7


!----------------------------------------------------------------------
! Name:		h5aget_space_f
!
! Purpose:  	Gets a copy of the dataspace for an attribute.
!
! Inputs:
!		attr_id		- attribute identifier
! Outputs:
!		space_id	- attribite dataspace identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces are added for
!			called C functions (it is needed for Windows
!			port).  February 27, 2001
!
!----------------------------------------------------------------------

          SUBROUTINE h5aget_space_f(attr_id, space_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
            INTEGER(HID_T), INTENT(OUT) :: space_id
                                            ! Attribute dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL:: h5aget_space_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aget_space_c(attr_id, space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_SPACE_C'::h5aget_space_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(OUT) :: space_id
              END FUNCTION h5aget_space_c
            END INTERFACE

            hdferr = h5aget_space_c(attr_id, space_id)
          END SUBROUTINE h5aget_space_f

!----------------------------------------------------------------------
! Name:		h5aget_type_f
!
! Purpose:  	Gets an attribute datatype.
!
! Inputs:
!		attr_id 	- attribute identifier
! Outputs:
!		type_id		- attribute datatype identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces are added for
!			called C functions (it is needed for Windows
!			port).  February 27, 2001
!
!----------------------------------------------------------------------

          SUBROUTINE h5aget_type_f(attr_id, type_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
            INTEGER(HID_T), INTENT(OUT) :: type_id
                                              ! Attribute datatype identifier
            INTEGER, INTENT(OUT) :: hdferr    ! Error code

!            INTEGER, EXTERNAL :: h5aget_type_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5aget_type_c(attr_id, type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_TYPE_C'::h5aget_type_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: attr_id
              INTEGER(HID_T), INTENT(OUT) :: type_id
              END FUNCTION h5aget_type_c
            END INTERFACE

            hdferr = h5aget_type_c(attr_id, type_id)
          END SUBROUTINE h5aget_type_f

!----------------------------------------------------------------------
! Name:		h5aget_name_f
!
! Purpose: 	Gets an attribute name.
!
! Inputs:
!		attr_id		- attribute identifier
!		size		- size of a buffer to read name in
! Outputs:
!		buf		- buffer to read name in
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces are added for
!			called C functions (it is needed for Windows
!			port).  February 27, 2001
!
!----------------------------------------------------------------------


  SUBROUTINE h5aget_name_f(attr_id, size, buf, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
    INTEGER(SIZE_T), INTENT(IN) :: size    ! Buffer size
    CHARACTER(LEN=*), INTENT(INOUT) :: buf
                                           ! Buffer to hold attribute name
    INTEGER, INTENT(OUT) :: hdferr ! Error code:
                                   ! name length is successful,
                                   ! -1 if fail

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aget_name_c(attr_id, size, buf)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_NAME_C'::h5aget_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(SIZE_T), INTENT(IN) :: size
         CHARACTER(LEN=*), INTENT(OUT) :: buf
       END FUNCTION h5aget_name_c
    END INTERFACE

    hdferr = h5aget_name_c(attr_id, size, buf)
  END SUBROUTINE h5aget_name_f

!----------------------------------------------------------------------
! Name:		h5aget_name_by_idx_f
!
! Purpose: 	Gets an attribute name, by attribute index position.
!
! Inputs:
!          loc_id   - Location of object to which attribute is attached
!          obj_name - Name of object to which attribute is attached, relative to location
!          idx_type - Type of index; Possible values are:
!
!                  H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!                  H5_INDEX_NAME_F          - Index on names
!                  H5_INDEX_CRT_ORDER_F     - Index on creation order
!                  H5_INDEX_N_F             - Number of indices defined
!
!          order    - Order in which to iterate over index; Possible values are:
!
!                  H5_ITER_UNKNOWN_F   - Unknown order
!                  H5_ITER_INC_F       - Increasing order
!                  H5_ITER_DEC_F       - Decreasing order
!                  H5_ITER_NATIVE_F    - No particular order, whatever is fastest
!                  H5_ITER_N_F	     - Number of iteration orders
!
!          order    - Index traversal order
!              n    - Attribute’s position in index
!
! Outputs:
!		name            - Attribute name
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!        lapl_id    - Link access property list
!           size    - Size, in bytes, of attribute name
!
! Programmer:   M.S. Breitenfeld
!		January, 2008
!
! Modifications: N/A
!----------------------------------------------------------------------

  SUBROUTINE h5aget_name_by_idx_f(loc_id, obj_name, idx_type, order, &
       n, name, hdferr, size, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id      ! Identifer for object to which attribute is attached
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object, relative to location,
                                              !  from which attribute is to be removed *TEST* check NULL
    INTEGER, INTENT(IN) :: idx_type ! Type of index; Possible values are:
                                    !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                    !    H5_INDEX_NAME_F       - Index on names
                                    !    H5_INDEX_CRT_ORDER_F  - Index on creation order
                                    !    H5_INDEX_N_F 	      - Number of indices defined

    INTEGER, INTENT(IN) :: order    ! Order in which to iterate over index; Possible values are:
                                    !    H5_ITER_UNKNOWN_F   - Unknown order
                                    !    H5_ITER_INC_F      - Increasing order
                                    !    H5_ITER_DEC_F       - Decreasing order
                                    !    H5_ITER_NATIVE_F    - No particular order, whatever is fastest
                                    !    H5_ITER_N_F 	    - Number of iteration orders

    INTEGER(HSIZE_T), INTENT(IN) :: n !  Attribute’s position in index

    CHARACTER(LEN=*), INTENT(OUT) :: name ! Attribute name


    INTEGER, INTENT(OUT) :: hdferr    ! Error code:
                                         ! Returns attribute name size,
                                         ! -1 if fail
    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id   ! Link access property list
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T), OPTIONAL, INTENT(OUT) :: size   ! Indicates the size, in the number of characters, of the attribute
    INTEGER(SIZE_T) :: size_default
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aget_name_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, &
            n, name, size_default, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_NAME_BY_IDX_C'::h5aget_name_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n

         CHARACTER(LEN=*), INTENT(OUT) :: name
         INTEGER(SIZE_T) :: size_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: obj_namelen
       END FUNCTION h5aget_name_by_idx_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    size_default = LEN(name)

    hdferr = h5aget_name_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, &
         n, name, size_default, lapl_id_default)

    IF(PRESENT(size)) size = size_default


  END SUBROUTINE h5aget_name_by_idx_f


!----------------------------------------------------------------------
! Name:		h5aget_num_attrs_f
!
! Purpose:  	Determines the number of attributes attached to an object.
!
! Inputs:
!		obj_id		- object (group, dataset, or named datatype)
!				  identifier
! Outputs:
!		attr_num	- number of attributes attached to the object
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces are added for
!			called C functions (it is needed for Windows
!			port).  February 27, 2001
!
!----------------------------------------------------------------------

  SUBROUTINE h5aget_num_attrs_f(obj_id, attr_num, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id  ! Object identifier
    INTEGER, INTENT(OUT) :: attr_num      ! Number of attributes of the
    ! object
    INTEGER, INTENT(OUT) :: hdferr         ! Error code

!            INTEGER, EXTERNAL :: h5aget_num_attrs_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aget_num_attrs_c(obj_id, attr_num)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_NUM_ATTRS_C'::h5aget_num_attrs_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: obj_id
         INTEGER, INTENT(OUT) :: attr_num
       END FUNCTION h5aget_num_attrs_c
    END INTERFACE

    hdferr = h5aget_num_attrs_c(obj_id, attr_num)
  END SUBROUTINE h5aget_num_attrs_f

!----------------------------------------------------------------------
! Name:		h5adelete_f
!
! Purpose:  	Deletes an attribute of an object (group, dataset or
!		named datatype)
!
! Inputs:
!		obj_id		- object identifier
!		name		- attribute name
! Outputs:
!
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces are added for
!			called C functions (it is needed for Windows
!			port).  February 27, 2001
!
!----------------------------------------------------------------------

  SUBROUTINE h5adelete_f(obj_id, name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: name    ! Attribute name
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    INTEGER(SIZE_T) :: namelen

!            INTEGER, EXTERNAL ::  h5adelete_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5adelete_c(obj_id, name, namelen)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ADELETE_C'::h5adelete_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER(SIZE_T) :: namelen
       END FUNCTION h5adelete_c
    END INTERFACE

    namelen = LEN(name)
    hdferr = h5adelete_c(obj_id, name, namelen)
  END SUBROUTINE h5adelete_f

!----------------------------------------------------------------------
! Name:		h5aclose_f
!
! Purpose:  	Closes the specified attribute.
!
! Inputs:
!		attr_id		- attribute identifier
! Outputs:
!
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces are added for
!			called C functions (it is needed for Windows
!			port).  February 27, 2001
!
!----------------------------------------------------------------------

  SUBROUTINE h5aclose_f(attr_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:

!            INTEGER, EXTERNAL :: h5aclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aclose_c(attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ACLOSE_C'::h5aclose_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: attr_id
       END FUNCTION h5aclose_c
    END INTERFACE

    hdferr = h5aclose_c(attr_id)
  END SUBROUTINE h5aclose_f

!----------------------------------------------------------------------
! Name:		h5aget_storage_size_f
!
! Purpose:  	Returns the amount of storage required for an attribute.
!
! Inputs:
!		attr_id		- attribute identifier
! Outputs:
!               size            - attribute storage size
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	M. S. Breitenfeld
!		January, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5aget_storage_size_f(attr_id, size, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier
    INTEGER(HSIZE_T), INTENT(OUT) :: size   ! Attribute storage requirement
    INTEGER, INTENT(OUT) :: hdferr         ! Error code

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aget_storage_size_c(attr_id, size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_STORAGE_SIZE_C'::h5aget_storage_size_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HSIZE_T), INTENT(OUT) :: size
       END FUNCTION h5aget_storage_size_c
    END INTERFACE

    hdferr = h5aget_storage_size_c(attr_id, size)
  END SUBROUTINE h5aget_storage_size_f

!----------------------------------------------------------------------
! Name:		h5aget_create_plist_f
!
! Purpose:  	Gets an attribute creation property list identifier
!
! Inputs:
!		attr_id		  - Identifier of the attribute
! Outputs:
!               creation_prop_id  - Identifier for the attribute’s creation property
!		hdferr:		  - error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	M. S. Breitenfeld
!		January, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5aget_create_plist_f(attr_id, creation_prop_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Identifier of the attribute
    INTEGER(HID_T), INTENT(OUT) :: creation_prop_id   ! Identifier for the attribute’s creation property
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aget_create_plist_c(attr_id, creation_prop_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_CREATE_PLIST_C'::h5aget_create_plist_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: attr_id
         INTEGER(HID_T), INTENT(OUT) :: creation_prop_id
       END FUNCTION h5aget_create_plist_c
    END INTERFACE

    hdferr = h5aget_create_plist_c(attr_id, creation_prop_id)
  END SUBROUTINE h5aget_create_plist_f

!----------------------------------------------------------------------
! Name:		h5arename_by_name_f
!
! Purpose: 	Renames an attribute
!
! Inputs:
!		loc_id        - Location or object identifier; may be dataset or group
!		obj_name      - Name of object, relative to location,
!                                whose attribute is to be renamed
!               old_attr_name - Prior attribute name
!               new_attr_name - New attribute name
!               lapl_id       - Link access property list identifier
!
! Outputs:
!		hdferr:	      - error code
!				 	Success:  0
!				 	Failure: -1
!
! Programmer:	M.S. Breitenfeld
!		January, 2008
!
! Modifications: N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5arename_by_name_f(loc_id, obj_name, old_attr_name, new_attr_name, &
        hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object, relative to location,
                                              !  whose attribute is to be renamed
    CHARACTER(LEN=*), INTENT(IN) :: old_attr_name ! Prior attribute name
    CHARACTER(LEN=*), INTENT(IN) :: new_attr_name ! New attribute name

    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier

    INTEGER(HID_T) :: lapl_id_default
    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(SIZE_T) :: old_attr_namelen
    INTEGER(SIZE_T) :: new_attr_namelen

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5arename_by_name_c(loc_id, obj_name, obj_namelen, &
            old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen, &
            lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ARENAME_BY_NAME_C'::h5arename_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name,  old_attr_name, new_attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T) :: obj_namelen
         CHARACTER(LEN=*), INTENT(IN) :: old_attr_name
         INTEGER(SIZE_T) :: old_attr_namelen
         CHARACTER(LEN=*), INTENT(IN) :: new_attr_name
         INTEGER(SIZE_T) :: new_attr_namelen
         INTEGER(HID_T) :: lapl_id_default

       END FUNCTION h5arename_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    old_attr_namelen = LEN(old_attr_name)
    new_attr_namelen = LEN(new_attr_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default=lapl_id

    hdferr = h5arename_by_name_c(loc_id, obj_name, obj_namelen, &
         old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen, &
         lapl_id_default)

  END SUBROUTINE h5arename_by_name_f

!----------------------------------------------------------------------
! Name:		h5aopen_f
!
! Purpose:  	Opens an attribute for an object specified by object
!               identifier and attribute name
!
! Inputs:
!		obj_id      - Identifer for object to which attribute is attached
!               attr_name   - Name of attribute to open
! Outputs:
!		attr_id     - attribute identifier
!		hdferr:     - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!		aapl_id     - Attribute access property list
!
! Programmer:	M.S. Breitenfeld
!		January, 2008
!
! Modifications: N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5aopen_f(obj_id, attr_name, attr_id, hdferr, aapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id      ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name
    INTEGER(HID_T), INTENT(OUT) :: attr_id    ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr            ! Error code
                                              !   Success:  0
                                              !   Failure: -1
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: aapl_id     ! Attribute access property list
    INTEGER(HID_T) :: aapl_id_default

    INTEGER(SIZE_T) :: attr_namelen

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aopen_c(obj_id, attr_name, attr_namelen, aapl_id_default, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AOPEN_C'::h5aopen_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(SIZE_T) :: attr_namelen
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION h5aopen_c
    END INTERFACE

    attr_namelen = LEN(attr_name)

    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id

    hdferr = h5aopen_c(obj_id, attr_name, attr_namelen, aapl_id_default, attr_id)

  END SUBROUTINE h5aopen_f

!----------------------------------------------------------------------
! Name:		h5adelete_by_idx_f
!
! Purpose:  	Deletes an attribute from an object according to index order
!
! Inputs:
!		loc_id     - Location or object identifier; may be dataset or group
!               obj_name   - Name of object, relative to location, from which attribute is to be removed
!               idx_type   - Type of index; Possible values are:
!
!                  H5_INDEX_UNKNOWN_F = -1  - Unknown index type
!                  H5_INDEX_NAME_F          - Index on names
!                  H5_INDEX_CRT_ORDER_F     - Index on creation order
!                  H5_INDEX_N_F	            - Number of indices defined
!
!               order - Order in which to iterate over index; Possible values are:
!
!                  H5_ITER_UNKNOWN_F   - Unknown order
!                  H5_ITER_INC_F       - Increasing order
!                  H5_ITER_DEC_F       - Decreasing order
!                  H5_ITER_NATIVE_F    - No particular order, whatever is fastest
!                  H5_ITER_N_F 	       - Number of iteration orders
!
!               n          - Offset within index
! Outputs:
!		hdferr:     - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!               lapl_id    - Link access property list
!
! Programmer:	M.S. Breitenfeld
!		January, 2008
!
! Modifications: N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5adelete_by_idx_f(loc_id, obj_name, idx_type, order, n, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id      ! Identifer for object to which attribute is attached
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object, relative to location,
                                              !  from which attribute is to be removed
    INTEGER, INTENT(IN) :: idx_type           ! Type of index; Possible values are:
                                              !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                              !    H5_INDEX_NAME_F      - Index on names
                                              !    H5_INDEX_CRT_ORDER_F - Index on creation order
                                              !    H5_INDEX_N_F	      - Number of indices defined

    INTEGER, INTENT(IN) :: order              ! Order in which to iterate over index; Possible values are:
                                              !    H5_ITER_UNKNOWN_F  - Unknown order
                                              !    H5_ITER_INC_F      - Increasing order
                                              !    H5_ITER_DEC_F      - Decreasing order
                                              !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest
                                              !    H5_ITER_N_F	    - Number of iteration orders
!
    INTEGER(HSIZE_T), INTENT(IN) :: n         ! Offset within index
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure
    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id     ! Link access property list

    INTEGER(HID_T) :: lapl_id_default

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5adelete_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ADELETE_BY_IDX_C'::h5adelete_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: obj_namelen
       END FUNCTION h5adelete_by_idx_c
    END INTERFACE

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    obj_namelen = LEN(obj_name)
    hdferr = h5adelete_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default)

  END SUBROUTINE h5adelete_by_idx_f

!----------------------------------------------------------------------
! Name:		h5adelete_by_name_f
!
! Purpose:  	Removes an attribute from a specified location
!
! Inputs:
!		loc_id     - Identifer for object to which attribute is attached
!               obj_name   - Name of attribute to open
!		attr_name  - Attribute access property list
!               lapl_id    - Link access property list
! Outputs:
!		hdferr:     - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	M.S. Breitenfeld
!		January, 2008
!
! Modifications: N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5adelete_by_name_f(loc_id, obj_name, attr_name, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id      ! Identifer for object to which attribute is attached
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object, relative to location,
                                              !  from which attribute is to be removed
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Name of attribute to delete
    INTEGER, INTENT(OUT) :: hdferr          ! Error code:
                                            ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id     ! Link access property list
    INTEGER(SIZE_T) :: attr_namelen
    INTEGER(SIZE_T) :: obj_namelen

    INTEGER(HID_T) :: lapl_id_default

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5adelete_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ADELETE_BY_NAME_C'::h5adelete_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: attr_namelen
         INTEGER(SIZE_T) :: obj_namelen
       END FUNCTION h5adelete_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    attr_namelen = LEN(attr_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5adelete_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default)

  END SUBROUTINE h5adelete_by_name_f

!----------------------------------------------------------------------
! Name:		h5aopen_by_idx_f
!
! Purpose:  	Opens an existing attribute that is attached to an object specified by location and name
!
! Inputs:
!		loc_id      - Location of object to which attribute is attached
!               obj_name    - Name of object to which attribute is attached, relative to location
!               idx_type    - Type of index
!               order       - Index traversal order
!               n           - Attribute’s position in index
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!               aapl_id     - Attribute access property list
!               lapl_id     - Link access property list
!
! Programmer:	M.S. Breitenfeld
!		January, 2008
!
! Modifications: N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5aopen_by_idx_f(loc_id, obj_name, idx_type, order, n, attr_id, hdferr, aapl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id      ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name  ! Name of object to which attribute is attached
    INTEGER, INTENT(IN) :: idx_type           ! Type of index; Possible values are:
                                              !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                              !    H5_INDEX_NAME_F      - Index on names
                                              !    H5_INDEX_CRT_ORDER_F - Index on creation order
                                              !    H5_INDEX_N_F	      - Number of indices defined
    INTEGER, INTENT(IN) :: order              ! Order in which to iterate over index; Possible values are:
                                              !    H5_ITER_UNKNOWN_F  - Unknown order
                                              !    H5_ITER_INC_F      - Increasing order
                                              !    H5_ITER_DEC_F      - Decreasing order
                                              !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest

    INTEGER(HSIZE_T), INTENT(IN) :: n      ! Attribute’s position in index

    INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code:
                                            ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: aapl_id  ! Attribute access property list
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list

    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default

!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aopen_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, &
            aapl_id_default, lapl_id_default, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AOPEN_BY_IDX_C'::h5aopen_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: obj_namelen
         INTEGER(HID_T), INTENT(OUT) :: attr_id  ! Attribute identifier
       END FUNCTION h5aopen_by_idx_c
    END INTERFACE

    obj_namelen = LEN(obj_name)

    aapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5aopen_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, &
         aapl_id_default, lapl_id_default, attr_id)

  END SUBROUTINE h5aopen_by_idx_f

!----------------------------------------------------------------------
! Name:		h5aget_info_f
!
! Purpose:  	Retrieves attribute information, by attribute identifier
!
! Inputs:
!		attr_id		- attribute identifier
!
! Outputs:  NOTE: In C it is defined as a structure: H5A_info_t
!
!    corder_valid   - indicates whether the creation order data is valid for this attribute
!    corder         - is a positive integer containing the creation order of the attribute
!    cset           - indicates the character set used for the attribute’s name
!    data_size      - indicates the size, in the number of characters, of the attribute
!
!    hdferr         - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	M. S. Breitenfeld
!		January, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5aget_info_f(attr_id, f_corder_valid, corder, cset, data_size,  hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: attr_id  ! Attribute identifier

    LOGICAL, INTENT(OUT) :: f_corder_valid ! Indicates whether the creation order data is valid for this attribute
    INTEGER, INTENT(OUT) :: corder ! Is a positive integer containing the creation order of the attribute
    INTEGER, INTENT(OUT) :: cset ! Indicates the character set used for the attribute’s name
    INTEGER(HSIZE_T), INTENT(OUT) :: data_size   ! Indicates the size, in the number of characters, of the attribute
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure

    INTEGER :: corder_valid

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aget_info_c(attr_id, corder_valid, corder, cset, data_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_INFO_C'::h5aget_info_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: attr_id

         INTEGER, INTENT(OUT) :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         INTEGER(HSIZE_T), INTENT(OUT) :: data_size
       END FUNCTION h5aget_info_c
    END INTERFACE

    hdferr = h5aget_info_c(attr_id, corder_valid, corder, cset, data_size)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.



  END SUBROUTINE h5aget_info_f

!----------------------------------------------------------------------
! Name:		h5aget_info_by_idx_f
!
! Purpose:  	Retrieves attribute information, by attribute index position
!
! Inputs:
!	loc_id - Location of object to which attribute is attached
!     obj_name - Name of object to which attribute is attached, relative to location
!     idx_type - Type of index
!        order - Index traversal order
!            n - Attribute’s position in index
!
! Outputs:  NOTE: In C it is defined as a structure: H5A_info_t
!    corder_valid   - indicates whether the creation order data is valid for this attribute
!    corder         - is a positive integer containing the creation order of the attribute
!    cset           - indicates the character set used for the attribute’s name
!    data_size      - indicates the size, in the number of characters, of the attribute
!    hdferr         - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!      lapl_id - Link access property list
!
! Programmer:	M. S. Breitenfeld
!		January, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5aget_info_by_idx_f(loc_id, obj_name, idx_type, order, n, &
       f_corder_valid, corder, cset, data_size, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name ! Name of object to which attribute is attached
    INTEGER, INTENT(IN) :: idx_type           ! Type of index; Possible values are:
                                              !    H5_INDEX_UNKNOWN_F   - Unknown index type
                                              !    H5_INDEX_NAME_F      - Index on names
                                              !    H5_INDEX_CRT_ORDER_F - Index on creation order
                                              !    H5_INDEX_N_F	      - Number of indices defined
    INTEGER, INTENT(IN) :: order              ! Order in which to iterate over index; Possible values are:
                                              !    H5_ITER_UNKNOWN_F  - Unknown order
                                              !    H5_ITER_INC_F      - Increasing order
                                              !    H5_ITER_DEC_F      - Decreasing order
                                              !    H5_ITER_NATIVE_F   - No particular order, whatever is fastest

    INTEGER(HSIZE_T), INTENT(IN) :: n      ! Attribute’s position in index


    LOGICAL, INTENT(OUT) :: f_corder_valid ! Indicates whether the creation order data is valid for this attribute
    INTEGER, INTENT(OUT) :: corder ! Is a positive integer containing the creation order of the attribute
    INTEGER, INTENT(OUT) :: cset ! Indicates the character set used for the attribute’s name
    INTEGER(HSIZE_T), INTENT(OUT) :: data_size   ! Indicates the size, in the number of characters, of the attribute
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
    INTEGER :: corder_valid
    INTEGER(SIZE_T)  :: obj_namelen
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list
    INTEGER(HID_T) :: lapl_id_default

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aget_info_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default, &
            corder_valid, corder, cset, data_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_INFO_BY_IDX_C'::h5aget_info_by_idx_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER, INTENT(IN) :: idx_type
         INTEGER, INTENT(IN) :: order
         INTEGER(HSIZE_T), INTENT(IN) :: n
         INTEGER(HID_T) :: lapl_id_default
         INTEGER, INTENT(OUT) :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         INTEGER(HSIZE_T), INTENT(OUT) :: data_size

         INTEGER(SIZE_T)  :: obj_namelen
       END FUNCTION h5aget_info_by_idx_c
    END INTERFACE

    obj_namelen = LEN(obj_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(present(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5aget_info_by_idx_c(loc_id, obj_name, obj_namelen, idx_type, order, n, lapl_id_default, &
            corder_valid, corder, cset, data_size)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE h5aget_info_by_idx_f

!----------------------------------------------------------------------
! Name:		h5aget_info_by_name_f
!
! Purpose:  	Retrieves attribute information, by attribute name
!
! Inputs:
!	loc_id - Location of object to which attribute is attached
!     obj_name - Name of object to which attribute is attached, relative to location
!    attr_name - Attribute name
!
! Outputs:  NOTE: In C it is defined as a structure: H5A_info_t
!    corder_valid   - indicates whether the creation order data is valid for this attribute
!    corder         - is a positive integer containing the creation order of the attribute
!    cset           - indicates the character set used for the attribute’s name
!    data_size      - indicates the size, in the number of characters, of the attribute
!    hdferr         - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!      lapl_id - Link access property list
!
! Programmer:	M. S. Breitenfeld
!		January, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5aget_info_by_name_f(loc_id, obj_name, attr_name, &
       f_corder_valid, corder, cset, data_size, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name ! Name of object to which attribute is attached
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name


    LOGICAL, INTENT(OUT) :: f_corder_valid ! Indicates whether the creation order data is valid for this attribute
    INTEGER, INTENT(OUT) :: corder ! Is a positive integer containing the creation order of the attribute
    INTEGER, INTENT(OUT) :: cset ! Indicates the character set used for the attribute’s name
    INTEGER(HSIZE_T), INTENT(OUT) :: data_size   ! Indicates the size, in the number of characters, of the attribute
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure
    INTEGER :: corder_valid
    INTEGER(SIZE_T)  :: obj_namelen
    INTEGER(SIZE_T)  :: attr_namelen
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Link access property list
    INTEGER(HID_T) :: lapl_id_default


!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aget_info_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, &
            corder_valid, corder, cset, data_size)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AGET_INFO_BY_NAME_C'::h5aget_info_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T) :: lapl_id_default
         INTEGER, INTENT(OUT) :: corder_valid
         INTEGER, INTENT(OUT) :: corder
         INTEGER, INTENT(OUT) :: cset
         INTEGER(HSIZE_T), INTENT(OUT) :: data_size

       END FUNCTION h5aget_info_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    attr_namelen = LEN(attr_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5aget_info_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, &
            corder_valid, corder, cset, data_size)

    f_corder_valid =.FALSE.
    IF (corder_valid .EQ. 1) f_corder_valid =.TRUE.

  END SUBROUTINE h5aget_info_by_name_f

!----------------------------------------------------------------------
! Name:	        H5Acreate_by_name_f
!
! Purpose:  	Creates an attribute attached to a specified object
!
! Inputs:
!   loc_id   	- Location or object identifier; may be dataset or group
!   obj_name 	- Name, relative to loc_id, of object that attribute is to be attached to
!   attr_name   - Attribute name
!   type_id 	- Attribute datatype identifier
!   space_id 	- Attribute dataspace identifier
!
! Outputs:
!       attr    - an attribute identifier
!     hdferr    - error code
!		       Success:  0
!		       Failure: -1
! Optional parameters:
!   acpl_id 	- Attribute creation property list identifier (Currently not used.)
!   aapl_id 	- Attribute access property list identifier (Currently not used.)
!   lapl_id 	- Link access property list
!
! Programmer:	M. S. Breitenfeld
!		February, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5acreate_by_name_f(loc_id, obj_name, attr_name, type_id, space_id, attr, hdferr, &
       acpl_id, aapl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name ! Name of object to which attribute is attached
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name

    INTEGER(HID_T), INTENT(IN) :: type_id  ! Attribute datatype identifier
    INTEGER(HID_T), INTENT(IN) :: space_id ! Attribute dataspace identifier

    INTEGER(HID_T), INTENT(OUT) :: attr ! an attribute identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure

    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: acpl_id ! Attribute creation property list identifier (Currently not used.)
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: aapl_id ! Attribute access property list identifier (Currently not used.)
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list

    INTEGER(SIZE_T)  :: obj_namelen
    INTEGER(SIZE_T)  :: attr_namelen

    INTEGER(HID_T) :: acpl_id_default
    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5acreate_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
            type_id, space_id, acpl_id_default, aapl_id_default, lapl_id_default, attr)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ACREATE_BY_NAME_C'::h5acreate_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HID_T) :: acpl_id_default
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: attr

       END FUNCTION h5acreate_by_name_c
    END INTERFACE

    obj_namelen = LEN(obj_name)
    attr_namelen = LEN(attr_name)

    acpl_id_default = H5P_DEFAULT_F
    aapl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(acpl_id)) acpl_id_default = acpl_id
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5acreate_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
            type_id, space_id, acpl_id_default, aapl_id_default, lapl_id_default, attr)
  END SUBROUTINE h5acreate_by_name_f

!----------------------------------------------------------------------
! Name:	        H5Aexists_f
!
! Purpose:  	Determines whether an attribute with a given name exists on an object
!
! Inputs:
!                 obj_id - Object identifier
!              attr_name - Attribute name
!
! Outputs:
!       attr_exists  - attribute exists status
!            hdferr  - error code
!		         Success:  0
!		         Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	M. S. Breitenfeld
!		February, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5aexists_f(obj_id, attr_name, attr_exists, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id     ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name
    LOGICAL, INTENT(OUT) :: attr_exists ! .TRUE. if exists, .FALSE. otherwise
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure
    INTEGER(HID_T) :: attr_exists_c
    INTEGER(SIZE_T) :: attr_namelen
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aexists_c(obj_id, attr_name, attr_namelen, attr_exists_c)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AEXISTS_C'::h5aexists_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: attr_name
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T) :: attr_namelen
         INTEGER(HID_T) :: attr_exists_c
       END FUNCTION h5aexists_c
    END INTERFACE

    attr_namelen = LEN(attr_name)

    hdferr = h5aexists_c(obj_id, attr_name, attr_namelen, attr_exists_c)

    attr_exists = .FALSE.
    IF(attr_exists_c.GT.0) attr_exists = .TRUE.

  END SUBROUTINE h5aexists_f

!----------------------------------------------------------------------
! Name:	        H5Aexists_by_name_f
!
! Purpose:  	Determines whether an attribute with a given name exists on an object
!
! Inputs:
!     loc_id - Location identifier
!   obj_name - Object name either relative to loc_id, absolute from the file’s root group, or '.' (a dot)
!  attr_name - Attribute name
!
! Outputs:
!       attr_exists  - attribute exists status
!            hdferr  - error code
!		         Success:  0
!		         Failure: -1
! Optional parameters:
!    lapl_id - Link access property list identifier
!
! Programmer:	M. S. Breitenfeld
!		February, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5aexists_by_name_f(loc_id, obj_name, attr_name, attr_exists, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Location identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name ! Object name either relative to loc_id,
                                             ! absolute from the file’s root group, or '.'
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name
    LOGICAL, INTENT(OUT) :: attr_exists ! .TRUE. if exists, .FALSE. otherwise
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier
    INTEGER :: attr_exists_c
    INTEGER(SIZE_T)  :: obj_namelen
    INTEGER(SIZE_T)  :: attr_namelen

    INTEGER(HID_T) :: lapl_id_default
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aexists_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, attr_exists_c)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AEXISTS_BY_NAME_C'::h5aexists_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T), INTENT(IN) :: lapl_id_default
         INTEGER, INTENT(OUT) :: attr_exists_c
       END FUNCTION h5aexists_by_name_c
    END INTERFACE

    attr_namelen = LEN(attr_name)
    obj_namelen = LEN(obj_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5aexists_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, lapl_id_default, attr_exists_c)

    attr_exists = .FALSE.
    IF(attr_exists_c.GT.0) attr_exists = .TRUE.

  END SUBROUTINE h5aexists_by_name_f
!----------------------------------------------------------------------
! Name:	        H5Aopen_by_name_f
!
! Purpose:  	Opens an attribute for an object by object name and attribute name.
!
! Inputs:
!     loc_id - Location from which to find object to which attribute is attached
!   obj_name - Object name either relative to loc_id, absolute from the file’s root group, or '.' (a dot)
!  attr_name - Attribute name
!
! Outputs:
!       attr_id  - attribute identifier
!        hdferr  - error code
!		         Success:  0
!		         Failure: -1
! Optional parameters:
!    aapl_id - Attribute access property list (Currently unused; should be passed in as H5P_DEFAULT.)
!    lapl_id - Link access property list identifier
!
! Programmer:	M. S. Breitenfeld
!		February, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------
  SUBROUTINE h5aopen_by_name_f(loc_id, obj_name, attr_name, attr_id, hdferr, aapl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Location identifier
    CHARACTER(LEN=*), INTENT(IN) :: obj_name ! Object name either relative to loc_id,
                                             ! absolute from the file’s root group, or '.'
    CHARACTER(LEN=*), INTENT(IN) :: attr_name ! Attribute name
    INTEGER(HID_T), INTENT(OUT) :: attr_id ! Attribute identifier
    INTEGER, INTENT(OUT) :: hdferr         ! Error code:
                                           ! 0 on success and -1 on failure
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: aapl_id ! Attribute access property list
                                                    ! (Currently unused; should be passed in as H5P_DEFAULT_F)
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link access property list identifier

    INTEGER(HID_T) :: aapl_id_default
    INTEGER(HID_T) :: lapl_id_default

    INTEGER(SIZE_T) :: obj_namelen
    INTEGER(SIZE_T) :: attr_namelen
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5aopen_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
            aapl_id_default, lapl_id_default, attr_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5AOPEN_BY_NAME_C'::h5aopen_by_name_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: obj_name, attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: obj_name
         INTEGER(SIZE_T), INTENT(IN) :: obj_namelen
         CHARACTER(LEN=*), INTENT(IN) :: attr_name
         INTEGER(SIZE_T), INTENT(IN) :: attr_namelen
         INTEGER(HID_T) :: aapl_id_default
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(HID_T), INTENT(OUT) :: attr_id
       END FUNCTION h5aopen_by_name_c
    END INTERFACE

    attr_namelen = LEN(attr_name)
    obj_namelen = LEN(obj_name)

    aapl_id_default = H5P_DEFAULT_F
    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(aapl_id)) aapl_id_default = aapl_id
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5aopen_by_name_c(loc_id, obj_name, obj_namelen, attr_name, attr_namelen, &
         aapl_id_default, lapl_id_default, attr_id)

  END SUBROUTINE h5aopen_by_name_f

!----------------------------------------------------------------------
! Name:		h5arename_f
!
! Purpose: 	Renames an attribute
!
! Inputs:
!		loc_id        - Location or object identifier; may be dataset or group
!               old_attr_name - Prior attribute name
!               new_attr_name - New attribute name
!
! Outputs:
!		hdferr:	      - error code
!				 	Success:  0
!				 	Failure: -1
!
! Programmer:	M.S. Breitenfeld
!		January, 2008
!
! Modifications: N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5arename_f(loc_id, old_attr_name, new_attr_name, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id    ! Object identifier
    CHARACTER(LEN=*), INTENT(IN) :: old_attr_name ! Prior attribute name
    CHARACTER(LEN=*), INTENT(IN) :: new_attr_name ! New attribute name
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure
    INTEGER(SIZE_T) :: old_attr_namelen
    INTEGER(SIZE_T) :: new_attr_namelen

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5arename_c(loc_id, &
            old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5ARENAME_C'::h5arename_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: old_attr_name, new_attr_name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: old_attr_name
         INTEGER(SIZE_T) :: old_attr_namelen
         CHARACTER(LEN=*), INTENT(IN) :: new_attr_name
         INTEGER(SIZE_T) :: new_attr_namelen

       END FUNCTION h5arename_c
    END INTERFACE

    old_attr_namelen = LEN(old_attr_name)
    new_attr_namelen = LEN(new_attr_name)

    hdferr = h5arename_c(loc_id, &
         old_attr_name, old_attr_namelen, new_attr_name, new_attr_namelen)

  END SUBROUTINE h5arename_f

END MODULE H5A


