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
! This file contains FORTRAN90 interfaces for H5T functions
!
MODULE H5T

  USE H5GLOBAL

CONTAINS

!----------------------------------------------------------------------
! Name:		h5topen_f
!
! Purpose: 	Opens named datatype.
!
! Inputs:
!		loc_id		- location identifier
!		name		- a datatype name
! Outputs:
!		type_id		- datatype identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!              tapl_id          - datatype access property list identifier.
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: Explicit Fortran interfaces were added for
!		 called C functions (it is needed for Windows
!		 port).  March 7, 2001
!
!                Added optional parameter 'tapl_id' for compatability
!                with H5Topen2. April 9, 2009.
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5topen_f(loc_id, name, type_id, hdferr, tapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id  ! File or group identifier
    CHARACTER(LEN=*), INTENT(IN) :: name ! Datatype name within file or group
    INTEGER(HID_T), INTENT(OUT) :: type_id  ! Datatype identifier
    INTEGER, INTENT(OUT) :: hdferr ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: tapl_id ! datatype access property list identifier

    INTEGER :: namelen                  ! Name length
    INTEGER(HID_T) :: tapl_id_default
!
!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5topen_c(loc_id, name, namelen, type_id, tapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TOPEN_C'::h5topen_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference ::name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(OUT) :: type_id
         INTEGER(HID_T) :: tapl_id_default
       END FUNCTION h5topen_c
    END INTERFACE

    namelen = LEN(name)

    tapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(tapl_id)) tapl_id_default = tapl_id

    hdferr = h5topen_c(loc_id, name, namelen, type_id, tapl_id_default)
  END SUBROUTINE h5topen_f

!----------------------------------------------------------------------
! Name:		h5tcommit_f
!
! Purpose: 	Commits a transient datatype to a file, creating a
!		new named datatype.
!
! Inputs:
!		loc_id		- location identifier
!		name		- name of the datatype to be stored
!				  at the specified location
!		type_id		- identifier of a datatype to be stored
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!	       lcpl_id          - Link creation property list
!              tcpl_id          - Datatype creation property list
!              tapl_id          - Datatype access property list
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: - Explicit Fortran interfaces were added for
!		   called C functions (it is needed for Windows
!	           port).  March 7, 2001
!
!                - Added optional parameters introduced in version 1.8
!                  M.S. Breitenfeld
!
!
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5tcommit_f(loc_id, name, type_id, hdferr, &
       lcpl_id, tcpl_id, tapl_id  )
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id  ! File or group identifier
    CHARACTER(LEN=*), INTENT(IN) :: name
                                  ! Datatype name within file or group
    INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lcpl_id ! Link creation property list
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: tcpl_id ! Datatype creation property list
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: tapl_id ! Datatype access property list


    INTEGER :: namelen          ! Name length

    INTEGER(HID_T) :: lcpl_id_default
    INTEGER(HID_T) :: tcpl_id_default
    INTEGER(HID_T) :: tapl_id_default

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5tcommit_c(loc_id, name, namelen, type_id, &
            lcpl_id_default, tcpl_id_default, tapl_id_default )
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TCOMMIT_C'::h5tcommit_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference ::name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER :: namelen
         INTEGER(HID_T), INTENT(IN) :: type_id
         INTEGER(HID_T) :: lcpl_id_default
         INTEGER(HID_T) :: tcpl_id_default
         INTEGER(HID_T) :: tapl_id_default
       END FUNCTION h5tcommit_c
    END INTERFACE

    lcpl_id_default = H5P_DEFAULT_F
    tcpl_id_default = H5P_DEFAULT_F
    tapl_id_default = H5P_DEFAULT_F

    IF (PRESENT(lcpl_id)) lcpl_id_default = lcpl_id
    IF (PRESENT(tcpl_id)) tcpl_id_default = tcpl_id
    IF (PRESENT(tapl_id)) tapl_id_default = tapl_id

    namelen = LEN(name)

    hdferr = h5tcommit_c(loc_id, name, namelen, type_id, &
         lcpl_id_default, tcpl_id_default, tapl_id_default )

  END SUBROUTINE h5tcommit_f

!----------------------------------------------------------------------
! Name:		h5tcopy_f
!
! Purpose: 	iCreates a copy of exisiting datatype.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		new_type_id	- identifier of datatype's copy
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tcopy_f(type_id, new_type_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(HID_T), INTENT(OUT) :: new_type_id
                                 ! Identifier of datatype's copy
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tcopy_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION  h5tcopy_c(type_id, new_type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TCOPY_C'::h5tcopy_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(HID_T), INTENT(OUT) :: new_type_id
              END FUNCTION h5tcopy_c
            END INTERFACE

            hdferr = h5tcopy_c(type_id, new_type_id)
          END SUBROUTINE h5tcopy_f

!----------------------------------------------------------------------
! Name:		h5tequal_f
!
! Purpose: 	Determines whether two datatype identifiers refer
!		to the same datatype.
!
! Inputs:
!		type1_id	- datatype identifier
!		type2_id	- datatype identifier
! Outputs:
!		flag		- TRUE/FALSE flag to indicate
!				  if two datatypes are equal
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tequal_f(type1_id, type2_id, flag, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type1_id ! Datatype identifier
            INTEGER(HID_T), INTENT(IN) :: type2_id ! Datatype identifier
            LOGICAL, INTENT(OUT) :: flag ! TRUE/FALSE flag to indicate if two
                                         ! datatypes are equal
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER :: c_flag

!            INTEGER, EXTERNAL :: h5tequal_c
!  MS FORTRAN needs explicit interface for C functions called here
            INTERFACE
              INTEGER FUNCTION h5tequal_c(type1_id, type2_id, c_flag)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TEQUAL_C'::h5tequal_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type1_id
              INTEGER(HID_T), INTENT(IN) :: type2_id
              INTEGER :: c_flag
              END FUNCTION h5tequal_c
            END INTERFACE

            flag = .FALSE.
            hdferr = h5tequal_c(type1_id, type2_id, c_flag)
            if(c_flag .gt. 0) flag = .TRUE.
          END SUBROUTINE h5tequal_f

!----------------------------------------------------------------------
! Name:		h5tclose_f
!
! Purpose:	Releases a datatype.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tclose_f(type_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL ::  h5tclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tclose_c(type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TCLOSE_C'::h5tclose_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              END FUNCTION h5tclose_c
            END INTERFACE

            hdferr = h5tclose_c(type_id)
          END SUBROUTINE h5tclose_f

!----------------------------------------------------------------------
! Name:		h5tget_class_f
!
! Purpose:	Returns the datatype class identifier.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		class		- class, possible values are:
!					 H5T_NO_CLASS_F (-1)
!					 H5T_INTEGER_F  (0)
!					 H5T_FLOAT_F (1)
!					 H5T_TIME_F  (2)
!					 H5T_STRING_F (3)
!					 H5T_BITFIELD_F (4)
!					 H5T_OPAQUE_F (5)
!					 H5T_COMPOUND_F (6)
!					 H5T_REFERENCE_F (7)
!					 H5T_ENUM_F (8)
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tget_class_f(type_id, class, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: class
                           ! Datatype class, possible values are:
                                          ! H5T_NO_CLASS_F (-1)
                                          ! H5T_INTEGER_F  (0)
                                          ! H5T_FLOAT_F (1)
                                          ! H5T_TIME_F  (2)
                                          ! H5T_STRING_F (3)
                                          ! H5T_BITFIELD_F (4)
                                          ! H5T_OPAQUE_F (5)
                                          ! H5T_COMPOUND_F (6)
                                          ! H5T_REFERENCE_F (7)
                                          ! H5T_ENUM_F (8)
          INTEGER, INTENT(OUT) :: hdferr        ! Error code

!          INTEGER, EXTERNAL :: h5tget_class_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_class_c(type_id, class)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_CLASS_C'::h5tget_class_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(OUT) :: class
              END FUNCTION h5tget_class_c
            END INTERFACE

          hdferr = h5tget_class_c(type_id, class)
          END SUBROUTINE h5tget_class_f

!----------------------------------------------------------------------
! Name:		h5tget_size_f
!
! Purpose: 	Returns the size of a datatype.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		size		- datatype size
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tget_size_f(type_id, size, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(SIZE_T), INTENT(OUT) :: size ! Datatype size
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_size_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_size_c(type_id, size)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_SIZE_C'::h5tget_size_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(SIZE_T), INTENT(OUT) :: size
              END FUNCTION h5tget_size_c
            END INTERFACE

            hdferr = h5tget_size_c(type_id, size)
          END SUBROUTINE h5tget_size_f

!----------------------------------------------------------------------
! Name:		h5tset_size_f
!
! Purpose: 	Sets the total size for an atomic datatype.
!
! Inputs:
!		type_id		- datatype identifier
!		size		- size of the datatype
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tset_size_f(type_id, size, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(SIZE_T), INTENT(IN) :: size ! Datatype size
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_size_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_size_c(type_id, size)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_SIZE_C'::h5tset_size_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(SIZE_T), INTENT(IN) :: size
              END FUNCTION h5tset_size_c
            END INTERFACE

            hdferr = h5tset_size_c(type_id, size)
          END SUBROUTINE h5tset_size_f

!----------------------------------------------------------------------
! Name:		h5tget_order_f
!
! Purpose: 	Returns the byte order of an atomic datatype.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		order		- byte order for the datatype, possible
!				  values are:
!					 H5T_ORDER_LE_F
!					 H5T_ORDER_BE_F
!					 H5T_ORDER_VAX_F (not implemented yet)
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tget_order_f(type_id, order, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: order
                              ! Datatype byte order, bossible values are:
                                          ! H5T_ORDER_LE_F
                                          ! H5T_ORDER_BE_F
                                          ! H5T_ORDER_VAX_F
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_order_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_order_c(type_id, order)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_ORDER_C'::h5tget_order_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(OUT) :: order
              END FUNCTION h5tget_order_c
            END INTERFACE

            hdferr = h5tget_order_c(type_id, order)
          END SUBROUTINE h5tget_order_f

!----------------------------------------------------------------------
! Name:		h5tset_order_f
!
! Purpose: 	Sets the byte ordering of an atomic datatype.
!
! Inputs:
!		type_id		- datatype identifier
!		order		- datatype byte order
!				  Possible values are:
!					 H5T_ORDER_LE_F
!					 H5T_ORDER_BE_F
!					 H5T_ORDER_VAX_F (not implemented yet)
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tset_order_f(type_id, order, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: order ! Datatype byte order, bossible values
                                          ! are:
                                          ! H5T_ORDER_LE_F
                                          ! H5T_ORDER_BE_F
                                          ! H5T_ORDER_VAX_F
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_order_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_order_c(type_id, order)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_ORDER_C'::h5tset_order_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: order
              END FUNCTION h5tset_order_c
            END INTERFACE

            hdferr = h5tset_order_c(type_id, order)
          END SUBROUTINE h5tset_order_f

!----------------------------------------------------------------------
! Name:		h5tget_precision_f
!
! Purpose: 	Returns the precision of an atomic datatype.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		precision	- precision of the datatype
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tget_precision_f(type_id, precision, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(SIZE_T), INTENT(OUT) :: precision ! Datatype precision
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_precision_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_precision_c (type_id, precision)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_PRECISION_C'::h5tget_precision_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(SIZE_T), INTENT(OUT) :: precision
              END FUNCTION h5tget_precision_c
            END INTERFACE

            hdferr = h5tget_precision_c(type_id, precision)
          END SUBROUTINE h5tget_precision_f

!----------------------------------------------------------------------
! Name:		h5tset_precision_f
!
! Purpose: 	Sets the precision of an atomic datatype.
!
! Inputs:
!		type_id		- datatype identifier
!		precision	- datatype precision
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tset_precision_f(type_id, precision, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(SIZE_T), INTENT(IN) :: precision ! Datatype precision
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_precision_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_precision_c (type_id, precision)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_PRECISION_C'::h5tset_precision_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(SIZE_T), INTENT(IN) :: precision
              END FUNCTION h5tset_precision_c
            END INTERFACE

            hdferr = h5tset_precision_c(type_id, precision)
          END SUBROUTINE h5tset_precision_f

!----------------------------------------------------------------------
! Name:		h5tget_offset_f
!
! Purpose: 	Retrieves the bit offset of the first significant bit.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		offset		- offset value
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_offset_f(type_id, offset, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(SIZE_T), INTENT(OUT) :: offset ! Datatype bit offset of the
                                           ! first significant bit
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_offset_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_offset_c(type_id, offset)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_OFFSET_C'::h5tget_offset_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(SIZE_T), INTENT(OUT) :: offset
              END FUNCTION h5tget_offset_c
            END INTERFACE

            hdferr = h5tget_offset_c(type_id, offset)
          END SUBROUTINE h5tget_offset_f

!----------------------------------------------------------------------
! Name:		h5tset_offset_f
!
! Purpose: 	Sets the bit offset of the first significant bit.
!
! Inputs:
!		type_id		- datatype identifier
!		offset		- offset value
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tset_offset_f(type_id, offset, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(SIZE_T), INTENT(IN) :: offset ! Datatype bit offset of the
                                           ! first significant bit
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_offset_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_offset_c(type_id, offset)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_OFFSET_C'::h5tset_offset_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(SIZE_T), INTENT(IN) :: offset
              END FUNCTION h5tset_offset_c
            END INTERFACE

            hdferr = h5tset_offset_c(type_id, offset)
          END SUBROUTINE h5tset_offset_f

!----------------------------------------------------------------------
! Name:		h5tget_pad_f
!
! Purpose: 	Retrieves the padding type of the least and
!		most-significant bit padding.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		lsbpad		- least-significant bit padding type
!		msbpad		- most-significant bit padding type
!					 Possible values of padding type are:
!					 H5T_PAD_ERROR_F      = -1
!					 H5T_PAD_ZERO_F = 0
!					 H5T_PAD_ONE_F = 1
!					 H5T_PAD_BACKGROUND_F = 2
!					 H5T_PAD_NPAD_F      = 3
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_pad_f(type_id, lsbpad, msbpad, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: lsbpad ! padding type of the
                                           ! least significant bit
            INTEGER, INTENT(OUT) :: msbpad ! padding type of the
                                           ! most significant bit
                                           ! Possible values of padding type are:
                                           ! H5T__PAD_ZERO_F = 0
                                           ! H5T__PAD_ONE_F = 1
                                           ! H5T__PAD_BACKGROUND_F = 2
                                           ! H5T_PAD_ERROR_F      = -1
                                           ! H5T_PAD_NPAD_F      = 3

            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_pad_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_pad_c(type_id, lsbpad, msbpad)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_PAD_C'::h5tget_pad_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(OUT) :: lsbpad
              INTEGER, INTENT(OUT) :: msbpad
              END FUNCTION h5tget_pad_c
            END INTERFACE

            hdferr = h5tget_pad_c(type_id, lsbpad, msbpad)
          END SUBROUTINE h5tget_pad_f

!----------------------------------------------------------------------
! Name:		h5tset_pad_f
!
! Purpose: 	Sets the least and most-significant bits padding types.
!
! Inputs:
!		type_id		- datatype identifier
!		lsbpad		- least-significant bit padding type
!		msbpad		- most-significant bit padding type
!					 Possible values of padding type are:
!					 H5T_PAD_ERROR_F      = -1
!					 H5T_PAD_ZERO_F = 0
!					 H5T_PAD_ONE_F = 1
!					 H5T_PAD_BACKGROUND_F = 2
!					 H5T_PAD_NPAD_F      = 3
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tset_pad_f(type_id, lsbpad, msbpad, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: lsbpad ! padding type of the
                                           ! least significant bit
            INTEGER, INTENT(IN) :: msbpad ! padding type of the
                                           ! most significant bit
                                           ! Possible values of padding type are:
                                           ! H5T_PAD_ZERO_F = 0
                                           ! H5T_PAD_ONE_F = 1
                                           ! H5T_PAD_BACKGROUND_F = 2
                                           ! H5T_PAD_ERROR_F      = -1
                                           ! H5T_PAD_NPAD_F      = 3
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5sget_pad_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_pad_c(type_id, lsbpad, msbpad)
              USE H5GLOBAL
              INTEGER(HID_T), INTENT(IN) :: type_id
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_PAD_C'::h5tset_pad_c
              !DEC$ENDIF
              INTEGER, INTENT(IN) :: lsbpad
              INTEGER, INTENT(IN) :: msbpad
              END FUNCTION h5tset_pad_c
            END INTERFACE

            hdferr = h5tset_pad_c(type_id, lsbpad, msbpad)
          END SUBROUTINE h5tset_pad_f

!----------------------------------------------------------------------
! Name:		h5tget_sign_f
!
! Purpose: 	Retrieves the sign type for an integer type.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		sign		- sign type
!					Possible values are:
!					Unsigned integer type H5T_SGN_NONE_F = 0
!					Two's complement signed integer type
!					H5T_SGN_2_F = 1
!					or error value: H5T_SGN_ERROR_F=-1
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_sign_f(type_id, sign, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: sign ! sign type for an integer type
                                         !possible values are:
                                         !Unsigned integer type H5T_SGN_NONE_F = 0
                                         !Two's complement signed integer type
                                         !H5T_SGN_2_F = 1
                                         !or error value: H5T_SGN_ERROR_F=-1
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_sign_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_sign_c(type_id, sign)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_SIGN_C'::h5tget_sign_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(OUT) :: sign
              END FUNCTION h5tget_sign_c
            END INTERFACE

            hdferr = h5tget_sign_c(type_id, sign)
          END SUBROUTINE h5tget_sign_f

!----------------------------------------------------------------------
! Name:		h5tset_sign_f
!
! Purpose: 	Sets the sign proprety for an integer type.
!
! Inputs:
!		type_id		- datatype identifier
!		sign		- sign type
!					Possible values are:
!					Unsigned integer type H5T_SGN_NONE_F = 0
!					Two's complement signed integer type
!					H5T_SGN_2_F = 1
!					or error value: H5T_SGN_ERROR_F=-1
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tset_sign_f(type_id, sign, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: sign !sign type for an integer type
                                         !possible values are:
                                         !Unsigned integer type H5T_SGN_NONE_F = 0
                                         !Two's complement signed integer type
                                         !H5T_SGN_2_F = 1
                                         !or error value: H5T_SGN_ERROR_F=-1
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_sign_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_sign_c(type_id, sign)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_SIGN_C'::h5tset_sign_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: sign
              END FUNCTION h5tset_sign_c
            END INTERFACE

            hdferr = h5tset_sign_c(type_id, sign)
          END SUBROUTINE h5tset_sign_f

!----------------------------------------------------------------------
! Name:		h5tget_fields_f
!
! Purpose: 	Retrieves floating point datatype bit field information.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		spos		- sign bit-position
!		epos		- exponent bit-position
!		esize		- size of exponent in bits
!		mpos		- mantissa position
!		msize		- size of mantissa in bits
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_fields_f(type_id, spos, epos, esize, mpos, msize, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(SIZE_T), INTENT(OUT) :: spos   ! sign bit-position
            INTEGER(SIZE_T), INTENT(OUT) :: epos   ! exponent bit-position
            INTEGER(SIZE_T), INTENT(OUT) :: esize  ! size of exponent in bits
            INTEGER(SIZE_T), INTENT(OUT) :: mpos   ! mantissa bit-position
            INTEGER(SIZE_T), INTENT(OUT) :: msize  ! size of mantissa in bits
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_fields_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_fields_c(type_id, spos, epos, esize, mpos, msize)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_FIELDS_C'::h5tget_fields_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(SIZE_T), INTENT(OUT) :: spos
              INTEGER(SIZE_T), INTENT(OUT) :: epos
              INTEGER(SIZE_T), INTENT(OUT) :: esize
              INTEGER(SIZE_T), INTENT(OUT) :: mpos
              INTEGER(SIZE_T), INTENT(OUT) :: msize
              END FUNCTION h5tget_fields_c
            END INTERFACE

            hdferr = h5tget_fields_c(type_id, spos, epos, esize, mpos, msize)
          END SUBROUTINE h5tget_fields_f

!----------------------------------------------------------------------
! Name:		h5tset_fields_f
!
! Purpose: 	Sets locations and sizes of floating point bit fields.
!
! Inputs:
!		type_id		- datatype identifier
!		spos		- sign bit-position
!		epos		- exponent bit-position
!		esize		- size of exponent in bits
!		mpos		- mantissa position
!		msize		- size of mantissa in bits
!		hdferr:		- error code
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tset_fields_f(type_id, spos, epos, esize, mpos, msize, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(SIZE_T), INTENT(IN) :: spos   ! sign bit-position
            INTEGER(SIZE_T), INTENT(IN) :: epos   ! exponent bit-position
            INTEGER(SIZE_T), INTENT(IN) :: esize  ! size of exponent in bits
            INTEGER(SIZE_T), INTENT(IN) :: mpos   ! mantissa bit-position
            INTEGER(SIZE_T), INTENT(IN) :: msize  ! size of mantissa in bits
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_fields_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_fields_c(type_id, spos, epos, esize, mpos, msize)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_FIELDS_C'::h5tset_fields_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(SIZE_T), INTENT(IN) :: spos
              INTEGER(SIZE_T), INTENT(IN) :: epos
              INTEGER(SIZE_T), INTENT(IN) :: esize
              INTEGER(SIZE_T), INTENT(IN) :: mpos
              INTEGER(SIZE_T), INTENT(IN) :: msize
              END FUNCTION h5tset_fields_c
            END INTERFACE

            hdferr = h5tset_fields_c(type_id, spos, epos, esize, mpos, msize)
          END SUBROUTINE h5tset_fields_f

!----------------------------------------------------------------------
! Name:		h5tget_ebias_f
!
! Purpose: 	Retrieves the exponent bias of a floating-point type.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		ebias		- datatype exponent bias
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_ebias_f(type_id, ebias, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(SIZE_T), INTENT(OUT) :: ebias ! Datatype exponent bias of a floating-point type
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_ebias_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_ebias_c(type_id, ebias)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_EBIAS_C'::h5tget_ebias_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(SIZE_T), INTENT(OUT) :: ebias
              END FUNCTION h5tget_ebias_c
            END INTERFACE

            hdferr = h5tget_ebias_c(type_id, ebias)
          END SUBROUTINE h5tget_ebias_f

!----------------------------------------------------------------------
! Name:		h5tset_ebias_f
!
! Purpose: 	Sets the exponent bias of a floating-point type.
!
! Inputs:
!		type_id		- datatype identifier
!		ebias		- datatype exponent bias
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tset_ebias_f(type_id, ebias, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER(SIZE_T), INTENT(IN) :: ebias !Datatype exponent bias of a floating-point type
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_ebias_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_ebias_c(type_id, ebias)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_EBIAS_C'::h5tset_ebias_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(SIZE_T), INTENT(IN) :: ebias
              END FUNCTION h5tset_ebias_c
            END INTERFACE

            hdferr = h5tset_ebias_c(type_id, ebias)
          END SUBROUTINE h5tset_ebias_f

!----------------------------------------------------------------------
! Name:		h5tget_norm_f
!
! Purpose: 	Retrieves mantissa normalization of a floating-point
!		datatype.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		norm		- normalization types, valid values are:
!					H5T_NORM_IMPLIED_F(0)
!					H5T_NORM_MSBSET_F(1)
!					H5T_NORM_NONE_F(2)
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_norm_f(type_id, norm, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: norm !mantissa normalization of a floating-point datatype
                                         !Valid normalization types are:
                                         !H5T_NORM_IMPLIED_F(0),MSB of mantissa is not
                                         !stored, always 1,  H5T_NORM_MSBSET_F(1), MSB of
                                         !mantissa is always 1, H5T_NORM_NONE_F(2)
                                         !Mantissa is not normalize
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_norm_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_norm_c(type_id, norm)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_NORM_C'::h5tget_norm_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(OUT) :: norm
              END FUNCTION h5tget_norm_c
            END INTERFACE

            hdferr = h5tget_norm_c(type_id, norm)
          END SUBROUTINE h5tget_norm_f

!----------------------------------------------------------------------
! Name:		h5tset_norm_f
!
! Purpose: 	Sets the mantissa normalization of a floating-point datatype.
!
! Inputs:
!		type_id		- datatype identifier
!		norm		- normalization types, valid values are:
!					H5T_NORM_IMPLIED_F(0)
!					H5T_NORM_MSBSET_F(1)
!					H5T_NORM_NONE_F(2)
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tset_norm_f(type_id, norm, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: norm !mantissa normalization of a floating-point datatype
                                         !Valid normalization types are:
                                         !H5T_NORM_IMPLIED_F(0),MSB of mantissa is not
                                         !stored, always 1,  H5T_NORM_MSBSET_F(1), MSB of
                                         !mantissa is always 1, H5T_NORM_NONE_F(2)
                                         !Mantissa is not normalize
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_norm_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_norm_c(type_id, norm)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_NORM_C'::h5tset_norm_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: norm
              END FUNCTION h5tset_norm_c
            END INTERFACE

            hdferr = h5tset_norm_c(type_id, norm)
          END SUBROUTINE h5tset_norm_f

!----------------------------------------------------------------------
! Name:		h5tget_inpad_f
!
! Purpose: 	Retrieves the internal padding type for unused bits
!		in floating-point datatypes.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		padtype		- padding type for unused bits
!				  Possible values of padding type are:
!					 H5T_PAD_ZERO_F = 0
!					 H5T_PAD_ONE_F = 1
!					 H5T_PAD_BACKGROUND_F = 2
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_inpad_f(type_id, padtype, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: padtype ! padding type for unused bits
                                            ! in floating-point datatypes.
                                            ! Possible values of padding type are:
                                            ! H5T__PAD_ZERO_F = 0
                                            ! H5T__PAD_ONE_F = 1
                                            ! H5T__PAD_BACKGROUND_F = 2

            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_inpad_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_inpad_c(type_id, padtype)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_INPAD_C'::h5tget_inpad_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(OUT) :: padtype
              END FUNCTION h5tget_inpad_c
            END INTERFACE

            hdferr = h5tget_inpad_c(type_id, padtype)
          END SUBROUTINE h5tget_inpad_f

!----------------------------------------------------------------------
! Name:		h5tset_inpad_f
!
! Purpose: 	Fills unused internal floating point bits.
!
! Inputs:
!		type_id		- datatype identifier
!		padtype		- padding type for unused bits
!				  Possible values of padding type are:
!					 H5T_PAD_ZERO_F = 0
!					 H5T_PAD_ONE_F = 1
!					 H5T_PAD_BACKGROUND_F = 2
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tset_inpad_f(type_id, padtype, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: padtype ! padding type for unused bits
                                           ! in floating-point datatypes.
                                           ! Possible values of padding type are:
                                           ! H5T__PAD_ZERO_F = 0
                                           ! H5T__PAD_ONE_F = 1
                                           ! H5T__PAD_BACKGROUND_F = 2
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_inpad_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_inpad_c(type_id, padtype)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_INPAD_C'::h5tset_inpad_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: padtype
              END FUNCTION h5tset_inpad_c
            END INTERFACE

            hdferr = h5tset_inpad_c(type_id, padtype)
          END SUBROUTINE h5tset_inpad_f

!----------------------------------------------------------------------
! Name:		h5tget_cset_f
!
! Purpose: 	Retrieves the character set type of a string datatype.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		cset		- character set type of a string datatype
!				  Possible values are:
!				                  H5T_CSET_ASCII_F = 0
!                                                 H5T_CSET_UTF8_F
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_cset_f(type_id, cset, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: cset ! character set type of a string datatype
                                            ! Possible values are:
                                            !    H5T_CSET_ASCII_F = 0
                                            !    H5T_CSET_UTF8_F
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_cset_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_cset_c(type_id, cset)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_CSET_C'::h5tget_cset_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(OUT) :: cset
              END FUNCTION h5tget_cset_c
            END INTERFACE

            hdferr = h5tget_cset_c(type_id, cset)
          END SUBROUTINE h5tget_cset_f

!----------------------------------------------------------------------
! Name:		h5tset_cset_f
!
! Purpose: 	Sets character set to be used.
!
! Inputs:
!		type_id		- datatype identifier
!		cset		- character set type of a string datatype
!				  Possible values are:
!				                  H5T_CSET_ASCII_F = 0
!                                                 H5T_CSET_UTF8_F
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tset_cset_f(type_id, cset, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: cset !character set type of a string datatype
                                           !Possible values are:
                                           !     H5T_CSET_ASCII_F = 0
                                           !     H5T_CSET_UTF8_F

            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_cset_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_cset_c(type_id, cset)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_CSET_C'::h5tset_cset_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: cset
              END FUNCTION h5tset_cset_c
            END INTERFACE

            hdferr = h5tset_cset_c(type_id, cset)
          END SUBROUTINE h5tset_cset_f

!----------------------------------------------------------------------
! Name:		h5tget_strpad_f
!
! Purpose: 	Retrieves the storage mechanism for a string datatype.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		strpad		- storage method for a string datatype
!				  Possible values are:
!				  H5T_STR_NULLTERM_F,
!				  H5T_STR_NULLPAD_F,
!				  H5T_STR_SPACEPAD_F
!				  H5T_STR_ERROR_F
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_strpad_f(type_id, strpad, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: strpad
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_strpad_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_strpad_c(type_id, strpad)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_STRPAD_C'::h5tget_strpad_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(OUT) :: strpad
              END FUNCTION h5tget_strpad_c
            END INTERFACE

            hdferr = h5tget_strpad_c(type_id, strpad)
          END SUBROUTINE h5tget_strpad_f

!----------------------------------------------------------------------
! Name:		h5tset_strpad_f
!
! Purpose: 	Defines the storage mechanism for character strings.
!
! Inputs:
!		type_id		- datatype identifier
!		strpad		- storage method for a string datatype
!				  Possible values are:
!				  H5T_STR_NULLTERM_F,
!				  H5T_STR_NULLPAD_F,
!				  H5T_STR_SPACEPAD_F
!				  H5T_STR_ERROR_F
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tset_strpad_f(type_id, strpad, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: strpad ! string padding method for a string datatype
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tset_strpad_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_strpad_c(type_id, strpad)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_STRPAD_C'::h5tset_strpad_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: strpad
              END FUNCTION h5tset_strpad_c
            END INTERFACE

            hdferr = h5tset_strpad_c(type_id, strpad)
          END SUBROUTINE h5tset_strpad_f

!----------------------------------------------------------------------
! Name:		h5tget_nmembers_f
!
! Purpose: 	Retrieves the number of fields in a compound datatype.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		num_members	- number of members
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tget_nmembers_f(type_id, num_members, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: num_members !number of fields in a compound datatype
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_nmembers_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_nmembers_c(type_id, num_members)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_NMEMBERS_C'::h5tget_nmembers_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(OUT) :: num_members
              END FUNCTION h5tget_nmembers_c
            END INTERFACE

            hdferr = h5tget_nmembers_c(type_id, num_members)
          END SUBROUTINE h5tget_nmembers_f

!----------------------------------------------------------------------
! Name:		h5tget_member_name_f
!
! Purpose: 	Retrieves the name of a field of a compound datatype.
!
! Inputs:
!		type_id		- datatype identifier
!		index		- filed index (0-based)
! Outputs:
!		member_name	- buffer to hold member's name
!		namelen		- name length
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_member_name_f(type_id, index, member_name,  namelen, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: index !Field index (0-based) of the field name to retrieve
            CHARACTER(LEN=*), INTENT(OUT) :: member_name !name of a field of
                                                         !a compound datatype
            INTEGER, INTENT(OUT) :: namelen ! Length of the name
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_member_name_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_member_name_c(type_id, index, member_name, namelen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_MEMBER_NAME_C'::h5tget_member_name_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: member_name
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: index
              CHARACTER(LEN=*), INTENT(OUT) :: member_name
              INTEGER, INTENT(OUT) :: namelen
              END FUNCTION
            END INTERFACE

            hdferr = h5tget_member_name_c(type_id, index, member_name, namelen)
          END SUBROUTINE h5tget_member_name_f

!----------------------------------------------------------------------
! Name:		h5tget_member_offset_f
!
! Purpose: 	Retrieves the offset of a field of a compound datatype.
!
! Inputs:
!		type_id		- datatype identifier
!		member_no 	- number of the field
! Outputs:
!		offset		- byte offset of the requested field
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_member_offset_f(type_id, member_no, offset, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: member_no !Number of the field
                                                       !whose offset is requested
            INTEGER(SIZE_T), INTENT(OUT) :: offset !byte offset of the beginning of the field
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_member_offset_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_member_offset_c(type_id, member_no, offset )
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_MEMBER_OFFSET_C'::h5tget_member_offset_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: member_no
              INTEGER(SIZE_T), INTENT(OUT) :: offset
              END FUNCTION h5tget_member_offset_c
            END INTERFACE

            hdferr = h5tget_member_offset_c(type_id, member_no, offset )
          END SUBROUTINE h5tget_member_offset_f
!----------------------------------------------------------------------
! Name:		h5tget_member_index_f
!
! Purpose: 	Retrieves the index of a compound or enumeration datatype member.
!
! Inputs:
!		type_id		- datatype identifier
!		name		- name of the field or member whose index to
!                                 to be retrieved from the datatype.
! Outputs:
!               index           - 0-based index of the filed or member (0 to N-1)
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		September 26, 2002
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_member_index_f(type_id, name, index, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier
            CHARACTER(LEN=*), INTENT(IN) :: name   ! Field or member name
            INTEGER, INTENT(OUT) :: index          ! Field or member index
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER :: namelen          ! Name length

            INTERFACE
              INTEGER FUNCTION h5tget_member_index_c(type_id, name, namelen, index)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_MEMBER_INDEX_C'::h5tget_member_index_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference ::name
              INTEGER(HID_T), INTENT(IN) :: type_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER, INTENT(IN)  :: namelen
              INTEGER, INTENT(OUT) :: index
              END FUNCTION h5tget_member_index_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5tget_member_index_c(type_id, name, namelen, index)
          END SUBROUTINE h5tget_member_index_f


!----------------------------------------------------------------------
! Name:		h5tget_member_dim_f
!
! Purpose: 	This function is not supported in hdf5-1.4.*
!
! Inputs:
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

!          SUBROUTINE h5tget_member_dims_f(type_id, field_idx,dims, field_dims, perm, hdferr)
!
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
!            INTEGER, INTENT(IN) :: field_idx !Field index (0-based) of
!                                             !field_dims, perm)
!            INTEGER, INTENT(OUT) :: dims     !number of dimensions of the field
!
!            INTEGER(SIZE_T),DIMENSION(*), INTENT(OUT) ::  field_dims !buffer to store the
!                                                                      !dimensions of the field
!            INTEGER, DIMENSION(*), INTENT(OUT)  ::  perm  !buffer to store the
!                                                                   !permutation vector of the field
!            INTEGER, INTENT(OUT) :: hdferr        ! Error code
!            INTEGER, EXTERNAL :: h5tget_member_dims_c
!            hdferr = h5tget_member_dims_c(type_id, field_idx, dims, field_dims, perm)
!
!          END SUBROUTINE h5tget_member_dims_f

!----------------------------------------------------------------------
! Name:		h5tget_array_dims_f
!
! Purpose: 	Returns sizes of array dimensions.
!
! Inputs:
!		type_id		- array datatype identifier
! Outputs:
!		dims		- buffer to store array datatype
!				  dimensions
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_array_dims_f(type_id, dims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Array datatype identifier
            INTEGER(HSIZE_T),DIMENSION(*), INTENT(OUT) ::  dims !buffer to store array datatype
                                                                ! dimensions
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_array_dims_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_array_dims_c(type_id, dims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_ARRAY_DIMS_C'::h5tget_array_dims_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(HSIZE_T),DIMENSION(*), INTENT(OUT) ::  dims
              END FUNCTION h5tget_array_dims_c
            END INTERFACE

            hdferr = h5tget_array_dims_c(type_id, dims)

          END SUBROUTINE h5tget_array_dims_f

!----------------------------------------------------------------------
! Name:		h5tget_array_ndims_f
!
! Purpose: 	Returns the rank of an array datatype.
!
! Inputs:
!		type_id		- array datatype identifier
! Outputs:
!		ndims		- number of array dimensions
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_array_ndims_f(type_id, ndims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Array datatype identifier
            INTEGER, INTENT(OUT) ::  ndims ! number of array dimensions
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_array_ndims_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_array_ndims_c(type_id, ndims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_ARRAY_NDIMS_C'::h5tget_array_ndims_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(OUT) ::  ndims
              END FUNCTION h5tget_array_ndims_c
            END INTERFACE

            hdferr = h5tget_array_ndims_c(type_id, ndims)

          END SUBROUTINE h5tget_array_ndims_f

!----------------------------------------------------------------------
! Name:		h5tget_super_f
!
! Purpose: 	Returns the base datatype from which a datatype is derived.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		base_type_id	- identifier of the base type
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_super_f(type_id, base_type_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! datatype identifier
            INTEGER(HID_T), INTENT(OUT) :: base_type_id ! identifier of the datatype
                                           ! from which datatype (type_id) was derived
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_super_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_super_c(type_id, base_type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_SUPER_C'::h5tget_super_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER(HID_T), INTENT(OUT) :: base_type_id
              END FUNCTION h5tget_super_c
            END INTERFACE

            hdferr = h5tget_super_c(type_id, base_type_id)

          END SUBROUTINE h5tget_super_f

!----------------------------------------------------------------------
! Name:		h5tget_member_type_f
!
! Purpose: 	Returns the datatype of the specified member.
!
! Inputs:
!		type_id		- compound datatype identifier
!		field_idx	- field index (0-based)
!
! Outputs:
!		datatype	- idnetifier of the member's datatype
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_member_type_f(type_id,  field_idx, datatype, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: field_idx !Field index (0-based) of the field type to retrieve
            INTEGER(HID_T), INTENT(OUT) :: datatype !identifier of a copy of
                                                    !the datatype of the field
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_member_type_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_member_type_c(type_id, field_idx , datatype)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_MEMBER_TYPE_C'::h5tget_member_type_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: field_idx
              INTEGER(HID_T), INTENT(OUT) :: datatype
              END FUNCTION h5tget_member_type_c
            END INTERFACE

            hdferr = h5tget_member_type_c(type_id, field_idx , datatype)
          END SUBROUTINE h5tget_member_type_f

!----------------------------------------------------------------------
! Name:		h5tcreate_f
!
! Purpose: 	Creates a new dataype
!
! Inputs:
!		class		- datatype class, possible values are:
!					 H5T_COMPOUND_F
!					 H5T_ENUM_F
!					 H5T_OPAQUE_F
!		size		- datattype size
! Outputs:
!		type_id		- datatype identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------


          SUBROUTINE h5tcreate_f(class, size, type_id, hdferr)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: class ! Datatype class can be one of
                                         ! H5T_COMPOUND_F
                                         ! H5T_ENUM_F
                                         ! H5T_OPAQUE_F
            INTEGER(SIZE_T), INTENT(IN) :: size ! Size of the datatype
            INTEGER(HID_T), INTENT(OUT) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tcreate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tcreate_c(class, size, type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TCREATE_C'::h5tcreate_c
              !DEC$ENDIF
              INTEGER, INTENT(IN) :: class
              INTEGER(SIZE_T), INTENT(IN) :: size
              INTEGER(HID_T), INTENT(OUT) :: type_id
              END FUNCTION h5tcreate_c
            END INTERFACE

           hdferr = h5tcreate_c(class, size, type_id)
          END SUBROUTINE h5tcreate_f

!----------------------------------------------------------------------
! Name:		h5tinsert_f
!
! Purpose: 	Adds a new member to a compound datatype.
!
! Inputs:
!		type_id		- compound dattype identifier
!		name		- name of the field to insert
!		offset		- start of the member in an instance of
!				  the compound datatype
!		field_id	- datatype identifier of the field to insert
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tinsert_f(type_id,  name, offset, field_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            CHARACTER(LEN=*), INTENT(IN) :: name !Name of the field to insert
            INTEGER(SIZE_T), INTENT(IN) :: offset !start of the member in an instance of
                                                   !the compound datatype
            INTEGER(HID_T), INTENT(IN) :: field_id !datatype identifier of the new member

            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER :: namelen

!            INTEGER, EXTERNAL :: h5tinsert_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tinsert_c(type_id, name, namelen, offset, field_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TINSERT_C'::h5tinsert_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: type_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER(SIZE_T), INTENT(IN) :: offset
              INTEGER(HID_T), INTENT(IN) :: field_id
              INTEGER :: namelen
              END FUNCTION h5tinsert_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5tinsert_c(type_id, name, namelen, offset, field_id )
          END SUBROUTINE h5tinsert_f

!----------------------------------------------------------------------
! Name:		h5tpack_f
!
! Purpose: 	Recursively removes padding from within a compound datatype.
!
! Inputs:
!		type_id		- compound datatype identifier
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tpack_f(type_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tpack_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tpack_c(type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TPACK_C'::h5tpack_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              END FUNCTION h5tpack_c
            END INTERFACE

            hdferr = h5tpack_c(type_id)
          END SUBROUTINE h5tpack_f

!----------------------------------------------------------------------
! Name:		h5tinsert_array_f
!
! Purpose: 	This function is not available on hdf5-1.4.*
!
! Inputs:
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

!          SUBROUTINE h5tinsert_array_f(parent_id,name,offset, ndims, dims, member_id, hdferr, perm)
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: parent_id ! identifier of the parent compound datatype
!            CHARACTER(LEN=*), INTENT(IN) :: name !Name of the new member
!            INTEGER(SIZE_T), INTENT(IN) :: offset !Offset to start of new member
!                                                   !within compound datatype
!            INTEGER, INTENT(IN) ::  ndims !Dimensionality of new member.
!                                          !Valid values are 0 (zero) through 4 (four)
!            INTEGER(SIZE_T), DIMENSION(*), INTENT(IN) :: dims !Size of new member array
!            INTEGER(HID_T), INTENT(IN) :: member_id ! identifier of the datatype of the new member
!            INTEGER, INTENT(OUT) :: hdferr        ! Error code
!
!            INTEGER, DIMENSION(*), OPTIONAL, INTENT(IN) :: perm
!                                                               !Pointer to buffer to store
!                                                               !the permutation vector of the field
!            INTEGER :: namelen, sizeofperm
!            INTEGER, EXTERNAL :: h5tinsert_array_c,  h5tinsert_array_c2
!            namelen = LEN(name)
!            if (present(perm)) then
!              hdferr = h5tinsert_array_c(parent_id, name, namelen, offset, ndims,dims, member_id, perm)
!            else
!              hdferr = h5tinsert_array_c2(parent_id, name, namelen, offset, ndims,dims, member_id)
!            end if
!
!         END SUBROUTINE h5tinsert_array_f

!----------------------------------------------------------------------
! Name:		h5tarray_create_f
!
! Purpose: 	Creates an array datatype object.
!
! Inputs:
!		base_id		- datatype identifier for the array
!				  base datatype
!		rank		- rank of the array
!		dims		- array dimension sizes
! Outputs:
!		type_id		- array datatype identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tarray_create_f(base_id, rank, dims, type_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: base_id ! identifier of array base datatype
            INTEGER, INTENT(IN) ::  rank ! Rank of the array
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims !Sizes of each array dimension
            INTEGER(HID_T), INTENT(OUT) :: type_id ! identifier of the array datatype
            INTEGER, INTENT(OUT) :: hdferr        ! Error code


!            INTEGER, EXTERNAL :: h5tarray_create_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tarray_create_c(base_id, rank, dims, type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TARRAY_CREATE_C'::h5tarray_create_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: base_id
              INTEGER, INTENT(IN) ::  rank
              INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: dims
              INTEGER(HID_T), INTENT(OUT) :: type_id
              END FUNCTION h5tarray_create_c
            END INTERFACE

            hdferr = h5tarray_create_c(base_id, rank, dims, type_id)

         END SUBROUTINE h5tarray_create_f

!----------------------------------------------------------------------
! Name:		h5tenum_create_f
!
! Purpose: 	Creates a new enumeration datatype.
!
! Inputs:
!		parent_id	- datatype identifier for base datatype
! Outputs:
!		new_type_id	- datatype identifier for the enumeration
!				  datatype
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tenum_create_f(parent_id, new_type_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: parent_id  ! Datatype identifier for
                                                     ! the  base datatype
            INTEGER(HID_T), INTENT(OUT) :: new_type_id
                                                     !datatype identifier for the
                                                     ! new enumeration datatype
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tenum_create_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tenum_create_c(parent_id, new_type_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TENUM_CREATE_C'::h5tenum_create_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: parent_id
              INTEGER(HID_T), INTENT(OUT) :: new_type_id
              END FUNCTION h5tenum_create_c
            END INTERFACE

            hdferr = h5tenum_create_c(parent_id, new_type_id)
          END SUBROUTINE h5tenum_create_f

!----------------------------------------------------------------------
! Name:		h5tenaum_insert_f
!
! Purpose: 	Inserts a new enumeration datatype member.
!
! Inputs:
!		type_id		- datatype identifier
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tenum_insert_f(type_id,  name, value, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            CHARACTER(LEN=*), INTENT(IN) :: name  !Name of  the new member
            INTEGER, INTENT(IN) :: value !value of the new member
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER :: namelen

!            INTEGER, EXTERNAL :: h5tenum_insert_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tenum_insert_c(type_id, name, namelen, value)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TENUM_INSERT_C'::h5tenum_insert_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: type_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER, INTENT(IN) :: value
              INTEGER :: namelen
              END FUNCTION h5tenum_insert_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5tenum_insert_c(type_id, name, namelen, value)
          END SUBROUTINE h5tenum_insert_f

!----------------------------------------------------------------------
! Name:		h5tenum_nameof_f
!
! Purpose: 	Returns the symbol name corresponding to a specified
!    		member of an enumeration datatype.
!
! Inputs:
!		type_id		- datatype identifier
!		value		- value of the enumeration datatype
!		namelen		- name buffer size
! Outputs:
!		name		- buffer to hold symbol name
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tenum_nameof_f(type_id,  value, namelen, name, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            CHARACTER(LEN=*), INTENT(OUT) :: name  !Name of the  enumeration datatype.
            INTEGER(SIZE_T), INTENT(IN) :: namelen !length of the name
            INTEGER, INTENT(IN) :: value !value of the  enumeration datatype.
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tenum_nameof_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tenum_nameof_c(type_id, value, name, namelen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TENUM_NAMEOF_C'::h5tenum_nameof_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: type_id
              CHARACTER(LEN=*), INTENT(OUT) :: name
              INTEGER(SIZE_T), INTENT(IN) :: namelen
              INTEGER, INTENT(IN) :: value
              END FUNCTION h5tenum_nameof_c
            END INTERFACE

            hdferr = h5tenum_nameof_c(type_id, value, name, namelen)
          END SUBROUTINE h5tenum_nameof_f

!----------------------------------------------------------------------
! Name:		h5tenum_valuof_f
!
! Purpose: 	Returns the value corresponding to a specified
!		member of an enumeration datatype.
!
! Inputs:
!		type_id		- datatype identifier
!		name		- symbol name
! Outputs:
!		value		- value of the enumeration datatype
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tenum_valueof_f(type_id,  name, value, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            CHARACTER(LEN=*), INTENT(IN) :: name  !Name of the  enumeration datatype.
            INTEGER, INTENT(OUT) :: value !value of the  enumeration datatype.
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER :: namelen

!            INTEGER, EXTERNAL :: h5tenum_valueof_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tenum_valueof_c(type_id, name, namelen,  value)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TENUM_VALUEOF_C'::h5tenum_valueof_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: name
              INTEGER(HID_T), INTENT(IN) :: type_id
              CHARACTER(LEN=*), INTENT(IN) :: name
              INTEGER, INTENT(IN) :: namelen
              INTEGER, INTENT(OUT) :: value
              END FUNCTION h5tenum_valueof_c
            END INTERFACE

            namelen = LEN(name)
            hdferr = h5tenum_valueof_c(type_id, name, namelen,  value)
          END SUBROUTINE h5tenum_valueof_f

!----------------------------------------------------------------------
! Name:		h5tget_member_value_f
!
! Purpose: 	Returns the value of an enumeration datatype member.
!
! Inputs:
!		type_id		- datatype identifier
!		member_no	- number of the enumeration datatype member
! Outputs:
!		value		- value of the enumeration datatype
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_member_value_f(type_id,  member_no, value, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            INTEGER, INTENT(IN) :: member_no !Number of the enumeration datatype member
            INTEGER, INTENT(OUT) :: value !value of the  enumeration datatype.
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_member_value_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_member_value_c(type_id, member_no, value)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_MEMBER_VALUE_C'::h5tget_member_value_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN) :: member_no
              INTEGER, INTENT(OUT) :: value
              END FUNCTION
            END INTERFACE

            hdferr = h5tget_member_value_c(type_id, member_no, value)
          END SUBROUTINE h5tget_member_value_f

!----------------------------------------------------------------------
! Name:		h5tset_tag_f
!
! Purpose: 	Tags an opaque datatype.
!
! Inputs:
!		type_id		- identifier for opaque datatype
!		tag		- unique ASCII string with which the opaque
!				  datatype is to be tagged.
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tset_tag_f(type_id, tag, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            CHARACTER(LEN=*), INTENT(IN) :: tag !Unique ASCII string with which
                                                !the opaque datatype is to be tagged
            INTEGER, INTENT(OUT) :: hdferr        ! Error code
            INTEGER :: taglen

!            INTEGER, EXTERNAL :: h5tset_tag_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tset_tag_c(type_id, tag, taglen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TSET_TAG_C'::h5tset_tag_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: tag
              INTEGER(HID_T), INTENT(IN) :: type_id
              CHARACTER(LEN=*), INTENT(IN) :: tag
              INTEGER :: taglen
              END FUNCTION h5tset_tag_c
            END INTERFACE

            taglen = LEN(tag)
            hdferr = h5tset_tag_c(type_id, tag, taglen)
          END SUBROUTINE h5tset_tag_f

!----------------------------------------------------------------------
! Name:		h5tget_tag_f
!
! Purpose: 	Gets the tag associated with an opaque datatype.
!
! Inputs:
!		type_id		- identifier for opaque datatype
! Outputs:
!		tag		- unique ASCII string associated with opaque
!				  datatype
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 7, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_tag_f(type_id, tag,taglen, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id ! Datatype identifier
            CHARACTER(LEN=*), INTENT(OUT) :: tag !Unique ASCII string with which
                                                !the opaque datatype is to be tagged
            INTEGER, INTENT(OUT) :: taglen !length of tag
            INTEGER, INTENT(OUT) :: hdferr        ! Error code

!            INTEGER, EXTERNAL :: h5tget_tag_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_tag_c(type_id, tag, taglen)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_TAG_C'::h5tget_tag_c
              !DEC$ENDIF
              !DEC$ATTRIBUTES reference :: tag
              INTEGER(HID_T), INTENT(IN) :: type_id
              CHARACTER(LEN=*), INTENT(OUT) :: tag
              INTEGER, INTENT(OUT) :: taglen
              END FUNCTION h5tget_tag_c
            END INTERFACE

            hdferr = h5tget_tag_c(type_id, tag, taglen)
          END SUBROUTINE h5tget_tag_f

!----------------------------------------------------------------------
! Name:		h5tvlen_create_f
!
! Purpose: 	Creates a new variable-length datatype.
!
! Inputs:
!		type_id		- identifier iof base datatype
! Outputs:
!		vltype_id	- identifier for VL datatype
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		Wednesday, October 23, 2002
!
! Modifications:
!
! Comment: Only basic Fortran base datatypes are supported
!----------------------------------------------------------------------

          SUBROUTINE h5tvlen_create_f(type_id, vltype_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN)  :: type_id    ! Datatype identifier
            INTEGER(HID_T), INTENT(OUT) :: vltype_id  ! VL datatype identifier
            INTEGER, INTENT(OUT) :: hdferr            ! Error code

            INTERFACE
              INTEGER FUNCTION h5tvlen_create_c(type_id, vltype_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TVLEN_CREATE_C'::h5tvlen_create_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN)  :: type_id
              INTEGER(HID_T), INTENT(OUT) :: vltype_id
              END FUNCTION h5tvlen_create_c
            END INTERFACE

            hdferr = h5tvlen_create_c(type_id, vltype_id)
          END SUBROUTINE h5tvlen_create_f

!----------------------------------------------------------------------
! Name:		h5tis_variable_str_f
!
! Purpose: 	Determines whether a dattype is a variable string.
!
! Inputs:
!		type_id	-  	- datartpe identifier
! Outputs:
!		status		- flag to indicate if datatype
!				  is a variable string
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		March 12, 2003
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tis_variable_str_f(type_id, status, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier
            LOGICAL, INTENT(OUT) :: status      ! Flag, idicates if datatype
                                                ! is a variable string or not ( TRUE or
                                                ! FALSE)
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER :: flag                     ! "TRUE/FALSE/ERROR from C"

!            INTEGER, EXTERNAL :: h5tis_variable_str_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tis_variable_str_c(type_id, flag)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TIS_VARIABLE_STR_C'::h5tis_variable_str_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER :: flag
              END FUNCTION h5tis_variable_str_c
            END INTERFACE

            hdferr = h5tis_variable_str_c(type_id, flag)
            status = .TRUE.
            if (flag .EQ. 0) status = .FALSE.

          END SUBROUTINE h5tis_variable_str_f

!----------------------------------------------------------------------
! Name:		h5tget_member_class_f
!
! Purpose:      Returns datatype class of compound datatype member.
!
! Inputs:
!		type_id	-  	- datartpe identifier
!               member_no       - index of compound datatype member
! Outputs:
!               class           - class type for compound dadtype member
!                                 Can be one of the follwoing classes:
!                                 H5T_NO_CLASS_F (error)
!                                 H5T_INTEGER_F
!                                 H5T_FLOAT_F
!                                 H5T_TIME_F
!                                 H5T_STRING_F
!                                 H5T_BITFIELD_F
!                                 H5T_OPAQUE_F
!                                 H5T_COMPOUND_F
!                                 H5T_REFERENCE_F
!                                 H5T_ENUM_F
!                                 H5T_VLEN_F
!                                 H5T_ARRAY_F
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	Elena Pourmal
!		April 6, 2005
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5tget_member_class_f(type_id, member_no, class, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: type_id  ! Datatype identifier
            INTEGER, INTENT(IN)       :: member_no  ! Member number
            INTEGER, INTENT(OUT)     :: class      ! Member class
            INTEGER, INTENT(OUT) :: hdferr      ! Error code

!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5tget_member_class_c(type_id, member_no, class)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_MEMBER_CLASS_C'::h5tget_member_class_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: type_id
              INTEGER, INTENT(IN)       :: member_no
              INTEGER, INTENT(OUT)     :: class
              END FUNCTION h5tget_member_class_c
            END INTERFACE

            hdferr = h5tget_member_class_c(type_id, member_no, class)

          END SUBROUTINE h5tget_member_class_f

!----------------------------------------------------------------------
! Name:		h5tcommit_anon_f
!
! Purpose: 	Commits a transient datatype to a file,
!               creating a new named datatype,
!               but does not link it into the file structure.
!
! Inputs:
!        loc_id - A file or group identifier specifying the file
!                 in which the new named datatype is to be created.
!      dtype_id - A datatype identifier.
!
! Outputs:
!	hdferr: - error code
!			Success:  0
!          		Failure: -1
! Optional parameters:
!       tcpl_id - A datatype creation property list identifier.
!                 (H5P_DEFAULT_F for the default property list.)
!       tapl_id - A datatype access property list identifier.
!                 should always be passed as the value H5P_DEFAULT_F.
!
! Programmer:	M.S. Breitenfeld
!		February 25, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5tcommit_anon_f(loc_id, dtype_id, hdferr, tcpl_id, tapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id  ! A file or group identifier specifying
                                          ! the file in which the new named datatype
                                          ! is to be created.
    INTEGER(HID_T), INTENT(IN) :: dtype_id  ! Datatype identifier
    INTEGER, INTENT(OUT) :: hdferr          ! Error code
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: tcpl_id ! A datatype creation property
                                                    ! list identifier.
                                                    ! (H5P_DEFAULT_F for the default property list.)
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: tapl_id ! A datatype access property list identifier.
                                                    ! should always be passed as the value H5P_DEFAULT_F.
    INTEGER(HID_T) :: tcpl_id_default
    INTEGER(HID_T) :: tapl_id_default

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5tcommit_anon_c(loc_id, dtype_id, &
            tcpl_id_default, tapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TCOMMIT_ANON_C'::h5tcommit_anon_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(HID_T), INTENT(IN) :: dtype_id
         INTEGER(HID_T) :: tcpl_id_default
         INTEGER(HID_T) :: tapl_id_default
       END FUNCTION h5tcommit_anon_c
    END INTERFACE

    tcpl_id_default = H5P_DEFAULT_F
    tapl_id_default = H5P_DEFAULT_F

    IF(PRESENT(tcpl_id)) tcpl_id_default = tcpl_id
    IF(PRESENT(tapl_id)) tapl_id_default = tapl_id

    hdferr = h5tcommit_anon_c(loc_id, dtype_id, &
         tcpl_id_default, tapl_id_default )

  END SUBROUTINE h5tcommit_anon_f

!----------------------------------------------------------------------
! Name:      h5tcommitted_f
!
! Purpose:   Determines whether a datatype is a named type or a transient type.
!
! Inputs:
!      dtype_id - A datatype identifier.
!
! Outputs:
!     committed - .TRUE., if the datatype has been committed
!                .FALSE., if the datatype has not been committed.
!	hdferr: - error code
!			Success:  0
!          		Failure: -1
! Optional parameters: None
!
! Programmer:	M.S. Breitenfeld
!		February 25, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5tcommitted_f(dtype_id, committed, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dtype_id  ! A datatype identifier
    LOGICAL, INTENT(OUT) :: committed ! .TRUE., if the datatype has been committed
                                      !.FALSE., if the datatype has not been committed.
    INTEGER, INTENT(OUT) :: hdferr     ! Error code:
!			                   Success:  0
!          		                   Failure: -1

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5tcommitted_c(dtype_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TCOMMITTED_C'::h5tcommitted_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dtype_id
       END FUNCTION h5tcommitted_c
    END INTERFACE

    hdferr = h5tcommitted_c(dtype_id)

    IF(hdferr.GT.0)THEN
       committed = .TRUE.
       hdferr = 0
    ELSE IF(hdferr.EQ.0)THEN
       committed = .FALSE.
       hdferr = 0
    ELSE
       hdferr = -1
    ENDIF


  END SUBROUTINE h5tcommitted_f

!----------------------------------------------------------------------
! Name:		H5Tdecode_f
!
! Purpose:	Decode a binary object description of data type and return a new object handle.
! Inputs:
!		buf -  Buffer for the data space object to be decoded.
!            obj_id - Object ID
! Outputs:
!           hdferr: - error code
!			Success:  0
!			Failure: -1
!
! Optional parameters:		- NONE
!
! Programmer:	M.S. Breitenfeld
!		April 9, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5tdecode_f(buf, obj_id, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: buf ! Buffer for the data space object to be decoded.
    INTEGER(HID_T), INTENT(OUT) :: obj_id  ! Object ID
    INTEGER, INTENT(OUT) :: hdferr     ! Error code

    INTERFACE
       INTEGER FUNCTION h5tdecode_c(buf, obj_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TDECODE_C'::h5tdecode_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         CHARACTER(LEN=*), INTENT(IN) :: buf
         INTEGER(HID_T), INTENT(OUT) :: obj_id  ! Object ID
       END FUNCTION h5tdecode_c
    END INTERFACE

    hdferr = h5tdecode_c(buf, obj_id)

  END SUBROUTINE h5tdecode_f

!----------------------------------------------------------------------
! Name:		H5Tencode_f
!
! Purpose:	Encode a data type object description into a binary buffer.
!
! Inputs:
!            obj_id - Identifier of the object to be encoded.
!		buf - Buffer for the object to be encoded into.
!            nalloc - The size of the allocated buffer.
! Outputs:
!            nalloc - The size of the buffer needed.
!           hdferr: - error code
!	                Success:  0
!		        Failure: -1
!
! Optional parameters:		- NONE
!
! Programmer:	M.S. Breitenfeld
!		April 9, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5tencode_f(obj_id, buf, nalloc, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id ! Identifier of the object to be encoded.
    CHARACTER(LEN=*), INTENT(OUT) :: buf ! Buffer for the object to be encoded into.
    INTEGER(SIZE_T), INTENT(INOUT) :: nalloc ! The size of the allocated buffer.
    INTEGER, INTENT(OUT) :: hdferr     ! Error code


    INTERFACE
       INTEGER FUNCTION h5tencode_c(buf, obj_id, nalloc)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TENCODE_C'::h5tencode_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(OUT) :: buf
         INTEGER(SIZE_T), INTENT(INOUT) :: nalloc
       END FUNCTION h5tencode_c
    END INTERFACE

    hdferr = h5tencode_c(buf, obj_id, nalloc)

  END SUBROUTINE h5tencode_f

!----------------------------------------------------------------------
! Name:		h5tget_create_plist_f
!
! Purpose:  	Returns a copy of a datatype creation property list.
!
! Inputs:
!		dtype_id   - Datatype identifier
! Outputs:
!               dtpl_id    - Datatype property list identifier
!		hdferr:    - Error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	M.S. Breitenfeld
!		April 9, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5tget_create_plist_f(dtype_id, dtpl_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dtype_id  ! Datatype identifier
    INTEGER(HID_T), INTENT(OUT) :: dtpl_id  ! Datatype property list identifier.
    INTEGER, INTENT(OUT) :: hdferr       ! Error code:
                                         ! 0 on success and -1 on failure

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5tget_create_plist_c(dtype_id, dtpl_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_CREATE_PLIST_C'::h5tget_create_plist_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dtype_id
         INTEGER(HID_T), INTENT(OUT) :: dtpl_id
       END FUNCTION h5tget_create_plist_c
    END INTERFACE

    hdferr = h5tget_create_plist_c(dtype_id, dtpl_id)
  END SUBROUTINE h5tget_create_plist_f

!----------------------------------------------------------------------
! Name:		h5tcompiler_conv_f
!
! Purpose:  	Check whether the library’s default conversion is hard conversion.R
!
! Inputs:
!           src_id - Identifier for the source datatype.
!           dst_id - Identifier for the destination datatype.
! Outputs:
!           flag - TRUE for compiler conversion, FALSE for library conversion
!          hdferr: - Error code
!			Success:  0
!			Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	M.S. Breitenfeld
!		April 9, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5tcompiler_conv_f( src_id, dst_id, flag, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: src_id ! Identifier for the source datatype.
    INTEGER(HID_T), INTENT(IN) :: dst_id ! Identifier for the destination datatype.
    LOGICAL, INTENT(OUT) :: flag  ! .TRUE. for compiler conversion, .FALSE. for library conversion
    INTEGER, INTENT(OUT) :: hdferr  ! Error code:
                                    ! 0 on success and -1 on failure
    INTEGER :: c_flag

    INTERFACE
       INTEGER FUNCTION h5tcompiler_conv_c(src_id, dst_id, c_flag)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TCOMPILER_CONV_C'::h5tcompiler_conv_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: src_id
         INTEGER(HID_T), INTENT(IN) :: dst_id
         INTEGER :: c_flag
       END FUNCTION h5tcompiler_conv_c
    END INTERFACE

    hdferr = h5tcompiler_conv_c(src_id, dst_id, c_flag)

    flag = .FALSE.
    IF(c_flag .GT. 0) flag = .TRUE.

  END SUBROUTINE h5tcompiler_conv_f

!----------------------------------------------------------------------
! Name:		h5tget_native_type_f
!
! Purpose:  	Returns the native datatype of a specified datatype.
!
! Inputs:
!		dtype_id   - Datatype identifier for the dataset datatype.
!                        *
!               direction  - Direction of search:
!                    H5T_DIR_DEFAULT     = 0,    /*default direction is inscendent */
!                    H5T_DIR_ASCEND      = 1,    /*in inscendent order             */
!                    H5T_DIR_DESCEND     = 2     /*in descendent order             */
!               * NOTE: In C it is defined as a structure: H5T_direction_t
!
! Outputs:
!               native_dtype_id  - The native datatype identifier for the specified dataset datatype
!		hdferr:          - Error code
!				     Success:  0
!				     Failure: -1
! Optional parameters:
!				NONE
!
! Programmer:	M.S. Breitenfeld
!		June 18, 2008
!
! Modifications:  N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5tget_native_type_f(dtype_id, direction, native_dtype_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: dtype_id  ! Datatype identifier
    INTEGER, INTENT(IN) :: direction  ! Direction of search:
                                      ! H5T_DIR_ASCEND_F      = 1  in inscendent order
                                      ! H5T_DIR_DESCEND_F     = 2  in descendent order
    INTEGER(HID_T), INTENT(OUT) :: native_dtype_id  ! The native datatype identifier
    INTEGER, INTENT(OUT) :: hdferr    ! Error code:
                                      ! 0 on success and -1 on failure
    INTERFACE
       INTEGER FUNCTION h5tget_native_type_c(dtype_id, direction, native_dtype_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5TGET_NATIVE_TYPE_C'::h5tget_native_type_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: dtype_id
         INTEGER, INTENT(IN) :: direction
         INTEGER(HID_T), INTENT(OUT) :: native_dtype_id
       END FUNCTION h5tget_native_type_c
    END INTERFACE

    hdferr = h5tget_native_type_c(dtype_id, direction, native_dtype_id)
  END SUBROUTINE h5tget_native_type_f

END MODULE H5T
