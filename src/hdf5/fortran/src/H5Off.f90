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
! This file contains Fortran90 interfaces for H5O functions.
!
MODULE H5O

  USE H5GLOBAL

CONTAINS

!----------------------------------------------------------------------
! Name:		h5olink_f
!
! Purpose:  	Creates a hard link to an object in an HDF5 file.
!
! Inputs:
!      object_id     - Object to be linked.
!      new_loc_id    - File or group identifier specifying location at which object is to be linked.
!      new_link_name - Name of link to be created, relative to new_loc_id.
! Outputs:
!      hdferr:       - error code
!	                Success:  0
!			Failure: -1
! Optional parameters:
!      lcpl_id       - Link creation property list identifier.
!      lapl_id       - Link access property list identifier.
!
! Programmer:	M.S. Breitenfeld
!		April 21, 2008
!
! Modifications: N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5olink_f(object_id, new_loc_id, new_link_name, hdferr, lcpl_id, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: object_id  ! Object to be linked
    INTEGER(HID_T), INTENT(IN) :: new_loc_id ! File or group identifier specifying
                                             ! location at which object is to be linked.
    CHARACTER(LEN=*), INTENT(IN) :: new_link_name ! Name of link to be created, relative to new_loc_id.
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          !   Success:  0
                                          !   Failure: -1
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lcpl_id ! Link creation property list identifier.
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id ! Link creation property list identifier.
    INTEGER(HID_T) :: lapl_id_default
    INTEGER(HID_T) :: lcpl_id_default

    INTEGER(SIZE_T) :: new_link_namelen

    INTERFACE
       INTEGER FUNCTION h5olink_c(object_id, new_loc_id, new_link_name, new_link_namelen, &
            lcpl_id_default, lapl_id_default)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OLINK_C'::h5olink_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: new_link_name
         INTEGER(HID_T), INTENT(IN) :: object_id
         INTEGER(HID_T), INTENT(IN) :: new_loc_id
         CHARACTER(LEN=*), INTENT(IN) :: new_link_name
         INTEGER(SIZE_T) :: new_link_namelen
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(HID_T) :: lcpl_id_default
       END FUNCTION h5olink_c
    END INTERFACE

    new_link_namelen = LEN(new_link_name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id
    lcpl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lcpl_id)) lcpl_id_default = lcpl_id

    hdferr = h5olink_c(object_id, new_loc_id, new_link_name, new_link_namelen, &
         lcpl_id_default, lapl_id_default)

  END SUBROUTINE h5olink_f

!----------------------------------------------------------------------
! Name:		h5oopen_f
!
! Purpose:  	Opens an object in an HDF5 file by location identifier and path name.O
!
! Inputs:
!           loc_id - File or group identifier
!             name - Path to the object, relative to loc_id.
! Outputs:
!           obj_id - Object identifier for the opened object
!      hdferr:     - error code
!	                Success:  0
!			Failure: -1
! Optional parameters:
!          lapl_id - Access property list identifier for the link pointing to the object
!
! Programmer:	M.S. Breitenfeld
!		April 18, 2008
!
! Modifications: N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5oopen_f(loc_id, name, obj_id, hdferr, lapl_id)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id  ! File or group identifier
    CHARACTER(LEN=*), INTENT(IN) :: name  ! Path to the object, relative to loc_id
    INTEGER(HID_T), INTENT(OUT) :: obj_id ! Object identifier for the opened object
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          !   Success:  0
                                          !   Failure: -1
    INTEGER(HID_T), OPTIONAL, INTENT(IN) :: lapl_id  ! Attribute access property list
    INTEGER(HID_T) :: lapl_id_default

    INTEGER(SIZE_T) :: namelen

!  MS FORTRAN needs explicit interface for C functions called here.
!
    INTERFACE
       INTEGER FUNCTION h5oopen_c(loc_id, name, namelen, lapl_id_default, obj_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OOPEN_C'::h5oopen_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: name
         INTEGER(HID_T), INTENT(IN) :: loc_id
         CHARACTER(LEN=*), INTENT(IN) :: name
         INTEGER(HID_T) :: lapl_id_default
         INTEGER(SIZE_T) :: namelen
         INTEGER(HID_T), INTENT(OUT) :: obj_id
       END FUNCTION h5oopen_c
    END INTERFACE

    namelen = LEN(name)

    lapl_id_default = H5P_DEFAULT_F
    IF(PRESENT(lapl_id)) lapl_id_default = lapl_id

    hdferr = h5oopen_c(loc_id, name, namelen, lapl_id_default, obj_id)

  END SUBROUTINE h5oopen_f

!----------------------------------------------------------------------
! Name:		h5oopen_by_addr_f
!
! Purpose:  	Opens an object using its address within an HDF5 file.
!
! Inputs:
!           loc_id - File or group identifier
!             addr - Object’s address in the file
! Outputs:
!           obj_id - Object identifier for the opened object
!      hdferr:     - error code
!	                Success:  0
!			Failure: -1
!
! Programmer:	M. Scot Breitenfeld
!		September 14, 2009
!
! Modifications: N/A
!
!----------------------------------------------------------------------

  SUBROUTINE h5oopen_by_addr_f(loc_id, addr, obj_id, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: loc_id  ! File or group identifier
    INTEGER(HADDR_T), INTENT(IN) :: addr  ! Object’s address in the file
    INTEGER(HID_T), INTENT(OUT) :: obj_id ! Object identifier for the opened object
    INTEGER, INTENT(OUT) :: hdferr        ! Error code
                                          !   Success:  0
                                          !   Failure: -1
    INTERFACE
       INTEGER FUNCTION h5oopen_by_addr_c(loc_id, addr, obj_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5OOPEN_BY_ADDR_C'::h5oopen_by_addr_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: loc_id
         INTEGER(HADDR_T), INTENT(IN) :: addr
         INTEGER(HID_T), INTENT(OUT) :: obj_id
       END FUNCTION h5oopen_by_addr_c
    END INTERFACE

    hdferr = h5oopen_by_addr_c(loc_id, addr, obj_id)

  END SUBROUTINE h5oopen_by_addr_f

END MODULE H5O
