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
! This file contains Fortran90 interfaces for H5S functions.
!
      MODULE H5S
        USE H5GLOBAL

        CONTAINS

!----------------------------------------------------------------------
! Name:		h5screate_simple_f
!
! Purpose: 	Creates a new simple data space and opens it for access	.
!
! Inputs:
!		rank		- number of dimensions
!		dims		- an array of the size of each dimension
! Outputs:
!		space_id	- dataspace identifier
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!		maxdims		- an array of the maximum size of each
!				  dimension
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------
          SUBROUTINE h5screate_simple_f(rank, dims, space_id, hdferr, maxdims)

            IMPLICIT NONE
            INTEGER, INTENT(IN) :: rank     ! Number of dataspace dimensions
            INTEGER(HSIZE_T), INTENT(IN) :: dims(rank)
                                                    ! Array with the dimension
                                                    ! sizes
            INTEGER(HID_T), INTENT(OUT) :: space_id ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER(HSIZE_T), OPTIONAL, INTENT(IN) :: maxdims(rank)
                                                    ! Array with the maximum
                                                    ! dimension sizes
            INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:) :: f_maxdims

!            INTEGER, EXTERNAL :: h5screate_simple_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5screate_simple_c(rank, dims, maxdims, space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SCREATE_SIMPLE_C'::h5screate_simple_c
              !DEC$ENDIF
              INTEGER, INTENT(IN) :: rank
              INTEGER(HSIZE_T), INTENT(IN) :: dims(rank)
              INTEGER(HSIZE_T), DIMENSION(:),INTENT(IN) :: maxdims(rank)
              INTEGER(HID_T), INTENT(OUT) :: space_id
              END FUNCTION h5screate_simple_c
            END INTERFACE

            allocate (f_maxdims(rank), stat=hdferr)
            if (hdferr .NE. 0) then
                hdferr = -1
                return
            endif
            if (present(maxdims)) then
                f_maxdims = maxdims
            else
                f_maxdims = dims
            endif
            hdferr = h5screate_simple_c(rank, dims, f_maxdims, space_id)
            deallocate(f_maxdims)

          END SUBROUTINE h5screate_simple_f

!----------------------------------------------------------------------
! Name:		h5sclose_f
!
! Purpose: 	Releases and terminates access to a dataspace.
!
! Inputs:
!		space_id	- identifier of dataspace to release
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sclose_f(space_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr         ! Error code

!            INTEGER, EXTERNAL :: h5sclose_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sclose_c(space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SCLOSE_C'::h5sclose_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              END FUNCTION h5sclose_c
            END INTERFACE

            hdferr = h5sclose_c(space_id)

          END SUBROUTINE h5sclose_f

!----------------------------------------------------------------------
! Name:		h5screate_f
!
! Purpose: 	Creates a new dataspace of a specified type.
!
! Inputs:
!		classtype	- the type of the dataspace to be created
! Outputs:
!		space_id	- dataspace identifier
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5screate_f(classtype, space_id, hdferr)
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: classtype     ! The type of the dataspace
                                                 ! to be created.
                                                 ! Possible values are:
                                                 !  H5S_SCALAR_F (0)
                                                 !  H5S_SIMPLE_F(1)
                                                 !  H5S_NULL_F(2)
            INTEGER(HID_T), INTENT(OUT) :: space_id ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5screate_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5screate_c(classtype, space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SCREATE_C'::h5screate_c
              !DEC$ENDIF
              INTEGER, INTENT(IN) :: classtype
              INTEGER(HID_T), INTENT(OUT) :: space_id
              END FUNCTION h5screate_c
            END INTERFACE

            hdferr = h5screate_c(classtype, space_id)

          END SUBROUTINE h5screate_f

!----------------------------------------------------------------------
! Name:		h5scopy_f
!
! Purpose: 	Creates an exact copy of a dataspace.
!
! Inputs:
!		space_id	- dataspace identifier
! Outputs:
!		new_space_id	- identifier of dataspace's copy
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5scopy_f(space_id, new_space_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER(HID_T), INTENT(OUT) :: new_space_id
                                             ! Identifier of dataspace's copy
            INTEGER, INTENT(OUT) :: hdferr   ! Error code

!            INTEGER, EXTERNAL :: h5scopy_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5scopy_c(space_id, new_space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SCOPY_C'::h5scopy_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(HID_T), INTENT(OUT):: new_space_id
              END FUNCTION h5scopy_c
            END INTERFACE

            hdferr = h5scopy_c(space_id, new_space_id)

          END SUBROUTINE h5scopy_f

!----------------------------------------------------------------------
! Name:		h5sget_select_hyper_nblocks_f
!
! Purpose: 	Get number of hyperslab blocks.
!
! Inputs:
!		space_id	- dataspace identifier
! Outputs:
!		num_blocks	- number of hyperslab blocks in the current
!				  hyperslab selection
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------
          SUBROUTINE h5sget_select_hyper_nblocks_f(space_id, num_blocks, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER(HSSIZE_T), INTENT(OUT) :: num_blocks
                                             !number of hyperslab blocks
                                             !in the current dataspace
                                             !selection
            INTEGER, INTENT(OUT) :: hdferr   ! Error code

!            INTEGER, EXTERNAL :: h5sget_select_hyper_nblocks_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sget_select_hyper_nblocks_c (space_id, num_blocks)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
!DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SELECT_HYPER_NBLOCKS_C'::h5sget_select_hyper_nblocks_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(HSSIZE_T), INTENT(OUT) :: num_blocks
              END FUNCTION h5sget_select_hyper_nblocks_c
            END INTERFACE

            hdferr =  h5sget_select_hyper_nblocks_c (space_id, num_blocks)

          END SUBROUTINE h5sget_select_hyper_nblocks_f

!----------------------------------------------------------------------
! Name:		h5sget_select_hyper_blocklist_f
!
! Purpose: 	Gets the list of hyperslab blocks currently selected.
!
! Inputs:
!		space_id	- dataspace identifier
!		startblock	- hyperslab block to start with
!		num_blocks	- number of blocks to get
! Outputs:
!		buf		- buffer to hold block list
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sget_select_hyper_blocklist_f(space_id, startblock, &
                                                    num_blocks, buf, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER(HSIZE_T), INTENT(IN) :: startblock
                                             !Hyperslab block to start with.
            INTEGER(HSIZE_T), INTENT(IN) :: num_blocks
                                             !number of hyperslab blocks
                                             !to get in the current dataspace
                                             !selection
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf
                                             !List of hyperslab blocks selected
            INTEGER, INTENT(OUT) :: hdferr   ! Error code


!            INTEGER, EXTERNAL :: h5sget_select_hyper_blocklist_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sget_select_hyper_blocklist_c(space_id, startblock, &
                                                              num_blocks, buf )
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
            !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SELECT_HYPER_BLOCKLIST_C'::h5sget_select_hyper_blocklist_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(HSIZE_T), INTENT(IN) :: startblock
              INTEGER(HSIZE_T), INTENT(IN) :: num_blocks
              INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf
              END FUNCTION h5sget_select_hyper_blocklist_c
            END INTERFACE


            hdferr =  h5sget_select_hyper_blocklist_c(space_id, startblock, &
                                                       num_blocks, buf )

          END SUBROUTINE h5sget_select_hyper_blocklist_f

!----------------------------------------------------------------------
! Name:		h5sget_select_bounds_f
!
! Purpose: 	Gets the bounding box containing the current selection.
!
! Inputs:
!		space_id	- dataspace identifier
!
! Outputs:
!		start		- starting coordinates of bounding box
!		end		- ending coordinates of bounding box
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE  h5sget_select_bounds_f(space_id, start, END, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: start
                                           ! Starting coordinates of the bounding box.
    INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: END
                                           !Ending coordinates of the bounding box,
                                           !i.e., the coordinates of the diagonally
                                           !opposite corner
    INTEGER, INTENT(OUT) :: hdferr         ! Error code

    INTERFACE
       INTEGER FUNCTION h5sget_select_bounds_c(space_id, start, END)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SELECT_BOUNDS_C'::h5sget_select_bounds_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: start
         INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: END
       END FUNCTION h5sget_select_bounds_c
    END INTERFACE

    hdferr =   h5sget_select_bounds_c(space_id, start, END)

  END SUBROUTINE h5sget_select_bounds_f

!----------------------------------------------------------------------
! Name:		h5sget_select_elem_npoints_f
!
! Purpose: 	Gets the number of element points in the current selection
!
! Inputs:
!		space_id	- dataspace identifier
! Outputs:
!		num_points	- number of element points in the current
!				  dataspace selection
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------
          SUBROUTINE h5sget_select_elem_npoints_f(space_id, num_points, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER(HSSIZE_T), INTENT(OUT) :: num_points
                                             !number of element points
                                             !in the current dataspace
                                             !selection
            INTEGER, INTENT(OUT) :: hdferr   ! Error code

!            INTEGER, EXTERNAL :: h5sget_select_elem_npoints_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sget_select_elem_npoints_c (space_id, num_points)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
!DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SELECT_ELEM_NPOINTS_C'::h5sget_select_elem_npoints_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(HSSIZE_T), INTENT(OUT) :: num_points
              END FUNCTION h5sget_select_elem_npoints_c
            END INTERFACE

            hdferr =  h5sget_select_elem_npoints_c (space_id, num_points)

          END SUBROUTINE h5sget_select_elem_npoints_f

!----------------------------------------------------------------------
! Name:		h5sget_select_elem_pointlist_f
!
! Purpose:	Gets the list of element points currently selected.
!
! Inputs:
!		space_id	- dataspace identifier
!		startpoint	- element point to start with
!		num_points	- number of elemnt points to get
! Outputs:
!		buf		- buffer with element points selected
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sget_select_elem_pointlist_f(space_id, startpoint, &
                                                    num_points, buf, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER(HSIZE_T), INTENT(IN) :: startpoint
                                             !Element point to start with.
            INTEGER(HSIZE_T), INTENT(IN) :: num_points
                                             !Number of element points to get
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf
                                             !List of element points selected
            INTEGER, INTENT(OUT) :: hdferr   ! Error code

            INTERFACE
              INTEGER FUNCTION h5sget_select_elem_pointlist_c(space_id, startpoint, &
                                                              num_points, buf )
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
!DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SELECT_ELEM_POINTLIST_C'::h5sget_select_elem_pointlist_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(HSIZE_T), INTENT(IN) :: startpoint
              INTEGER(HSIZE_T), INTENT(IN) :: num_points
              INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: buf
              END FUNCTION h5sget_select_elem_pointlist_c
            END INTERFACE

            hdferr =  h5sget_select_elem_pointlist_c(space_id, startpoint, &
                                                       num_points, buf )

          END SUBROUTINE h5sget_select_elem_pointlist_f

!----------------------------------------------------------------------
! Name:		h5sselect_elements_f
!
! Purpose:	Selects elements to be included in the selection for
!		a dataspace
!
! Inputs:
!		space_id	- dataspace identifier
!		operator	- flag, valid values are:
!				  H5S_SELECT_SET_F (0)
!				  H5S_SELECT_OR_F (1)
!		rank		- number of dataspace dimensions
!		num_elements	- number of elements to be selected
!		coord		- 2D (rank x num_elements) array with the
!				  elements coordinates
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------
  SUBROUTINE h5sselect_elements_f(space_id, OPERATOR, rank, &
       num_elements, coord, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
    INTEGER, INTENT(IN) :: OPERATOR    ! Flag, valid values are:
                                       ! H5S_SELECT_SET_F (0)
                                       ! H5S_SELECT_OR_F (1)
    INTEGER, INTENT(IN) :: rank     ! Number of dataspace dimensions
    INTEGER(SIZE_T), INTENT(IN) :: num_elements  ! Number of elements to be
                                                 ! selected
    INTEGER(HSIZE_T), DIMENSION(rank,num_elements), INTENT(IN) :: coord
                                          ! Array with the coordinates
                                          ! of the selected elements
                                          ! coord(rank, num_elements)
    INTEGER, INTENT(OUT) :: hdferr     ! Error code
    INTEGER(HSIZE_T), ALLOCATABLE, DIMENSION(:,:) :: c_coord
    INTEGER :: error, i

    INTERFACE
       INTEGER FUNCTION h5sselect_elements_c(space_id, OPERATOR,&
            num_elements,c_c_coord)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SSELECT_ELEMENTS_C'::h5sselect_elements_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: space_id
         INTEGER, INTENT(IN) :: OPERATOR
         INTEGER(SIZE_T), INTENT(IN) :: num_elements
         INTEGER(HSIZE_T),DIMENSION(*) :: c_c_coord
       END FUNCTION h5sselect_elements_c
    END INTERFACE

    ALLOCATE(c_coord(rank,num_elements), STAT = error)
    IF (error.NE. 0) THEN
       hdferr = -1
       RETURN
    ENDIF
    DO i = 1, rank
       c_coord(i,:) = coord(rank-i+1, :) - 1
    ENDDO
    hdferr = h5sselect_elements_c(space_id, OPERATOR, num_elements, c_coord)

    DEALLOCATE(c_coord)

  END SUBROUTINE h5sselect_elements_f

!----------------------------------------------------------------------
! Name:		h5sselect_all_f
!
! Purpose: 	Selects the entire dataspace.
!
! Inputs:
!		space_id	- identifier for the dataspace in which
!				  selection being made
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sselect_all_f(space_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id  ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5sselect_all_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sselect_all_c(space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SSELECT_ALL_C'::h5sselect_all_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              END FUNCTION h5sselect_all_c
            END INTERFACE

            hdferr = h5sselect_all_c(space_id)

          END SUBROUTINE h5sselect_all_f

!----------------------------------------------------------------------
! Name:		h5sselect_none_f
!
! Purpose: 	Resets the selection region to include no elements.
!
! Inputs:
!		space_id	- the identifier for the dataspace in which
!                                 the selection is being reset.
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sselect_none_f(space_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id  ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5sselect_none_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sselect_none_c(space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SSELECT_NONE_C'::h5sselect_none_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              END FUNCTION h5sselect_none_c
            END INTERFACE

            hdferr = h5sselect_none_c(space_id)

          END SUBROUTINE h5sselect_none_f

!----------------------------------------------------------------------
! Name:		h5sselect_valid_f
!
! Purpose:	Verifies that the selection is within the extent of
!		the dataspace.
!
! Inputs:
!		space_id	- identifier for the dataspace for which
!				  selection is verified
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------
          SUBROUTINE h5sselect_valid_f(space_id, status, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id  ! Dataspace identifier
            LOGICAL, INTENT(OUT) :: status          ! TRUE if the selection is
                                                    ! contained within the extent,
                                                    ! FALSE otherwise.
            INTEGER, INTENT(OUT) :: hdferr          ! Error code
            INTEGER :: flag ! "TRUE/FALSE/ERROR" flag from C routine

!            INTEGER, EXTERNAL :: h5sselect_valid_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sselect_valid_c(space_id, flag)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SSELECT_VALID_C'::h5sselect_valid_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER :: flag
              END FUNCTION h5sselect_valid_c
            END INTERFACE

            hdferr = h5sselect_valid_c(space_id, flag)
            status = .TRUE.
            if (flag .EQ. 0) status = .FALSE.

          END SUBROUTINE h5sselect_valid_f

!----------------------------------------------------------------------
! Name:		h5sget_simple_extent_npoints_f
!
! Purpose: 	Determines the number of elements in a dataspace.
!
! Inputs:
!		space_id	- dataspace identifier
! Outputs:
!		npoints		- number of elements in the dataspace
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sget_simple_extent_npoints_f(space_id, npoints, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id     ! Dataspace identifier
            INTEGER(HSIZE_T), INTENT(OUT) :: npoints  ! Number of elements in
                                                       ! dataspace
            INTEGER, INTENT(OUT) :: hdferr             ! Error code

!            INTEGER, EXTERNAL :: h5sget_simple_extent_npoints_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sget_simple_extent_npoints_c( space_id, npoints)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SIMPLE_EXTENT_NPOINTS_C'::h5sget_simple_extent_npoints_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(HSIZE_T), INTENT(OUT) :: npoints
              END FUNCTION h5sget_simple_extent_npoints_c
            END INTERFACE

            hdferr = h5sget_simple_extent_npoints_c( space_id, npoints)

          END SUBROUTINE h5sget_simple_extent_npoints_f

!----------------------------------------------------------------------
! Name:		h5sget_select_npoints_f
!
! Purpose: 	Determines the number of elements in a dataspace selection.
!
! Inputs:
!		space_id	- dataspace identifier
! Outputs:
!		npoints		- number of points in the dataspace selection
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sget_select_npoints_f(space_id, npoints, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id     ! Dataspace identifier
            INTEGER(HSSIZE_T), INTENT(OUT) :: npoints  ! Number of elements in the
                                                       ! selection
            INTEGER, INTENT(OUT) :: hdferr             ! Error code

!            INTEGER, EXTERNAL :: h5sget_select_npoints_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sget_select_npoints_c(space_id, npoints)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SELECT_NPOINTS_C'::h5sget_select_npoints_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(HSSIZE_T), INTENT(OUT) :: npoints
              END FUNCTION h5sget_select_npoints_c
            END INTERFACE

            hdferr = h5sget_select_npoints_c(space_id, npoints)

          END SUBROUTINE h5sget_select_npoints_f

!----------------------------------------------------------------------
! Name:		h5sget_simple_extent_ndims_f
!
! Purpose: 	Determines the dimensionality of a dataspace
!
! Inputs:
!		space_id	- dataspace identifier
! Outputs:
!		rank		- number of dataspace dimensions
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sget_simple_extent_ndims_f(space_id, rank, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id     ! Dataspace identifier
            INTEGER, INTENT(OUT) :: rank               ! Number of dimensions
            INTEGER, INTENT(OUT) :: hdferr             ! Error code

!            INTEGER, EXTERNAL :: h5sget_simple_extent_ndims_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sget_simple_extent_ndims_c(space_id, rank)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SIMPLE_EXTENT_NDIMS_C'::h5sget_simple_extent_ndims_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER, INTENT(OUT) :: rank
              END FUNCTION h5sget_simple_extent_ndims_c
            END INTERFACE

            hdferr = h5sget_simple_extent_ndims_c(space_id, rank)

          END SUBROUTINE h5sget_simple_extent_ndims_f

!----------------------------------------------------------------------
! Name:		h5sget_simple_extent_dims_f
!
! Purpose: 	Retrieves dataspace dimension size and maximum size.
!
! Inputs:
!		space_id	- dataspace identifier
! Outputs:
!		dims		- array to store size of each dimension
!		maxdims		- array to store maximum size of each
!				  dimension
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sget_simple_extent_dims_f(space_id, dims, maxdims, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: dims
                                                   ! Array to store dimension sizes
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: maxdims
                                                   ! Array to store max dimension
                                                   ! sizes
            INTEGER, INTENT(OUT) :: hdferr         ! Error code: -1 on failure,
                                                   ! number of dimensions on
                                                   ! on success

!            INTEGER, EXTERNAL :: h5sget_simple_extent_dims_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sget_simple_extent_dims_c(space_id, dims, maxdims)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SIMPLE_EXTENT_DIMS_C'::h5sget_simple_extent_dims_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: dims
              INTEGER(HSIZE_T), DIMENSION(*), INTENT(OUT) :: maxdims
              END FUNCTION h5sget_simple_extent_dims_c
            END INTERFACE

            hdferr = h5sget_simple_extent_dims_c(space_id, dims, maxdims)

          END SUBROUTINE h5sget_simple_extent_dims_f

!----------------------------------------------------------------------
! Name:		h5sget_simple_extent_type_f
!
! Purpose: 	Determine the current class of a dataspace
!
! Inputs:
!		space_id	- dataspace identifier
! Outputs:
!		classtype	- class type, possible values are:
!				  H5S_NO_CLASS_F (-1)
!				  H5S_SCALAR_F (0)
!				  H5S_SIMPLE_F (1)
!				  H5S_NULL_F   (2)
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sget_simple_extent_type_f(space_id, classtype, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER, INTENT(OUT) :: classtype      ! Class type , possible values
                                                   ! are:
                                                   !  H5S_NO_CLASS_F (-1)
                                                   !  H5S_SCALAR_F (0)
                                                   !  H5S_SIMPLE_F (1)
                                                   !  H5S_NULL_F   (2)
            INTEGER, INTENT(OUT) :: hdferr         ! Error code

!            INTEGER, EXTERNAL :: h5sget_simple_extent_type_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sget_simple_extent_type_c(space_id, classtype)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SIMPLE_EXTENT_TYPE_C'::h5sget_simple_extent_type_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER, INTENT(OUT) :: classtype
              END FUNCTION h5sget_simple_extent_type_c
            END INTERFACE

            hdferr = h5sget_simple_extent_type_c(space_id, classtype)

          END SUBROUTINE h5sget_simple_extent_type_f

!----------------------------------------------------------------------
! Name:		h5sset_extent_simple_f
!
! Purpose: 	Sets or resets the size of an existing dataspace.
!
! Inputs:
!		space_id	- dataspace identifier
!		rank		- dataspace number of dimensions
!		current_size	- array with the new sizes of dimensions
!		maximum_size	- array with the new maximum sizes of
!				  dimensions
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sset_extent_simple_f(space_id, rank, current_size, &
                                            maximum_size, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER, INTENT(IN) :: rank            ! Dataspace rank
            INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: current_size
                                                   ! Array with the new sizes
                                                   ! of dimensions
            INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: maximum_size
                                                   ! Array with the new maximum
                                                   ! sizes of dimensions
                                                   ! sizes
            INTEGER, INTENT(OUT) :: hdferr         ! Error code

!            INTEGER, EXTERNAL :: h5sset_extent_simple_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sset_extent_simple_c(space_id, rank, &
                               current_size,  maximum_size)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SSET_EXTENT_SIMPLE_C'::h5sset_extent_simple_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER, INTENT(IN) :: rank
              INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: current_size
              INTEGER(HSIZE_T), DIMENSION(rank), INTENT(IN) :: maximum_size
              END FUNCTION h5sset_extent_simple_c
            END INTERFACE

            hdferr = h5sset_extent_simple_c(space_id, rank, current_size, &
                                            maximum_size)

          END SUBROUTINE h5sset_extent_simple_f

!----------------------------------------------------------------------
! Name:		h5sis_simple_f
!
! Purpose: 	Determines whether a dataspace is a simple dataspace.
!
! Inputs:
!		space_id	- dataspace identifier
! Outputs:
!		status		- flag to indicate if dataspace
!				  is simple or not
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sis_simple_f(space_id, status, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id  ! Dataspace identifier
            LOGICAL, INTENT(OUT) :: status      ! Flag, idicates if dataspace
                                                ! is simple or not ( TRUE or
                                                ! FALSE)
            INTEGER, INTENT(OUT) :: hdferr      ! Error code
            INTEGER :: flag                     ! "TRUE/FALSE/ERROR from C"

!            INTEGER, EXTERNAL :: h5sis_simple_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sis_simple_c(space_id, flag)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SIS_SIMPLE_C'::h5sis_simple_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER :: flag
              END FUNCTION h5sis_simple_c
            END INTERFACE

            hdferr = h5sis_simple_c(space_id, flag)
            status = .TRUE.
            if (flag .EQ. 0) status = .FALSE.

          END SUBROUTINE h5sis_simple_f

!----------------------------------------------------------------------
! Name:		h5soffset_simple_f
!
! Purpose:	Sets the offset of a simple dataspace.
!
! Inputs:
!		space_id	- dataspace identifier
!		offset		- the offset at which to position the
!				  selection
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------
          SUBROUTINE h5soffset_simple_f(space_id, offset, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER(HSSIZE_T), DIMENSION(*), INTENT(IN) ::  offset
                                                   ! The offset at which to position
                                                   ! the selection
            INTEGER, INTENT(OUT) :: hdferr         ! Error code

!            INTEGER, EXTERNAL :: h5soffset_simple_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5soffset_simple_c(space_id, offset)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SOFFSET_SIMPLE_C'::h5soffset_simple_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER(HSSIZE_T), DIMENSION(*), INTENT(IN) ::  offset
              END FUNCTION h5soffset_simple_c
            END INTERFACE

            hdferr = h5soffset_simple_c(space_id, offset)

          END SUBROUTINE h5soffset_simple_f

!----------------------------------------------------------------------
! Name:		h5sextent_copy_f
!
! Purpose: 	Copies the extent of a dataspace.
!
! Inputs:
!		dest_space_id	- the identifier for the dataspace to which
!				  the extent is copied
!		source_space_id	- the identifier for the dataspace from
!				  which the extent is copied
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sextent_copy_f(dest_space_id, source_space_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: dest_space_id  ! Identifier of destination
                                                         ! dataspace
            INTEGER(HID_T), INTENT(IN) :: source_space_id ! Identifier of source
                                                          ! dataspace
            INTEGER, INTENT(OUT) :: hdferr                ! Error code

!            INTEGER, EXTERNAL :: h5sextent_copy_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sextent_copy_c(dest_space_id, source_space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SEXTENT_COPY_C'::h5sextent_copy_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: dest_space_id
              INTEGER(HID_T), INTENT(IN) :: source_space_id
              END FUNCTION h5sextent_copy_c
            END INTERFACE

            hdferr = h5sextent_copy_c(dest_space_id, source_space_id)

          END SUBROUTINE h5sextent_copy_f

!----------------------------------------------------------------------
! Name:		h5sset_extent_none_f
!
! Purpose:	Removes the extent from a dataspace.
!
! Inputs:
!		space_id	- dataspace identifier
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
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------
          SUBROUTINE h5sset_extent_none_f(space_id, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id  ! Dataspace identifier
            INTEGER, INTENT(OUT) :: hdferr          ! Error code

!            INTEGER, EXTERNAL :: h5sset_extent_none_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sset_extent_none_c(space_id)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SSET_EXTENT_NONE_C'::h5sset_extent_none_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              END FUNCTION h5sset_extent_none_c
            END INTERFACE

            hdferr = h5sset_extent_none_c(space_id)

          END SUBROUTINE h5sset_extent_none_f

!----------------------------------------------------------------------
! Name:		h5sselect_hyperslab_f
!
! Purpose:	Selects a hyperslab region to add to the current selected
!		region
!
! Inputs:
!		space_id	- dataspace identifier
!		operator	- flag, valid values are:
!				  H5S_SELECT_SET_F (0)
!				  H5S_SELECT_OR_F (1)
!		start		- array with hyperslab offsets
!		count		- number of blocks included in the
!				  hyperslab
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!		stride		- array with hyperslab strides
!		block		- array with hyperslab block sizes
!
! Programmer:	Elena Pourmal
!		August 12, 1999
!
! Modifications: 	Explicit Fortran interfaces were added for
!			called C functions (it is needed for Windows
!			port).  March 6, 2001
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sselect_hyperslab_f(space_id, operator, start, count, &
                                           hdferr, stride, block)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
            INTEGER, INTENT(IN) :: operator     ! Flag, valid values are:
                                                ! H5S_SELECT_SET_F (0)
                                                ! H5S_SELECT_OR_F (1)
                                                !
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: start
                                          ! Starting coordinates of the hyperslab
            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: count
                                          ! Number of blocks to select
                                          ! from dataspace
            INTEGER, INTENT(OUT) :: hdferr     ! Error code
            INTEGER(HSIZE_T), DIMENSION(:), OPTIONAL, INTENT(IN) :: stride
                                          ! Array of how many elements to move
                                          ! in each direction
            INTEGER(HSIZE_T), DIMENSION(:), OPTIONAL, INTENT(IN) :: block
                                          ! Sizes of element block
            INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: def_block
            INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: def_stride
            INTEGER :: rank
            INTEGER :: error1, error2

!            INTEGER, EXTERNAL :: h5sselect_hyperslab_c
!  MS FORTRAN needs explicit interface for C functions called here.
!
            INTERFACE
              INTEGER FUNCTION h5sselect_hyperslab_c(space_id, operator, &
                               start, count, stride, block)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SSELECT_HYPERSLAB_C'::h5sselect_hyperslab_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER, INTENT(IN) :: operator
              INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: start
              INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: count
              INTEGER(HSIZE_T), DIMENSION(*), OPTIONAL, INTENT(IN) :: stride
              INTEGER(HSIZE_T), DIMENSION(*), OPTIONAL, INTENT(IN) :: block
              END FUNCTION h5sselect_hyperslab_c
            END INTERFACE

            if (present(stride).and. present(block)) then
            hdferr = h5sselect_hyperslab_c(space_id, operator, start, count, &
                                           stride, block)
            return
            endif
            ! Case of optional parameters.
            !
            ! Find the rank of the dataspace to allocate memery for
            ! default stride and block arrays.
            !
            CALL h5sget_simple_extent_ndims_f(space_id, rank, hdferr)
            if( hdferr .EQ. -1) return
            !
            if (present(stride).and. .not.present(block)) then
            allocate(def_block(rank), stat=error1)
                if (error1.NE.0) then
                    hdferr = -1
                    return
                endif
            def_block = 1
            hdferr = h5sselect_hyperslab_c(space_id, operator, start, count, &
                                           stride, def_block)
            deallocate(def_block)
            return
            endif

            if (.not.present(stride).and. present(block)) then
            allocate(def_stride(rank), stat=error2)
                if (error2.NE.0) then
                    hdferr = -1
                    return
                endif
            def_stride = 1
            hdferr = h5sselect_hyperslab_c(space_id, operator, start, count, &
                                           def_stride, block)
            deallocate(def_stride)
            return
            endif
            allocate(def_block(rank), stat=error1)
            allocate(def_stride(rank), stat=error2)
                if ((error1.NE.0) .OR. (error2.NE.0)) then
                    hdferr = -1
                    return
                endif
            def_block = 1
            def_stride = 1
            hdferr = h5sselect_hyperslab_c(space_id, operator, start, count, &
                                           def_stride, def_block)
            deallocate(def_block)
            deallocate(def_stride)

          END SUBROUTINE h5sselect_hyperslab_f
!----------------------------------------------------------------------
! Name:		h5scombine_hyperslab_f
!
! Purpose:	Combine a hyperslab selection with the current
!               selection for a dataspace
!
! Inputs:
!		space_id	- dataspace of selection to use
!		operator	- flag, valid values are:
!				  H5S_SELECT_NOOP_F
!				  H5S_SELECT_SET_F
!				  H5S_SELECT_OR_F
!				  H5S_SELECT_AND_F
!				  H5S_SELECT_XOR_F
!				  H5S_SELECT_NOTB_F
!				  H5S_SELECT_NOTA_F
!				  H5S_SELECT_APPEND_F
!				  H5S_SELECT_PREPEND_F
!		start		- array with hyperslab offsets
!		count		- number of blocks included in the
!				  hyperslab
! Outputs:
!               hyper_id        - identifier for the new hyperslab
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:
!		stride		- array with hyperslab strides
!		block		- array with hyperslab block sizes
!
! Programmer:	Elena Pourmal
!		October 7, 2002
!
! Modifications:
!
! Comment:	Commented out until 1.6 ? 10/08/2002
!----------------------------------------------------------------------

!          SUBROUTINE h5scombine_hyperslab_f(space_id, operator, start, count, &
!                                            hyper_id,  hdferr, stride, block)
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: space_id ! Dataspace identifier
!            INTEGER, INTENT(IN) :: operator     ! Flag, valid values are:
						!  H5S_SELECT_NOOP_F
						!  H5S_SELECT_SET_F
						!  H5S_SELECT_OR_F
						!  H5S_SELECT_AND_F
						!  H5S_SELECT_XOR_F
						!  H5S_SELECT_NOTB_F
						!  H5S_SELECT_NOTA_F
						!  H5S_SELECT_APPEND_F
						!  H5S_SELECT_PREPEND_F
                                                !
!            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: start
                                          ! Starting coordinates of the hyperslab
!            INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: count
                                          ! Number of blocks to select
                                          ! from dataspace
!            INTEGER(HID_T), INTENT(OUT) :: hyper_id ! New hyperslab identifier
!            INTEGER, INTENT(OUT) :: hdferr     ! Error code
!            INTEGER(HSIZE_T), DIMENSION(:), OPTIONAL, INTENT(IN) :: stride
                                          ! Array of how many elements to move
                                          ! in each direction
!            INTEGER(HSIZE_T), DIMENSION(:), OPTIONAL, INTENT(IN) :: block
                                          ! Sizes of element block
!            INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: def_block
!            INTEGER(HSIZE_T), DIMENSION(:), ALLOCATABLE :: def_stride
!            INTEGER :: rank
!            INTEGER :: error1, error2

!            INTERFACE
!              INTEGER FUNCTION h5scombine_hyperslab_c(space_id, operator, &
!                               start, count, stride, block, hyper_id)
!              USE H5GLOBAL
!             !DEC$IF DEFINED(HDF5F90_WINDOWS)
!             !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SCOMBINE_HYPERSLAB_C'::h5scombine_hyperslab_c
!             !DEC$ENDIF
!              INTEGER(HID_T), INTENT(IN) :: space_id
!              INTEGER, INTENT(IN) :: operator
!              INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: start
!              INTEGER(HSIZE_T), DIMENSION(*), INTENT(IN) :: count
!              INTEGER(HSIZE_T), DIMENSION(*), OPTIONAL, INTENT(IN) :: stride
!              INTEGER(HSIZE_T), DIMENSION(*), OPTIONAL, INTENT(IN) :: block
!              INTEGER(HID_T), INTENT(OUT) :: hyper_id
!              END FUNCTION h5scombine_hyperslab_c
!            END INTERFACE

!            if (present(stride).and. present(block)) then
!            hdferr = h5scombine_hyperslab_c(space_id, operator, start, count, &
!                                           stride, block, hyper_id)
!            return
!            endif
            ! Case of optional parameters.
            !
            ! Find the rank of the dataspace to allocate memery for
            ! default stride and block arrays.
            !
!            CALL h5sget_simple_extent_ndims_f(space_id, rank, hdferr)
!            if( hdferr .EQ. -1) return
            !
!            if (present(stride).and. .not.present(block)) then
!            allocate(def_block(rank), stat=error1)
!                if (error1.NE.0) then
!                    hdferr = -1
!                    return
!                endif
!            def_block = 1
!            hdferr = h5scombine_hyperslab_c(space_id, operator, start, count, &
!                                           stride, def_block, hyper_id)
!            deallocate(def_block)
!            return
!            endif

!            if (.not.present(stride).and. present(block)) then
!            allocate(def_stride(rank), stat=error2)
!                if (error2.NE.0) then
!                    hdferr = -1
!                    return
!                endif
!            def_stride = 1
!            hdferr = h5scombine_hyperslab_c(space_id, operator, start, count, &
!                                           def_stride, block, hyper_id)
!            deallocate(def_stride)
!            return
!            endif
!            allocate(def_block(rank), stat=error1)
!            allocate(def_stride(rank), stat=error2)
!                if ((error1.NE.0) .OR. (error2.NE.0)) then
!                    hdferr = -1
!                    return
!                endif
!            def_block = 1
!            def_stride = 1
!            hdferr = h5scombine_hyperslab_c(space_id, operator, start, count, &
!                                           def_stride, def_block, hyper_id)
!            deallocate(def_block)
!            deallocate(def_stride)

!          END SUBROUTINE h5scombine_hyperslab_f

!----------------------------------------------------------------------
! Name:		h5scombine_select_f
!
! Purpose:	Combine two hyperslab selections with an operation
!               and return a dataspace with resulting selection.
!
! Inputs:
!		space1_id	- dataspace of selection to use
!		operator	- flag, valid values are:
!				  H5S_SELECT_NOOP_F
!				  H5S_SELECT_SET_F
!				  H5S_SELECT_OR_F
!				  H5S_SELECT_AND_F
!				  H5S_SELECT_XOR_F
!				  H5S_SELECT_NOTB_F
!				  H5S_SELECT_NOTA_F
!				  H5S_SELECT_APPEND_F
!				  H5S_SELECT_PREPEND_F
!		space2_id	- dataspace of selection to use
! Outputs:
!               ds_id           - idataspace identifier with the new selection
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:		- NONE
!
! Programmer:	Elena Pourmal
!		October 7, 2002
!
! Modifications:
!
! Comment: commented out until 1.6 release(?) 10/08/2002
!----------------------------------------------------------------------

!          SUBROUTINE h5scombine_select_f(space1_id, operator, space2_id, &
!                                            ds_id,  hdferr)
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(IN) :: space1_id ! First dataspace identifier
!            INTEGER(HID_T), INTENT(IN) :: space2_id ! Second dataspace identifier
!            INTEGER, INTENT(IN) :: operator     ! Flag, valid values are:
						!  H5S_SELECT_NOOP_F
						!  H5S_SELECT_SET_F
						!  H5S_SELECT_OR_F
						!  H5S_SELECT_AND_F
						!  H5S_SELECT_XOR_F
						!  H5S_SELECT_NOTB_F
						!  H5S_SELECT_NOTA_F
						!  H5S_SELECT_APPEND_F
						!  H5S_SELECT_PREPEND_F
                                                !
!            INTEGER(HID_T), INTENT(OUT) :: ds_id ! New dataspace identifier
!            INTEGER, INTENT(OUT) :: hdferr     ! Error code
!
!            INTERFACE
!              INTEGER FUNCTION h5scombine_select_c(space1_id, operator, &
!                               space2_id, ds_id)
!              USE H5GLOBAL
!             !DEC$IF DEFINED(HDF5F90_WINDOWS)
!             !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SCOMBINE_SELECT_C'::h5scombine_select_c
!             !DEC$ENDIF
!              INTEGER(HID_T), INTENT(IN) :: space1_id
!              INTEGER(HID_T), INTENT(IN) :: space2_id
!              INTEGER, INTENT(IN) :: operator
!              INTEGER(HID_T), INTENT(OUT) :: ds_id
!              END FUNCTION h5scombine_select_c
!            END INTERFACE

!            hdferr = h5scombine_select_c(space1_id, operator, space2_id, &
!                                         ds_id)
!            return

!          END SUBROUTINE h5scombine_select_f

!----------------------------------------------------------------------
! Name:		h5sselect_select_f
!
! Purpose:	Refine a hyperslab selection with an operation
!               using second hyperslab
!
! Inputs:
!		space1_id	- dataspace of selection  to modify
!		operator	- flag, valid values are:
!				  H5S_SELECT_NOOP_F
!				  H5S_SELECT_SET_F
!				  H5S_SELECT_OR_F
!				  H5S_SELECT_AND_F
!				  H5S_SELECT_XOR_F
!				  H5S_SELECT_NOTB_F
!				  H5S_SELECT_NOTA_F
!				  H5S_SELECT_APPEND_F
!				  H5S_SELECT_PREPEND_F
!		space2_id	- dataspace of selection to use
!
! Outputs:
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:		- NONE
!
! Programmer:	Elena Pourmal
!		October 7, 2002
!
! Modifications:
!
! Comment:Commented out until 1.6 release(?) 10/08/2002 EIP
!----------------------------------------------------------------------

!          SUBROUTINE h5sselect_select_f(space1_id, operator, space2_id, &
!                                        hdferr)
!            IMPLICIT NONE
!            INTEGER(HID_T), INTENT(INOUT) :: space1_id ! Dataspace identifier to
                                                       ! modify
!            INTEGER(HID_T), INTENT(IN) :: space2_id ! Second dataspace identifier
!            INTEGER, INTENT(IN) :: operator     ! Flag, valid values are:
						!  H5S_SELECT_NOOP_F
						!  H5S_SELECT_SET_F
						!  H5S_SELECT_OR_F
						!  H5S_SELECT_AND_F
						!  H5S_SELECT_XOR_F
						!  H5S_SELECT_NOTB_F
						!  H5S_SELECT_NOTA_F
						!  H5S_SELECT_APPEND_F
						!  H5S_SELECT_PREPEND_F
                                                !
!            INTEGER, INTENT(OUT) :: hdferr     ! Error code

!            INTERFACE
!              INTEGER FUNCTION h5sselect_select_c(space1_id, operator, &
!                               space2_id)
!              USE H5GLOBAL
!             !DEC$IF DEFINED(HDF5F90_WINDOWS)
!             !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SSELECT_SELECT_C'::h5sselect_select_c
!             !DEC$ENDIF
!              INTEGER(HID_T), INTENT(INOUT) :: space1_id
!              INTEGER(HID_T), INTENT(IN) :: space2_id
!              INTEGER, INTENT(IN) :: operator
!              END FUNCTION h5sselect_select_c
!            END INTERFACE

!            hdferr = h5sselect_select_c(space1_id, operator, space2_id)
!            return

!          END SUBROUTINE h5sselect_select_f

!----------------------------------------------------------------------
! Name:		h5sget_select_type_f
!
! Purpose:	Retrieve the type of selection
!
! Inputs:
!		space_id	- dataspace iidentifier with selection
! Outputs:
!		type    	- flag, valid values are:
!				  H5S_SEL_ERROR_F
!				  H5S_SEL_NONE_F
!				  H5S_SEL_POINTS_F
!				  H5S_SEL_HYPERSLABS_F
!				  H5S_SEL_ALL_F
!		hdferr:		- error code
!				 	Success:  0
!				 	Failure: -1
! Optional parameters:		- NONE
!
! Programmer:	Elena Pourmal
!		October 7, 2002
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

          SUBROUTINE h5sget_select_type_f(space_id, type, hdferr)
            IMPLICIT NONE
            INTEGER(HID_T), INTENT(INOUT) :: space_id ! Dataspace identifier to
            INTEGER, INTENT(OUT) :: type        ! Selection type
						!  H5S_SEL_ERROR_F
						!  H5S_SEL_NONE_F
						!  H5S_SEL_POINTS_F
						!  H5S_SEL_HYPERSLABS_F
						!  H5S_SEL_ALL_F
            INTEGER, INTENT(OUT) :: hdferr     ! Error code

            INTERFACE
              INTEGER FUNCTION h5sget_select_type_c(space_id, type)
              USE H5GLOBAL
              !DEC$IF DEFINED(HDF5F90_WINDOWS)
              !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SGET_SELECT_TYPE_C'::h5sget_select_type_c
              !DEC$ENDIF
              INTEGER(HID_T), INTENT(IN) :: space_id
              INTEGER, INTENT(OUT) :: type
              END FUNCTION h5sget_select_type_c
            END INTERFACE

            hdferr = h5sget_select_type_c(space_id, type)
            return

          END SUBROUTINE h5sget_select_type_f

!----------------------------------------------------------------------
! Name:		H5Sdecode_f
!
! Purpose:	Decode a binary object description of data space and return a new object handle.
!
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
!		March 26, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5sdecode_f(buf, obj_id, hdferr)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: buf ! Buffer for the data space object to be decoded.
    INTEGER(HID_T), INTENT(OUT) :: obj_id  ! Object ID
    INTEGER, INTENT(OUT) :: hdferr     ! Error code

    INTERFACE
       INTEGER FUNCTION h5sdecode_c(buf, obj_id)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SDECODE_C'::h5sdecode_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         CHARACTER(LEN=*), INTENT(IN) :: buf
         INTEGER(HID_T), INTENT(OUT) :: obj_id  ! Object ID
       END FUNCTION h5sdecode_c
    END INTERFACE

    hdferr = h5sdecode_c(buf, obj_id)

  END SUBROUTINE h5sdecode_f

!----------------------------------------------------------------------
! Name:		H5Sencode_f
!
! Purpose:	Encode a data space object description into a binary buffer.
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
!		March 26, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5sencode_f(obj_id, buf, nalloc, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: obj_id ! Identifier of the object to be encoded.
    CHARACTER(LEN=*), INTENT(OUT) :: buf ! Buffer for the object to be encoded into.
    INTEGER(SIZE_T), INTENT(INOUT) :: nalloc ! The size of the allocated buffer.
    INTEGER, INTENT(OUT) :: hdferr     ! Error code


    INTERFACE
       INTEGER FUNCTION h5sencode_c(buf, obj_id, nalloc)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SENCODE_C'::h5sencode_c
         !DEC$ENDIF
         !DEC$ATTRIBUTES reference :: buf
         INTEGER(HID_T), INTENT(IN) :: obj_id
         CHARACTER(LEN=*), INTENT(OUT) :: buf
         INTEGER(SIZE_T), INTENT(INOUT) :: nalloc
       END FUNCTION h5sencode_c
    END INTERFACE

    hdferr = h5sencode_c(buf, obj_id, nalloc)

  END SUBROUTINE h5sencode_f


!----------------------------------------------------------------------
! Name:		h5sextent_equal_f
!
! Purpose: 	Determines whether two dataspace extents are equal.
!
! Inputs:
!		space1_id - First dataspace identifier.
!               space2_id - Second dataspace identifier.
! Outputs:
!                   Equal - .TRUE. if equal, .FALSE. if unequal.
!		  hdferr: - error code
!				 Success:  0
!				 Failure: -1
! Optional parameters:
!				NONE
!
! Programmer: M.S. Breitenfeld
!             April 2, 2008
!
! Modifications:
!
! Comment:
!----------------------------------------------------------------------

  SUBROUTINE h5sextent_equal_f(space1_id, space2_id, equal, hdferr)
    IMPLICIT NONE
    INTEGER(HID_T), INTENT(IN) :: space1_id ! First dataspace identifier.
    INTEGER(HID_T), INTENT(IN) :: space2_id ! Second dataspace identifier.
    LOGICAL, INTENT(OUT) :: Equal ! .TRUE. if equal, .FALSE. if unequal.
    INTEGER, INTENT(OUT) :: hdferr                ! Error code

    INTEGER(HID_T) :: c_equal

    INTERFACE
       INTEGER FUNCTION h5sextent_equal_c(space1_id, space2_id, c_equal)
         USE H5GLOBAL
         !DEC$IF DEFINED(HDF5F90_WINDOWS)
         !DEC$ATTRIBUTES C,reference,decorate,alias:'H5SEXTENT_EQUAL_C'::h5sextent_equal_c
         !DEC$ENDIF
         INTEGER(HID_T), INTENT(IN) :: space1_id
         INTEGER(HID_T), INTENT(IN) :: space2_id
         INTEGER(HID_T) :: c_equal
       END FUNCTION h5sextent_equal_c
    END INTERFACE

    hdferr = h5sextent_equal_c(space1_id, space2_id, c_equal)


    equal = .FALSE.
    IF(c_equal.GT.0) equal = .TRUE.


  END SUBROUTINE h5sextent_equal_f

END MODULE H5S
