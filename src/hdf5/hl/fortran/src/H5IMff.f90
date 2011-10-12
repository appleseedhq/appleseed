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
! This file contains FORTRAN90 interfaces for H5IM functions
!

module h5im
use h5fortran_types
use hdf5
contains


!-------------------------------------------------------------------------
! Function: h5immake_image_8bit_f
!
! Purpose: Creates and writes an image an 8 bit image
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5immake_image_8bit_f(loc_id,&
                                 dset_name,&
                                 width,&
                                 height,&
                                 buf,&
                                 errcode )

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5immake_image_8bit_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
 character(len=*), intent(in) :: dset_name          ! name of the dataset
 integer(hsize_t), intent(in) :: width              ! width of image
 integer(hsize_t), intent(in) :: height             ! height of image
 integer, intent(in), dimension(*) :: buf           ! buffer
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5immake_image_8bit_c(loc_id,namelen,dset_name,width,height,buf)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMMAKE_IMAGE_8BIT_C'::h5immake_image_8bit_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset
  integer(hsize_t), intent(in) :: width                   ! width of image
  integer(hsize_t), intent(in) :: height                  ! height of image
  integer , intent(in), dimension(*) :: buf               ! buffer
  end function h5immake_image_8bit_c
 end interface

 namelen = len(dset_name)
 errcode = h5immake_image_8bit_c(loc_id,namelen,dset_name,width,height,buf)

end subroutine h5immake_image_8bit_f



!-------------------------------------------------------------------------
! Function: h5imread_image_f
!
! Purpose: Reads image data from disk.
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------


subroutine h5imread_image_f(loc_id,&
                            dset_name,&
                            buf,&
                            errcode )

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5imread_image_f
!DEC$endif
!
 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
 character(len=*), intent(in) :: dset_name          ! name of the dataset
 integer, intent(inout), dimension(*) :: buf        ! buffer
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5imread_image_c(loc_id,namelen,dset_name,buf)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMREAD_IMAGE_C'::h5imread_image_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset
  integer, intent(inout), dimension(*) :: buf             ! buffer
  end function h5imread_image_c
 end interface

 namelen = len(dset_name)
 errcode = h5imread_image_c(loc_id,namelen,dset_name,buf)

end subroutine h5imread_image_f


!-------------------------------------------------------------------------
! Function: h5immake_image_24bit_f
!
! Purpose: Creates and writes an image a 24 bit image
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5immake_image_24bit_f(loc_id,&
                                 dset_name,&
                                 width,&
                                 height,&
                                 il,&
                                 buf,&
                                 errcode )

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5immake_image_24bit_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
 character(len=*), intent(in) :: dset_name          ! name of the dataset
 integer(hsize_t), intent(in) :: width              ! width of image
 integer(hsize_t), intent(in) :: height             ! height of image
 character(len=*), intent(in) :: il                 ! interlace
 integer, intent(in), dimension(*) :: buf           ! buffer
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer :: ilen                                    ! name length

 interface
  integer function h5immake_image_24bit_c(loc_id,namelen,dset_name,ilen,il,width,height,buf)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMMAKE_IMAGE_24BIT_C'::h5immake_image_24bit_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: il
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  character(len=*), intent(in) :: dset_name               ! name of the dataset
  integer(hsize_t), intent(in) :: width                   ! width of image
  integer(hsize_t), intent(in) :: height                  ! height of image
  character(len=*), intent(in) :: il                      ! interlace
  integer, intent(in), dimension(*) :: buf                ! buffer
  integer :: namelen                                      ! lenght of name buffer
  integer :: ilen                                         ! name length

  end function h5immake_image_24bit_c
 end interface

 namelen = len(dset_name)
 ilen    = len(il)
 errcode = h5immake_image_24bit_c(loc_id,namelen,dset_name,ilen,il,width,height,buf)

end subroutine h5immake_image_24bit_f


!-------------------------------------------------------------------------
! Function: h5imget_image_info_f
!
! Purpose: Gets information about an image dataset (dimensions, interlace mode
!          and number of associated palettes).
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5imget_image_info_f(loc_id,&
                                dset_name,&
                                width,&
                                height,&
                                planes,&
                                interlace,&
                                npals,&
                                errcode )

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5imget_image_info_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
 character(len=*), intent(in) :: dset_name          ! name of the dataset
 integer(hsize_t), intent(inout) :: width           ! width of image
 integer(hsize_t), intent(inout) :: height          ! height of image
 integer(hsize_t), intent(inout) :: planes          ! color planes
 integer(hsize_t), intent(inout) :: npals           ! palettes
 character(len=*), intent(inout) :: interlace       ! interlace
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer :: ilen                                    ! name length

 interface
  integer function h5imget_image_info_c(loc_id,namelen,dset_name,width,height,planes,npals,ilen,interlace)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMGET_IMAGE_INFO_C'::h5imget_image_info_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: interlace
  integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
  character(len=*), intent(in) :: dset_name          ! name of the dataset
  integer(hsize_t), intent(inout) :: width           ! width of image
  integer(hsize_t), intent(inout) :: height          ! height of image
  integer(hsize_t), intent(inout) :: planes          ! color planes
  integer(hsize_t), intent(inout) :: npals           ! palettes
  character(len=*), intent(inout) :: interlace       ! interlace
  integer :: namelen                                 ! name length
  integer :: ilen                                    ! name length
  end function h5imget_image_info_c
 end interface

 namelen = len(dset_name)
 ilen    = len(interlace)
 errcode = h5imget_image_info_c(loc_id,namelen,dset_name,width,height,planes,npals,ilen,interlace)

end subroutine h5imget_image_info_f


!-------------------------------------------------------------------------
! Function: h5imis_image_f
!
! Purpose: Inquires if a dataset is an image
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

integer function h5imis_image_f(loc_id,&
                                dset_name)

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5imis_image_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
 character(len=*), intent(in) :: dset_name          ! name of the dataset
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5imis_image_c(loc_id,namelen,dset_name)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMIS_IMAGE_C'::h5imis_image_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset
  end function h5imis_image_c
 end interface

 namelen = len(dset_name)
 errcode = h5imis_image_c(loc_id,namelen,dset_name)
 h5imis_image_f = errcode

end function h5imis_image_f


!-------------------------------------------------------------------------
! Function: h5immake_palette_f
!
! Purpose: Creates and writes a palette
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5immake_palette_f(loc_id,&
                              dset_name,&
                              pal_dims,&
                              buf,&
                              errcode )

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5immake_palette_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id                 ! file or group identifier
 character(len=*), intent(in) :: dset_name              ! name of the dataset
 integer(hsize_t), intent(in), dimension(*) :: pal_dims ! dimensions
 integer, intent(in), dimension(*) :: buf               ! buffer
 integer :: errcode                                     ! error code
 integer :: namelen                                     ! name length

 interface
  integer function h5immake_palette_c(loc_id,namelen,dset_name,pal_dims,buf)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMMAKE_PALETTE_C'::h5immake_palette_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset
  integer(hsize_t), intent(in), dimension(*) :: pal_dims  ! dimensions
  integer, intent(in), dimension(*) :: buf                ! buffer
  end function h5immake_palette_c
 end interface

 namelen = len(dset_name)
 errcode = h5immake_palette_c(loc_id,namelen,dset_name,pal_dims,buf)

end subroutine h5immake_palette_f



!-------------------------------------------------------------------------
! Function: h5imlink_palette_f
!
! Purpose: This function attaches a palette to an existing image dataset
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5imlink_palette_f(loc_id,&
                              dset_name,&
                              pal_name,&
                              errcode )

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5imlink_palette_f
!DEC$endif
!
 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
 character(len=*), intent(in) :: dset_name          ! name of the dataset
 character(len=*), intent(in) :: pal_name           ! palette name
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer :: ilen                                    ! name length

 interface
  integer function h5imlink_palette_c(loc_id,namelen,dset_name,ilen,pal_name)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMLINK_PALETTE_C'::h5imlink_palette_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: pal_name
  integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
  character(len=*), intent(in) :: dset_name          ! name of the dataset
  character(len=*), intent(in) :: pal_name           ! palette name
  integer :: namelen                                 ! name length
  integer :: ilen                                    ! name length
  end function h5imlink_palette_c
 end interface

 namelen = len(dset_name)
 ilen    = len(pal_name)
 errcode = h5imlink_palette_c(loc_id,namelen,dset_name,ilen,pal_name)

end subroutine h5imlink_palette_f


!-------------------------------------------------------------------------
! Function: h5imunlink_palette_f
!
! Purpose: This function dettaches a palette to an existing image dataset
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5imunlink_palette_f(loc_id,&
                              dset_name,&
                              pal_name,&
                              errcode )

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5imunlink_palette_f
!DEC$endif
!


 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
 character(len=*), intent(in) :: dset_name          ! name of the dataset
 character(len=*), intent(in) :: pal_name           ! palette name
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length
 integer :: ilen                                    ! name length

 interface
  integer function h5imunlink_palette_c(loc_id,namelen,dset_name,ilen,pal_name)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMUNLINK_PALETTE_C'::h5imunlink_palette_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  !DEC$ATTRIBUTES reference :: pal_name
  integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
  character(len=*), intent(in) :: dset_name          ! name of the dataset
  character(len=*), intent(in) :: pal_name           ! palette name
  integer :: namelen                                 ! name length
  integer :: ilen                                    ! name length
  end function h5imunlink_palette_c
 end interface

 namelen = len(dset_name)
 ilen    = len(pal_name)
 errcode = h5imunlink_palette_c(loc_id,namelen,dset_name,ilen,pal_name)

end subroutine h5imunlink_palette_f



!-------------------------------------------------------------------------
! Function: h5imget_npalettes_f
!
! Purpose: Gets the number of palettes associated to an image
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 05, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5imget_npalettes_f(loc_id,&
                               dset_name,&
                               npals,&
                               errcode )

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5imget_npalettes_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
 character(len=*), intent(in) :: dset_name          ! name of the dataset
 integer(hsize_t), intent(inout) :: npals           ! palettes
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5imget_npalettes_c(loc_id,namelen,dset_name,npals)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMGET_NPALETTES_C'::h5imget_npalettes_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
  character(len=*), intent(in) :: dset_name          ! name of the dataset
  integer(hsize_t), intent(inout) :: npals           ! palettes
  integer :: namelen                                 ! name length
  end function h5imget_npalettes_c
 end interface

 namelen = len(dset_name)
 errcode = h5imget_npalettes_c(loc_id,namelen,dset_name,npals)

end subroutine h5imget_npalettes_f


!-------------------------------------------------------------------------
! Function: h5imget_palette_info_f
!
! Purpose: Get palette information
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

subroutine h5imget_palette_info_f(loc_id,&
                                  dset_name,&
                                  pal_number,&
                                  dims,&
                                  errcode )

 implicit none


!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5imget_palette_info_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id                ! file or group identifier
 character(len=*), intent(in) :: dset_name             ! name of the dataset
 integer, intent(in) :: pal_number                     ! palette number
 integer(hsize_t), dimension(*), intent(inout) :: dims ! dimensions
 integer :: errcode                                    ! error code
 integer :: namelen                                    ! name length

 interface
  integer function h5imget_palette_info_c(loc_id,namelen,dset_name,pal_number,dims)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMGET_PALETTE_INFO_C'::h5imget_palette_info_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                ! file or group identifier
  character(len=*), intent(in) :: dset_name             ! name of the dataset
  integer, intent(in) :: pal_number                     ! palette number
  integer(hsize_t), dimension(*), intent(inout) :: dims ! dimensions
  integer :: namelen                                    ! name length
  end function h5imget_palette_info_c
 end interface

 namelen = len(dset_name)
 errcode = h5imget_palette_info_c(loc_id,namelen,dset_name,pal_number,dims)

end subroutine h5imget_palette_info_f



!-------------------------------------------------------------------------
! Function: h5imget_palette_f
!
! Purpose: Reads palette
!
! Return: Success: 0, Failure: -1
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------


subroutine h5imget_palette_f(loc_id,&
                             dset_name,&
                             pal_number,&
                             buf,&
                             errcode )

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5imget_palette_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
 character(len=*), intent(in) :: dset_name          ! name of the dataset
 integer, intent(in) :: pal_number                  ! palette number
 integer, intent(inout), dimension(*) :: buf        ! buffer
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5imget_palette_c(loc_id,namelen,dset_name,pal_number,buf)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMGET_PALETTE_C'::h5imget_palette_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset
  integer, intent(in) :: pal_number                       ! palette number
  integer, intent(inout), dimension(*) :: buf             ! buffer
  end function h5imget_palette_c
 end interface

 namelen = len(dset_name)
 errcode = h5imget_palette_c(loc_id,namelen,dset_name,pal_number,buf)

end subroutine h5imget_palette_f


!-------------------------------------------------------------------------
! Function: h5imis_palette_f
!
! Purpose: Inquires if a dataset is a palette
!
! Return: true, false, fail
!
! Programmer: pvn@ncsa.uiuc.edu
!
! Date: October 06, 2004
!
! Comments:
!
! Modifications:
!
!-------------------------------------------------------------------------

integer function h5imis_palette_f(loc_id,&
                                  dset_name)

 implicit none

!
!This definition is needed for Windows DLLs
!DEC$if defined(BUILD_HDF5_DLL)
!DEC$attributes dllexport :: h5imis_palette_f
!DEC$endif
!

 integer(hid_t),   intent(in) :: loc_id             ! file or group identifier
 character(len=*), intent(in) :: dset_name          ! name of the dataset
 integer :: errcode                                 ! error code
 integer :: namelen                                 ! name length

 interface
  integer function h5imis_palette_c(loc_id,namelen,dset_name)
  use h5global
  !DEC$IF DEFINED(HDF5F90_WINDOWS)
  !DEC$ATTRIBUTES C,reference,decorate,alias:'H5IMIS_PALETTE_C'::h5imis_palette_c
  !DEC$ENDIF
  !DEC$ATTRIBUTES reference :: dset_name
  integer(hid_t),   intent(in) :: loc_id                  ! file or group identifier
  integer :: namelen                                      ! lenght of name buffer
  character(len=*), intent(in) :: dset_name               ! name of the dataset
  end function h5imis_palette_c
 end interface

 namelen = len(dset_name)
 errcode = h5imis_palette_c(loc_id,namelen,dset_name)
 h5imis_palette_f = errcode

end function h5imis_palette_f


!  end
!
end module H5IM





