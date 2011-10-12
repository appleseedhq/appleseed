#############################
Expected output for 'h5dump textlink.h5'
#############################
HDF5 "textlink.h5" {
GROUP "/" {
   EXTERNAL_LINK "extlink1" {
      TARGETFILE "filename"
      TARGETPATH "objname"
   }
   EXTERNAL_LINK "extlink2" {
      TARGETFILE "anotherfile"
      TARGETPATH "anotherobj"
   }
}
}
