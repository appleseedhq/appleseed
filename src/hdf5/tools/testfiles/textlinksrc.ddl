#############################
Expected output for 'h5dump textlinksrc.h5'
#############################
HDF5 "textlinksrc.h5" {
GROUP "/" {
   EXTERNAL_LINK "ext_link1" {
      TARGETFILE "textlinktar.h5"
      TARGETPATH "group"
         GROUP "group" {
            DATASET "dset" {
               DATATYPE  H5T_STD_I32LE
               DATASPACE  SIMPLE { ( 6 ) / ( 6 ) }
               DATA {
               (0): 1, 2, 3, 4, 5, 6
               }
            }
            EXTERNAL_LINK "elink_t1" {
               TARGETFILE "textlinksrc.h5"
               TARGETPATH "/"
            }
            EXTERNAL_LINK "elink_t2" {
               TARGETFILE "textlinksrc.h5"
               TARGETPATH "/ext_link4"
            }
            GROUP "subgroup" {
               GROUP "link_to_group" {
                  HARDLINK "/group"
               }
            }
         }
   }
   EXTERNAL_LINK "ext_link2" {
      TARGETFILE "textlinktar.h5"
      TARGETPATH "dset"
         DATASET "dset" {
            DATATYPE  H5T_STD_I32LE
            DATASPACE  SIMPLE { ( 6 ) / ( 6 ) }
            DATA {
            (0): 1, 2, 3, 4, 5, 6
            }
         }
   }
   EXTERNAL_LINK "ext_link3" {
      TARGETFILE "textlinktar.h5"
      TARGETPATH "type"
         DATATYPE "type" H5T_STD_I32LE;

   }
   EXTERNAL_LINK "ext_link4" {
      TARGETFILE "textlinktar.h5"
      TARGETPATH "group/elink_t2"
   }
   EXTERNAL_LINK "ext_link5" {
      TARGETFILE "textlinktar.h5"
      TARGETPATH "empty_group"
         GROUP "empty_group" {
         }
   }
}
}
