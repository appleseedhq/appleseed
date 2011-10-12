#############################
Expected output for 'h5dump -A tall.h5'
#############################
HDF5 "tall.h5" {
GROUP "/" {
   ATTRIBUTE "attr1" {
      DATATYPE  H5T_STD_I8BE
      DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
      DATA {
      (0): 97, 98, 99, 100, 101, 102, 103, 104, 105, 0
      }
   }
   ATTRIBUTE "attr2" {
      DATATYPE  H5T_STD_I32BE
      DATASPACE  SIMPLE { ( 2, 2 ) / ( 2, 2 ) }
      DATA {
      (0,0): 0, 1,
      (1,0): 2, 3
      }
   }
   GROUP "g1" {
      GROUP "g1.1" {
         DATASET "dset1.1.1" {
            DATATYPE  H5T_STD_I32BE
            DATASPACE  SIMPLE { ( 10, 10 ) / ( 10, 10 ) }
            ATTRIBUTE "attr1" {
               DATATYPE  H5T_STD_I8BE
               DATASPACE  SIMPLE { ( 27 ) / ( 27 ) }
               DATA {
               (0): 49, 115, 116, 32, 97, 116, 116, 114, 105, 98, 117, 116,
               (12): 101, 32, 111, 102, 32, 100, 115, 101, 116, 49, 46, 49,
               (24): 46, 49, 0
               }
            }
            ATTRIBUTE "attr2" {
               DATATYPE  H5T_STD_I8BE
               DATASPACE  SIMPLE { ( 27 ) / ( 27 ) }
               DATA {
               (0): 50, 110, 100, 32, 97, 116, 116, 114, 105, 98, 117, 116,
               (12): 101, 32, 111, 102, 32, 100, 115, 101, 116, 49, 46, 49,
               (24): 46, 49, 0
               }
            }
         }
         DATASET "dset1.1.2" {
            DATATYPE  H5T_STD_I32BE
            DATASPACE  SIMPLE { ( 20 ) / ( 20 ) }
         }
      }
      GROUP "g1.2" {
         EXTERNAL_LINK "extlink" {
            TARGETFILE "somefile"
            TARGETPATH "somepath"
         }
         GROUP "g1.2.1" {
            SOFTLINK "slink" {
               LINKTARGET "somevalue"
            }
         }
      }
   }
   GROUP "g2" {
      DATASET "dset2.1" {
         DATATYPE  H5T_IEEE_F32BE
         DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
      }
      DATASET "dset2.2" {
         DATATYPE  H5T_IEEE_F32BE
         DATASPACE  SIMPLE { ( 3, 5 ) / ( 3, 5 ) }
      }
      USERDEFINED_LINK "udlink" {
         LINKCLASS 187
      }
   }
}
}
