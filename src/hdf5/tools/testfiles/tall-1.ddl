#############################
Expected output for 'h5dump tall.h5'
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
            DATA {
            (0,0): 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            (1,0): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
            (2,0): 0, 2, 4, 6, 8, 10, 12, 14, 16, 18,
            (3,0): 0, 3, 6, 9, 12, 15, 18, 21, 24, 27,
            (4,0): 0, 4, 8, 12, 16, 20, 24, 28, 32, 36,
            (5,0): 0, 5, 10, 15, 20, 25, 30, 35, 40, 45,
            (6,0): 0, 6, 12, 18, 24, 30, 36, 42, 48, 54,
            (7,0): 0, 7, 14, 21, 28, 35, 42, 49, 56, 63,
            (8,0): 0, 8, 16, 24, 32, 40, 48, 56, 64, 72,
            (9,0): 0, 9, 18, 27, 36, 45, 54, 63, 72, 81
            }
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
            DATA {
            (0): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
            (17): 17, 18, 19
            }
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
         DATA {
         (0): 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9
         }
      }
      DATASET "dset2.2" {
         DATATYPE  H5T_IEEE_F32BE
         DATASPACE  SIMPLE { ( 3, 5 ) / ( 3, 5 ) }
         DATA {
         (0,0): 0, 0.1, 0.2, 0.3, 0.4,
         (1,0): 0, 0.2, 0.4, 0.6, 0.8,
         (2,0): 0, 0.3, 0.6, 0.9, 1.2
         }
      }
      USERDEFINED_LINK "udlink" {
         LINKCLASS 187
      }
   }
}
}
