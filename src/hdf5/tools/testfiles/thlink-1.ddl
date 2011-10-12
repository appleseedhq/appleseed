#############################
Expected output for 'h5dump thlink.h5'
#############################
HDF5 "thlink.h5" {
GROUP "/" {
   DATASET "dset1" {
      DATATYPE  H5T_STD_I32BE
      DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
      DATA {
      (0): 0, 1, 2, 3, 4
      }
   }
   GROUP "g1" {
      DATASET "dset2" {
         HARDLINK "/dset1"
      }
      GROUP "g1.1" {
         DATASET "dset3" {
            HARDLINK "/dset1"
         }
      }
   }
   GROUP "g2" {
      HARDLINK "/g1/g1.1"
   }
   GROUP "g3" {
      HARDLINK "/"
   }
}
}
