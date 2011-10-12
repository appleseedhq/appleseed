#############################
Expected output for 'h5dump tloop.h5'
#############################
HDF5 "tloop.h5" {
GROUP "/" {
   GROUP "g1" {
      GROUP "g1.1" {
         GROUP "g2.1" {
            HARDLINK "/g1"
         }
      }
   }
   GROUP "g2" {
      HARDLINK "/g1/g1.1"
   }
}
}
