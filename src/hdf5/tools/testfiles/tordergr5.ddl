#############################
Expected output for 'h5dump -q creation_order tordergr.h5'
#############################
HDF5 "tordergr.h5" {
GROUP "/" {
   GROUP "2" {
      GROUP "a" {
         GROUP "a1" {
         }
         GROUP "a2" {
            GROUP "a21" {
            }
            GROUP "a22" {
            }
         }
      }
      GROUP "b" {
      }
      GROUP "c" {
      }
   }
   GROUP "1" {
      GROUP "c" {
      }
      GROUP "b" {
      }
      GROUP "a" {
         GROUP "a1" {
         }
         GROUP "a2" {
            GROUP "a22" {
            }
            GROUP "a21" {
            }
         }
      }
   }
}
}
