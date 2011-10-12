#############################
Expected output for 'h5dump --group=/g2 --group / -g /y tgroup.h5'
#############################
HDF5 "tgroup.h5" {
GROUP "/g2" {
   GROUP "g2.1" {
      GROUP "g2.1.1" {
      }
      GROUP "g2.1.2" {
      }
      GROUP "g2.1.3" {
      }
   }
}
GROUP "/" {
   GROUP "g1" {
      GROUP "g1.1" {
      }
      GROUP "g1.2" {
      }
   }
   GROUP "g2" {
      GROUP "g2.1" {
         GROUP "g2.1.1" {
         }
         GROUP "g2.1.2" {
         }
         GROUP "g2.1.3" {
         }
      }
   }
   GROUP "g3" {
      GROUP "g3.1" {
      }
      GROUP "g3.2" {
      }
      GROUP "g3.3" {
      }
      GROUP "g3.4" {
      }
   }
}
GROUP "/y" {
   }
}
h5dump error: unable to open group "/y"
