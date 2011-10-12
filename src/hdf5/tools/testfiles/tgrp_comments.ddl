#############################
Expected output for 'h5dump tgrp_comments.h5'
#############################
HDF5 "tgrp_comments.h5" {
GROUP "/" {
   GROUP "g1" {
      COMMENT "Comment for group /g1"
      GROUP "g1.1" {
         COMMENT "Comment for group /g1/g1.1"
      }
      GROUP "g1.2" {
         COMMENT "Comment for group /g1/g1.2"
      }
   }
   GROUP "g2" {
      COMMENT "Comment for group /g2"
      GROUP "g2.1" {
         COMMENT "Comment for group /g2/g2.1"
         GROUP "g2.1.1" {
            COMMENT "Comment for group /g2/g2.1/g2.1.1"
         }
         GROUP "g2.1.2" {
            COMMENT "Comment for group /g2/g2.1/g2.1.2"
         }
         GROUP "g2.1.3" {
            COMMENT "Comment for group /g2/g2.1/g2.1.3"
         }
      }
   }
   GROUP "g3" {
      COMMENT "Comment for group /g3"
      GROUP "g3.1" {
         COMMENT "Comment for group /g3/g3.1"
      }
      GROUP "g3.2" {
         COMMENT "Comment for group /g3/g3.2"
      }
      GROUP "g3.3" {
         COMMENT "Comment for group /g3/g3.3"
      }
      GROUP "g3.4" {
         COMMENT "Comment for group /g3/g3.4"
      }
   }
   GROUP "glongcomment" {
      COMMENT "Comment for group /glongcomment with a really, really, really long, long, long comment"
   }
}
}
