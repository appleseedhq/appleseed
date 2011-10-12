#############################
Expected output for 'h5dump -H --sort_by=creation_order --sort_order=ascending torderattr.h5'
#############################
HDF5 "torderattr.h5" {
GROUP "/" {
   ATTRIBUTE "a" {
      DATATYPE  H5T_STD_U8LE
      DATASPACE  SCALAR
   }
   ATTRIBUTE "b" {
      DATATYPE  H5T_STD_U8LE
      DATASPACE  SCALAR
   }
   ATTRIBUTE "c" {
      DATATYPE  H5T_STD_U8LE
      DATASPACE  SCALAR
   }
   DATASET "d" {
      DATATYPE  H5T_STD_U8LE
      DATASPACE  SCALAR
      ATTRIBUTE "a" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "b" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "c" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
   }
   DATASET "dt" {
      DATATYPE  H5T_STD_U8LE
      DATASPACE  SCALAR
      ATTRIBUTE "c" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "b" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "a" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
   }
   GROUP "g" {
      ATTRIBUTE "a" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "b" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "c" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
   }
   GROUP "gt" {
      ATTRIBUTE "c" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "b" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "a" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
   }
   DATATYPE "t" H5T_STD_I32LE;
      ATTRIBUTE "a" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "b" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "c" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }

   DATATYPE "tt" H5T_STD_I32LE;
      ATTRIBUTE "c" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "b" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }
      ATTRIBUTE "a" {
         DATATYPE  H5T_STD_U8LE
         DATASPACE  SCALAR
      }

}
}
