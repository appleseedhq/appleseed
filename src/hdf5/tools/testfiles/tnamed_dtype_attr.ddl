#############################
Expected output for 'h5dump tnamed_dtype_attr.h5'
#############################
HDF5 "tnamed_dtype_attr.h5" {
GROUP "/" {
   DATASET "Dataset" {
      DATATYPE  "/Datatype"
      DATASPACE  SCALAR
      DATA {
      (0): 0
      }
      ATTRIBUTE "Attribute" {
         DATATYPE  "/Datatype"
         DATASPACE  SCALAR
         DATA {
         (0): 8
         }
      }
   }
   DATATYPE "Datatype" H5T_STD_I32LE;
      ATTRIBUTE "Attribute" {
         DATATYPE  H5T_STD_I32LE
         DATASPACE  SCALAR
         DATA {
         (0): 8
         }
      }

   DATATYPE "Link_to_Datatype" HARDLINK "/Datatype"

   GROUP "g1" {
      ATTRIBUTE "Attribute" {
         DATATYPE  "/Datatype"
         DATASPACE  SCALAR
         DATA {
         (0): 8
         }
      }
   }
}
}
