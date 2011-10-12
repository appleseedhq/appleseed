#############################
Expected output for 'h5dump -d /dset1 -M 0,1,1,1 tcompound.h5'
#############################
HDF5 "tcompound.h5" {
DATASET "/dset1" {
   DATATYPE  H5T_COMPOUND {
      H5T_STD_I32BE "a_name";
      H5T_IEEE_F32BE "b_name";
      H5T_IEEE_F64BE "c_name";
   }
   DATASPACE  SIMPLE { ( 5 ) / ( 5 ) }
   PACKED_BITS OFFSET=0 LENGTH=1
   DATA {
   (0): {
         0,
         0,
         1
      },
   (1): {
         1,
         1,
         0.5
      },
   (2): {
         0,
         4,
         0.333333
      },
   (3): {
         1,
         9,
         0.25
      },
   (4): {
         0,
         16,
         0.2
      }
   }
   PACKED_BITS OFFSET=1 LENGTH=1
   DATA {
   (0): {
         0,
         0,
         1
      },
   (1): {
         0,
         1,
         0.5
      },
   (2): {
         1,
         4,
         0.333333
      },
   (3): {
         1,
         9,
         0.25
      },
   (4): {
         0,
         16,
         0.2
      }
   }
}
}
