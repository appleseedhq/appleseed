#############################
Expected output for 'h5dump tarray1.h5'
#############################
HDF5 "tarray1.h5" {
GROUP "/" {
   DATASET "Dataset1" {
      DATATYPE  H5T_ARRAY { [4] H5T_STD_I32LE }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): [ 0, 1, 2, 3 ], [ 10, 11, 12, 13 ], [ 20, 21, 22, 23 ],
      (3): [ 30, 31, 32, 33 ]
      }
   }
}
}
