#############################
Expected output for 'h5dump tvms.h5'
#############################
HDF5 "tvms.h5" {
GROUP "/" {
   DATASET "Array" {
      DATATYPE  H5T_VAX_F64
      DATASPACE  SIMPLE { ( 5, 6 ) / ( 5, 6 ) }
      DATA {
      (0,0): 0, 1, 2, 3, 4, 5,
      (1,0): 1, 2, 3, 4, 5, 6,
      (2,0): 2, 3, 4, 5, 6, 7,
      (3,0): 3, 4, 5, 6, 7, 8,
      (4,0): 4, 5, 6, 7, 8, 9
      }
   }
}
}
