#############################
Expected output for 'h5dump tnestedcomp.h5'
#############################
HDF5 "tnestedcomp.h5" {
GROUP "/" {
   DATASET "ArrayOfStructures" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I32LE "a_name";
         H5T_IEEE_F32LE "b_name";
         H5T_IEEE_F64LE "c_name";
         H5T_COMPOUND {
            H5T_STRING {
               STRSIZE 1;
               STRPAD H5T_STR_NULLTERM;
               CSET H5T_CSET_ASCII;
               CTYPE H5T_C_S1;
            } "char_name";
            H5T_ARRAY { [2] H5T_IEEE_F32LE } "array_name";
         } "d_name";
      }
      DATASPACE  SIMPLE { ( 10 ) / ( 10 ) }
      DATA {
      (0): {
            0,
            0,
            1,
            {
               "A",
               [ -100, 100 ]
            }
         },
      (1): {
            1,
            1,
            0.5,
            {
               "B",
               [ -100, 100 ]
            }
         },
      (2): {
            2,
            4,
            0.333333,
            {
               "C",
               [ -100, 100 ]
            }
         },
      (3): {
            3,
            9,
            0.25,
            {
               "D",
               [ -100, 100 ]
            }
         },
      (4): {
            4,
            16,
            0.2,
            {
               "E",
               [ -100, 100 ]
            }
         },
      (5): {
            5,
            25,
            0.166667,
            {
               "F",
               [ -100, 100 ]
            }
         },
      (6): {
            6,
            36,
            0.142857,
            {
               "G",
               [ -100, 100 ]
            }
         },
      (7): {
            7,
            49,
            0.125,
            {
               "H",
               [ -100, 100 ]
            }
         },
      (8): {
            8,
            64,
            0.111111,
            {
               "I",
               [ -100, 100 ]
            }
         },
      (9): {
            9,
            81,
            0.1,
            {
               "J",
               [ -100, 100 ]
            }
         }
      }
   }
}
}
