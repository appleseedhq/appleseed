#############################
Expected output for 'h5dump tcompound_complex.h5'
#############################
HDF5 "tcompound_complex.h5" {
GROUP "/" {
   DATASET "CompoundComplex" {
      DATATYPE  H5T_COMPOUND {
         H5T_STD_I32BE "a_name";
         H5T_ARRAY { [4] H5T_STRING {
            STRSIZE H5T_VARIABLE;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } } "b_name";
         H5T_STRING {
            STRSIZE 6;
            STRPAD H5T_STR_NULLTERM;
            CSET H5T_CSET_ASCII;
            CTYPE H5T_C_S1;
         } "c_name";
         H5T_ARRAY { [5][6] H5T_STD_I16BE } "d_name";
         H5T_IEEE_F32BE "e_name";
         H5T_ARRAY { [10] H5T_IEEE_F64BE } "f_name";
         H5T_STD_I8LE "g_name";
      }
      DATASPACE  SIMPLE { ( 6 ) / ( 6 ) }
      DATA {
      (0): {
            0,
            [ "A fight is a contract that takes two people to honor.", "A combative stance means that you've accepted the contract.", "In which case, you deserve what you get.", "  --  Professor Cheng Man-ch'ing" ],
            "Hello!",
            [ 0, 1, 2, 3, 4, 5,
               1, 2, 3, 4, 5, 6,
               2, 3, 4, 5, 6, 7,
               3, 4, 5, 6, 7, 8,
               4, 5, 6, 7, 8, 9 ],
            0,
            [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
            109
         },
      (1): {
            1,
            [ "A fight is a contract that takes two people to honor.", "A combative stance means that you've accepted the contract.", "In which case, you deserve what you get.", "  --  Professor Cheng Man-ch'ing" ],
            "Hello!",
            [ 1, 2, 3, 4, 5, 6,
               2, 3, 4, 5, 6, 7,
               3, 4, 5, 6, 7, 8,
               4, 5, 6, 7, 8, 9,
               5, 6, 7, 8, 9, 10 ],
            0.96,
            [ 1024.96, 1024.96, 1024.96, 1024.96, 1024.96, 1024.96, 1024.96, 1024.96, 1024.96, 1024.96 ],
            109
         },
      (2): {
            2,
            [ "A fight is a contract that takes two people to honor.", "A combative stance means that you've accepted the contract.", "In which case, you deserve what you get.", "  --  Professor Cheng Man-ch'ing" ],
            "Hello!",
            [ 2, 3, 4, 5, 6, 7,
               3, 4, 5, 6, 7, 8,
               4, 5, 6, 7, 8, 9,
               5, 6, 7, 8, 9, 10,
               6, 7, 8, 9, 10, 11 ],
            1.92,
            [ 2049.93, 2049.93, 2049.93, 2049.93, 2049.93, 2049.93, 2049.93, 2049.93, 2049.93, 2049.93 ],
            109
         },
      (3): {
            3,
            [ "A fight is a contract that takes two people to honor.", "A combative stance means that you've accepted the contract.", "In which case, you deserve what you get.", "  --  Professor Cheng Man-ch'ing" ],
            "Hello!",
            [ 3, 4, 5, 6, 7, 8,
               4, 5, 6, 7, 8, 9,
               5, 6, 7, 8, 9, 10,
               6, 7, 8, 9, 10, 11,
               7, 8, 9, 10, 11, 12 ],
            2.88,
            [ 3074.89, 3074.89, 3074.89, 3074.89, 3074.89, 3074.89, 3074.89, 3074.89, 3074.89, 3074.89 ],
            109
         },
      (4): {
            4,
            [ "A fight is a contract that takes two people to honor.", "A combative stance means that you've accepted the contract.", "In which case, you deserve what you get.", "  --  Professor Cheng Man-ch'ing" ],
            "Hello!",
            [ 4, 5, 6, 7, 8, 9,
               5, 6, 7, 8, 9, 10,
               6, 7, 8, 9, 10, 11,
               7, 8, 9, 10, 11, 12,
               8, 9, 10, 11, 12, 13 ],
            3.84,
            [ 4099.85, 4099.85, 4099.85, 4099.85, 4099.85, 4099.85, 4099.85, 4099.85, 4099.85, 4099.85 ],
            109
         },
      (5): {
            5,
            [ "A fight is a contract that takes two people to honor.", "A combative stance means that you've accepted the contract.", "In which case, you deserve what you get.", "  --  Professor Cheng Man-ch'ing" ],
            "Hello!",
            [ 5, 6, 7, 8, 9, 10,
               6, 7, 8, 9, 10, 11,
               7, 8, 9, 10, 11, 12,
               8, 9, 10, 11, 12, 13,
               9, 10, 11, 12, 13, 14 ],
            4.8,
            [ 5124.82, 5124.82, 5124.82, 5124.82, 5124.82, 5124.82, 5124.82, 5124.82, 5124.82, 5124.82 ],
            109
         }
      }
   }
}
}
