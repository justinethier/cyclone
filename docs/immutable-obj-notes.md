
Immutable
      , cvar_tag                // 7
      , eof_tag                 // 9
      , mutex_tag               // 14
      , cond_var_tag            // 6
      , closure0_tag            // 3
      , closure1_tag            // 4
      , closureN_tag            // 5
      , macro_tag               // 13
      , integer_tag             // 11
      , bignum_tag              // 12
      , complex_num_tag         // 21
      , double_tag              // 8
      , primitive_tag           // 17
      , symbol_tag              // 19

Mutable
      , bytevector_tag          // 1
      , pair_tag                // 15
      , string_tag              // 18
      , vector_tag              // 20

      // weird case, I guess it goes here since we modify members constantly when doing I/O
      , port_tag                // 16 

      // Weird case, could be modified by C code
      , c_opaque_tag            // 2

      // Special case
      , forward_tag             // 10
