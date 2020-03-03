---
layout: post
title: Released Cyclone Scheme 0.16
excerpt: This release contains bug fixes and improvements for freeing memory allocated for Opaque C objects.
---

Features

- Updated the C API to optionally allow Cyclone's GC to free memory pointed to by an Opaque object. 

  For example:

      my_c_obj = calloc(1, sizeof(*my_c_obj_type));
      make_c_opaque(opq, my_c_obj);
      opaque_collect_ptr(&opq) = 1; // Cyclone's GC will free this memory

Bug Fixes

- Fixed a bug in `read-bytevector` where an extra byte could be introduced when reading multiple chunks of data.
- Fixed a bug where variables defined within `define-syntax` and `let-syntax` are reported as unbound by the compiler.
