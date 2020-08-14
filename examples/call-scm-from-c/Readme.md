This directory contains two sample applications to demonstrate how to call into Scheme code from C.

`full-with-gc.scm` - A sample application demonstrating how to call arbitrary Scheme code from C in a safe way. This is the recommended method and would be the best place to start when building your own applications.

`basic-no-gc.scm` - A sample application to demonstrate how to make simple, limited calls into Scheme code from C. This method is more efficient but also has significant limitations. See the code for more information.
