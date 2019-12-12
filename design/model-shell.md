# Model/shell integration
We need to move the presentation logic into the CAD side of things. If the shell
manages it, we end up with two problems:

1. `hint` tends to segfault, I assume because it's got the same types loaded in
   two places
2. The interface between the model and shell becomes too complicated and/or
   specialized

The shell really needs to be just gloss + hint and forward everything, including
viewstate and graphics stuff, into a view-library layer of the wumber backend.

This has the added advantage that it's easy to extend the shell without
rebuilding the process.
