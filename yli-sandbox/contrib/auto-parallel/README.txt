File description: 
----------------- 
0. README.txt: this file.

1. make.input: OpenAxiom scripts for compiling the whole framework.

2. genericalanalysis.spad: a generic program analysis framework
providing specifications of analysis interfaces, and a package
containing functions for visiting IR graphs.

3. spadir.spad: data structure for representing Spad program and
   utilities for manipulating IRs.

4. ir2spad.spad: a translator for generating Spad code from its IR.

5. spad2ir.spad: a translator for generating IR from Spad code. 

6. synhelp.spad: utilities for manipulating syntax tree.

7. usrassumptions.spad: examples of user assumptions.

8. checkassumption.spad: typechecks user-written assumptions, and
generate assumption environment from the assumption syntax.

9. operatorcats.spad: several examples of property categories.

10. ctorinfo.spad: utilities for collecting category extension of a
category constructor, and all the attributes allowed in OpenAxiom.

11. typeutil.spad: provides a light Spad typechecker used in properties
derivation, and a package for updating the value of fresh type
variables introduced at typechecking phase.

12. redec.spad: provides package for detecting and transforming
reductions.

13. ATTREG.spad: a category which registers all the built-in attributes
in OpenAxiom.

14. compinterface.spad: the main entry for parallelizing compilation.
