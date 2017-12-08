This is a collection of libraries implementing assemblers and
disassemblers for several architectures based on LLVM TableGen data.

Setup
=====

This package is supported on Ubuntu Xenial (16.04 LTS) and depends on
the following packages:

* ``binutils-multiarch``

To configure your system to build this package, run ``setup.sh``.

Library Concept
===============

The high level idea of this library is to generate assemblers and
disassemblers from the data provided by LLVM TableGen. Among other
things, this data includes lists of all of the instructions for the
Instruction Set Architectures (ISA) that we care about. Moreover, it
includes the encodings of those instructions, as well as their operands
and types of operands.

The ``dismantle-tablegen`` library provides tools for parsing the LLVM
TableGen data, as well as top-level helpers to generate assemblers and
disassemblers in Template Haskell.

Stability Notes
===============

Please note that the various architecture-specific ISA packages (such as
``dismantle-arm``) may be incomplete or incorrect for some instructions,
operands, or pretty-printed representations. This is because the
degree to which any of these backend packages has been completed is
a function of the particular binaries we used to test them (i.e.
the binaries found in ``tests/bin`` in each package). As a result
it's possible that when using ``dismantle`` with new binaries, some
instructions may not be supported by the disassembler, may reassemble
to incorrect bit sequences, or may have erroneous pretty-printing
representations when compared to the output of ``objdump``. As new
binaries are used with this software, code coverage of the ISA in
question may increase, revealing as-yet-unimplemented or incorrect
operand type implementations.

If any of the above cases are encountered, the operand type(s) and
instruction(s) in question will need to be supported. Fixing this
involves one or more of the following tasks:

* Determine which Tablegen descriptor entry is associated with the
  offending input byte sequence. If an instruction fails any checks,
  its bit pattern will be printed by the test suite; the bit pattern
  can then be checked against both the Tablegen descriptors and
  architecture reference manual to identify the instruction and its
  operand semantics.

* If the byte sequence fails to disassemble, ensure that the descriptor
  is not blacklisted by the ISA (e.g. due to metadata or other
  filtering). For this, check the ``isaInstructionFilter`` behavior.

* Ensure that all operand types required by the descriptor have been
  added to the ISA operand type list. For this, check the
  ``isaOperandPayloadTypes`` list to be sure that it includes all
  operand types mentioned in the descriptor's ``OutOperandList`` and
  ``InOperandList`` fields. Note that the entries in mapping must
  match the operand type names used in the Tablegen descriptors except
  that their first letters must be capitalized (since they get used
  to construct Haskell types). For example, an ``OutOperandList`` of
  ``(outs QPR:$Vd)`` indicates that a single operand ``$Vd`` has type
  ``QPR``, so ``QPR`` needs an entry in the ``isaOperandPayloadTypes``
  and we should also see ``$Vd`` in the bit pattern (``Bits``). We might
  also see it in the format string (``AsmString``), although not all
  operands will necessarily appear there.

* Ensure that the operand types decode the proper bit fields as
  indicated by the descriptor's bit pattern in its ``Inst`` field.

* Ensure that the operand types provide pretty printers that
  recover the original instruction operands and match the ``objdump``
  representation. For this, also see the format string given by the
  descriptor in the ``AsmString`` field to see how the operands are
  formatted in the overall pretty-printed output.

Generating TableGen Files
=========================

The file we take as inputs to this suite of tools are not actually in the
TableGen format (extension ``.td``); instead, we consume the output of the
``llvm-tblgen`` tool, which reads the real TableGen files and pre-processes
them. We use data files generated from sources of LLVM 3.9.

The real TableGen files are included in the LLVM source distribution.

.. code-block:: shell

   # Assuming that the LLVM source has been unpacked to ${LLVM_ROOT}
   cd ${LLVM_ROOT}/lib/Target

   # Choose the architecture you want to process, assume PowerPC
   cd PowerPC

   # Run tablegen
   llvm-tblgen -I${LLVM_ROOT}/include PPC.td > PPC.tgen


The ``.tgen`` extension is made up for this project, and not something
from LLVM.  The default output of the ``llvm-tblgen`` tool is a fully-expanded
version of the input TableGen files.  It is reasonably easy to parse, and the
format we consume in the ``dismantle-tablegen`` library to produce assemblers
and disassemblers.

Repairing TableGen Entries
==========================

In rare cases, LLVM's TableGen data can be broken in a variety of ways:

* An operand may appear some but not all of: the format string, bit
  pattern, or operand lists.

* An operand name may not be consistent across the format string, bit
  pattern, or operand lists.

In these cases a variety of failure modes can manifest:

* The pretty-printed instruction produced by ``dismantle`` will entirely
  lack some portion of the instruction and thus disagree with both
  ``objdump`` and the architecture reference manual.

* The instruction will be pretty-printed with ``UndefinedVar`` errors in
  place of any undefined operand variables.

* The instruction may reassemble but fail to preserve some input bits
  because an operand was mentioned in the bit pattern but not mentioned
  in either the input or output operand list.

Ideally these problems would not exist, and if they do, ideally we'd
file bug reports with LLVM and wait for those reports to be addressed.
But in the mean time, we may need to continue development and fix the
problems ourselves.

To resolve this, we provide a TableGen entry override feature. This
entails creating a new file with a ``.tgen`` suffix containing repaired
versions of the appropriate defs or classes, placing it in a
directory, and then adding that directory's path (relative to the
architecture-specific package root) to a list of override paths to the
Template Haskell functions ``genISA`` and ``genISARandomHelpers``. For
example, for the ``dismantle-aarch64`` package, we have some ``.tgen``
files in ``dismantle-aarch64/data/override/`` and then we have::

  $(genISA isa "data/AArch64.tgen" ["data/override"])
  $(genISARandomHelpers isa "data/AArch64.tgen" ["data/override"])

It's important to pass the same override paths to each of the above
Template Haskell functions to ensure that the same overrides are applied
to both code generation steps.

The overrides are processed as follows:

* All overrides (files with ``.tgen`` suffix) are loaded from all
  override paths. Override paths are not searched recursively. Files not
  ending in a ``.tgen`` extension are ignored.

* Each override file must be a valid standalone ``.tgen`` file, which
  means that it must take the following form::

    ------------- Classes -----------------
    (zero or more classes)
    ------------- Defs -----------------
    (zero or more defs)

* The override files are loaded in an undefined order. Every override
  file must provide defs or classes disjoint from all other override
  files; if not, it is undefined which duplicate def or class will
  affect the final ISA result. One way to avoid this problem is to
  provide each def or class in its own override file and use the class
  name or def name to name the file.

* The override files will be combined to form a collection of defs
  and classes that will override the same defs and classes in the
  main ``.tgen`` file for the architecture; overriding is done on a
  name basis, so if a def named ``foo`` is present in both the main
  architecture TableGen file and in an override file, the version from
  the override file will be used.

* The defs and classes in the overrides *completely replace* the ones in
  the original TableGen file. So if you need to repair a defective def
  or class, the entire entry (``def ... { ... }``) must be provided in
  the override file even if you only need to modify a single entry in
  the def or class.

* Override files may also provide entries that are not already present
  in the main TableGen file; in this case those entries will be added to
  the overall collection of TableGen records.

Developing in Template Haskell
==============================

Development of Template Haskell code can be frustrating, especially when things
do not type check as expected.  Some tips:

* Dumping Splices

  It is often helpful to see what code is actually being generated by
  TH. The ``-ddump-splices`` flag tells ghc to dump the code it
  generates (before type checking) to disk. The file will have the
  extension ``.dump-splices``. It can be hard to read, but it is much
  better than guessing.

  For example, if using Stack you can generate the splices for PPC
  using::

      stack clean dismantle-ppc
      stack build dismantle-ppc --ghc-options=-ddump-splices

  Or you can enable these options in the module using the TH functions::

      {-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

  Then you can find the splice files with::

      find .stack-work -name '*.dump-splices'

* Minimize TH

  TH is really horrible in many ways, so try to implement as much as
  possible in normal functions and just glue it together using TH.
