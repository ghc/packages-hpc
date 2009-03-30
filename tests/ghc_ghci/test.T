
test('hpc_ghc_ghci',
     [only_ways(['normal']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory hpc_ghc_ghci'])

