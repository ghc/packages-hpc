
test('hpc_ghc_ghci',
     [expect_broken(1779),
      only_ways(['normal']),
      only_compiler_types(['ghc'])],
     run_command,
     ['$MAKE -s --no-print-directory hpc_ghc_ghci'])

