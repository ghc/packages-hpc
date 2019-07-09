setTestOpts([omit_ways(['ghci','threaded2']), when(fast(), skip)])

hpc_prefix = "perl hpcrun.pl --clear --exeext={exeext} --hpc={hpc}"

test('hpc_fork',
     [extra_files(['../hpcrun.pl']),
      when(opsys('mingw32'), skip), # no forkProcess on Windows
      # profiling and forkProcess don't go well together. See #11645 and #8862.
      expect_broken_for(16907, prof_ways),
      cmd_prefix(hpc_prefix)],
     compile_and_run, ['-fhpc'])

