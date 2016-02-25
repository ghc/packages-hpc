setTestOpts([omit_ways(['ghci','threaded2']), when(fast(), skip)])

hpc_prefix = "perl ../hpcrun.pl --clear --exeext={exeext} --hpc={hpc}"

failing_ways_for_11645 = \
    [w for w in prof_ways if w not in threaded_ways and w in opt_ways]

# Skip on Windows, no forkProcess.
test('hpc_fork',
     [when(opsys('mingw32'), skip), cmd_prefix(hpc_prefix),
      expect_broken_for(11645, failing_ways_for_11645)],
     compile_and_run, ['-fhpc'])

