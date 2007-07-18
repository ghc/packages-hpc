setTestOpts(compose(only_compiler_types(['ghc']),
		    omit_ways(['ghci'])))

# Set up the hpc common variable(s).
hpc = config.top + '/../utils/hpc/hpc';

test('hpc001', normal, compile_and_run_with_prefix, ['perl hpcrun.pl --hpc=' + hpc,'-fhpc'])
