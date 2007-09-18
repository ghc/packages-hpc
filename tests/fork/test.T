setTestOpts(compose(only_compiler_types(['ghc']),
		    compose(omit_ways(['ghci','threaded2']),
			    skip_if_fast)))

# Set up the hpc common variable(s).
hpc = config.top + '/../utils/hpc/hpc'

hpc_args = ['perl ../hpcrun.pl --clear --exeext=' + config.exeext + ' --hpc=' + hpc,'-fhpc'];

test('hpc_fork', normal, compile_and_run_with_prefix, hpc_args)

