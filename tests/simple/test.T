setTestOpts(compose(only_compiler_types(['ghc']),
		    compose(omit_ways(['ghci']),
			    skip_if_fast)))

hpc_prefix = "perl ../hpcrun.pl --clear --exeext=" + config.exeext + " --hpc='" + config.hpc + "'";

test('hpc001', cmd_prefix(hpc_prefix), compile_and_run, ['-fhpc'])

