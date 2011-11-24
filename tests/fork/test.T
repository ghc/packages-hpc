setTestOpts(compose(only_compiler_types(['ghc']),
		    compose(omit_ways(['ghci','threaded2']),
			    skip_if_fast)))

hpc_prefix = "perl ../hpcrun.pl --clear --exeext=" + config.exeext + " --hpc='" + config.hpc + "'";

test('hpc_fork', [ if_os('mingw32', skip), # no forkProcess on Windows
                   cmd_prefix(hpc_prefix) ], compile_and_run, ['-fhpc'])

