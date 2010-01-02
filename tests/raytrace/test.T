setTestOpts(compose(only_compiler_types(['ghc']),
		    compose(omit_ways(['ghci']),
			    skip_if_fast)))

hpc_prefix = "perl ../hpcrun.pl --clear --exeext=" + config.exeext + " --hpc='" + config.hpc + "'";

test('hpc_raytrace', \
	[ skip_if_fast, cmd_prefix(hpc_prefix), reqlib('parsec') ], \
	multimod_compile_and_run, \
	['Main','-fhpc -package parsec'])

