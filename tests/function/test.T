setTestOpts(compose(only_compiler_types(['ghc']),
		    compose(omit_ways(['ghci']),
			    skip_if_fast)))

hpc_prefix = "perl ../hpcrun.pl --clear --exeext=" + config.exeext + " --hpc='" + config.hpc + "'";

test('tough',
     [cmd_prefix(hpc_prefix),
      extra_clean(['Main.hs.html', 'hpc_index.html', 'hpc_index_alt.html',
                   'hpc_index_exp.html', 'hpc_index_fun.html', 'tough.tix'])],
     compile_and_run, ['-fhpc'])

