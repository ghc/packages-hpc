setTestOpts(compose(only_compiler_types(['ghc']),
		    compose(omit_ways(['ghci']),
			    skip_if_fast)))

hpc_prefix = "perl ../hpcrun.pl --clear --exeext=" + config.exeext + " --hpc='" + config.hpc + "'";

test('tough', cmd_prefix(hpc_prefix), compile_and_run, ['-fhpc'])

# Test that -fhpc still works when (a) the source file is in a
# subdirectory and (b) it is a literate file.  We had a bug in this
# area caused by the handling of line pragmas in the preprocessed
# source file.
test('subdir/tough2',
     [ literate,
       cmd_prefix(hpc_prefix),
       omit_ways(['ghci'] + prof_ways) ], # profile goes in the wrong place
     compile_and_run, ['-fhpc'])
