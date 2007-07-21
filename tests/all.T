setTestOpts(compose(only_compiler_types(['ghc']),
		    omit_ways(['ghci'])))

# Set up the hpc common variable(s).
hpc = config.top + '/../utils/hpc/hpc';
hpc_args = ['perl hpcrun.pl --hpc=' + hpc,'-fhpc'];

test('hpc001', normal, compile_and_run_with_prefix, hpc_args)
test('hpc_help', normal, run_command, [hpc + ' help'])
test('hpc_help_help', normal, run_command, [hpc + ' help help'])

test('hpc_help_report', normal, run_command, [hpc + ' help report'])
test('hpc_report_001', normal, run_command, \
	[hpc + ' report hpc001.tix --xml-output']) 
test('hpc_report_002', normal, run_command, \
	[hpc + ' report hpc001.tix --xml-output --per-module']) 
test('hpc_report_003', normal, run_command, \
	[hpc + ' report hpc001.tix --per-module --decl-list']) 
test('hpc_report_error_001', exit_code(1), run_command, [hpc + ' report']) # no .tix file
test('hpc_report_error_002', exit_code(1), run_command, [hpc + ' report hpc001.hs']) # bad .tix file

test('hpc_help_markup', normal, run_command, [hpc + ' help markup'])
test('hpc_markup_error_001', exit_code(1), run_command, [hpc + ' markup']) # no .tix file
test('hpc_markup_error_002', exit_code(1), run_command, [hpc + ' markup hpc001.hs']) # bad .tix file
test('hpc_markup_001', normal, run_command, \
	['(' + hpc + ' markup hpc001.tix --highlight-covered ; cat Main.hs.html )']) 
test('hpc_markup_002', normal, run_command, \
	['(' + hpc + ' markup hpc001.tix --fun-entry-count ; cat Main.hs.html )']) 

test('hpc_help_show', normal, run_command, [hpc + ' help show'])
test('hpc_show', normal, run_command, [hpc + ' show hpc001.tix'])
test('hpc_show_error_001', exit_code(1), run_command, [hpc + ' show']) # no .tix file
test('hpc_show_error_002', exit_code(1), run_command, [hpc + ' show hpc001.hs']) # bad .tix file

test('hpc_help_version', normal, run_command, [hpc + ' help version'])
test('hpc_version', normal, run_command, [hpc + ' version'])

test('hpc_bad_001', exit_code(1), run_command, [hpc + ' bad arguments'])

test('hpc001', normal, compile_and_run_with_prefix, ['perl hpcrun.pl --hpc=' + hpc,'-fhpc'])
