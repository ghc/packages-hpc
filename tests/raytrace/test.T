setTestOpts(compose(only_compiler_types(['ghc']),
		    omit_ways(['ghci'])))

# Set up the hpc common variable(s), including
# a hack to find the true location of the hpc tools.
hpc = config.top
for dir in os.path.split(getTestDir()):
   if dir != '.' and dir != '':
     hpc = '../' + hpc 
hpc = hpc + '/../utils/hpc/hpc';

hpc_args = ['perl hpcrun.pl --hpc=' + hpc,'-fhpc'];

test('galois_raytrace', \
	skip_if_fast, \
	multimod_compile_and_run_with_prefix, \
	['perl ../hpcrun.pl --hpc=' + hpc, 'Main','-fhpc -package parsec'])

test('hpc_report_multi_001', normal, run_command, \
	[hpc + ' report galois_raytrace.tix --include=Geometry --per-module'])
test('hpc_report_multi_002', normal, run_command, \
	[hpc + ' report galois_raytrace.tix --exclude=Geometry --per-module'])
test('hpc_report_multi_003', normal, run_command, \
	[hpc + ' report galois_raytrace.tix --include=Geometry --per-module --decl-list']) 

test('hpc_markup_multi_001', normal, run_command, \
	[hpc + ' markup galois_raytrace.tix --include=Geometry'])
test('hpc_markup_multi_002', normal, run_command, \
	[hpc + ' markup galois_raytrace.tix --exclude=Geometry'])
test('hpc_markup_multi_003', normal, run_command, \
	[hpc + ' markup galois_raytrace.tix --fun-entry-count']) 

test('hpc_show_multi_001', normal, run_command, [hpc + ' show galois_raytrace.tix'])
test('hpc_show_multi_002', normal, run_command, [hpc + ' show galois_raytrace.tix --include=Geometry'])

clean_o_hi()
