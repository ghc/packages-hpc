setTestOpts([only_compiler_types(['ghc']),
             omit_ways(['ghci']),
             when(fast(), skip)])

test('hpc_report_multi_001', normal, run_command,
     ["{hpc} report hpc_sample --include=Geometry --per-module"])
test('hpc_report_multi_002', normal, run_command,
     ["{hpc} report hpc_sample --exclude=Geometry --per-module"])
test('hpc_report_multi_003', normal, run_command,
     ["{hpc} report hpc_sample --include=Geometry --per-module --decl-list"])

test('hpc_markup_multi_001', normal, run_command,
  ["{hpc} markup --srcdir=.. --hpcdir=tixs/.hpc hpc_sample --include=Geometry"])
test('hpc_markup_multi_002', normal, run_command,
  ["{hpc} markup --srcdir=.. --hpcdir=tixs/.hpc hpc_sample --exclude=Geometry"])
test('hpc_markup_multi_003', normal, run_command,
  ["{hpc} markup --srcdir=.. --hpcdir=tixs/.hpc hpc_sample --fun-entry-count"])

test('hpc_show_multi_001', normal, run_command,
     ["{hpc} show hpc_sample"])
test('hpc_show_multi_002', normal, run_command,
     ["{hpc} show hpc_sample --include=Geometry"])

# XXX We used to do clean_o_hi(), but that doesn't work any more
# Fix cleaning properly

