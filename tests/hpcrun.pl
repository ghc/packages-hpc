#
# This command wraps round a command execution, adding some hpc tests.
#

while($ARGV[0] =~ /^--/) {
  if ($ARGV[0] =~ /--hpc=(.*)/) {
    shift @ARGV;
    $HPC = $1;       
  }
  if ($ARGV[0] =~ /--report/) {
    shift @ARGV;
    $REPORT = 1;       
  }
  if ($ARGV[0] =~ /--exeext=(.*)/) {
    shift @ARGV;
    $exeext = $1;       
  }
}


die "no option --hpc=* provided\n" if (!defined($HPC));
        
$binary = $ARGV[0] . $exeext;

system(@ARGV);
print "\n\n";
system("$HPC report $binary.tix");
print "\n\n";
system("$HPC report $binary.tix --per-module");
print "\n\n";
open(MARKUP,"$HPC markup $binary.tix| ");
while(<MARKUP>) {
  my $line = $_;
  print $line;
  if (/Writing: (\S+.html)/) {
     system("cat $1");        
  }
}
print "\n\n";
