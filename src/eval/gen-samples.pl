#!/usr/bin/perl
################################################################################
# gen-samples.pl
#
# This file will generate a $_N .exe's and .o's for a specified $_SAMPLE program
# and $_LIBRARY. The files will be stored in the $_OUT directory and named
# $_NAME-###.o and $_NAME-###.exe.
################################################################################
use warnings;
use strict;
use Getopt::Long;

################################################################################
# Handle Command Line Arguments 
################################################################################

my $_USAGE = "usage: $0 -sample ../sample-test -library ../library -o ./output -template sample -n 100\n";

# The path to the program that will generate the object file
my $_SAMPLE;
# The path to the library to be used in generation
my $_LIBRARY;
# The number of samples to generate
my $_N;
# The output directory
my $_OUT;
# The template for naming the various generations
my $_NAME;
# The C file to compile against the generated object file
my $_MAIN;
# The path to the compiler to use to generate the executable
my $_GCC = "/usr/bin/i586-mingw32msvc-gcc";

GetOptions(
        "sample=s"      =>  \$_SAMPLE
    ,   "library=s"     =>  \$_LIBRARY
    ,   "out=s"         =>  \$_OUT
    ,   "template=s"    =>  \$_NAME
    ,   "n=i"           =>  \$_N
    ,   "main=s"        =>  \$_MAIN
    ,   "gcc=s"         =>  \$_GCC
);

# Ensure that all of the command line arguments are supplied and those that are
# files and directories actually exist.
die($_USAGE)
if not defined($_SAMPLE) or not -e $_SAMPLE
or not defined($_LIBRARY) or not -e $_LIBRARY
or not defined($_MAIN) or not -e $_MAIN
or not defined($_N)
or not defined($_OUT) or not -d $_OUT
or not defined($_NAME);

################################################################################
# Main
################################################################################

for my $i (0..($_N-1)){
    my $template = sprintf("$_OUT/$_NAME-%03d", $i);
    `$_SAMPLE $_LIBRARY $template.o`;
    `$_GCC -m32 $_MAIN $template.o -o $template.exe`;
    print ".";
}
print "\n";

