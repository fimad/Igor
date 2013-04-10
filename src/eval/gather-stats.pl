#!/usr/bin/perl
################################################################################
# gather-stats.pl
#
# This script will run the lcs and chi-squared stats on samples in $_DIR and
# collate the results for use in the paper.
################################################################################
use warnings;
use strict;
use Getopt::Long;

################################################################################
# Handle Command Line Arguments 
################################################################################

my $_USAGE = "usage: $0 -test factorial -test xor -library unif -library scanning -dir out/ -opcodes opcode.dist\n";

# The names of each test
my @_TESTS;
# The libraries that each test was generated with
my @_LIBS;
# The directory containing the generated object files
my $_DIR;
# The file containing the opcode distribution
my $_OPCODES;
# Chi-squared statistic max-bound for significance
my $_CHI2_MAX = 615.11150122;

GetOptions(
        "test=s"        =>  \@_TESTS
    ,   "library=s"     =>  \@_LIBS
    ,   "directory=s"   =>  \$_DIR
    ,   "opcodes=s"     =>  \$_OPCODES
);

die($_USAGE)
if not @_TESTS
or not @_LIBS;

################################################################################
# Main
################################################################################

sub getTemplate{
    my($test,$lib,$i) = @_;
    sprintf("$_DIR/$test-$lib-%03d", $i);
}

for my $test (@_TESTS){
    for my $lib (@_LIBS){
        my $i = 0;
        my $j = 0;
        my $file;
        my $file2;
        my $chi2_avg = 0;
        my $chi2_passed = 0;
        my $total_pairs = 0; #total pairs for lcs
        my $lcs_avg = 0;
        while( $file = getTemplate($test,$lib,$i) and -e "$file.o" ){

            # Perform the chi squared test
            my $chi2 = `./dist/build/eval-chi-squared/eval-chi-squared $_OPCODES $file.bin`;
            chomp $chi2;
            $chi2_avg += $chi2;
            $chi2_passed++ if $chi2 <= $_CHI2_MAX;
            
            #perform the LCS test
            $j=$i+1;
            while( $file2 = getTemplate($test,$lib,$j) and -e "$file2.o" ){
                my $lcs = `./dist/build/eval-lcs/eval-lcs $file.bin $file2.bin`;
                chomp $lcs;
                $lcs_avg += $lcs;
                $total_pairs++;
                $j++;
            }

            $i++;
        }

        # Report our findings!
        printf("$test-$lib:\n");
        printf("\tChi2 Avg:\t%.4f\n", $chi2_avg/$i);
        printf("\tChi2 Passed:\t%d\n", $chi2_passed);
        printf("\tLCS Avg:\t%.4f\n", $lcs_avg/$total_pairs);
    }
}

