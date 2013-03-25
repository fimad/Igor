#!/usr/bin/perl
use warnings;
use strict;

my @tests = ("xor", "factorial", "max");
my @libraries = ("w32", "scan", "unif");

for my $test (@tests){
    for my $lib (@libraries){
        print "$test-$lib:\n";
        system("./eval/gen-samples.pl -s dist/build/$test-test/$test-test -m tests/$test.c -l library.$lib -o out/ -te $test-$lib -n 100");
        print "\n";
    }
}
