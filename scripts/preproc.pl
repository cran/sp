#!/usr/bin/perl
#-*- mode: cperl -*-
#
#  script to produce version-specific S/R code from common source.
#
# Usage  preproc.pl -ver=version [-D=symbol ...] infile > outfile
# 
# versions R and (not)SP5 get special processing.

use Getopt::Long;
GetOptions ('ver=s', 'D=s@');

$defs{$opt_ver} = 1;
foreach $symbol (@opt_D) {$defs{$symbol} = 1;}

$current = $process = $prev = 1;

while(<>) 
{
  $ver = "";
  if (/^#ifdef/) {
    ($junk, $ver) = split;
    push @procstack, $process;
    $prev = $process;
    $current = exists $defs{$ver};
    $process = $current & $process;
    next;
  } 
  if (/^#ifndef/) {
    ($junk, $ver) = split;
    push @procstack, $process;
    $prev = $process;
    $current = !(exists $defs{$ver});
    $process = $current & $process;
    next;
  } 
  if (/^#else/) {
    $current = !$current;
    $process = $current & $prev;
    next;
  } 
  if (/^#endif/) {
    $ver = "";
    $process = pop @procstack;
    next;
  }

## EJP: I removed s/@/\$/go; from the first (ne "SP5") section
# standard manipulations
  if ($opt_ver ne "SP5") {
    s/oldClass/class/go;
    s/oldUnclass/unclass/go;
    s/logb\(/log\(/go;
  }
  if ($opt_ver eq "R") {
    s/single\(/double\(/go;
    s/"single"/"double"/go;
    if(!/^#/ && !/\"/) { # leave comments and strings alone
       s/([=|,]\s*)T([^A-Za-z0-9\.\"])/$1TRUE$2/go;
       s/([=|,]\s*)F([^A-Za-z0-9\.\"])/$1FALSE$2/go;
       s/<- F$/<- FALSE/o;
       s/<- T$/<- TRUE/o;
       s/<- F;/<- FALSE;/o;
       s/<- T;/<- TRUE;/o;
       s/<- F}/<- FALSE}/o;
       s/<- T}/<- TRUE}/o;
       s/\(F,/(FALSE,/o;
       s/\(F\)/(FALSE)/o;
       s/\(T,/(TRUE,/o;
       s/\(T\)/(TRUE)/o;
     }
    if(!/^#/) {
       s/,\s*f\s*=\s*1\)/)/go;
       s/,\s*frame\s*=\s*1\)/)/go;
       s/sys.parent\(\)/sys.frame(sys.parent())/go;
       s/dimnames\((\w+)\)\[\[2\]\]/colnames($1)/go;
       s/dimnames\((\w+)\)\[\[1\]\]/rownames($1)/go;
#       s/sort.list/order/go;
     }
  }

  if ($process) {
    print $_;
  }
}
