#/usr/bin/perl

use strict;
use warnings;

my $h=`rscript --vanilla  h.r`; 
$h=~s/\\//; 
$h=~s/\[1\]//;
$h=~s/\"//g;  

open CITES, "<tempcites.txt";
my %cites=();
while(<CITES>){
	chomp;
	$_=~m/(\S+)/;
	my $id=$1; my $cites=$2;
	$cites{$id}=$cites;
}
close CITES;

open FILE, "<markdown/pubs_temp.md"; 
open PUBS, ">markdown/pubs.md"; 
while(<FILE>){  
	if( $_=~m/CITES/ ){
		$_=~m/CITES:(\S+)/;
		my $tempid=$1; 
		print "$tempid\n";
		my $citecount=0;
		foreach(keys(%cites)){ $citecount=$cites{$tempid}; print "$_\t$citecount\n";}
		
		$_=~s/CITES:\S+/[[$citecount citations](http:\/\/scholar.google.com\/scholar?oi=bibs&hl=en&cites=$tempid )]/;
	}
		
		print PUBS $_;    
}
close PUBS;
close FILE;
