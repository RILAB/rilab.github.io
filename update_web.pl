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
	$_=~m/(\d+,*\d*)\s(\d+)/;
	my $id=$1; my $cites=$2;
	my @ids=split(/,/,$id);
	$cites{$_}=$cites foreach @ids;
}
close CITES;

open FILE, "<markdown/pubs_temp.txt"; 
open PUBS, ">markdown/pubs.md"; 
while(<FILE>){  
	if( $_=~m/CITES/ ){
		$_=~m/CITES:(\d+,*\d*)/;
		my $tempid=$1; 
		my $citecount=0;
		foreach(keys(%cites)){ if( $_ ~~ $tempid){ $citecount=$cites{$tempid}; }}
		$_=~s/CITES:\d+,*\d*/[[$citecount citations](http:\/\/scholar.google.com\/scholar?oi=bibs&hl=en&cites=$tempid )]/;
	}
		
		print PUBS $_;    
}
close PUBS;
close FILE;
