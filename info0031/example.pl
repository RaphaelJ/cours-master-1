#!/usr/bin/perl
##
## file: 'simple.pl'
## usage:  'perl simple.pl <trace>'
##
## Computes the percentage of packets from flow 2 (TCP) received by n3.
##
use strict;

# Open the trace file.
my $tracefile = $ARGV[0];
open TRACE, $ARGV[0]
    or die "Cannot open input trace file.\nUsage: perl analyse-trace-wrr.pl <trace>\n";

my $nbPacketsFlow2 = 0;
my $nbTotalPackets = 0;

while (<TRACE>) 
{
    chomp;
    if (/^([+-rd])\s([\d\.]+)\s(\d+)\s(\d+)\s(\w+)\s(\d+)\s([\w\-]+)\s(\d+)\s(\d+)\.(\d+)\s(\d+)\.(\d+)\s(\d+)\s(\d+).*$/) {
	my $status = $1;         # either 'r' (packet received), 'd' (packet dropped), '+' (entering queue), '-' (exiting queue)
	my $time = $2;		     # time when the event occured
	my $srclinknodeid = $3;  
	my $dstlinknodeid = $4;
	my $packettype = $5;
	my $packetSize = $6;
	my $flags = $7; 
	my $flowid = $8;         # fid_ or class_
	my $srcnodeid = $9;
	my $srcportid = $10;
	my $dstnodeid = $11;
	my $dstportid = $12;
	my $seqNum = $13;        # packet sequence number, as in TCP header (duplicate packets have same seqNum)
	my $packetId = $14;	     # unique packet identifier for the simulation (duplicate packets have distinct packetId)

	if (($status eq "r") && ($dstlinknodeid == 3)) 
	{
	    if ($flowid == 2) 
	    {
			$nbPacketsFlow2++;
	    } 
	    $nbTotalPackets++;
	}
	
    }
}

close TRACE;

printf "Total number of packets received by n3 = %d\nNumber of packets from flow 2 (TCP) received by n3 = %d\nPercentage of packets received by n3 beeing part of flow 2 = %.2f\n", $nbTotalPackets, $nbPacketsFlow2, $nbPacketsFlow2 / $nbTotalPackets;
