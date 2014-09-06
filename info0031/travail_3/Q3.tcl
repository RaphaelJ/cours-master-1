################################################################################
# File: Q3.tcl                                                           #
################################################################################

# Create a new simulator object
set ns [new Simulator]

# Create an output and a nam trace datafile
set tracefile [open Q3.tr w]
$ns trace-all $tracefile
set namfile [open Q3.nam w]
$ns namtrace-all $namfile

# Create all the nodes
set n0 [$ns node]
set n1 [$ns node]
set n2 [$ns node]
set n3 [$ns node]
set n4 [$ns node]
set n5 [$ns node]
set n6 [$ns node]
set n7 [$ns node]
set n8 [$ns node]
set n9 [$ns node]
set n10 [$ns node]

# Create the links going in both directions
$ns duplex-link $n0 $n2 20Mb 30ms DropTail
$ns duplex-link $n1 $n2 8Mb 35ms DropTail
$ns duplex-link $n2 $n3 8Mb 15ms DropTail
$ns duplex-link $n3 $n4 8Mb 15ms DropTail
$ns duplex-link $n3 $n5 8Mb 15ms DropTail
$ns duplex-link $n3 $n6 8Mb 15ms DropTail
$ns duplex-link $n6 $n7 8Mb 15ms DropTail
$ns duplex-link $n6 $n8 8Mb 15ms DropTail
$ns duplex-link $n8 $n9 8Mb 15ms DropTail
$ns duplex-link $n8 $n10 8Mb 15ms DropTail

# Create UDP flows

# udp : n5 -> n7
set udp [new Agent/UDP]
$udp set class_ 0
$ns attach-agent $n5 $udp

set cbr0 [new Application/Traffic/CBR]
$cbr0 attach-agent $udp
$cbr0 set packetSize_ 512
$cbr0 set rate_ 1.5Mb

set null0 [new Agent/Null]
$ns attach-agent $n7 $null0

$ns connect $udp $null0

# video : n1 -> n10
set video [new Agent/UDP]
$video set class_ 1
$ns attach-agent $n1 $video

set cbr1 [new Application/Traffic/Exponential]
$cbr1 attach-agent $video
$cbr1 set packetSize_ 1000
$cbr1 set burst_time_ 30ms
$cbr1 set idle_time_ 50ms
$cbr1 set rate_ 3.5Mb

set null1 [new Agent/Null]
$ns attach-agent $n10 $null1

$ns connect $video $null1
# Create a TCP flows

# tcp1 : n7 -> n10
set tcp1 [new Agent/TCP]
$tcp1 set class_ 2
$tcp1 set window_ 64
$tcp1 set packetSize_ 1000
$ns attach-agent $n7 $tcp1

set ftp1 [new Application/FTP]
$ftp1 attach-agent $tcp1

set sink1 [new Agent/TCPSink]
$ns attach-agent $n10 $sink1
$ns connect $tcp1 $sink1

# tcp2 : n4 -> n9
set tcp2 [new Agent/TCP]
$tcp2 set class_ 3
$tcp2 set window_ 64
$tcp2 set packetSize_ 1000
$ns attach-agent $n4 $tcp2

set ftp2 [new Application/FTP]
$ftp2 attach-agent $tcp2

set sink2 [new Agent/TCPSink]
$ns attach-agent $n9 $sink2
$ns connect $tcp2 $sink2

# Add 2 token bucket

# Add the third token bucket.
set tb1 [new TBF]
$tb1 set bucket_ 64000
$tb1 set rate_ 1750000
$tb1 set qlen_ 50
$video attach-tbf $tb1

# Add the fifth token bucket.
set tb2 [new TBF]
$tb2 set bucket_ 8000
$tb2 set rate_ 2400000
$tb2 set qlen_ 50
$video attach-tbf $tb2

# Start the traffic generators at different times
$ns at 0 "$cbr0 start"
$ns at 0 "$cbr1 start"
$ns at 0 "$ftp1 start"
$ns at 0 "$ftp2 start"

# End simulation at time 10 and call the finish procedure
$ns at 10 "finish"

# Define a finish procedure to flush the written files
# and automatically start nam (not mandatory !)
proc finish {} {
    global ns tracefile namfile
    $ns flush-trace
    close $tracefile
    close $namfile

    puts "running nam ..."
    exec nam Q3.nam &
    exit 0
}
################################################################################
#                                   NAM                                        #
################################################################################

# Define colors to be used by nam
$ns color 0 blue
$ns color 1 red
$ns color 2 green
$ns color 2 yellow


# Create layout for nam 
$ns duplex-link-op $n8 $n9 orient left-down
$ns duplex-link-op $n8 $n10 orient right-down
$ns duplex-link-op $n6 $n8 orient left-down
$ns duplex-link-op $n7 $n6 orient right-down
$ns duplex-link-op $n6 $n3 orient right
$ns duplex-link-op $n5 $n3 orient right-down
$ns duplex-link-op $n4 $n3 orient left-down
$ns duplex-link-op $n3 $n2 orient down
$ns duplex-link-op $n2 $n1 orient right-up
$ns duplex-link-op $n2 $n0 orient right-down



# Start the simulation
$ns run
