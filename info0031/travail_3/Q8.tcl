################################################################################
# File: Q8.tcl                                                           #
################################################################################

# Create a new simulator object
set ns [new Simulator]

# Create an output and a nam trace datafile
set tracefile [open Q8.tr w]
$ns trace-all $tracefile
set namfile [open Q8.nam w]
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
$ns duplex-link $n3 $n6 8Mb 15ms CBQ/WRR
$ns duplex-link $n6 $n7 8Mb 15ms DropTail
$ns duplex-link $n6 $n8 8Mb 15ms CBQ/WRR
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


# tcp3 : n0 -> n9
set tcp3 [new Agent/TCP]
$tcp3 set class_ 4
$tcp3 set window_ 64
$tcp3 set packetSize_ 1000
$ns attach-agent $n0 $tcp3

set ftp3 [new Application/FTP]
$ftp3 attach-agent $tcp3

set sink3 [new Agent/TCPSink]
$ns attach-agent $n9 $sink3
$ns connect $tcp3 $sink3

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

# n3 -> n6 (udp = 12.5%, video = 20%, tcp2 = 22.5%, tcp3 = 36%)
set n3n6q0 [new Queue/DropTail]
set n3n6q1 [new Queue/DropTail]
set n3n6q2 [new Queue/DropTail]
set n3n6q3 [new Queue/DropTail]
#udp
set n3n6class0 [new CBQClass]
$n3n6class0 install-queue $n3n6q0
$n3n6class0 setparams none true 0.125 auto 1 1 0 
#video
set n3n6class1 [new CBQClass]
$n3n6class1 install-queue $n3n6q1
$n3n6class1 setparams none true 0.20 auto 1 1 0 
#tcp2
set n3n6class2 [new CBQClass]
$n3n6class2 install-queue $n3n6q2
$n3n6class2 setparams none true 0.225 auto 1 1 0 
#tcp3
set n3n6class3 [new CBQClass]
$n3n6class3 install-queue $n3n6q3
$n3n6class3 setparams none true 0.36 auto 1 1 0 

set n3n6cbqlink [$ns link $n3 $n6]
$n3n6cbqlink insert $n3n6class0
$n3n6cbqlink insert $n3n6class1
$n3n6cbqlink insert $n3n6class2
$n3n6cbqlink insert $n3n6class3

$udp set fid_ 0
$n3n6cbqlink bind $n3n6class0 0

$video set fid_ 1
$n3n6cbqlink bind $n3n6class1 1

$tcp2 set fid_ 3
$n3n6cbqlink bind $n3n6class2 3

$tcp3 set fid_ 4
$n3n6cbqlink bind $n3n6class3 4

# n6 -> n3 (ack2, ack3)
set n6n3q0 [new Queue/DropTail]

set n6n3class0 [new CBQClass]
$n6n3class0 install-queue $n6n3q0
$n6n3class0 setparams none true 1 auto 1 1 0 

set n6n3cbqlink [$ns link $n6 $n3]
$n6n3cbqlink insert $n6n3class0

$n6n3cbqlink bind $n6n3class0 3

$n6n3cbqlink bind $n6n3class0 4

# n6 -> n8 (video = 20%,  tcp1 = 18.75%, tcp2 = 22.5%, tcp3 = 36%)
set n6n8q0 [new Queue/DropTail]
set n6n8q1 [new Queue/DropTail]
set n6n8q2 [new Queue/DropTail]
set n6n8q3 [new Queue/DropTail]

set n6n8class0 [new CBQClass]
$n6n8class0 install-queue $n6n8q0
$n6n8class0 setparams none true 0.20 auto 1 1 0 

set n6n8class1 [new CBQClass]
$n6n8class1 install-queue $n6n8q1
$n6n8class1 setparams none true 0.1875 auto 1 1 0 

set n6n8class2 [new CBQClass]
$n6n8class2 install-queue $n6n8q2
$n6n8class2 setparams none true 0.225 auto 1 1 0 

set n6n8class3 [new CBQClass]
$n6n8class3 install-queue $n6n8q3
$n6n8class3 setparams none true 0.36 auto 1 1 0 

set n6n8cbqlink [$ns link $n6 $n8]
$n6n8cbqlink insert $n6n8class0
$n6n8cbqlink insert $n6n8class1
$n6n8cbqlink insert $n6n8class2
$n6n8cbqlink insert $n6n8class3
#video
$n6n8cbqlink bind $n6n8class0 1
#tcp1
$tcp1 set fid_ 2
$n6n8cbqlink bind $n6n8class1 2
#tcp2
$n6n8cbqlink bind $n6n8class2 3
#tcp3
$n6n8cbqlink bind $n6n8class3 4

# n8 -> n6 (ack3, ack2, ack1)
set n8n6q0 [new Queue/DropTail]

set n8n6class0 [new CBQClass]
$n8n6class0 install-queue $n8n6q0
$n8n6class0 setparams none true 1 auto 1 1 0 

set n8n6cbqlink [$ns link $n8 $n6]
$n8n6cbqlink insert $n8n6class0

$n8n6cbqlink bind $n8n6class0 4

$n8n6cbqlink bind $n8n6class0 3

$n8n6cbqlink bind $n8n6class0 2

# Start the traffic generators at different times
$ns at 0 "$cbr0 start"
$ns at 0 "$cbr1 start"
$ns at 0 "$ftp1 start"
$ns at 0 "$ftp2 start"
$ns at 0 "$ftp3 start"

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
    exec nam Q8.nam &
    exit 0
}
################################################################################
#                                   NAM                                        #
################################################################################

# Define colors to be used by nam
$ns color 0 blue
$ns color 1 red
$ns color 2 green
$ns color 3 yellow
$ns color 4 purple


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
