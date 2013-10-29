################################################################################
# File: project2.tcl                                                           #
################################################################################

# Create a new simulator object
set ns [new Simulator]

# Create an output and a nam trace datafile
set tracefile [open project2.tr w]
$ns trace-all $tracefile
set namfile [open project2.nam w]
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
set n11 [$ns node]
set n12 [$ns node]
set n13 [$ns node]

# Create the links going in both dirtions
$ns duplex-link $n0 $n1 5Mb 10ms DropTail
$ns duplex-link $n0 $n2 5Mb 10ms CBQ/WRR
$ns duplex-link $n1 $n3 5Mb 10ms CBQ/WRR
$ns duplex-link $n2 $n4 5Mb 10ms DropTail
$ns duplex-link $n3 $n5 5Mb 10ms DropTail
$ns duplex-link $n4 $n6 5Mb 10ms DropTail

$ns duplex-link $n0 $n9 20Mb 16ms DropTail
$ns duplex-link $n0 $n10 20Mb 16ms DropTail
$ns duplex-link $n1 $n11 20Mb 16ms DropTail
$ns duplex-link $n1 $n12 20Mb 16ms DropTail
$ns duplex-link $n2 $n8 20Mb 16ms DropTail
$ns duplex-link $n3 $n13 20Mb 16ms DropTail
$ns duplex-link $n4 $n7 20Mb 16ms DropTail

# Create UDP flows

# udp0 : n5 -> n10
set udp0 [new Agent/UDP]
$udp0 set class_ 0
$ns attach-agent $n5 $udp0

set cbr0 [new Application/Traffic/CBR]
$cbr0 attach-agent $udp0
$cbr0 set packetSize_ 512
$cbr0 set rate_ 2Mb

set null0 [new Agent/Null]
$ns attach-agent $n10 $null0

$ns connect $udp0 $null0

# udp1 : n8 -> n11
set udp1 [new Agent/UDP]
$udp1 set class_ 1
$ns attach-agent $n5 $udp1

set cbr1 [new Application/Traffic/CBR]
$cbr1 attach-agent $udp1
$cbr1 set packetSize_ 512
$cbr1 set rate_ 0.5Mb

set null1 [new Agent/Null]
$ns attach-agent $n11 $null1

$ns connect $udp1 $null1

# udp2 : n6 -> n5
set udp2 [new Agent/UDP]
$udp2 set class_ 2
$ns attach-agent $n6 $udp2

set cbr2 [new Application/Traffic/CBR]
$cbr2 attach-agent $udp2
$cbr2 set packetSize_ 512
$cbr2 set rate_ 1.5Mb

set null2 [new Agent/Null]
$ns attach-agent $n5 $null2

$ns connect $udp2 $null2

# udp3 : n13 -> n12
set udp3 [new Agent/UDP]
$udp3 set class_ 3
$ns attach-agent $n13 $udp3

set cbr3 [new Application/Traffic/CBR]
$cbr3 attach-agent $udp3
$cbr3 set packetSize_ 512
$cbr3 set rate_ 0.5Mb

set null3 [new Agent/Null]
$ns attach-agent $n12 $null3

$ns connect $udp3 $null3

# udp4 : n5 -> n12
set udp4 [new Agent/UDP]
$udp4 set class_ 4
$ns attach-agent $n5 $udp4

set cbr4 [new Application/Traffic/CBR]
$cbr4 attach-agent $udp4
$cbr4 set packetSize_ 512
$cbr4 set rate_ 1.5Mb

set null4 [new Agent/Null]
$ns attach-agent $n12 $null4

$ns connect $udp4 $null4

# Create a TCP flows

# tcp0 : n13 -> n8
set tcp0 [new Agent/TCP]
$udp0 set class_ 5
$tcp0 set window_ 32
$tcp0 set packetSize_ 960
$ns attach-agent $n13 $tcp0


set ftp0 [new Application/FTP]
$ftp0 attach-agent $tcp0

set sink0 [new Agent/TCPSink]
$ns attach-agent $n8 $sink0
$ns connect $tcp0 $sink0

# tcp1 : n11 -> n7
set tcp1 [new Agent/TCP]
$tcp1 set class_ 6
$tcp1 set window_ 32
$tcp1 set packetSize_ 960
$ns attach-agent $n11 $tcp1

set ftp1 [new Application/FTP]
$ftp1 attach-agent $tcp1

set sink1 [new Agent/TCPSink]
$ns attach-agent $n7 $sink1
$ns connect $tcp1 $sink1

# tcp2 : n9 -> n8
set tcp2 [new Agent/TCP]
$tcp2 set class_ 7
$tcp2 set window_ 32
$tcp2 set packetSize_ 960
$ns attach-agent $n9 $tcp2

set ftp2 [new Application/FTP]
$ftp2 attach-agent $tcp2

set sink2 [new Agent/TCPSink]
$ns attach-agent $n8 $sink2
$ns connect $tcp2 $sink2

#
# Creating the different classes of traffic
#

# n3 -> n1 (30% tcp0, 10% udp3, 30% udp0, 30% udp4)
set n3n1q0 [new Queue/DropTail]
set n3n1q1 [new Queue/DropTail]
set n3n1q2 [new Queue/DropTail]
set n3n1q3 [new Queue/DropTail]

set n3n1class0 [new CBQClass]
$n3n1class0 install-queue $n3n1q0
$n3n1class0 setparams none true 0.30 auto 1 1 0 

set n3n1class1 [new CBQClass]
$n3n1class1 install-queue $n3n1q1
$n3n1class1 setparams none true 0.10 auto 1 1 0 

set n3n1class2 [new CBQClass]
$n3n1class2 install-queue $n3n1q2
$n3n1class2 setparams none true 0.30 auto 1 1 0 

set n3n1class3 [new CBQClass]
$n3n1class3 install-queue $n3n1q3
$n3n1class3 setparams none true 0.30 auto 1 1 0 

set n3n1cbqlink [$ns link $n3 $n1]
$n3n1cbqlink insert $n3n1class0
$n3n1cbqlink insert $n3n1class1
$n3n1cbqlink insert $n3n1class2
$n3n1cbqlink insert $n3n1class3

$tcp0 set fid_ 0
$n3n1cbqlink bind $n3n1class0 0

$udp3 set fid_ 1
$n3n1cbqlink bind $n3n1class1 1

$udp0 set fid_ 2
$n3n1cbqlink bind $n3n1class2 2

$udp4 set fid_ 3
$n3n1cbqlink bind $n3n1class3 3

# n1 -> n3 (100% udp2 & acks tcp0)
set n1n3q0 [new Queue/DropTail]

set n1n3class0 [new CBQClass]
$n1n3class0 install-queue $n1n3q0
$n1n3class0 setparams none true 1 auto 1 1 0 

set n1n3cbqlink [$ns link $n1 $n3]
$n1n3cbqlink insert $n1n3class0

$n1n3cbqlink bind $n1n3class0 0 # tcp0

$udp2 set fid_ 4
$n1n3cbqlink bind $n1n3class0 4

# n0 -> n2 (30% tcp0, 35% tcp1, 35% tcp2)

set n0n2q0 [new Queue/DropTail]
set n0n2q1 [new Queue/DropTail]
set n0n2q2 [new Queue/DropTail]

set n0n2class0 [new CBQClass]
$n0n2class0 install-queue $n0n2q0
$n0n2class0 setparams none true 0.3 auto 1 1 0 

set n0n2class1 [new CBQClass]
$n0n2class1 install-queue $n0n2q1
$n0n2class1 setparams none true 0.35 auto 1 1 0 

set n0n2class2 [new CBQClass]
$n0n2class2 install-queue $n0n2q2
$n0n2class2 setparams none true 0.35 auto 1 1 0 

set n0n2cbqlink [$ns link $n0 $n2]
$n0n2cbqlink insert $n0n2class0
$n0n2cbqlink insert $n0n2class1
$n0n2cbqlink insert $n0n2class2

$n0n2cbqlink bind $n0n2class0 0 # tcp0

$tcp1 set fid_ 5
$n0n2cbqlink bind $n0n2class1 5

$tcp2 set fid_ 6
$n0n2cbqlink bind $n0n2class2 6

# n2 -> n0 (acks { tcp0, tcp1, tcp2 }, udp1, udp2)

set n2n0q0 [new Queue/DropTail]

set n2n0class0 [new CBQClass]
$n2n0class0 install-queue $n2n0q0
$n2n0class0 setparams none true 1 auto 1 1 0 

set n2n0cbqlink [$ns link $n0 $n2]
$n2n0cbqlink insert $n2n0class0

$n2n0cbqlink bind $n2n0class0 0 # tcp0
$n2n0cbqlink bind $n2n0class0 5 # tcp1
$n2n0cbqlink bind $n2n0class0 6 # tcp2

$udp1 set fid_ 7
$n2n0cbqlink bind $n2n0class0 7

$udp2 set fid_ 8
$n2n0cbqlink bind $n2n0class0 8

#
# Start the traffic generators at different times
#

$ns at 0 "$cbr0 start"
$ns at 0 "$cbr1 start"
$ns at 0 "$cbr2 start"
$ns at 0 "$cbr3 start"
$ns at 0 "$cbr4 start"
$ns at 0 "$ftp0 start"
$ns at 0 "$ftp1 start"
$ns at 0 "$ftp2 start"

# End simulation at time 10 and call the finish procedure
$ns at 2 "finish"

# Define a finish procedure to flush the written files
# and automatically start nam (not mandatory !)
proc finish {} {
    global ns tracefile namfile
    $ns flush-trace
    close $tracefile
    close $namfile

    puts "running nam ..."
    exec nam project2.nam &
    exit 0
}
################################################################################
#                                   NAM                                        #
################################################################################

# Define colors to be used by nam
$ns color 0 pink
$ns color 1 red
$ns color 2 purple
$ns color 3 blue
$ns color 4 green
$ns color 5 orange
$ns color 6 orange
$ns color 7 orange

# Create layout for nam 
$ns duplex-link-op $n4 $n6 orient right
$ns duplex-link-op $n2 $n4 orient right
$ns duplex-link-op $n0 $n2 orient right-down
$ns duplex-link-op $n0 $n1 orient right-up
$ns duplex-link-op $n1 $n3 orient right
$ns duplex-link-op $n3 $n5 orient right-down
$ns duplex-link-op $n4 $n7 orient down
$ns duplex-link-op $n2 $n8 orient down
$ns duplex-link-op $n0 $n9 orient left-down
$ns duplex-link-op $n0 $n10 orient left-up
$ns duplex-link-op $n1 $n11 orient left-up
$ns duplex-link-op $n1 $n12 orient right-up
$ns duplex-link-op $n3 $n13 orient right

# Start the simulation
$ns run
