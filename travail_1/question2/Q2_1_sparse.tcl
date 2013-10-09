<<<<<<< HEAD
source /usr/local/ns-allinone-2.35/ns-2.35/tcl/mcast/ST.tcl

=======
>>>>>>> f8ce873c19c4522778e9c6f643881168f571cf42
################################################################################
# File: Q2_1.tcl                                                               #
################################################################################

<<<<<<< HEAD

# Create a new simulator object
set ns [new Simulator -multicast on]

=======
# Create a new simulator object
set ns [new Simulator -multicast on]

# Ask NS to model the routing with a distance vector algorithm
$ns rtproto DV

>>>>>>> f8ce873c19c4522778e9c6f643881168f571cf42
# Create an output and a nam trace datafile
set tracefile [open Q2_1.tr w]
$ns trace-all $tracefile
set namfile [open Q2_1.nam w]
$ns namtrace-all $namfile

<<<<<<< HEAD
=======
# Create a multicast group.
set grp [Node allocaddr]

$ns mrtproto DM

>>>>>>> f8ce873c19c4522778e9c6f643881168f571cf42
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

# Create the links going in both directions
$ns duplex-link $n0 $n1 3.5Mb 10ms DropTail
$ns duplex-link $n1 $n2 3.5Mb 10ms DropTail
$ns duplex-link $n1 $n7 3.5Mb 10ms DropTail
$ns duplex-link $n2 $n3 3.5Mb 10ms DropTail
$ns duplex-link $n2 $n5 3.5Mb 10ms DropTail
$ns duplex-link $n2 $n8 3.5Mb 10ms DropTail
$ns duplex-link $n3 $n4 3.5Mb 10ms DropTail
$ns duplex-link $n3 $n9 3.5Mb 10ms DropTail
$ns duplex-link $n4 $n5 3.5Mb 10ms DropTail
$ns duplex-link $n4 $n10 3.5Mb 10ms DropTail
$ns duplex-link $n5 $n6 3.5Mb 10ms DropTail
$ns duplex-link $n5 $n7 3.5Mb 35ms DropTail
$ns duplex-link $n6 $n7 3.5Mb 20.5ms DropTail
$ns duplex-link $n6 $n11 3.5Mb 10ms DropTail
$ns duplex-link $n6 $n12 5Mb 10ms DropTail
$ns duplex-link $n7 $n1 3.5Mb 10ms DropTail

<<<<<<< HEAD
# Allocate group addresses
set group1 [Node allocaddr]

=======
>>>>>>> f8ce873c19c4522778e9c6f643881168f571cf42
# Change the Queue Size of links
$ns queue-limit $n7 $n5 35
$ns queue-limit $n6 $n5 5

# Create an UDP transport agent 
<<<<<<< HEAD
set udp0 [new Agent/UDP]
$ns attach-agent $n0 $udp0       ;# Attach agent udp0 to node n0
$udp0 set fid_ 0
$udp0 set class_ 0
$udp0 set dst_addr_ $group1
$udp0 set dst_port_ 0

# Create an UDP transport agent 
set udp9 [new Agent/UDP]
$ns attach-agent $n9 $udp9       ;# Attach agent udp9 to node n9
$udp9 set fid_ 0
$udp9 set class_ 0
$udp9 set dst_addr_ $group1
$udp9 set dst_port_ 0

set rcvr0 [new Agent/Null]
$ns attach-agent $n0 $rcvr0

set rcvr9 [new Agent/Null]
$ns attach-agent $n9 $rcvr9

set rcvr5 [new Agent/Null]
$ns attach-agent $n5 $rcvr5

# Create a constant bitrate traffic generator
set cbr0 [new Application/Traffic/CBR]
$cbr0 attach-agent $udp0          ;# Attach the traffic generator to the UDP agent
$cbr0 set packetSize_ 512      ;# Set the size of generated packets (will be the size of UDP packets)
$cbr0 set rate_ 1.5Mb

# Create a constant bitrate traffic generator
set cbr9 [new Application/Traffic/CBR]
$cbr9 attach-agent $udp9          ;# Attach the traffic generator to the UDP agent
$cbr9 set packetSize_ 512      ;# Set the size of generated packets (will be the size of UDP packets)
$cbr9 set rate_ 1.5Mb

# Create receivers and leavers
set rcvr8 [new Agent/Null]
$ns attach-agent $n8 $rcvr8
$ns at 0.2 "$n8 join-group $rcvr8 $group1"
set rcvr10 [new Agent/Null]
$ns attach-agent $n10 $rcvr10
$ns at 0.4 "$n10 join-group $rcvr10 $group1"
$ns at 2.0 "$n8 leave-group $rcvr8 $group1"
set rcvr11 [new Agent/Null]
$ns attach-agent $n11 $rcvr11
$ns at 2.2 "$n11 join-group $rcvr11 $group1"
set rcvr8 [new Agent/Null]
$ns attach-agent $n8 $rcvr8
$ns at 2.2 "$n8 join-group $rcvr8 $group1"
$ns at 4.0 "$n10 leave-group $rcvr10 $group1"
$ns at 4.5 "$n11 leave-group $rcvr11 $group1"
$ns at 4.6 "$n8 leave-group $rcvr8 $group1"

# Start the traffic generators at different times
$ns at 0.0 "$cbr0 start"
$ns at 0.0 "$cbr9 start"
=======
set udp [new Agent/UDP]
$ns attach-agent $n4 $udp       ;# Attach agent udp0 to node n4
$udp set class_ 0

# Create a constant bitrate traffic generator
set cbr [new Application/Traffic/CBR]
$cbr attach-agent $udp          ;# Attach the traffic generator to the UDP agent
$cbr set packetSize_ 1000       ;# Set the size of generated packets (will be the size of UDP packets)
$cbr set rate_ 1.5Mb

# Creates sinks for UDP agent
set null [new Agent/Null]
$ns attach-agent $n2 $null

# Connects UDP sources to sinks
$ns connect $udp $null

# Create a TCP transport agent (TCP Tahoe)
set tcp [new Agent/TCP]
$tcp set class_ 2               ;# Define the class, will use color 2 (green)
$tcp set window_ 64             ;# max bound on window size (simulate the receiver's window)
$tcp set packetSize_ 960        ;# packet size used by sender
$ns attach-agent $n3 $tcp

# Creates sink for TCP
set sink [new Agent/TCPSink]
$ns attach-agent $n1 $sink
$ns connect $tcp $sink

# Create a FTP traffic generator (simulates bulk data transfers)
set ftp [new Application/FTP]
$ftp attach-agent $tcp

# Start the traffic generators at different times
$ns at 0.5 "$cbr start"
$ns at 0.5 "$ftp start"
>>>>>>> f8ce873c19c4522778e9c6f643881168f571cf42

# End simulation at time 5 and call the finish procedure
$ns at 5.0 "finish"

# Define a finish procedure to flush the written files
# and automatically start nam (not mandatory !)
proc finish {} {
    global ns tracefile namfile
    $ns flush-trace
    close $tracefile
    close $namfile

    puts "running nam ..."
    exec nam Q2_1.nam &
    exit 0
}

################################################################################
#                                   NAM                                        #
################################################################################

# Define colors to be used by nam
$ns color 0 blue
$ns color 1 red
$ns color 2 green

# Create layout for nam 
$ns duplex-link-op $n0 $n1 orient right
$ns duplex-link-op $n1 $n2 orient right-up
$ns duplex-link-op $n1 $n7 orient down
$ns duplex-link-op $n2 $n3 orient right
$ns duplex-link-op $n2 $n5 orient right-down
$ns duplex-link-op $n2 $n8 orient up
$ns duplex-link-op $n3 $n4 orient right-down
$ns duplex-link-op $n3 $n9 orient up
$ns duplex-link-op $n4 $n5 orient left
$ns duplex-link-op $n4 $n10 orient down
$ns duplex-link-op $n5 $n6 orient down
$ns duplex-link-op $n5 $n7 orient left-down
$ns duplex-link-op $n6 $n7 orient left
$ns duplex-link-op $n6 $n11 orient down
$ns duplex-link-op $n6 $n12 orient left-down

<<<<<<< HEAD
ST set RP_($group1) $n5
$ns mrtproto ST
=======
>>>>>>> f8ce873c19c4522778e9c6f643881168f571cf42
# Start the simulation
$ns run
