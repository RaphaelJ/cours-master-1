################################################################################
# File: Q2_1_dense.tcl                                                         #
################################################################################

# Create a new simulator object
set ns [new Simulator -multicast on]

# Ask NS to model the routing with a distance vector algorithm with the dense mode protocol
$ns rtproto DV

# Create an output and a nam trace datafile
set tracefile [open Q2_1_dense.tr w]
$ns trace-all $tracefile
set namfile [open Q2_1_dense.nam w]
$ns namtrace-all $namfile

set mproto DM
set mrthandle [$ns mrtproto $mproto {}]

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

# Routing protocol: say distance vector
#Protocols: CtrMcast, DM, ST, BST
set mproto DM
set mrthandle [$ns mrtproto $mproto {}]

# Allocate group addresses
set group1 [Node allocaddr]

# Change the Queue Size of links
$ns queue-limit $n7 $n5 35
$ns queue-limit $n6 $n5 5

# Create an UDP transport agent 
set udp0 [new Agent/UDP]
$ns attach-agent $n0 $udp0       ;# Attach agent udp0 to node n4
$udp0 set class_ 0
$udp0 set dst_addr_ $group1
$udp0 set dst_port_ 0

# Create an UDP transport agent 
set udp9 [new Agent/UDP]
$ns attach-agent $n9 $udp9       ;# Attach agent udp0 to node n4
$udp9 set class_ 0
$udp9 set dst_addr_ $group1
$udp9 set dst_port_ 0

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
$ns at 0.1 "$cbr0 start"
$ns at 0.1 "$cbr9 start"

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
    exec nam Q2_1_dense.nam &
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

# Start the simulation
$ns run
