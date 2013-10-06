################################################################################
# File: Q1_3.tcl                                                               #
################################################################################

# Create a new simulator object
set ns [new Simulator]

# Ask NS to model the routing with a distance vector algorithm
$ns rtproto DV

# Create an output and a nam trace datafile
set tracefile [open Q1_3.tr w]
$ns trace-all $tracefile
set namfile [open Q1_3.nam w]
$ns namtrace-all $namfile

# Create all the nodes
set n0 [$ns node]
set n1 [$ns node]
set n2 [$ns node]
set n3 [$ns node]
set n4 [$ns node]
set n5 [$ns node]

# Create the links going in both directions
$ns duplex-link $n0 $n2 5Mb 22ms DropTail
$ns duplex-link $n0 $n4 5Mb 22ms DropTail
$ns duplex-link $n1 $n5 5Mb 22ms DropTail
$ns duplex-link $n2 $n5 5Mb 22ms DropTail
$ns duplex-link $n0 $n1 6Mb 22ms DropTail
$ns duplex-link $n0 $n3 6Mb 22ms DropTail

# Change the Queue Size of link n0-n1 to 10
$ns queue-limit $n0 $n1 10

# Create an UDP transport agent 
set udp [new Agent/UDP]
$ns attach-agent $n4 $udp       ;# Attach agent udp0 to node n4
$udp set class_ 0

# Create a constant bitrate traffic generator
set cbr [new Application/Traffic/CBR]
$cbr attach-agent $udp          ;# Attach the traffic generator to the UDP agent
$cbr set packetSize_ 1000       ;# Set the size of generated packets (will be the size of UDP packets)
$cbr set rate_ 1536Kb

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

# Simulates a link down between n0 and n2 at 3.0 sec
$ns rtmodel-at 3 down $n0 $n2 

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
    exec nam Q1_3.nam &
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
$ns duplex-link-op $n4 $n0 orient right-up
$ns duplex-link-op $n3 $n0 orient left-up
$ns duplex-link-op $n0 $n1 orient left-up
$ns duplex-link-op $n0 $n2 orient right-up
$ns duplex-link-op $n1 $n5 orient right-up
$ns duplex-link-op $n2 $n5 orient left-up

# Show the queues between these nodes in nam
$ns duplex-link-op $n0 $n1 queuePos 0.5
$ns duplex-link-op $n0 $n2 queuePos 0.5

# Start the simulation
$ns run
