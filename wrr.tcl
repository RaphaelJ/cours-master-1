################################################################################
# File: wrr.tcl                                                                #
# Description: Testing the CBQ/WRR queue                                       #
################################################################################

# Create a new simulator object
set ns [new Simulator]

# Create an output and a nam trace datafile
set tracefile [open wrr.tr w]
$ns trace-all $tracefile
set namfile [open wrr.nam w]
$ns namtrace-all $namfile

# Create all the nodes
set n0 [$ns node]
set n1 [$ns node]
set n2 [$ns node]
set n3 [$ns node]

# Create the links going in both directions
# Syntax: duplex-link <node0> <node1> <bandwidth> <delay> <queue_type>
#    examples of <queue_type>: DropTail, DRR, RED, CBQ/WRR
$ns duplex-link $n0 $n2 1Mb 10ms DropTail
$ns duplex-link $n1 $n2 1Mb 10ms DropTail
$ns duplex-link $n2 $n3 1Mb 10ms CBQ/WRR

# Define colors to be used by nam
$ns color 0 blue
$ns color 1 red

# Create layout for nam 
$ns duplex-link-op $n0 $n2 orient right-down
$ns duplex-link-op $n1 $n2 orient right-up
$ns duplex-link-op $n2 $n3 orient right

# Show the queues between these nodes in nam
$ns duplex-link-op $n2 $n3 queuePos 0.5

# Create an UDP transport agent 
set udp0 [new Agent/UDP]
$ns attach-agent $n0 $udp0        ;# Attach agent udp0 to node n0
$udp0 set fid_ 0		          ;# Define the flow id, will use color 0 (blue)

# Create a constant bitrate traffic generator
set cbr0 [new Application/Traffic/CBR]
$cbr0 attach-agent $udp0	          ;# Attach the traffic generator to the UDP agent
$cbr0 set packetSize_ 500             ;# Set the size of generated packets (will be the size of UDP packets
$cbr0 set rate_ 800Kb		          ;# Set the source bitrate

# Create a second UDP Agent and attach a second CBR agent to it
set udp1 [new Agent/UDP]
$ns attach-agent $n1 $udp1
$udp1 set fid_ 1
set cbr1 [new Application/Traffic/CBR]
$cbr1 attach-agent $udp1
$cbr1 set packetSize_ 500
$cbr1 set rate_ 800Kb

# Creates sinks for both UDP agents
set null0 [new Agent/Null]
$ns attach-agent $n3 $null0
set null1 [new Agent/Null]
$ns attach-agent $n3 $null1

# Connects UDP sources to sinks
$ns connect $udp0 $null0
$ns connect $udp1 $null1

#
# Creating the different classes of traffic
#


# Start the traffic generators at time 0
$ns at 0 "$cbr0 start"
$ns at 0 "$cbr1 start"

# End simulation at time 3 and call the finish procedure
$ns at 3 "finish"

# Define a finish procedure to flush the written files
# and automatically start nam (not mandatory !)
proc finish {} {
    global ns tracefile namfile
    $ns flush-trace
    close $tracefile
    close $namfile
    
    puts "running nam ..."
    exec nam wrr.nam &
    exit 0
    
}

# Start the simulation
$ns run

