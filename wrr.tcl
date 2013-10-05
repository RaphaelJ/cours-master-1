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

set qlim 20 ;# Size of queues associated to the WRR scheduler

# First we need to create the two queues, one for each class
set q0 [new Queue/DropTail]	
$q0 set limit_ $qlim
set q1 [new Queue/DropTail]
$q1 set limit_ $qlim

# Then we create the two classes of traffic
set classflow0 [new CBQClass]	
$classflow0 install-queue $q0                    ;# Associate queue q0 to class classflow0
$classflow0 setparams none true 0.4 auto 1 1 0   ;# 0.4 is for 40% of bandwidth (real number between 0.0 and 1.0)
set classflow1 [new CBQClass]
$classflow1 install-queue $q1			         ;# Associate queue q0 to class classflow0
$classflow1 setparams none true 0.6 auto 1 1 0	 ;# 0.6 is for 60% of bandwidth (real number between 0.0 and 1.0)

# We need to insert each classflow in the link n2-n3
set cbqlink [$ns link $n2 $n3]
$cbqlink insert $classflow0
$cbqlink insert $classflow1

# Finally we bind the flow IDs to the classflow
$cbqlink bind $classflow0 0 ;# packets with fid_ 0 are in classflow0
$cbqlink bind $classflow1 1 ;# packets with fid_ 1 are in classflow1

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

