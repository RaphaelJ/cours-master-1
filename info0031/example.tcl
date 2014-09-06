################################################################################
# File: simple.tcl                                                             #
################################################################################

# Create a new simulator object
set ns [new Simulator]

# Create an output and a nam trace datafile
set tracefile [open simple.tr w]
$ns trace-all $tracefile
set namfile [open simple.nam w]
$ns namtrace-all $namfile

# Create all the nodes
set n0 [$ns node]
set n1 [$ns node]
set n2 [$ns node]
set n3 [$ns node]


# Create the links going in both directions
# Syntax: duplex-link <node0> <node1> <bandwidth> <delay> <queue_type>
#    examples of <queue_type>: DropTail, DRR, RED, CBQ/WRR
$ns duplex-link $n0 $n2 5Mb 2ms DropTail
$ns duplex-link $n1 $n2 5Mb 2ms DropTail
$ns duplex-link $n2 $n3 1.5Mb 10ms DropTail

# Change the Queue Size of link n2-n3 to 10
$ns queue-limit $n2 $n3 10

# Create an UDP transport agent 
set udp0 [new Agent/UDP]
$ns attach-agent $n0 $udp0            ;# Attach agent udp0 to node n0
$udp0 set fid_ 0		              ;# Define the flow id, will use color 0 (blue)

# Create a constant bitrate traffic generator
set cbr0 [new Application/Traffic/CBR]
$cbr0 attach-agent $udp0	          ;# Attach the traffic generator to the UDP agent
$cbr0 set packetSize_ 500             ;# Set the size of generated packets (will be the size of UDP packets)
$cbr0 set rate_ 800Kb		          ;# Set the source bitrate

# Create a second UDP Agent and attach a second CBR agent to it
set udp1 [new Agent/UDP]
$ns attach-agent $n3 $udp1
$udp1 set fid_ 1
set cbr1 [new Application/Traffic/CBR]
$cbr1 attach-agent $udp1

# Creates sinks for both UDP agents
set null0 [new Agent/Null]
$ns attach-agent $n3 $null0
set null1 [new Agent/Null]
$ns attach-agent $n1 $null1

# Connects UDP sources to sinks
$ns connect $udp0 $null0
$ns connect $udp1 $null1


# Create a TCP transport agent (TCP Tahoe)
# To create other TCP versions, use Agent/TCP/Newreno for instance
set tcp [new Agent/TCP]
$tcp set class_ 2		     ;# Define the class, will use color 2 (green)
$tcp set window_ 50          ;# max bound on window size (simulate the receiver's window)
$tcp set packetSize_ 1000    ;# packet size used by sender
$ns attach-agent $n0 $tcp

# Creates sink for TCP
set sink [new Agent/TCPSink]
$ns attach-agent $n3 $sink
$ns connect $tcp $sink

# Create a FTP traffic generator (simulates bulk data transfers)
set ftp [new Application/FTP]
$ftp attach-agent $tcp

# Start the traffic generators at different times
$ns at 1.0 "$cbr0 start"
$ns at 1.1 "$cbr1 start"
$ns at 1.2 "$ftp start"

# End simulation at time 3 and call the finish procedure
$ns at 3.0 "finish"

# Display these two parameters on the terminal
puts [$cbr0 set packetSize_]
puts [$cbr0 set interval_]

# Define a finish procedure to flush the written files
# and automatically start nam (not mandatory !)
proc finish {} {
    global ns tracefile namfile
    $ns flush-trace
    close $tracefile
    close $namfile
    
    puts "running nam ..."
    exec nam simple.nam &
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
$ns duplex-link-op $n0 $n2 orient right-down
$ns duplex-link-op $n1 $n2 orient right-up
$ns duplex-link-op $n2 $n3 orient right


# Show the queues between these nodes in nam
$ns duplex-link-op $n2 $n3 queuePos 0.5

# Start the simulation
$ns run
