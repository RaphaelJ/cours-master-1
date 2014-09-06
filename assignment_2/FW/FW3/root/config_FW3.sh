echo 1 > /proc/sys/net/ipv4/ip_forward

## Firewall

iptables -P FORWARD DROP

# Accepts already opened connections.
iptables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT

# Lets R1 and the SSH relay access the processor by SSH
iptables -A FORWARD -i eth1 -p tcp -s 192.168.3.10 -o eth0 -d 192.168.4.10 --dport 22 -j ACCEPT
iptables -A FORWARD -i eth1 -p tcp -s 192.168.1.10 -o eth0 -d 192.168.4.10 --dport 22 -j ACCEPT

# Lets processor access the FTP.
iptables -A FORWARD -i eth0 -p tcp -s 192.168.4.10 -o eth1 -d 192.168.1.11 --dport 20 -j ACCEPT
iptables -A FORWARD -i eth0 -p tcp -s 192.168.4.10 -o eth1 -d 192.168.1.11 --dport 21 -j ACCEPT
