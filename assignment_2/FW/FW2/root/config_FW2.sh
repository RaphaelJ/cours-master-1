echo 1 > /proc/sys/net/ipv4/ip_forward

## Firewall

iptables -P FORWARD DROP

# Accepts already opened connections.
iptables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT

# Permits the access to the LDNS, the HTTP/HTTPS proxies and the mail server to
# U1.
iptables -A FORWARD -i eth1 -p udp -s 192.168.6.10 -o eth0 -d 192.168.5.10 --dport 53 -j ACCEPT
iptables -A FORWARD -i eth1 -p tcp -s 192.168.6.10 -o eth0 -d 192.168.5.11 --dport 3128 -j ACCEPT # The HTTP proxy is on port 3128
iptables -A FORWARD -i eth1 -p tcp -s 192.168.6.10 -o eth0 -d 192.168.5.12 --dport 3128 -j ACCEPT # The HTTPS proxy is on port 3128
iptables -A FORWARD -i eth1 -p tcp -s 192.168.6.10 -o eth0 -d 192.168.5.13 --dport 25 -j ACCEPT
iptables -A FORWARD -i eth1 -p tcp -s 192.168.6.10 -o eth0 -d 192.168.5.13 --dport 143 -j ACCEPT

# Permits the access to the LDNS, the HTTP/HTTPS proxies and the mail server to
# U2.
iptables -A FORWARD -i eth1 -p udp -s 192.168.6.11 -o eth0 -d 192.168.5.10 --dport 53 -j ACCEPT
iptables -A FORWARD -i eth1 -p tcp -s 192.168.6.11 -o eth0 -d 192.168.5.11 --dport 3128 -j ACCEPT # The HTTP proxy is on port 3128
iptables -A FORWARD -i eth1 -p tcp -s 192.168.6.11 -o eth0 -d 192.168.5.12 --dport 3128 -j ACCEPT # The HTTPS proxy is on port 3128
iptables -A FORWARD -i eth1 -p tcp -s 192.168.6.11 -o eth0 -d 192.168.5.13 --dport 25 -j ACCEPT
iptables -A FORWARD -i eth1 -p tcp -s 192.168.6.11 -o eth0 -d 192.168.5.13 --dport 143 -j ACCEPT