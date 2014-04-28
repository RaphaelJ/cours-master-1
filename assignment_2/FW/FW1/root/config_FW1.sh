echo 1 > /proc/sys/net/ipv4/ip_forward

## NAT

# Enable NAT outgoing traffic for the whole network.
iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE

# Permits the access to Web, PDNS, RSYNC (by SSH on port 2222) and SMTP/IMAP from the Internet.
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 80 -j DNAT --to 192.168.7.10:80
iptables -t nat -A PREROUTING -i eth0 -p udp --dport 53 -j DNAT --to 192.168.7.11:53
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 873 -j DNAT --to 192.168.7.12:873
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 2222 -j DNAT --to 192.168.7.12:22
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 25 -j DNAT --to 192.168.2.13:25
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 143 -j DNAT --to 192.168.2.13:143

# Permits the access to the SSH relay from the Internet.
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 22 -j DNAT --to 192.168.1.10:22

## Firewall

iptables -P FORWARD DROP

# Accepts already opened connections.
iptables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT

# Allows the NAT PREROUTING traffic
iptables -A FORWARD -i eth0 -p tcp -o eth1 -d 192.168.7.10 --dport 80 -j ACCEPT
iptables -A FORWARD -i eth0 -p udp -o eth1 -d 192.168.7.11 --dport 53 -j ACCEPT
iptables -A FORWARD -i eth0 -p tcp -o eth1 -d 192.168.7.12 --dport 873 -j ACCEPT
iptables -A FORWARD -i eth0 -p tcp -o eth1 -d 192.168.7.12 --dport 22 -j ACCEPT
iptables -A FORWARD -i eth0 -p tcp -o eth3 -d 192.168.2.13 --dport 25 -j ACCEPT
iptables -A FORWARD -i eth0 -p tcp -o eth3 -d 192.168.2.13 --dport 143 -j ACCEPT
iptables -A FORWARD -i eth0 -p tcp -o eth2 -d 192.168.1.10 --dport 22 -j ACCEPT

# Permits LDNS to query the Internet and the public DNS server over the DNS
# port.
iptables -A FORWARD -i eth3 -p udp -s 192.168.2.10 -o eth0 --dport 53 -j ACCEPT
iptables -A FORWARD -i eth3 -p udp -s 192.168.2.10 -o eth1 -d 192.168.7.11 --dport 53 -j ACCEPT

# Permits HTTP to query the Internet and the public web server over the HTTP
# port.
iptables -A FORWARD -i eth3 -p tcp -s 192.168.2.11 -o eth0 --dport 80 -j ACCEPT
iptables -A FORWARD -i eth3 -p tcp -s 192.168.2.11 -o eth1 -d 192.168.7.10 --dport 80 -j ACCEPT

# Permits HTTPS to query the Internet over the HTTPS port.
iptables -A FORWARD -i eth3  -p tcp -s 192.168.2.12 -o eth0 --dport 443 -j ACCEPT

# Permits SMTP/IMAP to query the Internet over the SMTP port.
iptables -A FORWARD -i eth3  -p tcp -s 192.168.2.13 -o eth0 --dport 25 -j ACCEPT

# Lets full access to Internet for the research department
iptables -A FORWARD -i eth4 -s 192.168.3.10 -o eth0 -j ACCEPT
iptables -A FORWARD -i eth4 -s 192.168.3.11 -o eth0 -j ACCEPT

# Lets the research department access the mail server.
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.10 -o eth3 -d 192.168.2.13 --dport 25 -j ACCEPT
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.10 -o eth3 -d 192.168.2.13 --dport 143 -j ACCEPT
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.11 -o eth3 -d 192.168.2.13 --dport 25 -j ACCEPT
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.11 -o eth3 -d 192.168.2.13 --dport 143 -j ACCEPT

# Lets the research department access the ssh relay, the ftp server.
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.10 -o eth2 -d 192.168.1.10 --dport 22 -j ACCEPT
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.10 -o eth2 -d 192.168.1.11 --dport 20 -j ACCEPT
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.10 -o eth2 -d 192.168.1.11 --dport 21 -j ACCEPT

# Lets the research department access the NFS.
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.10 -o eth2 -d 192.168.1.12 --dport 111 -j ACCEPT
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.10 -o eth2 -d 192.168.1.12 --dport 2046:2049 -j ACCEPT
iptables -A FORWARD -i eth4 -p udp -s 192.168.3.10 -o eth2 -d 192.168.1.12 --dport 111 -j ACCEPT
iptables -A FORWARD -i eth4 -p udp -s 192.168.3.10 -o eth2 -d 192.168.1.12 --dport 2046:2049 -j ACCEPT

iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.11 -o eth2 -d 192.168.1.12 --dport 111 -j ACCEPT
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.11 -o eth2 -d 192.168.1.12 --dport 2046:2049 -j ACCEPT
iptables -A FORWARD -i eth4 -p udp -s 192.168.3.11 -o eth2 -d 192.168.1.12 --dport 111 -j ACCEPT
iptables -A FORWARD -i eth4 -p udp -s 192.168.3.11 -o eth2 -d 192.168.1.12 --dport 2046:2049 -j ACCEPT

# Lets the research department access the RSYNC.
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.10 -o eth1 -d 192.168.7.12 --dport 873 -j ACCEPT
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.10 -o eth1 -d 192.168.7.12 --dport 22 -j ACCEPT
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.11 -o eth1 -d 192.168.7.12 --dport 873 -j ACCEPT
iptables -A FORWARD -i eth4 -p tcp -s 192.168.3.11 -o eth1 -d 192.168.7.12 --dport 22 -j ACCEPT
