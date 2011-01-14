#!/bin/sh

iptables -t nat -A PREROUTING -i eth0 -p tcp --dport https -j REDIRECT --to-ports 9000