ip-lookup
=========

A number of solutions to the *Longest-Prefix Matching (LPM)* problem
are presented, based on the binary-tree approach, with the following
capabilities:

- partition into memory pages;
- search of the longest matching prefix;
- insertion/deletion of prefixes;
- support of IPv4 and IPv6 networks.

At the same time, the solutions provide:

- IP-lookup requires a small number of memory accesses.
- The worst-case memory usage is close to optimal.
- Incremental updates are supported.

LPM problem
-----------

Referring to network routers, forwarding is moving an incoming IP
packet to the appropriate interface. Usually, the interface is the
next closest router (*next hop*) the packet can go through. Each entry
in a forwarding table specifies a sub-network and next-hop
information, where the sub-network is given by *route prefix*. In
turn, each prefix is represented as a pair of IP address and subnet
mask; for instance, IPv4 prefix `192.168.20.16/28` specifies IP
address of `192.168.20.16` and subnet mask of the length of `28`. A
table entry *matches* an incoming packet, if the prefix's address and
the packet's destination address have the same sequence of most
significant bits (MSB), which size equals the prefix's mask length.
One destination address may match mere than one forwarding table
entry; in such a case, the next hop is given by the most specific of
the matching table entries&mdash;the one with the longest subnet mask.
The LPM problem refers to finding that forwarding entry.

Code execution
--------------

Initiate the sandbox environment and build the program executable:

    cabal sandbox init
    cabal update
    cabal install --only-dependecies
    cabal build

To run the program, type command:

    cabal run -- < path/to/routing/table

Information of routing-table entry is given in columns (IP-address,
mask length, VPN number, and next-hop ID):

- **IPv4 network**

        255.220.192.0    19    d09c    0
         209.155.80.0    24    ce6a    1
        103.99.132.94    31    6443    2

- **IPv6 network**

         d15f:7c:1220:854:216:231b:5190:cd00    120    f9e2    0
                  ffdc:c233:c1a7:c4c9:ce40::     74    d09c    1
        a571:30a3:436c:6125:f33a:b105:6400:0    105    3f2d    2
