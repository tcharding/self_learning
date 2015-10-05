UNIX Network Programming - Chapter 2
=====================================
Exercise answers - Tobin Harding.

1. IPv5 is the Internet Stream Protocol. see www.iana.org
2. For more information on IPv5 one could search the RFC's.
3. If TCP does not receive an MSS option from it's peer it chooses 536 bytes
   because this is the minimum reassembly buffer size guaranteed by the protocol.
4. paper
5. Let host on Ethernet be h1 and host on Token Ring be h2. Although h2
   advertises an MSS of 4096, h1 never sends packets larger than 1460 since its
   network (Ethernet) has an MTU (Maximum Transfer Units) of 1500 bytes. H2
   cannot send packets larger than 1460 because that is the MSS it recieved.
6. The protocols section of www.iana.org shows 89 for OSPF.
7. Data cannot be freed until it is acknowledged by all associations. WRONG see
   answers (page 914)for explanation.

Key 
===  
paper = done on paper.
