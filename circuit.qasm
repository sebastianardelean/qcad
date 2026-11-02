OPENQASM 3.0;
include "stdgates.inc";

qubit[2] register_1;
qubit[2] register_2;

h register_1[0];
h register_1[1];
h register_2[0];
t register_2[0];
x register_1[0];
x register_1[1];
