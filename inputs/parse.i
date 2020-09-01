l oad  r1 => r2
// COMP 412, Rice University
// ILOC Front End
// 
// This ILOC file has some (perhaps unexpected) problems
loadI 20 => r1
load  r1 => r2
loadI 8 => r3 //loadI r24 => r3
load  r3 => r4
add   r2 ,r3   => r4//add   r2, 3 => r4
mult  r1, r2 => r9
add   r4, r1 => r6
store r6 => r7
output 20
