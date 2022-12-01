exists A .
exists B .
B = all {
  exists C .
  exists D .
  C = D;
  A = C;
  D = false;
};
A = true;
(A, B)
