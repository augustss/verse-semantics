exists A
exists B
B = all {
  exists C
  exists D
  C = all{(A = true)|(D = false)};
  D = true;
  C
};
A = true;
B
