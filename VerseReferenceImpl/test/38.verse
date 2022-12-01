exists X .
exists Y .
Y = all{
  exists Z .
  (X = 0; Z = 1; Z)|(X = 1; Z = 2; Z)|(X = 0; Z = 3; Z)|(X = 2; Z = 4; Z)
};
X = 2|0|1;
Y
