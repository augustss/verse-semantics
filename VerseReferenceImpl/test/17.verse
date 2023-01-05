exists F
F = function (exists X). all. (X = true; true)|(X = false; F(true))
exists X
exists Y
Y = F(X);
X = false;
Y
