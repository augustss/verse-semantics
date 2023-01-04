any := lambda (X):
  X

Tail := lambda (Xs):
  all:
    I:any
    I > 0
    Xs(I)

Tail(all{1})
