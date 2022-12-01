exists If .
exists F .
If = (lambda P . lambda X . lambda Y . all{P = true; X()} | all{P = false; Y()});
If(true)(lambda _ . false)(lambda _ . true)
