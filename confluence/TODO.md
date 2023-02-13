## TODO

|      | Uni | App | Norm | Fail | Spec |
|------|:-----|:-----|:-----|:-----|:-----|
| Uni  |  ?   |  ?   |  ?   |   ?  |  ?   |
| App  |      |  ?   |  ?   |   ?  |  ?   |
| Norm |      |      |  ?   |   ?  |  ?   |
| Fail |      |      |      |   ?  |  ?   |
| Spec |      |      |      |      |  ?   |

% Thm A'' (Koen): (v1=w1; ..; vn=wn; C[v1,..,vn]) and (w1=v1; ..; wn=vn; C[w1,..,wn]) are joinable
% Proof: by induction on the sum of sizes of terms vi and wj

% * case FAIL:

% hnf1=hnf2; … ⟶ fail
%                 |
%                             "joins"
%                 |
% hnf2=hnf1; … ⟶ fail


% * case VAR-VAR:

% (assume x < y)

% x=y; ..; C[x,..] ⟶ y=x; ..; C[x,..]    (all y have become x)
% 			 |
% 		         "joins"
% 			 |
% y=x; ..; C[y,..] ⟶ y=x; ..; C[x,..]    (all y have become x)

% * case VAR-VAL:

% (assume x not in v)

% x=v; ..; C[x,..] ⟶ x=v; ..; C[v,..]  (all x have become v)
% 			 |
% 		         "joins"
% 			 |
% v=x; ..; C[v,..] ⟶ x=v; ..; C[v,..] (all x have become v)

% * case TUP-TUP:

% <a1,a2>=<b1,b2>; ..; C[<a1,b1>,..] ⟶ a1=b1; a2=b2; ..; C[<a1,a2>,..]
% 						    |
% "joins"
%     |
% <b1,b2>=<a1,a2>; ..; C[<b1,b2>,..] ⟶ b1=a1; b2=a2; ..; C[<b1,b2>,..]


% Thm B (maybe not needed):  (v1=v2; e) and (v2=v1; e) are joinable.  Prove by case analysis on form of v1,v2

% Thm A is needed if you start from (x=v1; x=v2; x) and substitute in two different ways
\end{verbatim}

\end{proof}

% Alex: some perhaps useful references:
% Takahashi, 1995, Information & Computation. “Parallel Reductions in λ-calculus”
% https://www.sciencedirect.com/science/article/pii/S0890540185710577
% Fritz Muller, 1992, Information Processing Letters. “Confluence of the lambda calculus with left-linear algebraic rewriting”
% https://www.sciencedirect.com/science/article/pii/002001909290155O
% [Hindley and Rosen] Let →1 and →2 be binary relations over Λ which commute, and suppose that both satisfy the diamond property. Define →12 to be the union of the two relations, and →→12 the transitive closure of →12. Then →→12 satisfies the diamond property.
