% Wumpus world grammar

s --> np, vp.

np --> det, noun | noun.

vp --> verb, np | verb, pp.

pp --> prep, np.

det --> [the].

noun --> [wumpus] | digit, digit.

verb --> [is].

prep --> [at] | [in].

digit --> [1] | [2] | [3] | [4] | [5] | [6] | [7] | [8] | [9].


% Wumpus grammar with semantics

s(Meaning) --> np(Something), vp(Something,Meaning).

np(Something) --> det1, noun(Something) | noun(Something).

vp(Something1,VP) --> transverb(Something1,Something2,VP), np(Something2).

vp(Something,PP) --> linkverb, pp(Something,PP).

pp(Something,PP) --> prep(Something,Somewhere,PP), np(Somewhere).

det1 --> [a] | [the].

noun(wumpus) --> [wumpus].
noun(agent) --> [agent].
noun([X,Y]) --> digit(X), digit(Y).

transverb(Something1,Something2,ate(Something1,Something2)) --> [ate].

linkverb --> [is].

prep(Something,Somewhere,at(Something,Somewhere)) --> [at].
prep(Something,Somewhere,at(Something,Somewhere)) --> [in].

digit(1) --> [1].
digit(2) --> [2].
digit(3) --> [3].
digit(4) --> [4].
digit(5) --> [5].
digit(6) --> [6].
digit(7) --> [7].
digit(8) --> [8].
digit(9) --> [9].
