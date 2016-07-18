:- dynamic([s/2,vp/2,np/2,det/2,noun/2,verb/2]).
:- dynamic([s/3,vp/3,np/3,det/3,noun/3,verb/3]).
:- dynamic([s/4,vp/4,np/4,det/4,noun/4,verb/4]).


% Simple English grammar

s --> np, vp.

vp --> verb, np.

np --> det, noun.

det --> [a] | [the].

noun --> [cat] | [mouse] | [cats] | [mice].

verb --> [scares] | [hates] | [scare] | [hate].


% Simple English grammar with noun-verb number

s(Number) --> np(Number), vp(Number).

vp(Number) --> verb(Number), np(Number1).

np(Number) --> det(Number), noun(Number).

det(singular) --> [a] | [the].

det(plural) --> [the].

noun(singular) --> [cat] | [mouse].

noun(plural) --> [cats] | [mice].

verb(singular) --> [scares] | [hates].

verb(plural) --> [scare] | [hate].


% Simple English grammar constructing parse tree

s(Number,s(NP,VP)) --> np(Number,NP), vp(Number,VP).

vp(Number,vp(Verb,NP)) --> verb(Number,Verb), np(Number1,NP).

np(Number,np(Det,Noun)) --> det(Number,Det), noun(Number,Noun).

det(singular,det(a)) --> [a].
det(singular,det(the)) --> [the].
det(plural,det(the)) --> [the].

noun(singular,noun(cat)) --> [cat].
noun(singular,noun(mouse)) --> [mouse].
noun(plural,noun(cats)) --> [cats].
noun(plural,noun(mice)) --> [mice].

verb(singular,verb(scares)) --> [scares].
verb(singular,verb(hates)) --> [hates].
verb(plural,verb(scare)) --> [scare].
verb(plural,verb(hate)) --> [hate].
