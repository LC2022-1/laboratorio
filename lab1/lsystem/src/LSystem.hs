module LSystem where

data Dir = F | R | L

type Path = [Dir]

axiom = [F, R, F, R, F, R, F, R]

rule :: Dir -> Path
rule F = [F, R, F, L, F, L, F, F, R, F, R, F, L, F]
rule d = [d]

evolve :: Path -> Path
evolve p = concatMap rule p

