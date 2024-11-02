HOL-Light definition of real numbers in Coq
-------------------------------------------

This library provides a representation in Coq of the definition of real numbers in HOL-Light.

It has been automatically generated from HOL-Light using [hol2dk](https://github.com/Deducteam/hol2dk) and [lambdapi](https://github.com/Deducteam/lambdapi).

Proofs are not included but can be regenerated and rechecked by running [reproduce](https://github.com/Deducteam/coq-hol-light-real/blob/main/reproduce).

As HOL-Light is based on higher-order logic, this library assumes classical logic, Hilbert's ε operator, functional and propositional extensionnality (see [HOLLight.v](https://github.com/Deducteam/coq-hol-light-real/blob/main/HOLLight.v) for more details):

```
Axiom classic : forall P:Prop, P \/ ~ P.
Axiom constructive_indefinite_description : forall (A : Type) (P : A->Prop), (exists x, P x) -> { x : A | P x }.
Axiom fun_ext : forall {A B : Type} {f g : A -> B}, (forall x, (f x) = (g x)) -> f = g.
Axiom prop_ext : forall {P Q : Prop}, (P -> Q) -> (Q -> P) -> P = Q.
Axiom proof_irrelevance : forall (P:Prop) (p1 p2:P), p1 = p2.
```

Remark: the files `real*.v` are copied from [coq-fourcolor](https://github.com/coq-community/fourcolor) (commit c028f9b).

**Installation using [opam](https://opam.ocaml.org/)**

```
opam repo add coq-released https://coq.inria.fr/opam/released
opam install coq-hol-light-real
```

**Usage in a Coq file**

```
Require Import HOLLight_Real.theorems.
Check tmp_REAL_COMPLETE.
```
