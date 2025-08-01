/* Map from dk/lp symbols on the right to Coq expressions on the left:
The declarations of dk/lp symbols are removed and their occurrences are
replaced by the given Coq expressions. */

// primitive types
builtin "Type'" ≔ Set;
builtin "el" ≔ el;
builtin "Prop" ≔ bool;
builtin "arr" ≔ fun;

// connectives
builtin "eq" ≔ =;

builtin "imp" ≔ ⇒;
builtin "imp_def" ≔ ⇒_def;

builtin "all" ≔ ∀;
builtin "all_def" ≔ ∀_def;

builtin "or" ≔ ∨;
builtin "or_def" ≔ ∨_def;

builtin "and" ≔ ∧;
builtin "and_def" ≔ ∧_def;

builtin "ex" ≔ ∃;
builtin "ex_def" ≔ ∃_def;

builtin "not" ≔ ¬;
builtin "not_def" ≔ ¬_def;

builtin "True" ≔ ⊤;
builtin "T_def" ≔ ⊤_def;

builtin "False" ≔ ⊥;
builtin "F_def" ≔ ⊥_def;

builtin "ex1" ≔ ∃₁;
builtin "ex1_def" ≔ ∃₁_def;

// natural deduction rules
builtin "Logic.I" ≔ ⊤ᵢ;
builtin "conj" ≔ ∧ᵢ;
builtin "proj1" ≔ ∧ₑ₁;
builtin "proj2" ≔ ∧ₑ₂;
builtin "or_intro1" ≔ ∨ᵢ₁;
builtin "or_intro2" ≔ ∨ᵢ₂;
builtin "or_elim" ≔ ∨ₑ;
builtin "ex_intro" ≔ ∃ᵢ;
builtin "ex_elim" ≔ ∃ₑ;

// HOL-Light rules
builtin "eq_refl" ≔ REFL;
builtin "EQ_MP" ≔ EQ_MP;
builtin "MK_COMB" ≔ MK_COMB;
builtin "eq_trans" ≔ TRANS;
builtin "eq_sym" ≔ SYM;

// HOL-Light axioms
builtin "ε" ≔ ε;
builtin "axiom_0" ≔ axiom_0;
builtin "axiom_1" ≔ axiom_1;
builtin "fun_ext" ≔ fun_ext;
builtin "prop_ext" ≔ prop_ext;

// unit type
builtin "unit" ≔ unit;
builtin "tt" ≔ one;
builtin "one_def" ≔ one_def;
builtin "one_ABS" ≔ one_ABS;
builtin "one_REP" ≔ one_REP;
builtin "axiom_2" ≔ axiom_2;
builtin "axiom_3" ≔ axiom_3;

// product type
builtin "mk_pair" ≔ mk_pair;
builtin "prod" ≔ prod;
builtin "ABS_prod" ≔ ABS_prod;
builtin "REP_prod" ≔ REP_prod;
builtin "axiom_4" ≔ axiom_4;
builtin "axiom_5" ≔ axiom_5;
builtin "pair" ≔ ̦‚;
builtin "pair_def" ≔ ̦‚_def;
builtin "fst" ≔ FST;
builtin "FST_def" ≔ FST_def;
builtin "snd" ≔ SND;
builtin "SND_def" ≔ SND_def;

// infinite type ind
builtin "ind" ≔ ind;
builtin "ONE_ONE" ≔ ONE_ONE;
builtin "ONTO" ≔ ONTO;
builtin "axiom_6" ≔ axiom_6;
builtin "IND_SUC" ≔ IND_SUC;
builtin "IND_0" ≔ IND_0;

// type of natural numbers
builtin "nat" ≔ num;
builtin "0" ≔ _0;
builtin "S" ≔ SUC;
builtin "mk_num" ≔ mk_num;
builtin "dest_num" ≔ dest_num;
builtin "NUM_REP" ≔ NUM_REP;
builtin "_0_def" ≔ _0_def;
builtin "SUC_def" ≔ SUC_def;
builtin "axiom_7" ≔ axiom_7;
builtin "axiom_8" ≔ axiom_8;

// usual mathematical functions on natural numbers
builtin "NUMERAL" ≔ NUMERAL;
builtin "NUMERAL_def" ≔ NUMERAL_def;
builtin "BIT0" ≔ BIT0;
builtin "BIT0_def" ≔ BIT0_def;
builtin "BIT1" ≔ BIT1;
builtin "Nat.pred" ≔ PRE;
builtin "PRE_def" ≔ PRE_def;
builtin "Nat.add" ≔ +;
builtin "add_def" ≔ +_def;
builtin "Nat.mul" ≔ *;
builtin "mul_def" ≔ *_def;
builtin "Nat.pow" ≔ EXP;
builtin "EXP_def" ≔ EXP_def;
builtin "Peano.le" ≔ <=;
builtin "le_def" ≔ <=_def;
builtin "Peano.lt" ≔ <;
builtin "lt_def" ≔ <_def;
builtin "Peano.ge" ≔ >=;
builtin "ge_def" ≔ >=_def;
builtin "Peano.gt" ≔ >;
builtin "gt_def" ≔ >_def;
builtin "COND" ≔ COND;
builtin "Nat.max" ≔ MAX;
builtin "MAX_def" ≔ MAX_def;
builtin "Nat.min" ≔ MIN;
builtin "MIN_def" ≔ MIN_def;
builtin "Nat.sub" ≔ -;
builtin "minus_def" ≔ -_def;
builtin "Factorial.fact" ≔ FACT;
builtin "FACT_def" ≔ FACT_def;
builtin "Nat.div" ≔ DIV;
builtin "DIV_def" ≔ DIV_def;
builtin "Nat.modulo" ≔ MOD;
builtin "MOD_def" ≔ MOD_def;
builtin "Stdlib.Arith.PeanoNat.Nat.Even" ≔ EVEN;
builtin "EVEN_def" ≔ EVEN_def;
builtin "Stdlib.Arith.PeanoNat.Nat.Odd" ≔ ODD;
builtin "ODD_def" ≔ ODD_def;

// indtypes
builtin "NUMLEFT" ≔ NUMLEFT; // not mandatory
builtin "NUMLEFT_def" ≔ NUMLEFT_def; // not mandatory
builtin "NUMRIGHT" ≔ NUMRIGHT; // not mandatory
builtin "NUMRIGHT_def" ≔ NUMRIGHT_def; // not mandatory
builtin "ZRECSPACE" ≔ ZRECSPACE;
builtin "ZRECSPACE_def" ≔ ZRECSPACE_def;
builtin "recspace" ≔ recspace;
builtin "_mk_rec" ≔ _mk_rec;
builtin "_dest_rec" ≔ _dest_rec;
builtin "axiom_9" ≔ axiom_9;
builtin "axiom_10" ≔ axiom_10;
builtin "option" ≔ option;
builtin "_mk_option" ≔ _mk_option;
builtin "_dest_option" ≔ _dest_option;
builtin "axiom_13" ≔ axiom_13;
builtin "axiom_14" ≔ axiom_14;
builtin "None" ≔ NONE;
builtin "NONE_def" ≔ NONE_def;
builtin "Some" ≔ SOME;
builtin "SOME_def" ≔ SOME_def;
builtin "Datatypes.sum" ≔ Sum;
builtin "_mk_sum" ≔ _mk_sum;
builtin "_dest_sum" ≔ _dest_sum;
builtin "axiom_11" ≔ axiom_11;
builtin "axiom_12" ≔ axiom_12;
builtin "inl" ≔ INL;
builtin "INL_def" ≔ INL_def;
builtin "inr" ≔ INR;
builtin "INR_def" ≔ INR_def;

// lists
builtin "list" ≔ list;
builtin "FCONS" ≔ FCONS;
builtin "FCONS_def" ≔ FCONS_def;
builtin "_dest_list" ≔ _dest_list;
builtin "_mk_list" ≔ _mk_list;
builtin "axiom_15" ≔ axiom_15;
builtin "axiom_16" ≔ axiom_16;
builtin "nil" ≔ NIL;
builtin "NIL_def" ≔ NIL_def;
builtin "cons" ≔ CONS;
builtin "CONS_def" ≔ CONS_def;
builtin "ExtensionalityFacts.is_inverse" ≔ ISO;
builtin "ISO_def" ≔ ISO_def;
builtin "List.app" ≔ APPEND;
builtin "APPEND_def" ≔ APPEND_def;
builtin "List.rev" ≔ REVERSE;
builtin "REVERSE_def" ≔ REVERSE_def;
builtin "List.length" ≔ LENGTH;
builtin "LENGTH_def" ≔ LENGTH_def;
builtin "List.map" ≔ MAP;
builtin "MAP_def" ≔ MAP_def;
builtin "List.removelast" ≔ BUTLAST;
builtin "BUTLAST_def" ≔ BUTLAST_def;
builtin "List.Forall" ≔ ALL;
builtin "ALL_def" ≔ ALL_def;
builtin "List.ForallOrdPairs" ≔ PAIRWISE;
builtin "PAIRWISE_def" ≔ PAIRWISE_def;
//builtin "List.filter" ≔ FILTER;
//builtin "FILTER_def" ≔ FILTER_def;
builtin "List.In" ≔ MEM;
builtin "MEM_def" ≔ MEM_def;
builtin "repeat_with_perm_args" ≔ REPLICATE;
builtin "REPLICATE_def" ≔ REPLICATE_def;
//builtin "fold_right_with_perm_args" ≔ ITLIST;
//builtin "ITLIST_def" ≔ ITLIST_def;
builtin "hd" ≔ HD;
builtin "HD_def" ≔ HD_def;
builtin "tl" ≔ TL;
builtin "TL_def" ≔ TL_def;
//builtin "EL" ≔ EL;
//builtin "EL_def" ≔ EL_def;
builtin "Ascii.ascii" ≔ char;
builtin "_dest_char" ≔ _dest_char;
builtin "_mk_char" ≔ _mk_char;
builtin "axiom_17" ≔ axiom_17;
builtin "axiom_18" ≔ axiom_18;

// realax
builtin "dist" ≔ dist;
builtin "is_nadd" ≔ is_nadd;
builtin "nadd" ≔ nadd;
builtin "dest_nadd" ≔ dest_nadd;
builtin "mk_nadd" ≔ mk_nadd;
builtin "axiom_19" ≔ axiom_19;
builtin "axiom_20" ≔ axiom_20;

builtin "nadd_of_num" ≔ nadd_of_num;
builtin "nadd_le" ≔ nadd_le;
builtin "nadd_eq" ≔ nadd_eq;
builtin "nadd_add" ≔ nadd_add;
builtin "nadd_mul" ≔ nadd_mul;
builtin "nadd_rinv" ≔ nadd_rinv;
builtin "nadd_inv" ≔ nadd_inv;

builtin "hreal" ≔ hreal;
builtin "mk_hreal" ≔ mk_hreal;
builtin "dest_hreal" ≔ dest_hreal;
builtin "axiom_21" ≔ axiom_21;
builtin "axiom_22" ≔ axiom_22;

builtin "hreal_of_num" ≔ hreal_of_num;
builtin "hreal_le" ≔ hreal_le;
builtin "hreal_add" ≔ hreal_add;
builtin "hreal_mul" ≔ hreal_mul;
builtin "hreal_inv" ≔ hreal_inv;

builtin "treal_of_num" ≔ treal_of_num;
builtin "treal_le" ≔ treal_le;
builtin "treal_eq" ≔ treal_eq;
builtin "treal_add" ≔ treal_add;
builtin "treal_mul" ≔ treal_mul;
builtin "treal_inv" ≔ treal_inv;
builtin "treal_neg" ≔ treal_neg;

builtin "real" ≔ Real;
builtin "mk_real" ≔ mk_real;
builtin "dest_real" ≔ dest_real;
builtin "axiom_23" ≔ axiom_23;
builtin "axiom_24" ≔ axiom_24;
