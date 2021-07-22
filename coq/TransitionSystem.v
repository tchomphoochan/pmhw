Require Import Coq.Arith.PeanoNat.
Require Import Coq.Arith.Wf_nat.
Require Import Coq.Bool.Bool.
Require Import Coq.Lists.List.
Require Import Coq.Sorting.Permutation.
Require Import Coq.micromega.Lia.
Require Import Coq.micromega.ZifyBool.

Import Coq.Lists.List.ListNotations.

Scheme Equality for list.

(* Object set: sorted list of numbers (ascending). *)
Definition obj_set := list nat.

Definition empty_set : obj_set := [].

Definition set_eq (a b : obj_set) : bool := list_beq nat Nat.eqb a b.

Fixpoint set_add (set : obj_set) (obj : nat) : obj_set :=
    match set with
    | [] => [obj]
    | elt :: set' => if obj <? elt then obj :: set
                     else if obj =? elt then set
                     else elt :: set_add set' obj
    end.

Fixpoint set_union (a b : obj_set) : obj_set :=
    let fix set_union' b :=
    match a with
    | [] => b
    | a_elt :: a' => match b with
                   | [] => a
                   | b_elt :: b' => if a_elt <? b_elt then a_elt :: set_union a' b
                                    else if a_elt =? b_elt then a_elt :: set_union a' b'
                                    else b_elt :: set_union' b'
                   end
    end
    in set_union' b.

Ltac destruct_ifs :=
    repeat match goal with
    | _ : _ = (if (?x <? ?y) then _ else _) |- _ => destruct_with_eqn (x <? y)
    | _ : (if (?x <? ?y) then _ else _) = _ |- _ => destruct_with_eqn (x <? y)
    | _ : _ = (if (?x =? ?y) then _ else _) |- _ => destruct_with_eqn (x =? y)
    | _ : (if (?x =? ?y) then _ else _) = _ |- _ => destruct_with_eqn (x =? y)
    | |- _ = (if (?x <? ?y) then _ else _) => destruct_with_eqn (x <? y)
    | |- (if (?x <? ?y) then _ else _) = _ => destruct_with_eqn (x <? y)
    | |- _ = (if (?x =? ?y) then _ else _) => destruct_with_eqn (x =? y)
    | |- (if (?x =? ?y) then _ else _) = _ => destruct_with_eqn (x =? y)
    end; try lia.

Lemma set_union_empty : forall s,
  set_union s empty_set = s.
Proof.
  intros; destruct s; simpl; reflexivity.
Qed.

Lemma set_union_both_empty : forall s1 s2,
  set_union s1 s2 = empty_set
  -> s1 = empty_set /\ s2 = empty_set.
Proof.
  induction s1; simpl; intros; destruct s2; intuition; destruct_ifs; discriminate.
Qed.

Lemma set_union_sym : forall s1 s2,
  set_union s1 s2 = set_union s2 s1.
Proof.
  induction s1; induction s2; simpl in *; destruct_ifs; try rewrite IHs1; f_equal; intuition.
Qed.

Lemma set_union_assoc : forall s1 s2 s3,
  set_union (set_union s1 s2) s3 = set_union s1 (set_union s2 s3).
Proof.
  induction s1; simpl; intro.
  - destruct s2; simpl.
    + destruct s3; reflexivity.
    + destruct s3; try reflexivity.
      destruct (n <? n0); try reflexivity.
      destruct (n =? n0); reflexivity.
  - induction s2; simpl.
    + destruct s3; reflexivity.
    + destruct_with_eqn (a <? a0); simpl.
      * induction s3; simpl.
        -- rewrite Heqb.
           reflexivity.
        -- destruct_with_eqn (a <? a1); simpl.
           ++ destruct_with_eqn (a0 <? a1); simpl.
              ** rewrite Heqb.
                 rewrite IHs1.
                 simpl.
                 rewrite Heqb1.
                 reflexivity.
              ** destruct_with_eqn (a0 =? a1); simpl.
                 --- rewrite Heqb.
                     rewrite IHs1.
                     simpl.
                     rewrite Heqb1.
                     rewrite Heqb2.
                     reflexivity.
                 --- rewrite Heqb0.
                     rewrite IHs1.
                     simpl.
                     rewrite Heqb1.
                     rewrite Heqb2.
                     reflexivity.
           ++ destruct_with_eqn (a =? a1); simpl.
              ** destruct_with_eqn (a0 <? a1); try lia; simpl.
                 destruct_with_eqn (a0 =? a1); try lia; simpl.
                 rewrite Heqb0.
                 rewrite Heqb1.
                 rewrite IHs1.
                 simpl.
                 reflexivity.
              ** destruct_with_eqn (a0 <? a1); try lia; simpl.
                 destruct_with_eqn (a0 =? a1); try lia; simpl.
                 rewrite Heqb0.
                 rewrite Heqb1.
                 rewrite IHs3.
                 reflexivity.
      * destruct_with_eqn (a =? a0); simpl.
        -- induction s3; simpl.
           ++ rewrite Heqb.
              rewrite Heqb0.
              reflexivity.
           ++ destruct_with_eqn (a <? a1); simpl.
              ** destruct_with_eqn (a0 <? a1); try lia; simpl.
                 rewrite Heqb.
                 rewrite Heqb0.
                 rewrite IHs1.
                 reflexivity.
              ** destruct_with_eqn (a =? a1); simpl.
                 --- destruct_with_eqn (a0 <? a1); try lia; simpl.
                     destruct_with_eqn (a0 =? a1); try lia; simpl.
                     rewrite Heqb.
                     rewrite Heqb0.
                     rewrite IHs1.
                     reflexivity.
                 --- destruct_with_eqn (a0 <? a1); try lia; simpl.
                     destruct_with_eqn (a0 =? a1); try lia; simpl.
                     rewrite Heqb1.
                     rewrite Heqb2.
                     rewrite IHs3.
                     reflexivity.
        -- induction s3; simpl.
           ++ rewrite Heqb.
              rewrite Heqb0.
              reflexivity.
           ++ destruct_with_eqn (a0 <? a1); simpl.
              ** rewrite Heqb.
                 rewrite Heqb0.
                 rewrite IHs2.
                 reflexivity.
              ** destruct_with_eqn (a0 =? a1); simpl.
                 --- rewrite Heqb.
                     rewrite Heqb0.
                     rewrite IHs2.
                     reflexivity.
                 --- destruct_with_eqn (a =? a1); try lia; simpl.
                     destruct_with_eqn (a <? a1); try lia; simpl.
                     rewrite IHs3.
                     reflexivity.
Qed.

Lemma set_union_comm_fold_right : forall s1 s2,
  set_union (fold_right set_union empty_set s2) s1 = fold_right set_union s1 s2.
Proof.
  induction s1; induction s2; intros; try reflexivity.
  - simpl.
    unfold empty_set.
    destruct (set_union a (fold_right set_union [] s2)); simpl; reflexivity.
  - unfold empty_set in *.
    simpl fold_right.
    rewrite <- IHs2.
    apply set_union_assoc.
Qed.

Fixpoint set_inter (a b : obj_set) : obj_set :=
    let fix set_inter' b :=
    match a with
    | [] => []
    | a_elt :: a' => match b with
                   | [] => []
                   | b_elt :: b' => if a_elt <? b_elt then set_inter a' b
                                   else if a_elt =? b_elt then a_elt :: set_inter a' b'
                                   else set_inter' b'
                   end
    end
    in set_inter' b.

Lemma set_inter_sym : forall s1 s2,
    set_inter s1 s2 = set_inter s2 s1.
Proof.
  induction s1; induction s2; simpl in *; destruct_ifs; try rewrite IHs1; f_equal; intuition.
Qed.

Lemma set_inter_distr_union : forall s1 s2 s3,
  set_inter s1 (set_union s2 s3)
  = set_union (set_inter s1 s2) (set_inter s1 s3).
Admitted.

(* Transaction type. *)
Record transaction := {
    WriteSet : obj_set;
    ReadSet : obj_set;
}.

(* Actions shown in the trace. *)
Inductive action :=
| Add (t: transaction)
| Start (t: transaction)
| Finish (t: transaction).

(* State for spec. *)
Record spec_state := mkSpecState {
    SpecQueued : list transaction;
    SpecRunning : list transaction;
}.

(* Helper function that states when two transactions are compatible. *)
Definition compatible (t1 t2 : transaction) : Prop :=
    set_inter (ReadSet t1) (WriteSet t2) = empty_set
    /\ set_inter (WriteSet t1) (ReadSet t2) = empty_set
    /\ set_inter (WriteSet t1) (WriteSet t2) = empty_set.

Lemma compatible_sym : forall t1 t2,
    compatible t1 t2
    -> compatible t2 t1.
Proof.
    intros; unfold compatible in *; intuition; rewrite set_inter_sym in * |-; assumption.
Qed.
Local Hint Resolve compatible_sym : core.

(* Specification traces. *)
Inductive spec_trace : spec_state -> list action -> spec_state -> Prop :=
| SpecNoop : forall s s',
    Permutation (SpecQueued s) (SpecQueued s')
    -> Permutation (SpecRunning s) (SpecRunning s')
    -> spec_trace s [] s'
| SpecAdd : forall s s' s'' tr new_t,
    spec_trace s' tr s''
    -> Permutation (new_t :: SpecQueued s) (SpecQueued s')
    -> Permutation (SpecRunning s) (SpecRunning s')
    -> spec_trace s (Add new_t :: tr) s''
| SpecStart : forall s s' s'' tr started_t,
    spec_trace s' tr s''
    -> Permutation (SpecQueued s) (started_t :: SpecQueued s')
    -> Permutation (started_t :: SpecRunning s) (SpecRunning s')
    -> List.Forall (compatible started_t) (SpecRunning s)
    -> spec_trace s (Start started_t :: tr) s''
| SpecFinish : forall s s' s'' tr finished_t,
    spec_trace s' tr s''
    -> Permutation (SpecQueued s) (SpecQueued s')
    -> Permutation (SpecRunning s) (finished_t :: SpecRunning s')
    -> spec_trace s (Finish finished_t :: tr) s''.

(* State for implementation. *)
Record pm_state := mkState {
    Queued : list transaction;
    Renamed : list transaction;
    Scheduled : list transaction;
    Running : list transaction;
    Finished : list transaction;
}.

(* Renaming step. TODO: pick a transaction non-deterministically. *)
Definition rename_transaction (state : pm_state) : pm_state :=
    match Queued state with
    | nil => state
    | t :: rest => mkState rest (t :: Renamed state) (Scheduled state) (Running state) (Finished state)
    end.

(* Scheduling helpers. *)
Record transaction_set := mkTrSet {
    SetTransactions : list transaction;
    SetReadSet : obj_set;
    SetWriteSet : obj_set;
}.

Definition tr_set_valid (tr_set : transaction_set) :=
  let trs := (SetTransactions tr_set) in
  ForallOrdPairs compatible trs
  /\ SetReadSet tr_set = fold_right set_union empty_set (map ReadSet trs)
  /\ SetWriteSet tr_set = fold_right set_union empty_set (map WriteSet trs).

Definition merge_tr_sets (a: transaction_set) (b : transaction_set) : transaction_set :=
    mkTrSet ((SetTransactions a) ++ (SetTransactions b)) (set_union (SetReadSet a) (SetReadSet b)) (set_union (SetWriteSet a) (SetWriteSet b)).

Definition set_compatible (a : transaction_set) (b : transaction_set) :=
    andb  (set_eq (set_inter (SetReadSet a) (SetWriteSet b)) empty_set)
    (andb (set_eq (set_inter (SetWriteSet a) (SetWriteSet b)) empty_set)
          (set_eq (set_inter (SetWriteSet a) (SetReadSet b)) empty_set)).

Ltac invert_Foralls' :=
  repeat lazymatch goal with
  | H : Forall _ (_ :: _) |- _ => inversion_clear H
  | H : ForallOrdPairs _ (_ :: _) |- _ => inversion_clear H
  | |- Forall _ (_ :: _) => constructor
  | |- ForallOrdPairs _ (_ :: _) => constructor
  | H : Forall _ _ /\ _ |- _ => inversion_clear H
  | H : ForallOrdPairs _ _ /\ _ |- _ => inversion_clear H
  | |- Forall _ _ /\ _ => split
  | |- ForallOrdPairs _ _ /\ _ => split
  | H : Forall _ (_ ++ _) |- _ => rewrite Forall_app in H
  | |- Forall _ (_ ++ _) => rewrite Forall_app
  end.

(* Helper lemma to help manipulate ValidPmState. *)
Lemma Forall_Forall_comm : forall A (R : A -> A -> Prop) l1 l2,
  (forall x y, R x y -> R y x)
  -> Forall (fun x => Forall (R x) l1) l2 -> Forall (fun x => Forall (R x) l2) l1.
Proof.
  induction l1; trivial; intros; constructor; try apply IHl1; try eapply Forall_impl;
    try eassumption; simpl; intros; invert_Foralls'; auto.
Qed.

Lemma FOP_app : forall A (R : A -> A -> Prop) l1 l2,
  (forall x y, R x y -> R y x)
  -> List.ForallOrdPairs R (l1 ++ l2)
  <-> List.ForallOrdPairs R l1
      /\ List.ForallOrdPairs R l2
      /\ List.Forall (fun x => List.Forall (R x) l1) l2.
Proof.
  induction l1; intros.
  - intuition; try constructor.
    apply Forall_impl with (P := fun x => True); trivial.
    apply Forall_forall; trivial.
  - rewrite <- app_comm_cons.
    intuition; invert_Foralls'; try rewrite IHl1 in *; invert_Foralls';
      try solve [rewrite Forall_forall in *; auto]; eapply Forall_impl; try eassumption;
      simpl; intros; invert_Foralls'; auto.
Qed.

Lemma set_compatible_correct : forall trs2 trs1,
  let ts1 := {|
    SetTransactions := trs1;
    SetReadSet := fold_right set_union empty_set (map ReadSet trs1);
    SetWriteSet := fold_right set_union empty_set (map WriteSet trs1);
  |} in
  let ts2 := {|
    SetTransactions := trs2;
    SetReadSet := fold_right set_union empty_set (map ReadSet trs2);
    SetWriteSet := fold_right set_union empty_set (map WriteSet trs2);
  |} in
  set_compatible ts1 ts2 = true
  -> Forall (fun t => Forall (compatible t) trs1) trs2.
Proof.
  induction trs2; simpl; try solve [intros; constructor].
  induction trs1; simpl; intros.
  - apply Forall_Forall_comm; auto.
  - apply Forall_Forall_comm; auto.
    constructor; try constructor.
    + unfold set_compatible in H.
      simpl in H.
      repeat rewrite andb_true_iff in H.
      inversion_clear H as [Hcomp1 H'].
      inversion_clear H' as [Hcomp2 Hcomp3].
      apply internal_list_dec_bl in Hcomp1, Hcomp2, Hcomp3; try apply Nat.eqb_eq.
      rewrite set_inter_distr_union in Hcomp1, Hcomp2, Hcomp3.
      rewrite set_inter_sym in Hcomp1, Hcomp2, Hcomp3.
      rewrite set_inter_distr_union in Hcomp1, Hcomp2, Hcomp3.
      apply set_union_both_empty in Hcomp1, Hcomp2, Hcomp3.
      inversion_clear Hcomp1 as [Hcomp1' _].
      inversion_clear Hcomp2 as [Hcomp2' _].
      inversion_clear Hcomp3 as [Hcomp3' _].
      apply set_union_both_empty in Hcomp1', Hcomp2', Hcomp3'.
      inversion_clear Hcomp1' as [Hcomp1 _].
      inversion_clear Hcomp2' as [Hcomp2 _].
      inversion_clear Hcomp3' as [Hcomp3 _].
      rewrite set_inter_sym in Hcomp1, Hcomp2, Hcomp3.
      unfold compatible.
      intuition.
    + eapply Forall_impl; try apply IHtrs2 with (trs1 := [a0]).
      * simpl; intros ? Hcomp; inversion Hcomp; auto.
      * unfold set_compatible in H |- *; simpl in *.
        repeat rewrite andb_true_iff in H.
        inversion_clear H as [Hcomp1 H'].
        inversion_clear H' as [Hcomp2 Hcomp3].
        apply internal_list_dec_bl in Hcomp1, Hcomp2, Hcomp3; try apply Nat.eqb_eq.
        rewrite set_inter_distr_union in Hcomp1, Hcomp2, Hcomp3.
        apply set_union_both_empty in Hcomp1, Hcomp2, Hcomp3.
        inversion_clear Hcomp1 as [_ Hcomp1'].
        inversion_clear Hcomp2 as [_ Hcomp2'].
        inversion_clear Hcomp3 as [_ Hcomp3'].
        rewrite set_inter_sym in Hcomp1', Hcomp2', Hcomp3'.
        rewrite set_inter_distr_union in Hcomp1', Hcomp2', Hcomp3'.
        apply set_union_both_empty in Hcomp1', Hcomp2', Hcomp3'.
        inversion_clear Hcomp1' as [Hcomp1 _].
        inversion_clear Hcomp2' as [Hcomp2 _].
        inversion_clear Hcomp3' as [Hcomp3 _].
        rewrite set_inter_sym in Hcomp1, Hcomp2, Hcomp3.
        repeat rewrite set_union_empty.
        rewrite Hcomp1, Hcomp2, Hcomp3.
        reflexivity.
    + apply Forall_Forall_comm; auto.
      apply IHtrs1.
      unfold set_compatible in H |- *; simpl in *.
      repeat rewrite andb_true_iff in H.
      inversion_clear H as [Hcomp1 H'].
      inversion_clear H' as [Hcomp2 Hcomp3].
      apply internal_list_dec_bl in Hcomp1, Hcomp2, Hcomp3; try apply Nat.eqb_eq.
      rewrite set_inter_sym in Hcomp1, Hcomp2, Hcomp3.
      rewrite set_inter_distr_union in Hcomp1, Hcomp2, Hcomp3.
      apply set_union_both_empty in Hcomp1, Hcomp2, Hcomp3.
      inversion_clear Hcomp1 as [_ Hcomp1'].
      inversion_clear Hcomp2 as [_ Hcomp2'].
      inversion_clear Hcomp3 as [_ Hcomp3'].
      rewrite set_inter_sym in Hcomp1', Hcomp2', Hcomp3'.
      rewrite Hcomp1', Hcomp2', Hcomp3'.
      reflexivity.
Qed.

Lemma merge_compatible : forall ts1 ts2,
  tr_set_valid ts1
  -> tr_set_valid ts2
  -> set_compatible ts1 ts2 = true
  -> tr_set_valid (merge_tr_sets ts1 ts2).
Proof.
  intros.
  unfold tr_set_valid in *.
  simpl.
  intuition.
  - apply FOP_app; intuition.
    eapply set_compatible_correct.
    destruct ts1; destruct ts2; simpl in *; subst; assumption.
  - rewrite H0.
    rewrite H3.
    rewrite map_app.
    rewrite fold_right_app.
    apply set_union_comm_fold_right.
  - rewrite H5.
    rewrite H6.
    rewrite map_app.
    rewrite fold_right_app.
    apply set_union_comm_fold_right.
Qed.

(* Single round of the tournament. Returns (merged sets, filtered out transactions).
   The sets in the input are merged pairwise if they are compatible, or the first one
   is kept and the transactions from the second one are appended to the second list. *)
Fixpoint do_tournament_round (tr_sets : list transaction_set) :
                             list transaction_set * list transaction :=
    match tr_sets with
    | t1 :: t2 :: rest => match set_compatible t1 t2 with
                          | true => let result := do_tournament_round rest
                                    in (merge_tr_sets t1 t2 :: fst result, snd result)
                          | false => let result := do_tournament_round rest
                                     in (t1 :: fst result, (SetTransactions t2) ++ snd result)
                          end
    | t1 :: [] => ([t1], [])
    | [] => ([], [])
    end.

Lemma do_tournament_round_sched_size : forall tr_sets,
  length (fst (do_tournament_round tr_sets)) = (length tr_sets + 1) / 2.
Proof.
  fix IHtr_sets 1.
  destruct tr_sets; try reflexivity.
  destruct tr_sets; try reflexivity.
  rewrite <- Nat.div2_div.
  simpl.
  destruct (set_compatible t t0); simpl; rewrite IHtr_sets; rewrite Nat.div2_div; reflexivity.
Qed.

Lemma do_tournament_round_sched_size_2 : forall tr_sets,
  [] <> tr_sets
  -> (forall tr_set : transaction_set, [tr_set] <> tr_sets)
  -> length (fst (do_tournament_round tr_sets)) < length tr_sets.
Proof.
  intros tr_sets Hempty Hone.
  rewrite do_tournament_round_sched_size.
  destruct tr_sets; intuition.
  rewrite <- Nat.div2_div.
  simpl.
  destruct tr_sets.
  - specialize Hone with (1:=eq_refl).
    exfalso.
    assumption.
  - simpl.
    rewrite <- Nat.succ_lt_mono.
    rewrite Nat.add_1_r.
    apply Nat.lt_div2.
    lia.
Qed.
Local Hint Resolve do_tournament_round_sched_size_2 : core.

Lemma do_tournament_round_trs : forall tr_sets,
  let result := do_tournament_round tr_sets in
  Permutation (concat (map SetTransactions (fst result)) ++ snd result)
              (concat (map SetTransactions tr_sets)).
Proof.
  simpl.
  fix IHtr_sets 1.
  destruct tr_sets; simpl; try constructor.
  destruct tr_sets; ssimpl_list; try apply Permutation_refl.
  destruct (set_compatible t t0); simpl.
  - repeat rewrite <- app_assoc.
    repeat apply Permutation_app_head.
    apply IHtr_sets.
  - rewrite <- app_assoc.
    apply Permutation_app_head.
    rewrite Permutation_app_swap_app.
    apply Permutation_app_head.
    apply IHtr_sets.
Qed.

Lemma do_tournament_round_compatible : forall tr_sets,
  Forall tr_set_valid tr_sets
  -> Forall tr_set_valid (fst (do_tournament_round tr_sets)).
Proof.
  fix IH 1.
  intros.
  destruct tr_sets; try solve [simpl; assumption].
  destruct tr_sets; try solve [simpl; assumption].
  Guarded.
  simpl.
  destruct_with_eqn (set_compatible t t0); simpl; inversion H; inversion H3; constructor;
    assumption || apply merge_compatible || apply IH; assumption.
Qed.

(* Do tournament-style elimination on `tr_sets`. Each round halves the no. of sets,
   while some transactions are filtered out. Returns the transactions from the first set
   and the remaining transactions ++ filtered out transactions.*)
Fixpoint do_tournament' (tr_sets : list transaction_set) (rounds_left : nat) :
                       list transaction * list transaction :=
    match rounds_left with
    | 0 => match tr_sets with
           | [] => ([], [])
           | head :: rest => (SetTransactions head, (concat (map SetTransactions rest)))
           end
    | S n => let round_result := do_tournament_round tr_sets in
             let result := do_tournament' (fst round_result) n in
             (fst result, snd result ++ snd round_result)
    end.
Definition do_tournament (tr_sets : list transaction_set) :
                         list transaction * list transaction :=
  do_tournament' tr_sets (Nat.log2_up (length tr_sets)).

Lemma concat_map_wrap : forall A (l : list A),
  concat (map (fun x => [x]) l) = l.
Proof.
  induction l; simpl; auto using f_equal.
Qed.

Lemma firstn_app_first : forall A (l1 l2 l : list A),
  firstn (length l1 + length l2) l = l1 ++ l2 -> firstn (length l1) l = l1.
Proof.
  induction l1; simpl; intros; try reflexivity.
  destruct l; try discriminate.
  inversion H.
  f_equal.
  eapply IHl1.
  eassumption.
Qed.

Lemma firstn_app_all : forall A (l1 l2: list A),
  firstn (length l1) (l1 ++ l2) = l1.
Proof.
  intros.
  replace (length l1) with (length l1 + 0) by lia.
  rewrite firstn_app_2.
  ssimpl_list.
  reflexivity.
Qed.

Lemma do_tournament'_nil : forall n,
  do_tournament' [] n = ([], []).
Proof.
  induction n; simpl; try rewrite IHn; reflexivity.
Qed.

Lemma do_tournament'_one : forall t n,
  do_tournament' [t] n = (SetTransactions t, []).
Proof.
  induction n; simpl; repeat rewrite IHn; reflexivity.
Qed.

Lemma log2_up_le_double : forall n,
  n <> 0
  -> Nat.log2_up (2 * S (n / 2)) <= Nat.log2_up (S n).
Proof.
  intros.
  rewrite <- Nat.div2_div.
  destruct_with_eqn (Nat.odd n).
  - replace (2 * S (Nat.div2 n)) with (S (2 * Nat.div2 n + Nat.b2n true)) by lia.
    rewrite <- Heqb.
    rewrite <- Nat.div2_odd.
    constructor.
  - replace (2 * S (Nat.div2 n)) with (S (S (2 * Nat.div2 n + Nat.b2n false))) by lia.
    rewrite <- Heqb.
    rewrite <- Nat.div2_odd.
    pose proof (Nat.log2_up_succ_or n) as Hs.
    inversion_clear Hs as [Hs' | Hs'].
    + rewrite Hs'.
      rewrite <- Nat.log2_up_double by lia.
      apply Nat.log2_up_le_mono.
      destruct n; lia || destruct n; cbn in *; lia.
    + pose proof (Nat.log2_up_succ_or (S n)) as Hs.
      inversion_clear Hs as [Hs'' | Hs'']; rewrite Hs''; try lia.
      apply Nat.log2_up_eq_succ_is_pow2 in Hs''.
      inversion_clear Hs'' as [a Hs].
      destruct a; try solve [simpl in Hs; inversion Hs; lia].
      rewrite <- Nat.even_succ in Heqb.
      apply (f_equal negb) in Heqb.
      rewrite Nat.negb_even in Heqb.
      simpl in Heqb.
      apply Nat.odd_spec in Heqb.
      apply Nat.Even_Odd_False in Heqb; firstorder.
Qed.

Lemma dtr_log2_bound : forall tr_sets,
  Nat.log2_up (S (length (fst (do_tournament_round tr_sets)))) <=
  Nat.log2 (S (length tr_sets)).
Proof.
  intros.
  rewrite do_tournament_round_sched_size.
  apply Nat.succ_le_mono.
  rewrite <- Nat.log2_up_double by lia.
  rewrite Nat.add_1_r.
  eapply Nat.le_trans; try apply log2_up_le_double; unfold Nat.log2_up; simpl; lia.
Qed.

Lemma do_tournament'_idem : forall n tr_sets,
  Nat.log2_up (length tr_sets) <= n
  -> do_tournament' tr_sets n = do_tournament tr_sets.
Proof.
  unfold do_tournament.
  induction n as [n IHn] using lt_wf_ind; intros.
  destruct tr_sets; try solve [repeat rewrite do_tournament'_nil; reflexivity].
  destruct tr_sets; try solve [repeat rewrite do_tournament'_one; reflexivity].
  destruct n; cbn in *; try lia.
  rewrite <- Nat.succ_le_mono in *.
  destruct (set_compatible t t0); cbn;
    rewrite IHn by (lia || eapply Nat.le_trans; eassumption || apply dtr_log2_bound);
    erewrite <- IHn; reflexivity || lia || apply dtr_log2_bound.
Qed.

Lemma do_tournament_first : forall rest n tr_set,
  let ts := SetTransactions tr_set in
  length ts = n
  -> firstn n (fst (do_tournament (tr_set :: rest))) = ts.
Proof.
  simpl.
  induction rest as [rest IHrest] using (induction_ltof1 _ (@length _));
    unfold ltof in IHrest; intros; subst.
  destruct rest; simpl; try apply firstn_all.
  destruct (set_compatible tr_set t); simpl.
  - destruct rest; try solve [simpl; apply firstn_app_all].
    destruct rest.
    + simpl.
      destruct (set_compatible (merge_tr_sets tr_set t) t0); simpl;
        try rewrite <- app_assoc; apply firstn_app_all.
    + remember (t0 :: t1 :: rest) as rest'.
      erewrite firstn_app_first; try reflexivity.
      rewrite <- app_length.
      rewrite do_tournament'_idem; simpl; apply dtr_log2_bound || rewrite IHrest;
        reflexivity || simpl.
      apply Nat.lt_lt_succ_r.
      apply do_tournament_round_sched_size_2; subst; intuition; discriminate.
  - destruct rest; try solve [simpl; apply firstn_all].
    destruct rest.
    + simpl.
      destruct (set_compatible tr_set t0); simpl; apply firstn_app_all || apply firstn_all.
    + remember (t0 :: t1 :: rest) as rest'.
      rewrite do_tournament'_idem; simpl; apply dtr_log2_bound || apply IHrest;
        reflexivity || simpl.
      apply Nat.lt_lt_succ_r.
      apply do_tournament_round_sched_size_2; subst; intuition; discriminate.
Qed.

Lemma do_tournament_rest : forall rest n tr_set,
  let ts := SetTransactions tr_set in
  let result := do_tournament (tr_set :: rest) in
  let sched := fst result in
  let rem := snd result in
  length ts = n
  -> Permutation (skipn n sched ++ rem) (concat (map SetTransactions rest)).
Proof.
  simpl.
  induction rest as [rest IHrest] using (induction_ltof1 _ (@length _));
    unfold ltof in IHrest; intros; subst.
  destruct rest; try rewrite skipn_all; simpl; try constructor.
  destruct (set_compatible tr_set t); simpl.
  - rewrite do_tournament'_idem; simpl; try apply dtr_log2_bound.
    erewrite <- firstn_skipn with (l := fst _).
    rewrite do_tournament_first; try reflexivity.
    simpl SetTransactions at 2.
    Opaque merge_tr_sets.
    rewrite <- app_assoc.
    rewrite skipn_app.
    rewrite skipn_all.
    rewrite Nat.sub_diag.
    simpl.
    rewrite <- app_assoc.
    apply Permutation_app_head.
    rewrite app_assoc.
    rewrite IHrest; try reflexivity; try apply do_tournament_round_trs.
    simpl.
    destruct rest; try solve [simpl; lia].
    destruct rest; try solve [simpl; lia].
    apply Nat.lt_lt_succ_r.
    apply do_tournament_round_sched_size_2; intuition; discriminate.
  - rewrite do_tournament'_idem; simpl; try apply dtr_log2_bound.
    rewrite app_assoc.
    rewrite Permutation_app_swap_app.
    apply Permutation_app_head.
    rewrite IHrest; try reflexivity; try apply do_tournament_round_trs.
    simpl.
    destruct rest; try solve [simpl; lia].
    destruct rest; try solve [simpl; lia].
    apply Nat.lt_lt_succ_r.
    apply do_tournament_round_sched_size_2; intuition; discriminate.
Qed.

Lemma do_tournament_compatible : forall tr_sets sched,
  Forall tr_set_valid tr_sets
  -> sched = fst (do_tournament tr_sets)
  -> ForallOrdPairs compatible sched.
Proof.
  induction tr_sets as [tr_sets IHtr_sets] using (induction_ltof1 _ (@length _));
    unfold ltof in IHtr_sets; intros; subst.
  destruct tr_sets; simpl; try constructor.
  destruct tr_sets; simpl; try solve [unfold tr_set_valid in H; inversion H; intuition].
  destruct_with_eqn (set_compatible t t0); simpl.
  - rewrite do_tournament'_idem; simpl; try apply dtr_log2_bound.
    eapply IHtr_sets; try reflexivity; simpl.
    + apply Nat.lt_succ_r.
      destruct tr_sets; try solve [simpl; lia].
      destruct tr_sets; try solve [simpl; lia].
      apply Nat.le_le_succ_r.
      apply do_tournament_round_sched_size_2; intuition; discriminate.
    + inversion H.
      inversion H3.
      constructor; apply merge_compatible || apply do_tournament_round_compatible; assumption.
  - rewrite do_tournament'_idem; simpl; try apply dtr_log2_bound.
    eapply IHtr_sets; try reflexivity; simpl.
    + apply Nat.lt_succ_r.
      destruct tr_sets; try solve [simpl; lia].
      destruct tr_sets; try solve [simpl; lia].
      apply Nat.le_le_succ_r.
      apply do_tournament_round_sched_size_2; intuition; discriminate.
    + inversion H.
      inversion H3.
      constructor; try apply do_tournament_round_compatible; assumption.
Qed.

(* Merge transactions into a transaction set. *)
Definition trs_to_set (trs : list transaction) : transaction_set :=
  mkTrSet trs (fold_right set_union empty_set (map ReadSet trs))
          (fold_right set_union empty_set (map WriteSet trs)).
Definition tr_to_set (tr : transaction) : transaction_set := trs_to_set [tr].

Lemma tr_to_set_list_valid : forall ts,
  Forall tr_set_valid (map tr_to_set ts).
Proof.
  induction ts; simpl; trivial; unfold tr_set_valid; repeat (constructor; simpl; trivial).
Qed.
Local Hint Resolve tr_to_set_list_valid : core.

(* Schedule transactions from the first n renamed, via tournament elimination. *)
Definition schedule_transactions (n : nat) (s : pm_state) : pm_state :=
  if 0 <? (length (Scheduled s)) then s else
  let running_set := trs_to_set (Running s) in
  let renamed_trs_as_sets := map tr_to_set (firstn n (Renamed s)) in
  let tournament_result := do_tournament (running_set :: renamed_trs_as_sets) in
  let real_sched := skipn (length (Running s)) (fst tournament_result) in
  mkState (Queued s)
          ((snd tournament_result) ++ skipn n (Renamed s))
          (Scheduled s ++ real_sched)
          (Running s)
          (Finished s).

(* Implementation traces. *)
Inductive pm_trace : pm_state -> list action -> pm_state -> Prop :=
| PmNoop : forall s,
    pm_trace s [] s
| PmAdd : forall s s' s'' tr new_t ts1 ts2,
    pm_trace s' tr s''
    -> Queued s = ts1 ++ ts2
    -> Queued s' = ts1 ++ [new_t] ++ ts2
    -> Renamed s = Renamed s'
    -> Scheduled s = Scheduled s'
    -> Running s = Running s'
    -> Finished s = Finished s'
    -> pm_trace s (Add new_t :: tr) s''
| PmRename : forall s s' s'' tr,
    pm_trace s' tr s''
    -> s' = rename_transaction s
    -> pm_trace s tr s''
| PmSchedule : forall s s' s'' tr n,
    pm_trace s' tr s''
    -> s' = schedule_transactions n s
    -> pm_trace s tr s''
| PmStart : forall s s' s'' tr started_t,
    pm_trace s' tr s''
    -> Queued s = Queued s'
    -> Renamed s = Renamed s'
    -> Scheduled s = started_t :: Scheduled s'
    -> started_t :: Running s = Running s'
    -> Finished s = Finished s'
    -> pm_trace s (Start started_t :: tr) s''
| PmFinish : forall s s' s'' tr finished_t ts1 ts2,
    pm_trace s' tr s''
    -> Queued s = Queued s'
    -> Renamed s = Renamed s'
    -> Scheduled s = Scheduled s'
    -> Running s = ts1 ++ [finished_t] ++ ts2
    -> Running s' = ts1 ++ ts2
    -> finished_t :: Finished s = Finished s'
    -> pm_trace s (Finish finished_t :: tr) s''.

Definition ValidPmState (s : pm_state) :  Prop :=
  List.ForallOrdPairs compatible (Scheduled s)
  /\ List.ForallOrdPairs compatible (Running s)
  /\ List.Forall (fun t => List.Forall (compatible t) (Scheduled s)) (Running s).

Ltac invert_Foralls :=
  repeat (invert_Foralls' || lazymatch goal with
  | H : ForallOrdPairs _ (_ ++ _) |- _ => rewrite FOP_app in H by auto
  | |- ForallOrdPairs _ (_ ++ _) => rewrite FOP_app by auto
  end).

Lemma pm_trace_preserves_ValidPmState : forall tr s s',
  ValidPmState s
  -> pm_trace s tr s'
  -> ValidPmState s'.
Proof.
  unfold ValidPmState; induction 2; intros; trivial; apply IHpm_trace.
  - destruct s; simpl in *; subst; assumption.
  - subst; unfold rename_transaction; destruct (Queued s); destruct s; simpl; trivial.
  - subst; unfold schedule_transactions.
    destruct_with_eqn (0 <? length (Scheduled s)); try assumption; simpl.
    destruct (Scheduled s); simpl in *; try lia.
    pose proof do_tournament_compatible as Hcompat.
    setoid_rewrite <- firstn_skipn at 5 in Hcompat.
    eapply FOP_app in Hcompat; reflexivity || unfold tr_set_valid; invert_Foralls; eauto.
    apply Forall_Forall_comm; auto.
    eapply Forall_impl; try eassumption.
    rewrite do_tournament_first; trivial.
  - destruct s; destruct s'; simpl in *; subst; invert_Foralls; try eapply Forall_impl;
      try eassumption; simpl; intros; invert_Foralls; auto.
  - destruct s; destruct s'; simpl in *; subst; invert_Foralls; auto.
Qed.

Definition R_pm (s : pm_state) (s' : spec_state) : Prop :=
    Permutation (Queued s ++ Renamed s ++ Scheduled s) (SpecQueued s')
    /\ Permutation (Running s) (SpecRunning s')
    /\ ValidPmState s.

Ltac permutations_once :=
  match goal with
  (* Solve trivial permutations. *)
  | |- Permutation ?x ?x => apply Permutation_refl
  | |- Permutation (?x ++ ?y) (?y ++ ?x) => apply Permutation_app_swap
  (* Use relevant hypotheses. *)
  | H : Permutation ?x ?y |- Permutation ?x ?y => assumption
  | H : Permutation ?x _ |- Permutation ?x _ => apply (perm_trans H); clear H
  | H : Permutation _ ?x |- Permutation ?x _ => apply Permutation_sym in H
  | H : Permutation _ ?x |- Permutation _ ?x => apply Permutation_sym
  (* Simplify goal. *)
  | |- Permutation (?x :: _) (?x :: _) => apply perm_skip
  | |- Permutation (?x ++ _) (?x ++ _) => apply Permutation_app_head
  | |- Permutation (_ ++ ?x) (_ ++ ?x) => apply Permutation_app_tail
  (* Rewrite goal into a form that the above tactics can handle. *)
  | |- Permutation (_ ++ _ :: _) _ => rewrite Permutation_app_swap
  | |- Permutation _ (_ ++ ?x :: ?y) => rewrite Permutation_app_swap with (l' := x :: y)
  | |- Permutation (_ ++ ?x) (?x ++ _) => rewrite Permutation_app_swap
  | |- Permutation (_ ++ ?x) (_ ++ ?x ++ _) => rewrite Permutation_app_rot with (l2 := x)
  | |- Permutation (?x ++ _ ++ _) (_ ++ _ ++ ?x) => rewrite Permutation_app_rot
  | |- Permutation (_ ++ (_ :: _) ++ _) _ => rewrite Permutation_app_swap_app
  | |- Permutation (_ ++ _ ++ _ :: _) _ => rewrite Permutation_app_rot
  | |- Permutation _ (_ ++ (?x :: ?y) ++ _) => rewrite Permutation_app_swap_app with (l2 := x :: y)
  | |- Permutation _ _ => match goal with
                          | |- context[(_ ++ _ :: _) ++ _] => rewrite <- app_assoc
                          | |- context[(_ :: _) ++ _] => rewrite <- app_comm_cons
                          | |- context[_ ++ (_ :: _) ++ _] => fail (* break cycles *)
                          | |- context[_ ++ _ ++ _] => rewrite app_assoc
                          end
  end.

Ltac permutations := repeat permutations_once.

Lemma schedule_preserves_R : forall pm_s spec_s n,
    R_pm pm_s spec_s
    -> R_pm (schedule_transactions n pm_s) spec_s.
Proof.
  intros.
  unfold R_pm in *.
  intuition.
  - unfold schedule_transactions.
    destruct (0 <? length (Scheduled pm_s)); trivial.
    simpl.
    permutations.
    rewrite <- firstn_skipn with (n := n) at 1.
    permutations.
    rewrite do_tournament_rest by auto using f_equal.
    rewrite map_map.
    simpl.
    rewrite concat_map_wrap.
    reflexivity.
  - eapply perm_trans; try eassumption.
    unfold schedule_transactions; simpl in *.
    destruct (0 <? length (Scheduled pm_s)); trivial.
  - eapply pm_trace_preserves_ValidPmState in H2; try eassumption.
    eapply PmSchedule; try reflexivity.
    apply PmNoop.
Qed.

Lemma pm_refines_spec' : forall tr pm_s pm_s',
    pm_trace pm_s tr pm_s'
    -> forall spec_s spec_s',
        R_pm pm_s spec_s
        -> R_pm pm_s' spec_s'
        -> spec_trace spec_s tr spec_s'.
Proof.
  induction 1; intros.
  - unfold R_pm in *.
    destruct spec_s; destruct spec_s'.
    simpl in *; intuition.
    constructor; simpl in *; permutations.
  - unfold R_pm in *.
    destruct spec_s; destruct spec_s'; destruct s; destruct s'.
    simpl in *; intuition.
    eapply SpecAdd with (s' := {| SpecQueued := Queued1 ++ Renamed1 ++ Scheduled1;
                                  SpecRunning := Running1 |});
      simpl; subst; try permutations.
    apply IHpm_trace; auto.
  - subst.
    apply IHpm_trace; try assumption.
    unfold R_pm in *; unfold rename_transaction in *. 
    destruct s; destruct spec_s; simpl in *.
    destruct Queued0; simpl in *; intuition.
    permutations.
  - subst.
    apply IHpm_trace; try assumption.
    apply schedule_preserves_R.
    assumption.
  - unfold R_pm in *; unfold ValidPmState in *.
    destruct spec_s; destruct s; destruct s'.
    simpl in *; intuition.
    eapply SpecStart with (s' := {| SpecQueued := Queued1 ++ Renamed1 ++ Scheduled1;
                                    SpecRunning := Running1 |});
      simpl; subst; try permutations.
    + apply IHpm_trace; invert_Foralls; auto.
      intuition; constructor; try assumption; eapply Forall_impl; try eassumption;
        intros; simpl in *; invert_Foralls; auto.
    + eapply Permutation_Forall; try eassumption.
      eapply Forall_impl; try eassumption.
      intros; simpl in *; invert_Foralls; auto.
  - unfold R_pm in *; unfold ValidPmState in *.
    destruct spec_s; destruct s; destruct s'.
    simpl in *; intuition.
    eapply SpecFinish with (s' := {| SpecQueued := Queued1 ++ Renamed1 ++ Scheduled1;
                                      SpecRunning := Running1 |});
      simpl; subst; try permutations.
    apply IHpm_trace; auto.
    intuition; invert_Foralls; auto.
Qed.

(* Main theorem: traces generates by the implementation can be generated by the spec. *)
Theorem pm_refines_spec : forall pm_finish trace,
    pm_trace (mkState [] [] [] [] []) trace pm_finish
    -> exists spec_finish, spec_trace (mkSpecState [] []) trace spec_finish.
Proof.
    intros.
    pose proof (pm_trace_preserves_ValidPmState trace (mkState [] [] [] [] []) pm_finish) as Hv.
    destruct pm_finish.
    exists {| SpecQueued := Queued0 ++ Renamed0 ++ Scheduled0; SpecRunning := Running0 |}.
    eapply pm_refines_spec'; unfold R_pm; eauto; simpl in *;
      repeat (apply Hv || split); auto; constructor.
Qed.
