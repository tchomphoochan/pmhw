Require Import Coq.Arith.PeanoNat.
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

Ltac destruct_ifs :=
    repeat match goal with
    | |- _ = (if (?x <? ?y) then _ else _) => destruct_with_eqn (x <? y)
    | |- (if (?x <? ?y) then _ else _) = _ => destruct_with_eqn (x <? y)
    | |- _ = (if (?x =? ?y) then _ else _) => destruct_with_eqn (x =? y)
    | |- (if (?x =? ?y) then _ else _) = _ => destruct_with_eqn (x =? y)
    end; try lia.

Lemma set_inter_sym : forall s1 s2,
    set_inter s1 s2 = set_inter s2 s1.
Proof.
  induction s1; induction s2; simpl in *; destruct_ifs; try rewrite IHs1; f_equal; intuition.
Qed.

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

Definition merge_tr_sets (a: transaction_set) (b : transaction_set) : transaction_set :=
    mkTrSet ((SetTransactions a) ++ (SetTransactions b)) (set_union (SetReadSet a) (SetReadSet b)) (set_union (SetWriteSet a) (SetWriteSet b)).

Definition set_compatible (a : transaction_set) (b : transaction_set) :=
    andb  (set_eq (set_inter (SetReadSet a) (SetWriteSet b)) empty_set)
    (andb (set_eq (set_inter (SetWriteSet a) (SetWriteSet b)) empty_set)
          (set_eq (set_inter (SetWriteSet a) (SetReadSet b)) empty_set)).

(* Single round of the tournament. Returns (merged sets, filtered out transactions).
   The sets in the input are merged pairwise if they are compatible, or the first one
   is kept and the transactions from the second one are appended to the second list. *)
Fixpoint do_tournament_round (tr_sets : list transaction_set) :
                             list transaction_set * list transaction :=
    match tr_sets with
    | t1 :: t2 :: rest => match set_compatible t1 t2 with
                          | true => let (sched, rem) := do_tournament_round rest
                                    in (merge_tr_sets t1 t2 :: sched, rem)
                          | false => let (sched, rem) := do_tournament_round rest
                                     in (t1 :: sched, (SetTransactions t2) ++ rem)
                          end
    | t1 :: [] => ([t1], [])
    | [] => ([], [])
    end.

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
    | S n => let (sched_round, rem_round) := do_tournament_round tr_sets in
             let (sched, rem) := do_tournament' sched_round n in
             (sched, rem ++ rem_round)
    end.
Definition do_tournament (tr_sets : list transaction_set) :
                         list transaction * list transaction :=
  do_tournament' tr_sets (Nat.log2 (length tr_sets) + 1).

(* Merge transactions into a transaction set. *)
Fixpoint trs_to_set (trs : list transaction) : transaction_set :=
  match trs with
  | [] => mkTrSet [] [] []
  | head :: rest => let rest_set := trs_to_set rest in
                    mkTrSet (head :: (SetTransactions rest_set))
                            (set_union (ReadSet head) (SetReadSet rest_set))
                            (set_union (WriteSet head) (SetWriteSet rest_set))
  end.
Definition tr_to_set (tr : transaction) : transaction_set := trs_to_set [tr].

(* Schedule transactions from the first n renamed, via tournament elimination. *)
Definition schedule_transactions (n : nat) (s : pm_state) : pm_state :=
  if 0 <? (length (Scheduled s)) then s else
  let running_set := trs_to_set (Running s) in
  let renamed_trs_as_sets := map tr_to_set (firstn n (Renamed s)) in
  let (sched, rem) := do_tournament (running_set :: renamed_trs_as_sets) in
  let real_sched := skipn (length (Running s)) sched in
  mkState (Queued s)
          (rem ++ skipn n (Renamed s))
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

(* Helper lemma to help manipulate ValidPmState. *)
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
    apply Forall_forall.
    trivial.
  - rewrite <- app_comm_cons.
    constructor; intros.
    + inversion H0.
      apply IHl1 in H4; try assumption.
      apply Forall_app in H3; intuition.
      * constructor; assumption.
      * rewrite Forall_forall in *.
        intuition.
    + intuition.
      inversion H1.
      constructor.
      * apply Forall_app; intuition.
        eapply Forall_impl; try eassumption.
        intuition.
        inversion H7.
        intuition.
      * apply IHl1; intuition.
        eapply Forall_impl; try eassumption.
        intuition.
        inversion H7.
        assumption.
Qed.

Lemma pm_trace_preserves_ValidPmState : forall tr s s',
  ValidPmState s
  -> pm_trace s tr s'
  -> ValidPmState s'.
Proof.
Admitted.

Definition R_pm (s : pm_state) (s' : spec_state) : Prop :=
    Permutation (Queued s ++ Renamed s ++ Scheduled s) (SpecQueued s')
    /\ Permutation (Running s) (SpecRunning s')
    /\ ValidPmState s.

Lemma schedule_preserves_R : forall pm_s spec_s n,
    R_pm pm_s spec_s
    -> R_pm (schedule_transactions n pm_s) spec_s.
Proof.
Admitted.

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
  | |- Permutation (_ ++ ?x) (_ ++ ?x) => apply Permutation_app_tail
  (* Rewrite goal into a form that the above tactics can handle. *)
  | |- Permutation (_ ++ _ :: _) _ => rewrite Permutation_app_swap
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

Ltac invert_Foralls :=
  repeat lazymatch goal with
  | H : Forall _ (_ :: _) |- _ => inversion_clear H
  | H : Forall _ (_ ++ _) |- _ => rewrite Forall_app in H; inversion_clear H
  | H : ForallOrdPairs _ (_ :: _) |- _ => inversion_clear H
  | H : ForallOrdPairs _ (_ ++ _) |- _ => rewrite FOP_app in H by auto; decompose [and] H; clear H
  end.

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
  - apply IHpm_trace; try assumption.
    unfold R_pm in *; unfold rename_transaction in *. 
    destruct s; destruct s'; destruct spec_s; simpl in *.
    destruct Queued0; simpl in *; inversion H0; subst; intuition.
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
    + apply IHpm_trace; auto.
      invert_Foralls.
      intuition; constructor; try assumption; eapply Forall_impl; try eassumption;
        intros; simpl in *; invert_Foralls; auto.
    + apply (Permutation_Forall H6).
      eapply Forall_impl; try eassumption.
      intros; simpl in *; invert_Foralls; auto.
  - unfold R_pm in *; unfold ValidPmState in *.
    destruct spec_s; destruct s; destruct s'.
    simpl in *; intuition.
    eapply SpecFinish with (s' := {| SpecQueued := Queued1 ++ Renamed1 ++ Scheduled1;
                                      SpecRunning := Running1 |});
      simpl; subst; try permutations.
    apply IHpm_trace; auto.
    intuition; rewrite FOP_app || rewrite Forall_app; invert_Foralls; auto.
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
