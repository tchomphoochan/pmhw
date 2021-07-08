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
    -> List.Forall (compatible started_t) (SpecRunning s')
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

Definition tr_to_set (tr : transaction) : transaction_set := mkTrSet  [tr] (ReadSet tr) (WriteSet tr).

Definition set_compatible (a : transaction_set) (b : transaction_set) :=
    andb  (set_eq (set_inter (SetReadSet a) (SetWriteSet b)) empty_set)
    (andb (set_eq (set_inter (SetWriteSet a) (SetWriteSet b)) empty_set)
          (set_eq (set_inter (SetWriteSet a) (SetReadSet b)) empty_set)).

(* Single round of the tournament. *)
Fixpoint tournament_round (source : list transaction_set) (target : list transaction_set) : list transaction_set * list transaction :=
    match source with
    | t1 :: t2 :: rest => match set_compatible t1 t2 with
                          | true => tournament_round rest (merge_tr_sets t1 t2 :: target)
                          | false => let (sched, rem) := tournament_round rest (t1 :: target) in (sched, (SetTransactions t2) ++ rem)
                          end
    | t1 :: nil => (t1 :: target, nil)
    | nil => (target, nil)
    end.

(* Do at most `rounds_left` rounds of the tournament. *)
Fixpoint do_tournament (trs : list transaction_set * list transaction) (rounds_left : nat) : list transaction * list transaction :=
    match rounds_left with
    | 0 => match trs with
           | (nil, rem) => (nil, rem)
           | (head :: rest, rem) => (SetTransactions head, (concat (map SetTransactions rest)) ++ rem)
           end
    | S n => let (sched, rem) := do_tournament (tournament_round (fst trs) nil) n in (sched, rem ++ snd trs)
    end.

(* Wrapper around do_tournament that calculates number of needed rounds. *)
Definition tournament_schedule (trs : list transaction) : list transaction * list transaction :=
    do_tournament ((map tr_to_set trs), nil) (Nat.log2 (length trs) + 1).

(* Scheduling step. *)
Definition schedule_transactions (n : nat) (s : pm_state) : pm_state :=
    match tournament_schedule (firstn n (Renamed s)) with
    | (nil, _) => s
    | (sched, rem) => mkState (Queued s)
                      (rem ++ skipn n (Renamed s))
                      (Scheduled s ++ sched)
                      (Running s)
                      (Finished s)
    end.

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

Definition R_pm (s : pm_state) (s' : spec_state) : Prop :=
    Permutation (Queued s ++ Renamed s ++ Scheduled s) (SpecQueued s')
    /\ Permutation (Running s) (SpecRunning s').

Lemma pm_refines_spec' : forall tr pm_s pm_s',
    pm_trace pm_s tr pm_s'
    -> forall spec_s spec_s',
        R_pm pm_s spec_s
        -> R_pm pm_s' spec_s'
        -> spec_trace spec_s tr spec_s'.
Proof.
Admitted.

(* Main theorem: traces generates by the implementation can be generated by the spec. *)
Theorem pm_refines_spec : forall pm_finish trace,
    pm_trace (mkState [] [] [] [] []) trace pm_finish
    -> exists spec_finish, spec_trace (mkSpecState [] []) trace spec_finish.
Proof.
    intros.
    destruct pm_finish.
    exists {| SpecQueued := Queued0 ++ Renamed0 ++ Scheduled0; SpecRunning := Running0 |}.
    eapply pm_refines_spec'; unfold R_pm; eauto.
Qed.
