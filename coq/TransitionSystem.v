Require Import Coq.Arith.PeanoNat.
Require Import Coq.Bool.Bool.
Require Import Coq.Lists.List.

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
    | [] => b
    | a_elt :: a' => match b with
                   | [] => []
                   | b_elt :: b' => if a_elt <? b_elt then set_inter a' b
                                   else if a_elt =? b_elt then a_elt :: set_inter a' b'
                                   else set_inter' b'
                   end
    end
    in set_inter' b.

(* Transaction type. *)
Record transaction := {
    WriteSet : obj_set;
    ReadSet : obj_set;
}.

(* State for our transition system. *)
Record pm_state := mkState {
    Queued : list transaction;
    Renamed : list transaction;
    Scheduled : list transaction;
    Running : list transaction;
    Finished : list transaction;
}.

(* Initial states: only queued transactions. *)
Inductive pm_init : pm_state -> Prop :=
| PmInit : forall state,
  Renamed state = nil
  -> Scheduled state = nil
  -> Running state = nil
  -> Finished state = nil
  -> pm_init state.

(* Final states: only finished transactions. *)
Inductive pm_final : pm_state -> Prop :=
| PmFinal : forall state,
  Queued state = nil
  -> Renamed state = nil
  -> Scheduled state = nil
  -> Running state = nil
  -> pm_final state.

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

Definition tr_compatible (a : transaction_set) (b : transaction_set) :=
    andb  (set_eq (set_inter (SetReadSet a) (SetWriteSet b)) empty_set)
    (andb (set_eq (set_inter (SetWriteSet a) (SetWriteSet b)) empty_set)
          (set_eq (set_inter (SetWriteSet a) (SetReadSet b)) empty_set)).

(* Single round of the tournament. *)
Fixpoint tournament_round (source : list transaction_set) (target : list transaction_set) : list transaction_set * list transaction :=
    match source with
    | t1 :: t2 :: rest => match tr_compatible t1 t2 with
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

(* Full transition system. *)
Inductive pm_step : pm_state -> pm_state -> Prop :=
| PmStepRename : forall state, pm_step state (rename_transaction state)
| PmStepSchedule : forall state n, pm_step state (schedule_transactions n state).
