Require Import Coq.Lists.List.

(* Transaction type. *)
Record transaction := {
    WriteSet : list nat;
    ReadSet : list nat;
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
  -> Finished state = nil
  -> pm_final state.

(* Renaming step. TODO: pick a transaction non-deterministically. *)
Definition rename_transaction (state : pm_state) : pm_state :=
    match Queued state with
    | nil => state
    | t :: rest => mkState rest (t :: Renamed state) (Scheduled state) (Running state) (Finished state)
    end.

Definition tournament_state := list (list transaction).

Fixpoint tournament_schedule tournament_state (n : nat) : tournament_state.
Admitted.

Definition schedule_transactions (n : nat) (state : pm_state) : pm_state.
Admitted.

Inductive pm_step : pm_state -> pm_state -> Prop :=
| PmStepRename : forall state, pm_step state (rename_transaction state).
