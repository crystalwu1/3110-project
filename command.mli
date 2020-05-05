open Graphics
open Board
open Game
open State

open Unix

exception NoKeyPress

val keyboard : Game.t -> State.t -> (State.t)

val keyboard2 : Game.t -> State.t -> (State.t)

val end_keyboard : unit -> bool


