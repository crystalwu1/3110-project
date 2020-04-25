open Graphics
open Board
open Game
open State

exception NoKeyPress

val keyboard : Game.t -> State.t -> (State.t)


