module Index

open Elmish
open Fable.React
open Fable.React.Props

type Model = int

type Msg =
    | Increment
    | Decrement

let init(): Model * Cmd<Msg> =
    0, Cmd.none

let update (msg:Msg) (model:Model) : Model * Cmd<Msg> =
    match msg with
    | Increment ->
        model + 1, Cmd.none
    | Decrement ->
        model - 1, Cmd.none

let view model dispatch =
    div []
        [ button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ]
          div [] [ str (model.ToString()) ]
          button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ] ]