module Index

open Fable.React
open Fable.React.Props
open Elmish
open Fulma

module Counter =

    type Model =
        { count : int }

    let init() =
        printfn "Counter.init()"
        { count = 0 }, Cmd.none // no initial command

    type Msg =
        | Increment
        | Decrement

    let update msg model : Model * Cmd<Msg> =
        printfn "Counter.update()"
        match msg with
        | Increment ->
            { model with count = model.count + 1 }, Cmd.none

        | Decrement ->
            { model with count = model.count - 1 }, Cmd.none

    let view model dispatch =
        div []
            [ button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ] 
              div [] [ str (model.ToString()) ]
              button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ]]

type Model = { Counters : Counter.Model list }

type ID = int

type Msg =
    | Add
    | Remove
    | Modify of ID * Counter.Msg

let init() =
    printfn "Index.init()"
    { Counters = [] }, Cmd.none

// let updateCorrectCounter (id : ID) (counterMsg : Counter.Msg) (i : int) (counterModel : Counter.Model) : Counter.Model * Cmd<Counter.Msg> =
let updateCorrectCounter (id : ID) (counterMsg : Counter.Msg) (i : int) (counterModel : Counter.Model) : Counter.Model =
    if i = id then
        let model, _ = Counter.update counterMsg counterModel
        model
    else
        counterModel

let update msg model : Model * Cmd<Msg> =
    printfn "Index.update()"
    match msg with
    | Add ->
        let m, _ = Counter.init()
        { Counters = m :: model.Counters }, Cmd.none
    | Remove  ->
        { Counters = 
            match model.Counters with
            | [] -> []
            | x :: rest -> rest}, Cmd.none
    | Modify (id, counterMsg) ->
        let theFunction = updateCorrectCounter id counterMsg
        let counters = model.Counters |> List.mapi (fun i counterModel -> theFunction i counterModel )
        { Counters = counters }, Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
    let counterDispatch i msg = dispatch (Modify (i, msg))

    let counters = model.Counters |> List.mapi (fun i counter -> Counter.view counter (counterDispatch i))

    Hero.hero [
        Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ ]
            ]
        ]

        Hero.body [ ] [
            Container.container [ ] [
                Column.column [
                    Column.Width (Screen.All, Column.Is6)
                    Column.Offset (Screen.All, Column.Is3)
                ] [
                    yield button [ OnClick (fun _ -> dispatch Remove) ] [ str "Remove" ]
                    yield button [ OnClick (fun _ -> dispatch Add) ] [ str "Add" ]
                    yield! counters
                ]
            ]
        ]
    ]
