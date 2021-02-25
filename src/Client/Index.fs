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

    let update msg model =
        printfn "Counter.update()"
        match msg with
        | Increment ->
            { model with count = model.count + 1 }, Cmd.none

        | Decrement ->
            { model with count = model.count - 1 }, Cmd.none

    let view model dispatch =
        div []
            [ button [ OnClick (fun _ -> dispatch Decrement) ] [ str "-" ]
              div [] [ str (model.ToString()) ]
              button [ OnClick (fun _ -> dispatch Increment) ] [ str "+" ] ]

type Model =
    {
        top : Counter.Model
        bottom : Counter.Model }

type Msg =
    | Reset
    | Top of Counter.Msg
    | Bottom of Counter.Msg

let init() =
    printfn "Index.init()"
    let top, topCmd = Counter.init()
    let bottom, bottomCmd = Counter.init()
    { top = top
      bottom = bottom },
    Cmd.batch [ Cmd.map Top topCmd
                Cmd.map Bottom bottomCmd ]

let update msg model : Model * Cmd<Msg> =
    printfn "Index.update()"
    match msg with
    | Reset ->
        let top, topCmd = Counter.init()
        let bottom, bottomCmd = Counter.init()
        { top = top
          bottom = bottom },
        Cmd.batch [ Cmd.map Top topCmd
                    Cmd.map Bottom bottomCmd ]
    | Top msg' ->
            let res, cmd = Counter.update msg' model.top
            { model with top = res }, Cmd.map Top cmd

    | Bottom msg' ->
        let res, cmd = Counter.update msg' model.bottom
        { model with bottom = res }, Cmd.map Bottom cmd

let view (model : Model) (dispatch : Msg -> unit) =
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
                    yield Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "safe_azure_ad" ]
                    yield button [ OnClick (fun _ -> dispatch Reset) ] [ str "Reset" ]
                    yield Counter.view model.top (Top >> dispatch)
                    yield Counter.view model.bottom (Bottom >> dispatch)
                ]
            ]
        ]
    ]
