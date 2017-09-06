module Main exposing (..)
import Task exposing (..)
import Html exposing (Html, text, div, img, span, p)
import Html.Attributes exposing (src, class, style)
import Html.Events exposing (onClick)
import Time exposing (now, Time)

type Answer =
    Animal | Plant | Other

type alias Prompt = {
        name : String,
        answer : Answer}

type alias Model = {
        screen : Screen,
        version : String,
        objects : List Prompt,
        reactions : List Reaction,
        startTime : Time,
        lastAnswer : Answer}  -- should be Maybe Answer

type Msg
    = StartVersion String | StartTimeAt Time | StopTime  Answer | NextObject Time | Restart

type Screen
    = VersionChooser | Experiment | Summary

type alias Reaction = {
        rt : Int,
        correct : Bool }

---- MODEL ----

wordlist = [
    Prompt "chair" Other,
    Prompt "sage" Plant,
    Prompt "brick" Other,
    Prompt "saguaro" Plant,
    Prompt "elephant" Animal,
    Prompt "ivy" Plant,
    Prompt "meerkat" Animal,
    Prompt "willow" Plant,
    Prompt "helicopter" Other,
    Prompt "fern" Plant,
    Prompt "globe" Other,
    Prompt "watch" Other,
    Prompt "motorcycle" Other,
    Prompt "mouse" Animal,
    Prompt "pointsettia" Plant,
    Prompt "boomerang" Other,
    Prompt "ostrich" Animal,
    Prompt "lamp" Other,
    Prompt "racoon" Animal,
    Prompt "conifer" Plant,
    Prompt "lion" Animal,
    Prompt "peacock" Animal,
    Prompt "camel" Animal,
    Prompt "elm" Plant,
    Prompt "birch" Plant,
    Prompt "agenda" Other,
    Prompt "palm" Plant,
    Prompt "lemur" Animal,
    Prompt "gear" Other  ]

instructions = """
In this experiment you will be asked to read thirty words.  Your task: To decide
if each word is an animal, plant or is something else (other) and press the “Animal”,
“Plant” or “Other” button as fast as you can.

For example, if the word is “oak” you would press on the “Plant” button.  If the
word was “wall” you would press on the “Other” button.  Please try to make your
response as fast as possible while remaining as accurate as you can.

Your data will be anonymous and will not be associated with you or your name in any way.
Thanks in advance for participating!
"""

init : (Model, Cmd Msg)
init =
    ({
        screen = VersionChooser,
        version = "",
        objects = wordlist,
        reactions = [],
        startTime = 0,
        lastAnswer = Other
      },
      Cmd.none)

---- UPDATE ----

nextObjectUpdate : Model -> Time -> (Model, Cmd Msg)
nextObjectUpdate model time =
    let
        rt = (time - model.startTime)
        correct = (.answer (Maybe.withDefault (Prompt "" Animal) (List.head model.objects))) == model.lastAnswer -- not pretty TODO
        newObjects = List.drop 1 model.objects
        newReaction = Reaction (round (time - model.startTime)) correct
        newScreen = if List.length model.objects > 1
                    then Experiment else Summary
    in
        ({ model | screen = newScreen,
                    objects = newObjects,
                    reactions = newReaction :: model.reactions,
                    startTime = time},
             Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        StartVersion newVersion ->
            ({ model | version = newVersion},
              Task.perform StartTimeAt Time.now)
        StartTimeAt time ->
            ({ model | startTime = time, screen = Experiment },
              Cmd.none)
        StopTime answer ->
            ({ model | lastAnswer = answer},
              Task.perform NextObject Time.now)
        NextObject time ->
            nextObjectUpdate model time
        Restart ->
            init


---- VIEW ----

check : Html msg
check = span [class "check"] []

cross : Html msg
cross = span [class "cross"] []

formatReaction : Reaction -> Html msg
formatReaction reaction =
    case reaction.correct of
        True ->
          span [] [check, text (toString reaction.rt)]
        False ->
          span [] [cross, text (toString reaction.rt)]

computeAverages : List Reaction -> (Int, Int)
computeAverages reactionList =
    let
        (right, wrong) = List.partition .correct reactionList
    in
        (
         round (toFloat (List.sum (List.map .rt right)) /
                 toFloat (List.length right)),
         round (toFloat (List.sum (List.map .rt wrong)) /
                 toFloat (List.length wrong))
        )

computeAccuracy : List Reaction -> String
computeAccuracy reactionList =
    toString (round (
            toFloat (List.length (List.filter .correct reactionList) * 100) /
            toFloat (List.length reactionList))) ++
    "% "

versionChooserScreen : Model -> Html Msg
versionChooserScreen model =
    div [] [
         Html.h1 [] [text "Choose a version to start"] ,
          p [class "instructions"] [text instructions],
         div [] [
              Html.button [onClick (StartVersion "versionA")] [text "Version A"]] ,
         div [] [
              Html.button [onClick (StartVersion "versionB")] [text "Version B"]]]

experimentScreen : Model -> Html Msg
experimentScreen model =
    let
        promptName = .name (Maybe.withDefault (Prompt "" Other)  (List.head model.objects))
    in
        div [class model.version] [
           Html.h1 [class "stimulus"] [text promptName] ,
           div [class "container"] [
                 span [class "buttonAnimal"] [
                      Html.button [onClick (StopTime Animal)] [text "Animal"]] ,
                 span [class "buttonPlant"] [
                      Html.button [onClick (StopTime Plant)] [text "Plant"]] ,
                 span [class "buttonOther"] [
                      Html.button [onClick (StopTime Other)] [text "Other"]]
            ]]

summaryScreen : Model -> Html Msg
summaryScreen model =
    let
        formattedReactions = List.reverse (List.map formatReaction model.reactions)
        (rightM, wrongM) = computeAverages model.reactions

    in
        div [] [
            Html.h1 [] [text "Summary"] ,
            div [] [
                  p  [] [text "Accuracy:  " ,
                         text (computeAccuracy model.reactions)],
                  p  [] [text "Average reaction times: ",
                         span [style [("white-space", "nowrap")]] [ 
                             check,  text (toString rightM),
                             cross, text (toString wrongM)]]
            ],
            div [class "rtblock"] [
                  p [] [text "List of reaction times: "],
                  p [] formattedReactions
            ],
            div [] [
                 Html.button [onClick Restart] [text "Restart"]]
        ]


view : Model -> Html Msg
view model =
    case model.screen of
        VersionChooser ->
            versionChooserScreen model
        Summary ->
            summaryScreen model
        Experiment ->
            experimentScreen model


---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program {
        view = view,
        init = init,
        update = update,
        subscriptions = always Sub.none
        }
