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
        answer : Answer }
        

type alias Model = {
        screen : Screen,
        version : String,  
        objects : List Prompt,
        rts : List Time,
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

      
init : ( Model, Cmd Msg )
init =
    ( { screen = VersionChooser
      , version = ""
      , objects = [Prompt "platipus" Animal, Prompt "daisy" Plant, Prompt "brick" Other ]
      , rts = []
      , reactions = []
      , startTime = 0
      , lastAnswer = Other }  
    , Cmd.none )



---- UPDATE ----


nextObjectUpdate : Model -> Time -> (Model, Cmd Msg )
nextObjectUpdate model time =
    
    let
        rt = (time - model.startTime)
        correct = (.answer (Maybe.withDefault (Prompt "" Animal) (List.head model.objects))) == model.lastAnswer -- not pretty TODO
        newObjects = List.drop 1 model.objects
        newReaction = Reaction (round (time - model.startTime)) correct
        newScreen = if List.length model.objects > 1
                    then
                        Experiment
                    else
                        Summary
                      
    in
        ( { model | screen = newScreen,
                    objects = newObjects,
                    rts = (time - model.startTime) :: model.rts,
                    reactions = newReaction :: model.reactions,
                    startTime = time},
             Cmd.none )
    

      
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartVersion newVersion ->
            ( { model | version = newVersion}, Task.perform StartTimeAt Time.now )
        StartTimeAt time ->
            ( { model | startTime = time, screen = Experiment }, Cmd.none )
        StopTime answer ->
            ( { model | lastAnswer = answer}, Task.perform NextObject Time.now) 
        NextObject time ->
            nextObjectUpdate model time
        Restart ->
            init
                 



---- VIEW ----


check : Html msg
check = span [class "check" ] []

cross : Html msg
cross = span [class "cross"] []
        
                     

formatReaction : Reaction -> Html msg
formatReaction reaction =
    case reaction.correct of
        True -> span [] [check, text (toString reaction.rt)]
        False -> span [] [cross, text (toString reaction.rt)]
                
computeAverages : List Reaction -> ( Int, Int)
computeAverages reactionList =
    let
        (right, wrong) = List.partition .correct reactionList
    in
        (
         round ( toFloat (List.sum (List.map .rt right)) /
                 toFloat( List.length right)),
         round ( toFloat (List.sum (List.map .rt wrong)) /
                 toFloat( List.length wrong))
        )

computeAccuracy : List Reaction -> String
computeAccuracy reactionList =
    toString ( round (
            toFloat ( List.length ( List.filter .correct reactionList) * 100 ) /
            toFloat ( List.length reactionList))) ++
    "% "
    
                 
versionChooserScreen : Model -> Html Msg
versionChooserScreen model =
    div [] [
         Html.h1 [] [ text "Choose a version to start" ] ,
         div [] [
              Html.button [ onClick ( StartVersion "versionA" ) ] [ text "Version A" ] ] ,
         div [] [
              Html.button [ onClick ( StartVersion "versionB" ) ] [ text "Version B" ] ] ]

        
experimentScreen : Model -> Html Msg
experimentScreen model =
    let
        promptName = .name (Maybe.withDefault (Prompt "" Other)  ( List.head model.objects ))
    in
        div [ class model.version ] [
             Html.h1 [class "stimulus"] [ text promptName ] ,
                 span [ class "buttonAnimal" ] [
                      Html.button [ onClick (StopTime Animal) ] [ text "Animal" ]  ] ,
                 span [ class "buttonPlant" ] [
                      Html.button [ onClick (StopTime Plant) ] [ text "Plant" ]  ] ,
                 span [ class "buttonOther" ] [
                      Html.button [ onClick (StopTime Other) ] [ text "Other"  ]  ]
                             ]
        
summaryScreen : Model -> Html Msg 
summaryScreen model =
    let
        averageRT = round ((List.sum model.rts ) / toFloat (List.length model.rts))
        listRTs = String.join "," ( List.reverse ( List.map toString model.rts ))
        formattedReactions = List.reverse (List.map formatReaction model.reactions )
        (rightM, wrongM) = computeAverages model.reactions
                  
    in 
        div [] [
            Html.h1 [] [ text "Summary" ] ,
            div [] [
                  p  [] [text "Accuracy:  " ,
                         text (computeAccuracy model.reactions) ],
                  p  [] [text "Average reaction times: ",
                         check,  text ( toString rightM),
                         cross, text( toString wrongM ) ]
                ],
            div [ class "rtblock" ] [
                  p [] [ text "List of reaction times: "],
                  p [] formattedReactions  
                ],
            div [] [
                 Html.button [ onClick Restart ] [ text "Restart" ]]
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
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
