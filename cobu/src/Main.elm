module Main exposing (..)
import Task exposing (..)
import Html exposing (Html, text, div, img)
-- import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Time exposing (now, Time)

---- MODEL ----


type alias Model = {
        screen : Screen,
        version : String,  -- TODO ignored for now
        objects : List String,
        rts : List Time,
        startTime : Time}    


init : ( Model, Cmd Msg )
init =
    ( { screen = VersionChooser
      , version = ""
      , objects = [ "platypus", "daisy", "brick"  ]
      , rts = []
      , startTime = 0 }
    , Cmd.none )



---- UPDATE ----


type Msg 
    = StartVersion String | StartTimeAt Time | StopTime  | NextObject Time

type Screen
    = VersionChooser | Experiment | Summary
      
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartVersion versionLetter ->
            ( { model | version = versionLetter}, Task.perform StartTimeAt Time.now )
        StartTimeAt time ->
            ( { model | startTime = time, screen = Experiment }, Cmd.none )
        StopTime ->
            (model, Task.perform NextObject Time.now )
        NextObject time ->
            if List.length model.objects > 1
                then
                    ( { model | screen = Experiment
                                     , objects = (List.drop 1 model.objects)
                                     , rts = (time - model.startTime) :: model.rts
                                     , startTime = time}
                      , Cmd.none )
                else
                    ( { model | screen = Summary
                                     , objects = [ "finished" ] 
                                     , rts = (time - model.startTime) :: model.rts
                                     , startTime = 0}
                      , Cmd.none )

                 



---- VIEW ----
 

view : Model -> Html Msg
view model =

    case model.screen of
        VersionChooser ->
                    div [] [ Html.h1 [] [ text "Choose a version to start" ] 
                           , div [] [ Html.button [ onClick ( StartVersion "A" ) ] [ text "Version A" ] ]
                           , div [] [ Html.button [ onClick ( StartVersion "B" ) ] [ text "Version B" ] ] ]
        Summary -> 
                            div [] [ Html.h1 [] [ text "All done Summary" ]
                                   , div [] [ text ( "Average reaction time " ++ toString ( round ( (List.sum model.rts ) / toFloat (List.length model.rts)))) ]
                                   , div [] [ text ( "List of reaction times: " ++ ( String.join ","    ( List.reverse ( List.map toString model.rts )))) ]
                                   ]
        Experiment ->
                    div [] [ Html.h1 [] [ text ( Maybe.withDefault "<oops>" ( List.head model.objects )) ]  
                           , div [] [ Html.button [ onClick StopTime ] [ text "Animal" ]  ]
                           , div [] [ Html.button [ onClick StopTime ] [ text "Plant" ]  ]
                           , div [] [ Html.button [ onClick StopTime ] [ text "Other"  ]  ]
                           ]
                                                     
                    



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
