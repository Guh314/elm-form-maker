module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main = 
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Question 
    = TextQuestion Title Answer
    | CheckboxQuestion Title CheckboxAnswer


type alias Title = String
type alias Answer = String
type alias CheckboxAnswer = Bool



{-
    Change tail in type Form from List to Array to 
    reference and answer a given 
    question 
-}
type alias Form =
    { head : Question
    , tail : List Question
    , draft : String
    }


init : Form
init =
    { head = TextQuestion "" "" 
    , tail = []
    , draft = ""
    }


type Msg
    = Answer Title Answer
    | DraftQuestion String
    | CreateQuestion Kind String


type Kind
    = Text
    | Check


update : Msg -> Form -> Form
update msg model =
    case msg of 
        Answer titulo answer->
            model

        DraftQuestion d ->
            { model | draft = d }

        CreateQuestion k q->
            case k of
                Text ->
                    { model | tail = List.append model.tail [ TextQuestion q "" ] }

                Check ->
                    { model | tail = List.append model.tail [ CheckboxQuestion q True ] }


view : Form -> Html Msg
view model =
    div []
        [ text "Make a form of questions"
        , div []
            [ text "Insert your text question: "
            , input [ type_ "input", onInput DraftQuestion ] []
            , button [ onClick (CreateQuestion Text model.draft) ] [ text "Submit" ]
            ]
        , div []
            [ text "Insert your checkbox question: "
            , input [ type_ "input", onInput DraftQuestion ] []
            , button [ onClick (CreateQuestion Check model.draft) ] [ text "Submit" ]
            ]
        , text "Here are the questions."
        , div []
            (List.map showQuestion (model.head :: model.tail))
        ]


showQuestion : Question -> Html msg
showQuestion question =
    case question of 
        TextQuestion q a ->
            div [] 
                [ text (q ++ " ")
                , input [ type_ "input" ] []
                ]

        CheckboxQuestion q a ->
            div []
                [ text (q ++ " ")
                , text "Yes"
                , input [ type_ "checkbox" ] []
                , text "No"
                , input [ type_ "checkbox" ] []
                ]

