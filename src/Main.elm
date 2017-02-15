module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }


type Msg
    = AddPiece Int
    | Restart


type Piece
    = Empty
    | Player1
    | Player2


type Player
    = P1
    | P2


type alias Model =
    { rows : List (List Piece)
    , turn : Player
    , winner : Maybe Player
    }


initRows : List (List Piece)
initRows =
    List.range 1 6
        |> List.map
            (\_ ->
                List.range 1 7
                    |> List.map (always Empty)
            )


initModel : Model
initModel =
    Model initRows P1 Nothing


view model =
    div []
        [ div [ class "text-center" ] [ button [ class "btn btn-primary", onClick Restart ] [ text "New Game" ] ]
        , div [ class "text-center" ] [ text (winnerText model.winner) ]
        , div [ class "col-xs-12 col-sm-7 col-sm-offset-4" ]
            [ div [] <|
                (model.rows
                    |> List.map
                        (\row ->
                            div [ class "row" ] <|
                                List.indexedMap
                                    (\colIndex piece ->
                                        div [ class "col-xs-1", outerPieceStyle, onClick (AddPiece colIndex) ]
                                            [ div [ pieceStyle piece ]
                                                []
                                            ]
                                    )
                                    row
                        )
                )
            ]
        ]


winnerText : Maybe Player -> String
winnerText winner =
    case winner of
        Just player ->
            case player of
                P1 ->
                    "Player 1 wins!"

                p2 ->
                    "Player 2 wins!"

        Nothing ->
            ""


pieceStyle piece =
    let
        pieceColor =
            case piece of
                Empty ->
                    "white"

                Player1 ->
                    "blue"

                Player2 ->
                    "red"
    in
        style
            [ ( "background", pieceColor )
            , ( "width", "100%" )
            , ( "height", "100%" )
            , ( "position", "absolute" )
            , ( "left", "0" )
            , ( "top", "0" )
            , ( "border-radius", "50%" )
            ]


outerPieceStyle =
    style
        [ ( "width", "50px" )
        , ( "height", "50px" )
        , ( "background-color", "black" )
        , ( "border", "1px solid black" )
        , ( "position", "relative" )
        ]


playerToPiece : Player -> Piece
playerToPiece player =
    case player of
        P1 ->
            Player1

        P2 ->
            Player2


getPiece : Int -> Int -> List (List Piece) -> Piece
getPiece colIndex rowIndex rows =
    rows
        |> List.drop rowIndex
        |> List.head
        |> Maybe.withDefault []
        |> List.drop colIndex
        |> List.head
        |> Maybe.withDefault Empty


checkFourPieces : Bool -> Bool -> Piece -> List (List Piece) -> Bool
checkFourPieces checkRows checkCols playerPiece rows =
    {- If you want to check just horizontal four pieces to see if there are
       any matches, then you would call it like this:
           `checkFourPieces True False playerPiece rows`
       If you want to check diagonals, you set both checkRows and checkCols to True.
    -}
    let
        ( rowsInt, rowEnd ) =
            if checkRows then
                ( 1, List.length rows - 4 )
            else
                ( 0, List.length rows - 1 )

        ( colsInt, colEnd ) =
            let
                colLength =
                    rows
                        |> List.head
                        |> Maybe.withDefault []
                        |> List.length
            in
                if checkCols then
                    ( 1, colLength - 4 )
                else
                    ( 0, colLength - 1 )
    in
        List.range 0 rowEnd
            |> List.any
                (\rowIndex ->
                    List.range 0 colEnd
                        |> List.any
                            (\colIndex ->
                                let
                                    fourPieces =
                                        [ getPiece colIndex rowIndex rows
                                        , getPiece (colIndex + 1 * colsInt) (rowIndex + 1 * rowsInt) rows
                                        , getPiece (colIndex + 2 * colsInt) (rowIndex + 2 * rowsInt) rows
                                        , getPiece (colIndex + 3 * colsInt) (rowIndex + 3 * rowsInt) rows
                                        ]

                                    {- We have to also check the right diagonal four pieces. If we exclude
                                       this right diagonal check, then the fourPieces variable only checks
                                       the left diagonal.
                                    -}
                                    rightDiagonalFourPieces =
                                        [ getPiece (colIndex + 3) rowIndex rows
                                        , getPiece (colIndex + 2) (rowIndex + 1) rows
                                        , getPiece (colIndex + 1) (rowIndex + 2) rows
                                        , getPiece colIndex (rowIndex + 3) rows
                                        ]
                                in
                                    (fourPieces |> List.all (\piece -> piece == playerPiece))
                                        || (rightDiagonalFourPieces |> List.all (\piece -> piece == playerPiece))
                            )
                )


isWin : List (List Piece) -> Player -> Bool
isWin rows player =
    {- Checks if the player passed in got four in a row. -}
    let
        playerPiece =
            playerToPiece player

        horizontalWin =
            checkFourPieces True False playerPiece rows

        verticalWin =
            checkFourPieces False True playerPiece rows

        diagonalWin =
            checkFourPieces True True playerPiece rows
    in
        horizontalWin || verticalWin || diagonalWin


update msg model =
    case msg of
        AddPiece colIndex ->
            let
                ( canAdd, rowIndex ) =
                    model.rows
                        |> List.indexedMap
                            (\rowIndex row ->
                                let
                                    entryInColumn =
                                        row
                                            |> List.drop colIndex
                                            |> List.head
                                            |> Maybe.withDefault Empty
                                in
                                    case entryInColumn of
                                        Empty ->
                                            ( True, rowIndex )

                                        _ ->
                                            ( False, rowIndex )
                            )
                        |> List.reverse
                        |> List.filter
                            (\( isEmpty, rowIndex ) -> isEmpty)
                        |> List.head
                        |> Maybe.withDefault ( False, 0 )

                newRows =
                    model.rows
                        |> List.indexedMap
                            (\i row ->
                                let
                                    piece =
                                        case model.turn of
                                            P1 ->
                                                Player1

                                            P2 ->
                                                Player2
                                in
                                    if i == rowIndex then
                                        (List.take colIndex row) ++ [ piece ] ++ (List.drop (colIndex + 1) row)
                                    else
                                        row
                            )

                newTurn =
                    case model.turn of
                        P1 ->
                            P2

                        P2 ->
                            P1

                gameover =
                    case model.winner of
                        Just player ->
                            True

                        Nothing ->
                            False

                winner =
                    case model.winner of
                        Just player ->
                            Just player

                        Nothing ->
                            if isWin newRows model.turn then
                                Just model.turn
                            else
                                Nothing
            in
                if canAdd && not gameover then
                    { model | rows = newRows, turn = newTurn, winner = winner }
                else
                    model

        Restart ->
            initModel
