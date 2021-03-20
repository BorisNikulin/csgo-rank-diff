module Main where

import Prelude

import Partial.Unsafe
import Data.Ord
import Data.Int
import Data.Number.Format
import Data.Maybe
import Data.Enum
import Data.Array as Array
import Data.Newtype
import Data.Monoid.Additive
import Data.Foldable
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Console (logShow)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Web.HTML as HTML
import Web.HTML.Window as HTML
import Web.HTML.HTMLDocument as HTML
import Web.HTML.HTMLSelectElement as HTMLSelect
import Web.DOM.Document hiding (toNode) as DOM
import Web.DOM.NonElementParentNode as DOM
import Web.DOM.Element as DOM
import Web.DOM.Node (nodeValue) as DOM

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type State = { teamARanks :: Array Int, teamBRanks :: Array Int }

data Action = SelectChange Team Int

data Team = TeamA | TeamB

derive instance genericAction :: Generic Action _
derive instance genericTeam :: Generic Team _

instance showAction :: Show Action where
    show = genericShow

instance showTeam :: Show Team where
    show = genericShow

idFromTeamIx :: Team -> Int -> String
idFromTeamIx team ix = show $ ix + teamToOffset team

idToTeamIx :: String -> { team :: Team, ix :: Int }
idToTeamIx id = { team: numToTeam $ idNum `div` 5, ix: idNum `rem` 5 }
    where
    numToTeam = case _ of
        0 -> TeamA
        _ -> TeamB
    idNum = unsafePartial $ fromJust $ fromString id

teamToOffset :: Team -> Int
teamToOffset = case _ of
    TeamA -> 0
    TeamB -> 5

ranksByTeam :: Team -> State -> Array Int
ranksByTeam = case _ of
    TeamA -> _.teamARanks
    TeamB -> _.teamBRanks

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval H.defaultEval { handleAction = handleAction }
        }

initialState :: forall input. input -> State
initialState _ = { teamARanks: Array.replicate 5 1, teamBRanks: Array.replicate 5 1 }

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction (SelectChange team ix) = do
    document <- HTML.toDocument <$> (H.liftEffect $ HTML.document =<< HTML.window)
    let nonElementParentNode = DOM.toNonElementParentNode document
    mElement <- H.liftEffect $ DOM.getElementById (idFromTeamIx team ix) nonElementParentNode
    let select = unsafePartial $ fromJust $ HTMLSelect.fromElement =<< mElement
    value <- H.liftEffect $ (unsafePartial $ fromJust <<< fromString) <$> HTMLSelect.value select
    H.modify_ \state -> case team of
        TeamA -> state { teamARanks = unsafePartial $ fromJust $ Array.updateAt ix value state.teamARanks }
        TeamB -> state { teamBRanks = unsafePartial $ fromJust $ Array.updateAt ix value state.teamBRanks }

render :: forall m. State -> H.ComponentHTML Action () m
render ranks =
    HH.div
        [ HP.classes [ HH.ClassName "wrapper" ]
        ]
        [ HH.div
            [ HP.classes [ HH.ClassName "scoreboard" ]
            ]
            do
                team <- [TeamA, TeamB]
                pure (HH.h1_ [ HH.text $ show team ])
                    <> do
                       ix <- enumFromTo 0 4
                       pure $ selectRank team ix
                    <>
                        [ HH.p_ [ HH.text $ "Rank sum: " <> show (rankSum team) ]
                        , HH.p_ [ HH.text $ "Rank average: " <> toStringWith (precision 2) (rankAverage team) ]
                        ]
        , HH.div
            [ HP.classes [ HH.ClassName "summary" ]
            ]
            [ HH.p_ [ HH.text $ "Rank ratio (A/B): " <> toStringWith (precision 3) rankRatio ]
            , HH.p_ [ HH.text $ "Rank diff (abs(A-B)): " <> show rankDiff ]
            ]
        ]
    where
    rankSum = case _ of
        TeamA -> ala Additive foldMap $ ranksByTeam TeamA ranks
        TeamB -> ala Additive foldMap $ ranksByTeam TeamB ranks
    rankAverage team = toNumber (rankSum team) / 5.0
    rankRatio = rankAverage TeamA / rankAverage TeamB
    rankDiff = abs $ rankSum TeamA - rankSum TeamB

selectRank :: forall i. Team -> Int -> HH.HTML i Action
selectRank team ix =
    HH.select
        [ HP.classes [ HH.ClassName "scoreboard__rank" ]
        , HP.id $ idFromTeamIx team ix
        , HE.onChange (const $ SelectChange team ix)
        ]
        do
            Array.mapWithIndex
                toOption
                [ "Silver 1"
                , "Silver 2"
                , "silver 3"
                , "Silver Elite"
                , "Silver Elite Master"
                , "Gold Nova 1"
                , "Gold Nova 2"
                , "Gold Nova 3"
                , "Gold Nova Master"
                , "Master Guardian 1"
                , "Master Guardian 2"
                , "Master Guardian Elite"
                , "Distinguished Master Guardian"
                , "Legendary Eagle"
                , "Legendary Eagle Master"
                , "Supreme Master First Class"
                , "The Global Elite"
                ]
    where
    toOption ix label = HH.option [ HP.value $ show $ ix + 1 ] [ HH.text label ]
