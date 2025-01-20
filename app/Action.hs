{-# LANGUAGE OverloadedStrings #-}

module Action (Action (..), handleAction, handleUpdate, handleMutation, HandledAction (..)) where

import AI.Chat qualified as Chat
import AI.Prompts qualified as Prompts
import Context qualified
import Control.Applicative ((<$>), (<|>))
import Control.Applicative qualified as Applicative
import Control.Monad ((>>), (>>=))
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans.Class as Trans.Class
import Data.Either qualified as Either
import Data.Function (($))
import Data.Function qualified as Function
import Data.Int qualified as Int
import Data.Maybe qualified as Maybe
import Data.Ord ((>), (>=))
import Data.Semigroup ((<>))
import Data.String qualified as String
import Data.Text qualified as Text
import Domain.Character qualified as Character
import Domain.Character.Class qualified as Class
import Domain.Character.Inventory qualified as Inventory
import Domain.Combat qualified as Combat
import Domain.Enemies qualified
import Domain.Enemies qualified as Enemies
import Domain.Entity.Attacker qualified as Attacker
import Domain.Entity.Damageable qualified as Damageable
import Domain.Entity.Healable qualified as Healable
import Domain.Entity.Health qualified as Health
import GHC.Base (Bool (..), not, (&&), (<$), (<=))
import Model qualified
import PrettyPrint qualified
import System.Random qualified as Random
import Telegram.Bot.API qualified as Telegram
import Telegram.Bot.Simple qualified as Bot
import Telegram.Bot.Simple.UpdateParser qualified as Simple.UpdateParser
import Text.Read qualified as Read
import Text.Show qualified as Show

newtype HandledAction a = HandledAction {unwrap :: a}

instance Bot.GetAction (HandledAction Action) Action where
    getNextAction botMAction = do
        action <- botMAction
        case unwrap action of
            NoAction -> Applicative.pure Maybe.Nothing
            _ -> Applicative.pure $ Maybe.Just $ unwrap action

-- | Actions bot can perform.
data Action
    = NoAction
    | Start
    | Message Text.Text
    | NewMessageRequest (Maybe.Maybe Model.MessageRequest)
    | NewName Text.Text
    | NewClass Class.Class
    | RestoreHealth
    | SelectClass Class.Class
    | SubtractGold Int.Int32
    | Tavern
    | CallbackTavern
    | Rest
    | Heal
    | ShowInventory
    | CallbackStartCombat
    | StartCombat Domain.Enemies.Enemies
    | Attack
    | TakeLoot Inventory.Inventory
    deriving (Show.Show, Read.Read)

-- | How to process incoming 'Telegram.Update's
handleUpdate :: Model.Model -> Telegram.Update -> Maybe.Maybe Action
handleUpdate _ =
    Simple.UpdateParser.parseUpdate $
        Start <$ Simple.UpdateParser.command "start"
            <|> Simple.UpdateParser.callbackQueryDataRead
            <|> Message <$> Simple.UpdateParser.text

handleMutation :: Model.Model -> Action -> Model.Model
handleMutation model@(Model.UserForm{}) (NewMessageRequest request) = model{Model.messageRequest = request}
handleMutation model@(Model.UserForm{}) (NewName name) = model{Model.name = Maybe.Just $ Text.unpack name}
handleMutation (Model.UserForm name _ _) (NewClass characterClass) = Model.emptyTavern (Maybe.fromMaybe "" name) characterClass
handleMutation model@(Model.Tavern character) RestoreHealth = model{Model.character = Healable.heal 25 character}
handleMutation model@(Model.Tavern character) (SubtractGold gold) = model{Model.character = Character.chargeGold gold character}
handleMutation (Model.Tavern character) (StartCombat enemies) = Model.emptyCombat character enemies
handleMutation model@(Model.Combat _ combat) Attack = model{Model.combat = combat >>= newRound}
  where
    newRound (Combat.InProgress character enemies) = Attacker.attack character >> Attacker.attack enemies
    newRound state@(Combat.Finished{}) = Applicative.pure state
handleMutation (Model.Combat initial combat) Tavern = Model.Tavern $ Combat.character $ Combat.state combat initial
handleMutation (Model.Combat initial combat) (StartCombat enemies) = Model.emptyCombat (Combat.character $ Combat.state combat initial) enemies
handleMutation model@(Model.Combat _ combat) (TakeLoot loot) = model{Model.combat = combat >> withLootTaken}
  where
    withLootTaken = Combat.emitAction $ Combat.TakeLoot loot
handleMutation model _ = model

handleAction :: Model.Model -> Action -> Context.ContextM Action
handleAction model@(Model.UserForm{}) Start = handleStart model
handleAction Model.UserForm{} (NewName _) = Applicative.pure Start
handleAction Model.UserForm{} (SelectClass characterClass) = Trans.Class.lift $ do
    Bot.replyText ("Твой класс: " <> Text.pack (Show.show characterClass))
    answerCallbackQuery
    Applicative.pure $ NewClass characterClass
handleAction model@(Model.UserForm{}) (Message text) = Trans.Class.lift $ handleText text model
handleAction Model.Tavern{} (NewClass _) = Trans.Class.lift $ Applicative.pure Tavern
handleAction Model.Tavern{} Tavern = do
    chat <- Reader.reader Context.chat
    resp <- MonadIO.liftIO $ Chat.runM chat $ Chat.completions Prompts.tavernDescription
    Trans.Class.lift $ do
        Bot.reply
            (String.fromString resp)
                { Bot.replyMessageReplyMarkup =
                    Maybe.Just $
                        Telegram.SomeInlineKeyboardMarkup $
                            Telegram.InlineKeyboardMarkup [[btnInventory, btnRest], [btnCombat]]
                }
        Applicative.pure NoAction
  where
    btnRest = Bot.actionButton "Отдыхать" Rest
    btnInventory = Bot.actionButton "Инвентарь" ShowInventory
    btnCombat = Bot.actionButton "Спуститься в подземелье" CallbackStartCombat
handleAction model@(Model.Tavern{}) Heal =
    Trans.Class.lift $ do
        if currentGold >= healCost
            then
                Bot.replyText
                    "Молодая официантка бесшумно принесла ужин, который ты поглотил быстрее, чем уходят последние золотые монеты из кошеля.\
                    \Остаток ночи ты провёл в своей комнате, под завывания ветра за окном.\
                    \К утру твои раны затянулись, оставив лишь болезненные шрамы."
                    >> answerCallbackQuery
                    >> Applicative.pure RestoreHealth
            else
                Bot.replyText "Шаря по карманам, ты нашёл лишь пыль и обрывки надежд. Ни хлеба, ни крова — только ночь, холод и пустота твоего кошеля."
                    >> answerCallbackQuery
                    >> Applicative.pure Tavern
  where
    currentGold = Inventory.gold (Character.inventory (Model.character model))
    healCost = 5
handleAction Model.Tavern{} RestoreHealth = Applicative.pure $ SubtractGold 5
handleAction model@(Model.Tavern{}) ShowInventory = Trans.Class.lift $ do
    Bot.reply $
        (String.fromString $ "Ваш инвентарь:\n\n" <> PrettyPrint.prettyPrint (Character.inventory $ Model.character model))
            { Bot.replyMessageReplyMarkup =
                Maybe.Just $
                    Telegram.SomeInlineKeyboardMarkup $
                        Telegram.InlineKeyboardMarkup [[btnBack]]
            }
    answerCallbackQuery
    Applicative.pure NoAction
  where
    btnBack = Bot.actionButton "Назад" CallbackTavern
handleAction Model.Tavern{} CallbackTavern = Trans.Class.lift $ answerCallbackQuery >> Applicative.pure Tavern
handleAction Model.Combat{} CallbackTavern = Trans.Class.lift $ answerCallbackQuery >> Applicative.pure Tavern
handleAction Model.Combat{} CallbackStartCombat = do
    chat <- Reader.reader Context.chat
    resp <- MonadIO.liftIO $ Chat.runM chat $ Chat.completions $ Prompts.combatStartEnemyDescription 3
    Trans.Class.lift $ do
        health :: Int.Int32 <- MonadIO.liftIO $ Random.randomRIO (3, 10)
        damage :: Int.Int32 <- MonadIO.liftIO $ Random.randomRIO (1, 4)

        answerCallbackQuery
        Applicative.pure (StartCombat $ Domain.Enemies.Enemies resp (Health.Health health health) damage)
handleAction Model.Tavern{} CallbackStartCombat = do
    chat <- Reader.reader Context.chat
    resp <- MonadIO.liftIO $ Chat.runM chat $ Chat.completions $ Prompts.combatStartEnemyDescription 3
    Trans.Class.lift $ do
        answerCallbackQuery
        Applicative.pure (StartCombat $ Domain.Enemies.Enemies resp (Health.Health 3 3) 1)
handleAction (Model.Combat (Combat.InProgress character _) _) (StartCombat enemies) = Trans.Class.lift $ do
    Bot.reply
        ( String.fromString $
            PrettyPrint.prettyPrint enemies
                <> "\n"
                <> "У тебя "
                <> PrettyPrint.prettyPrint (Character.health character)
        )
            { Bot.replyMessageReplyMarkup =
                Maybe.Just $
                    Telegram.SomeInlineKeyboardMarkup $
                        Telegram.InlineKeyboardMarkup [[btnAttack], [btnRetreat]]
            }
    Applicative.pure NoAction
handleAction (Model.Combat initState@(Combat.InProgress character enemies) combat) Attack = do
    chat <- Reader.reader Context.chat

    Trans.Class.lift $ do
        randomGold :: Int.Int32 <- MonadIO.liftIO $ Random.randomRIO (5, 10)
        shouldGiveItem <- MonadIO.liftIO $ (> (5 :: Int.Int32)) <$> Random.randomRIO (1, 10)
        items <- lootItems chat shouldGiveItem

        let loot = Inventory.Inventory{Inventory.gold = randomGold, Inventory.items = items}

        resp <- MonadIO.liftIO $ Chat.runM chat $ Chat.completions $ prompt state
        Bot.reply
            (String.fromString $ text resp loot)
                { Bot.replyMessageReplyMarkup =
                    Maybe.Just $
                        Telegram.SomeInlineKeyboardMarkup $
                            Telegram.InlineKeyboardMarkup $
                                case roundState of
                                    Combat.InProgress{} -> [[btnAttack], [btnRetreat]]
                                    Combat.Finished _ Combat.Character -> [[btnNextCombat], [btnRetreat]]
                                    Combat.Finished _ Combat.Enemies -> [[btnRetreat]]
                }

        answerCallbackQuery
        Applicative.pure $ case state of
            Combat.Finished _ Combat.Character -> TakeLoot loot
            _ -> NoAction
  where
    lootItems chat shouldGiveItem =
        if shouldGiveItem
            then do
                randomItemLevel :: Int.Int32 <- MonadIO.liftIO $ Random.randomRIO (1, 5)
                itemName <- MonadIO.liftIO $ Chat.runM chat $ Chat.completions $ "Название (не больше 3 слов) средневекового оружия " <> Show.show randomItemLevel <> "/5 уровня" <> " для класса " <> Show.show (Character.cClass character)
                itemDescription <- MonadIO.liftIO $ Chat.runM chat $ Chat.completions $ "Описание (одно предложение) средневекового оружия " <> Show.show randomItemLevel <> "/5 уровня" <> " для класса " <> Show.show (Character.cClass character)
                Applicative.pure [Inventory.Item itemName itemDescription Inventory.Weapon randomItemLevel]
            else Applicative.pure []
    state = Combat.state combat initState
    history = Combat.history combat initState
    prompt (Combat.InProgress c e) = Prompts.combatRoundDescription (PrettyPrint.prettyPrint e) (Show.show $ Character.cClass c) (Damageable.hp c) (Damageable.hp e) history
    prompt (Combat.Finished _ side) = Prompts.combatFinishDescription (Enemies.description enemies) (Show.show $ Character.cClass character) (Show.show side)
    btnNextCombat = Bot.actionButton "В следующую комнату" CallbackStartCombat
    roundState = Combat.state combat initState
    text resp loot = case roundState of
        Combat.InProgress c (Enemies.Enemies _ health dmg) ->
            resp
                <> "\n\n"
                <> "У них "
                <> PrettyPrint.prettyPrint health
                <> ", "
                <> Show.show dmg
                <> " DMG"
                <> "\n"
                <> "У тебя "
                <> PrettyPrint.prettyPrint (Character.health c)
        Combat.Finished c Combat.Character ->
            resp
                <> "\n\n"
                <> "У тебя "
                <> PrettyPrint.prettyPrint (Character.health c)
                <> "\n\n"
                <> "Добыча: "
                <> PrettyPrint.prettyPrint loot
        Combat.Finished c Combat.Enemies ->
            resp
                <> "\n\n"
                <> "У тебя "
                <> PrettyPrint.prettyPrint (Character.health c)
handleAction (Model.Tavern character) Rest = Trans.Class.lift $ do
    Bot.reply
        (String.fromString $ PrettyPrint.prettyPrint (Character.health character) <> "\n\n" <> "Готов обменять 5 монет на тепло очага, мягкую постель и иллюзию покоя?")
            { Bot.replyMessageReplyMarkup =
                Maybe.Just $
                    Telegram.SomeInlineKeyboardMarkup $
                        Telegram.InlineKeyboardMarkup [[btnHeal], [btnBack]]
            }
    answerCallbackQuery
    Applicative.pure NoAction
  where
    btnHeal = Bot.actionButton "Да" Heal
    btnBack = Bot.actionButton "Назад" CallbackTavern
handleAction _ _ = Applicative.pure NoAction

btnRetreat :: Telegram.InlineKeyboardButton
btnRetreat = Bot.actionButton "Отступить" CallbackTavern

btnAttack :: Telegram.InlineKeyboardButton
btnAttack = Bot.actionButton "Атаковать" Attack

validateUserName :: Text.Text -> Bool
validateUserName name =
    let trimmedName = Text.strip name
     in Text.length trimmedName <= 10 && not (Text.null trimmedName)

handleText :: Text.Text -> Model.Model -> Bot.BotM Action
handleText _ Model.UserForm{Model.messageRequest = Maybe.Nothing} = Applicative.pure Start
handleText text Model.UserForm{Model.messageRequest = Maybe.Just Model.CharNameRequest} =
    if validateUserName text
        then do
            Bot.replyText ("Хорошо, твоё имя: " <> text)
            Applicative.pure $ NewName text
        else do
            Bot.replyText "Как, как ты сказал? Придумай что-нибудь покороче, я не буду тратить столько бумаги."
            Applicative.pure $ NewMessageRequest $ Maybe.Just Model.CharNameRequest
handleText _ _ = Applicative.pure NoAction

handleStart :: Model.Model -> Context.ContextM Action
handleStart model = Trans.Class.lift $ Either.either Function.id Function.id $ do
    _ <- ifEmpty (Model.name model) $ do
        Bot.replyText "В этих землях твоё имя никому не интересно... кроме нас. Нужно же знать, что выгравировать на твоей надгробной плите."
        Applicative.pure $ NewMessageRequest (Maybe.Just Model.CharNameRequest)

    ifEmpty (Model.characterClass model) $ do
        Bot.reply
            "Кто ты, воин? Или лучник? \x1F44B"
                { Bot.replyMessageReplyMarkup =
                    Maybe.Just $
                        Telegram.SomeInlineKeyboardMarkup $
                            Telegram.InlineKeyboardMarkup [[btnFighter, btnArcher]]
                }
        Applicative.pure $ NewMessageRequest Maybe.Nothing
  where
    ifEmpty :: Maybe.Maybe a -> Bot.BotM Action -> Either.Either (Bot.BotM Action) (Bot.BotM Action)
    ifEmpty mb reply = Maybe.maybe (Either.Left reply) (Function.const $ Either.Right $ Applicative.pure NoAction) mb
    btnFighter = Bot.actionButton "Воин" (SelectClass Class.Fighter)
    btnArcher = Bot.actionButton "Лучник" (SelectClass Class.Ranger)

answerCallbackQuery :: Bot.BotM ()
answerCallbackQuery = do
    maybeUpdate <- Reader.reader Bot.botContextUpdate
    Maybe.maybe (Applicative.pure ()) reply maybeUpdate
  where
    answer callbackQuery =
        Monad.void $
            Bot.liftClientM $
                Telegram.answerCallbackQuery $
                    Telegram.AnswerCallbackQueryRequest (Telegram.callbackQueryId callbackQuery) Maybe.Nothing Maybe.Nothing Maybe.Nothing Maybe.Nothing
    reply update = do
        Maybe.maybe (Applicative.pure ()) answer $ Telegram.updateCallbackQuery update
        Applicative.pure ()
