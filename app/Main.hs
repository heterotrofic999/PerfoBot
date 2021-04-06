{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative              ((<|>))
import           Control.Monad.Trans              (liftIO)
import           Data.Text                        (Text, head, unpack, pack)
import qualified Data.Text                        as Text
import           Data.Time

import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser
import Data.Char

import System.Random (randomRIO)

data Model = Model
  { currentNum :: [Int]
  } deriving (Show)


data Action
  = NoOp
  | Echo Text
  | Start
  | Rand [Int]
  | Surrender

inititalModel :: IO Model -- модель с рандомным, загаданным числом
inititalModel = do
  nn <- randomDigits (7)
  pure Model { currentNum = nn }

echoBot :: IO (BotApp Model Action) -- модель бота
echoBot = do
  model <- inititalModel
  pure BotApp
    { botInitialModel = model
    , botAction = flip handleUpdate
    , botHandler = handleAction
    , botJobs = []
    }

handleUpdate -- действия по команде пользователя
  :: Model -> Telegram.Update -> Maybe Action
handleUpdate _model = parseUpdate
   $ Start      <$  command "start"
 <|> Surrender <$ command "сдаюсь"
 <|> Echo   <$> text

modelUpdate :: Model -> [Int] -> Model -- перерандом для загаданного числа
modelUpdate model num = model {currentNum = num}

parseToIntList :: String -> String -- делает из 1234567, [1,2,3,4,5,6,7]
parseToIntList msg = "[" ++ [(Prelude.head msg)] ++ "," ++ [(msg !! 1)] ++ "," ++ [(msg !! 2)] ++ "," ++ [(msg !! 3)] ++ "," ++ [(msg !! 4)] ++ "," ++ [(msg !! 5)] ++ "," ++ [(msg !! 6)] ++ "]"

isDig :: String -> Bool -- проверяет каждый символ, если вся строка состоит из "чисел", то вернёт истину
isDig msg | (isDigit (Prelude.head msg) == True) && (isDigit (msg !! 1) == True) && (isDigit (msg !! 2) == True) && (isDigit (msg !! 3) == True) && (isDigit (msg !! 4) == True) && (isDigit (msg !! 5) == True) && (isDigit (msg !! 6) == True) = True
          | otherwise = False

handleAction :: Action -> Model -> Eff Action Model 
handleAction action model = case action of
  NoOp -> pure model
  Echo msg -> model <# do -- ввод с сообщений пользователя 
    rand <- liftIO $ randomDigits (4)
    if (msg == "Расскажи анекдот")
      then do
        num <- liftIO (randomRIO (1::Integer, 7))
        if (num == 1)
          then replyText anek1 
          else if (num == 2) 
            then replyText anek2
            else if (num == 3)
              then replyText anek3
              else if num == 4
                then replyText anek4
                else if num == 5
                  then replyText anek5
                  else if num == 6
                    then replyText anek6
                    else if num == 7
                      then replyText anek7
                      else replyText ""
      else if (msg == "Давай поиграем")
        then do
          reply (toReplyMessage bullsMessage)
          --let aue = currentNum model -- вывод для быстрого прохождения игры
          --replyText $ pack $ show aue
        else if length (unpack msg) == 7
          then do
            if isDig $ unpack msg
              then do
                let num = currentNum model
                let hodik = unpack msg
                let hod = parseToIntList hodik
                if (diffOK (read hod::[Int]) == True) 
                  then do
                        let b = countBulls (read hod::[Int]) num
                        let e = countElem (read hod::[Int]) num

                        let c = e - b
                        if b /= 7
                          then do
                            replyText $ pack $ "Не угадали! " ++ (show b) ++ " быков " ++ (show c) ++ " коров"
                            ans <- liftIO (randomRIO (0::Integer, 9))
                            if ans == 0 then replyText "Почти угадали!"
                            else if ans == 1 then replyText "Не отчаивайтесь!"
                            else if ans == 2 then replyText "Ещё немного усилий и вы победите!"
                            else if ans == 3 then replyText "Подумайте ещё немного!"
                            else if ans == 4 then replyText "Включите уже вашу логику!"
                            else if ans == 5 then replyText "Возможно, стоит немного подумать.."
                            else if ans == 6 then replyText "Воспользуйтесь хотя бы интуицией..."
                            else if ans == 7 then replyText "Кажется, удача вас подводит"
                            else if ans == 8 then replyText "Ну, рано или поздно, вы точно угадаете"
                            else if ans == 9 then replyText "А вы точно учились в школе? Хотя не надо - не отвечайте"
                            else replyText ""
                          else do
                            replyText "Верно! Поздравляю с победой - из вас выйдет отличный программист! Чтоы начать новую игру, введите /start"
                  else replyText "Все цифры должны быть различны!"
              else replyText "Может мне тоже начать всякую #@%*ю писать?"
          else replyText "Введите СЕМИЗНАЧНУЮ последовательность цифр, например 1234567"
    return NoOp
  Start -> model <# do -- старт по команде
    reply (toReplyMessage startMessage)
      { replyMessageReplyMarkup = Just
          (Telegram.SomeReplyKeyboardMarkup startMessageKeyboard)
      }
    gg <- liftIO $ randomDigits (7)
    return $ Rand gg
  Surrender -> model <# do -- сдача по команде
    replyText "Какой позор! Вас киберунизил искуственный интеллект. Правильный ответ:"
    let aue = currentNum model 
    replyText $ pack $ show aue
    gg <- liftIO $ randomDigits (7)
    return $ Rand gg
  Rand num -> modelUpdate model num <# do -- обновление модели для смены рандомного числа
    return NoOp

startMessage :: Text -- приветствие 
startMessage = Text.unlines
 [ "Hello, я PerfoBot!"
 , "Могу рассказать вам пару шуток про программистов или сыграть с вами в Быки И Коровы"
 ]

startMessageKeyboard :: Telegram.ReplyKeyboardMarkup -- клавиатура для старта
startMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ [ "Расскажи анекдот" ] , ["Давай поиграем"] , ["/start"] , ["/сдаюсь"]
      ]
  , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
  , Telegram.replyKeyboardMarkupSelective = Just True
  }

run :: Telegram.Token -> IO () -- запускает бота по API токену
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  coolBot <- echoBot
  startBot_ (conversationBot Telegram.updateChatId coolBot) env

main :: IO ()
{-# LANGUAGE OverloadedStrings #-}
main = run "1660408361:AAHeT22pu95T4cqDICvvMXZVwaL6BL5emEA"


randomDigits :: Int -> IO [Int] -- рагдомный список интов
randomDigits 0 = return []
randomDigits n = do
     r <- randomRIO (0,9)
     rs <- randomDigits (n-1)
     if (elem r rs) == False
          then return (r:rs)
          else randomDigits n

countBulls :: [Int] -> [Int] -> Int -- подсчёт быков
countBulls [] _ = 0
countBulls _ [] = 0
countBulls (n:ns) (g:gs) = if g == n
                        then 1 + countBulls ns gs
                        else countBulls ns gs
    
countElem :: [Int] -> [Int] -> Int -- подсчёт всех совпадений
countElem [] _ = 0
countElem _ [] = 0
countElem (n:ns) gs = if elem n gs
                           then 1 + countElem ns gs
                           else countElem ns gs

diffOK :: [Int] -> Bool -- проверка на различность
diffOK [] = True
diffOK [_] = True
diffOK guess = if elem (Prelude.head guess) (tail guess)
                     then False
                     else diffOK (tail guess)
                     
bullsMessage :: Text -- приветствие 
bullsMessage = Text.unlines
 [ "Быки и коровы!"
 , "Я загадываю семизначную последовательность цифр, вы пытаетесь её угадать."
 , "Когда вы вводите своё предположение, я отвечаю сколько коров и быков в вашей последовательности, где:"
 , "Быки - это угаданные цифры на своих местах,"
 , "Коровы - это угаданные цифры не на своих местах."
 , "Чтобы победить, вам нужно угадать последовательность целиком."
 , "Помните, что последовательность может начинаться с нуля!"
 , "Чтобы сдаться, нажмите кнопку /сдаюсь"
 , "Всё просто! Начинаем!"
 ]

anek1 :: Text
anek1 = Text.unlines
 [ "Сколько нужно программистов чтобы закрутить лампочку? "
 , "."
 , "."
 , "."
 , "."
 , "."
 , "."
 , "."
 , "10, один закручивает, а второй думает в чем прикол"
 ]

anek2 :: Text
anek2 = Text.unlines
 [ "В Мексике все программисты - сеньоры."
 ]

anek3 :: Text
anek3 = Text.unlines
 [ "У обычного человека после встречи с гопником 32 выбитых зуба...",
   "У человека-программиста после встречи с гопником 4 выбайтых зуба."
 ]

anek4 :: Text
anek4 = Text.unlines
 [ "Лебедев демонстрирует программистам новый сайт. "
 , "— Вот, товарищи программисты, это новый секретный сайт. "
 , "— Петров! "
 , "— Я! "
 , "— Поправь шрифты! "
 , "Петров тужится, пыжится, не поправить. "
 , "— Не могу. "
 , "— Сидоров, помоги Петрову. "
 , "Пытаются вдвоем, та же ситуация. "
 , "— Не можем. "
 , "— Иванов, помогай. "
 , "Пыхтят втроем. Поправить не могут. "
 , "— Никак не поправить, Артемий Татьянович! "
 , "— А чего вы хотели? Адаптивный дизайн!"
 ]

anek5 :: Text
anek5 = Text.unlines
 [ "Работодатель претенденту на должность программиста:  "
 , "- В своем резюме вы написали, что знаете следующие языки и технологии программирования: Basic (TB, QB, VB, VBA, VB.NET ) C (C and C++ for Unix, FreeBSD, QNX), C++, VC++, C++.NET, C#, в идеале знаете ассемблеры следующих процессоров I-4004 - IP4, Amiga: (ну и тут список на 2 страницы, мелко и подробно). Ну что же, зарплата у нас по договоренности, но гор золота мы вам не сулили. Максимум на что вы можете рассчитывать это 150 тыс. евро.  "
 , "Претендент в немом удивлении. "
 , "- И только не надо весь рабочий день мечтать о коттедже на Канарах, максимум, что мы можем предложить нашим сотрудникам - это 6-комнатную в центре Москвы. Претендент пытается усидеть на месте, и вести себя пристойно  "
 , "- И не надо думать, что если у шефа красавец Порше, то вы его тоже получите. Ваш максимум - это BMW Z8. "
 , "Претендент, не выдержав:  "
 , "- Да вы гоните!  "
 , "Работодатель, чинно:  "
 , "- Ты первый начал. "
 ]

anek6 :: Text
anek6 = Text.unlines
 [ "Что и так есть у команды программистов, но этого им в то же время яростно не хватает? "
 , " "
 , " "
 , "Devchat"
 ]

anek7 :: Text
anek7 = Text.unlines
 [ "Срубил Добрыня голову Змею Горынычу, а у того вместо одной две выросло. Срубил Добрыня эти две головы, а вместо них четыре выросло. Интересно стало Добрыне, и долго он ещё издевался над бедной зверушкой, пока не срубил он ей за раз 65536 голов и после они перестали появляться "
 , "— Шестнадцатибитная, — подумал богатырь."
 ]

anek8 :: Text
anek8 = Text.unlines
 [ "Студент останавливает лифт, там преподаватель по программированию. "
 , "С: Лифт едет вверх или вниз?"
 , "П: Invalid Syntax"
 , "С: Лифт едет вверх или лифт едет вниз?"
 , "П: True"
 , "Двери закрываются, лифт уезжает"
 ]