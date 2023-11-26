{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.IO()
import Text.Read (readMaybe)
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Text.Parsec as P
import Text.Parsec.String

data DrinkType = Frappe | Tea | Espresso | IceCoffee | NonCoffee
    deriving (Show, Generic, Eq)

data PastryType = WithFilling | WithoutFilling
    deriving (Show, Generic)

data Additive = Additive { addName :: String, addPrice :: Int }
    deriving (Show, Read, Eq, Generic, FromJSON)

data Filling = Filling { fillName :: String, fillPrice :: Int }
    deriving (Show, Read, Eq, Generic, FromJSON)

data Product = Drink { drinkType :: DrinkType, drinkName :: String, drinkPrice :: Int }
             | Pastry { pastryType :: PastryType, pastryName :: String, pastryPrice :: Int }
             | AdditiveProduct Additive
             | FillingProduct Filling
    deriving (Show, Generic, Eq)

data EspressoSize = LargeEspresso | MediumEspresso | StandardEspresso
    deriving (Read, Show)

instance FromJSON DrinkType where
    parseJSON = genericParseJSON defaultOptions

instance FromJSON PastryType where
    parseJSON = genericParseJSON defaultOptions

instance Read Product where
    readsPrec _ value = case words value of
        "Drink" : rest -> case reads (unwords rest) of
            [(drinkType, rest')] -> [(Drink drinkType "" 0, rest')]
            _                    -> []
        "Pastry" : rest -> case reads (unwords rest) of
            [(pastryType, rest')] -> [(Pastry pastryType "" 0, rest')]
            _                    -> []
        "Additive" : rest -> case reads (unwords rest) of
            [(additive, rest')] -> [(AdditiveProduct additive, rest')]
            _                    -> []
        "Filling" : rest -> case reads (unwords rest) of
            [(filling, rest')] -> [(FillingProduct filling, rest')]
            _                    -> []
        _               -> []

instance Read DrinkType where
    readsPrec _ value = case value of
        "Frappe"    -> [(Frappe, "")]
        "Tea"       -> [(Tea, "")]
        "Espresso"  -> [(Espresso, "")]
        "IceCoffee" -> [(IceCoffee, "")]
        "NonCoffee" -> [(NonCoffee, "")]
        _           -> []

instance Read PastryType where
    readsPrec _ value = case value of
        "WithFilling"    -> [(WithFilling, "")]
        "WithoutFilling" -> [(WithoutFilling, "")]
        _                -> []

instance Eq PastryType where
    WithFilling == WithFilling = True
    WithoutFilling == WithoutFilling = True
    _ == _ = False

instance FromJSON Product where
    parseJSON = withObject "Product" $ \o -> do
        mDrinkType <- o .:? "drinkType"
        mPastryType <- o .:? "pastryType"
        mAdditiveName <- o .:? "addName"
        mFillName <- o .:? "fillName"
        case (mDrinkType, mPastryType, mAdditiveName, mFillName) of
            (Just dt, Nothing, Nothing, Nothing) ->
                Drink dt <$> o .: "drinkName" <*> o .: "drinkPrice"
            (Nothing, Just pt, Nothing, Nothing) ->
                Pastry pt <$> o .: "pastryName" <*> o .: "pastryPrice"
            (Nothing, Nothing, Just addName, Nothing) ->
                AdditiveProduct <$> (Additive addName <$> o .: "addPrice")
            (Nothing, Nothing, Nothing, Just fillName) ->
                FillingProduct <$> (Filling fillName <$> o .: "fillPrice")
            _ -> fail "Invalid product"

-- Функции для добавления объектов в меню
parseMenu :: String -> Either P.ParseError [Product]
parseMenu = P.parse menuParser ""

menuParser :: Parser [Product]
menuParser = do
    _ <- P.string' "["
    products <- P.many productParser
    _ <- P.string "]"
    return products

productParser :: Parser Product
productParser = do
    _ <- P.spaces
    prodType <- P.many1 P.letter P.<?> "product type"
    _ <- P.spaces
    prodName <- P.many1 (P.noneOf "',") P.<?> "product name"
    _ <- P.spaces
    prodPrice <- read <$> P.many1 P.digit P.<?> "product price"
    _ <- P.spaces
    _ <- P.char ',' P.<|> P.char ']'
    case prodType of
        "Drink"   -> do
            dt <- parseDrinkType "Drink"
            pure $ Drink dt prodName prodPrice
        "Tea"     -> do
            dt <- parseDrinkType "Tea"
            pure $ Drink dt prodName prodPrice
        "Espresso"-> do
            dt <- parseDrinkType "Espresso"
            pure $ Drink dt prodName prodPrice
        "IceCoffee"-> do
            dt <- parseDrinkType "IceCoffee"
            pure $ Drink dt prodName prodPrice
        "NonCoffee"-> do
            dt <- parseDrinkType "NonCoffee"
            pure $ Drink dt prodName prodPrice
        "Pastry"  -> do
            pt <- parsePastryType "Pastry"
            pure $ Pastry pt prodName prodPrice
        "Additive" -> AdditiveProduct <$> parseAdditive
        "Filling"  -> FillingProduct <$> parseFilling
        _         -> error "Invalid product type"

parseDrinkType :: String -> Parser DrinkType
parseDrinkType "Drink" = do
    _ <- P.spaces
    read <$> P.many1 P.letter P.<?> "drink type"
parseDrinkType _ = error "Invalid drink type"

parsePastryType :: String -> Parser PastryType
parsePastryType "Pastry" = do
    _ <- P.spaces
    read <$> P.many1 P.letter P.<?> "pastry type"
parsePastryType _ = error "Invalid pastry type"

parseAdditive :: Parser Additive
parseAdditive = do
    _ <- P.string "Additive"
    _ <- P.spaces
    Additive <$> P.many1 (P.noneOf "',") <*> (read <$> P.many1 P.digit)

parseFilling :: Parser Filling
parseFilling = do
    _ <- P.string "Filling"
    _ <- P.spaces
    Filling <$> P.many1 (P.noneOf "',") <*> (read <$> P.many1 P.digit)

-- Вспомогательная функция для вывода опций добавок
printAdditiveOptions :: [Product] -> IO ()
printAdditiveOptions options = do
    let additives = filter isAdditive options
    putStrLn $ unlines $ zipWith (\n option -> show n ++ ". " ++ additiveInfoFromOption option) [1..] additives

-- Функция для проверки, является ли продукт добавкой
isAdditive :: Product -> Bool
isAdditive (AdditiveProduct _) = True
isAdditive _ = False

-- Вспомогательная функция для получения информации о добавке
additiveInfoFromOption :: Product -> String
additiveInfoFromOption (AdditiveProduct additive) = addName additive ++ " - " ++ show (addPrice additive) ++ " руб."
additiveInfoFromOption _ = ""

-- Функция для выбора добавок к напитку
chooseAdditives :: [Product] -> IO [Product]
chooseAdditives menu = do
    putStrLn "Желаете добавки к напитку?"
    putStrLn "1. Да"
    putStrLn "2. Нет"
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Выберите до 3 добавок:"
            additives <- chooseMultipleAdditives menu []
            return additives
        "2" -> return []
        _   -> do
            putStrLn "Неверный выбор. Пожалуйста, выберите 1 или 2."
            chooseAdditives menu

-- Функция для выбора нескольких добавок
chooseMultipleAdditives :: [Product] -> [Product] -> IO [Product]
chooseMultipleAdditives menu chosenAdditives
    | length chosenAdditives >= 3 = do
        putStrLn "Достигнуто максимальное количество добавок (3)."
        return chosenAdditives
    | otherwise = do
        printAdditiveOptions menu
        putStrLn "Выберите номер добавки (или введите '0' для завершения выбора):"
        choice <- getLine
        case readMaybe choice of
            Just 0 -> return chosenAdditives
            Just index -> do
                let selectedAdditive = menu !! (index - 1)
                putStrLn $ "Добавка '" ++ additiveInfoFromOption selectedAdditive ++ "' выбрана."
                chooseMultipleAdditives menu (chosenAdditives ++ [selectedAdditive])
            Nothing -> do
                putStrLn "Неверный выбор. Пожалуйста, выберите номер добавки или введите '0' для завершения."
                chooseMultipleAdditives menu chosenAdditives

-- Вспомогательная функция для вывода опций напитков
printDrinkOptions :: [Product] -> IO ()
printDrinkOptions options = putStrLn $ unlines $ zipWith (\n option -> show n ++ ". " ++ drinkInfoFromOption option) [1..] options

-- Вспомогательная функция для получения информации о напитке
drinkInfoFromOption :: Product -> String
drinkInfoFromOption (Drink _ name price) = name ++ " - " ++ show price ++ " руб."
drinkInfoFromOption _ = ""

-- Вспомогательная функция для обновления напитка с добавками
updateDrinkWithAdditives :: Product -> [Product] -> Product
updateDrinkWithAdditives (Drink drinkType name price) additives =
    Drink drinkType (name ++ " с добавками") (price + sum (map calculateItemTotal additives))
updateDrinkWithAdditives _ _ = error "Неверный тип продукта"

-- Новая функция обновления напитка с размером
updateDrinkWithSize :: Product -> EspressoSize -> Product
updateDrinkWithSize (Drink Espresso name price) size = case size of
    LargeEspresso   -> Drink Espresso (name ++ " (Большой)") (price * 2)
    MediumEspresso  -> Drink Espresso (name ++ " (Средний)") (round $ fromIntegral price * 1.5)
    StandardEspresso -> Drink Espresso (name ++ " (Стандарт)") price
updateDrinkWithSize _ _ = error "Неверный тип продукта"

-- Функция выбора размера эспрессо
chooseEspressoSize :: IO EspressoSize
chooseEspressoSize = do
    putStrLn "Выберите размер эспрессо:"
    putStrLn "1. Большой"
    putStrLn "2. Средний"
    putStrLn "3. Стандарт"
    choice <- getLine
    case readMaybe choice of
        Just index -> case index of
            1 -> return LargeEspresso
            2 -> return MediumEspresso
            3 -> return StandardEspresso
            _ -> error "Неверный выбор"
        Nothing -> do
            putStrLn "Неверный выбор. Пожалуйста, выберите размер эспрессо."
            chooseEspressoSize

-- Функция выбора напитка
chooseDrink :: [Product] -> IO Product
chooseDrink menu = do
    putStrLn "Выберите тип напитка:"
    putStrLn "1. Фраппе"
    putStrLn "2. Чай"
    putStrLn "3. Эспрессо"
    putStrLn "4. Кофе со льдом"
    putStrLn "5. Не кофе"
    choice <- getLine
    case readMaybe choice of
        Just index -> do
            let drinkType = case index of
                    1 -> Frappe
                    2 -> Tea
                    3 -> Espresso
                    4 -> IceCoffee
                    5 -> NonCoffee
                    _ -> error "Неверный выбор"
            let drinkOptions = filter (\case Drink t _ _ -> t == drinkType; _ -> False) menu
            if null drinkOptions
                then do
                    putStrLn "Неверный выбор. Пожалуйста, выберите существующий напиток."
                    chooseDrink menu
                else do
                    printDrinkOptions drinkOptions
                    putStrLn "Выберите номер напитка:"
                    drinkChoice <- getLine
                    case readMaybe drinkChoice of
                        Just choiceIndex -> do
                            let selectedDrink = drinkOptions !! (choiceIndex - 1)
                            putStrLn "Напиток выбран."
                            case drinkType of
                                Espresso -> do
                                    size <- chooseEspressoSize
                                    return $ updateDrinkWithSize selectedDrink size
                                _ -> do
                                    additives <- chooseAdditives menu
                                    putStrLn $ "Напиток '" ++ drinkInfoFromOption selectedDrink ++ "' выбран."
                                    return $ updateDrinkWithAdditives selectedDrink additives
                        Nothing -> do
                            putStrLn "Неверный выбор. Пожалуйста, выберите существующий напиток."
                            chooseDrink menu
        _ -> do
            putStrLn "Неверный выбор. Пожалуйста, выберите существующий напиток."
            chooseDrink menu

-- Вспомогательная функция для вывода опций начинок
printFillingOptions :: [Product] -> IO ()
printFillingOptions options = do
    let fillings = filter isFilling options
    putStrLn $ unlines $ zipWith (\n option -> show n ++ ". " ++ fillingInfoFromOption option) [1..] fillings

-- Вспомогательная функция для получения информации о начинке
fillingInfoFromOption :: Product -> String
fillingInfoFromOption (FillingProduct filling) = fillName filling ++ " - " ++ show (fillPrice filling) ++ " руб."
fillingInfoFromOption _ = ""

-- Функция для проверки, является ли продукт начинкой
isFilling :: Product -> Bool
isFilling (FillingProduct _) = True
isFilling _ = False

-- Функция для выбора начинки
chooseFilling :: [Product] -> IO [Product]
chooseFilling menu = do
    putStrLn "Желаете начинку к выпечке?"
    putStrLn "1. Да"
    putStrLn "2. Нет"
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Выберите начинку (введите номер или '0' для отмены):"
            printFillingOptions menu
            choice <- getLine
            case readMaybe choice of
                Just 0 -> return []
                Just index -> do
                    let selectedFilling = menu !! (index - 1)
                    putStrLn $ "Начинка '" ++ fillingInfoFromOption selectedFilling ++ "' выбрана."
                    return [selectedFilling]
                Nothing -> do
                    putStrLn "Неверный выбор. Пожалуйста, выберите номер начинки или введите '0' для отмены."
                    chooseFilling menu
        "2" -> return []
        _   -> do
            putStrLn "Неверный выбор. Пожалуйста, выберите 1 или 2."
            chooseFilling menu

-- Вспомогательная функция для вывода опций выпечки
printPastryOptions :: [Product] -> IO ()
printPastryOptions options = putStrLn $ unlines $ zipWith (\n option -> show n ++ ". " ++ pastryInfoFromOption option) [1..] options

-- Вспомогательная функция для получения информации о выпечке
pastryInfoFromOption :: Product -> String
pastryInfoFromOption (Pastry _ name price) = name ++ " - " ++ show price ++ " руб."
pastryInfoFromOption _ = ""

-- Вспомогательная функция для выбора размера выпечки
chooseSize :: IO Double
chooseSize = do
    putStrLn "Выберите размер выпечки:"
    putStrLn "1. Стандарт"
    putStrLn "2. Большой"
    choice <- getLine
    case readMaybe choice of
        Just 1 -> return 1.0
        Just 2 -> return 2.0
        _ -> do
            putStrLn "Неверный выбор. Пожалуйста, выберите 1 или 2."
            chooseSize

-- Обновленная функция для обновления выпечки с начинками и размером
updatePastryWithFillingsAndSize :: Product -> [Product] -> IO Product
updatePastryWithFillingsAndSize (Pastry pastryType name price) fillings = do
    size <- chooseSize
    if null fillings
        then do
            putStrLn "Выбрана выпечка без начинки."
            let updatedPrice = round $ fromIntegral price * size
            return $ Pastry pastryType (name ++ " (" ++ show size ++ "x1) (WithoutFilling)") updatedPrice
        else do
            putStrLn "Выбрана выпечка с начинкой."
            let quantity = length fillings
            let fillingCost = fromIntegral $ calculateItemTotal $ head fillings
            let updatedPrice = round $ (fromIntegral price * size) + fillingCost
            return $ Pastry pastryType (name ++ " (" ++ show size ++ "x" ++ show quantity ++ ") (WithFilling)") updatedPrice
updatePastryWithFillingsAndSize _ _ = error "Неверный тип продукта"

-- Функция выбора выпечки с размером
choosePastryWithSize :: [Product] -> IO Product
choosePastryWithSize menu = do
    putStrLn "Выберите тип выпечки:"
    putStrLn "1. С начинкой"
    putStrLn "2. Без начинки"
    choice <- getLine
    case readMaybe choice of
        Just index -> do
            let pastryType = case index of
                    1 -> WithFilling
                    2 -> WithoutFilling
                    _ -> error "Invalid choose"
            let pastryOptions = filter (\case Pastry t _ _ -> t == pastryType; _ -> False) menu
            if null pastryOptions
                then do
                    putStrLn "Неверный выбор. Пожалуйста, выберите существующую выпечку."
                    choosePastryWithSize menu
                else do
                    printPastryOptions pastryOptions
                    putStrLn "Выберите номер выпечки:"
                    pastryChoice <- getLine
                    case readMaybe pastryChoice of
                        Just choiceIndex -> do
                            let selectedPastry = pastryOptions !! (choiceIndex - 1)
                            putStrLn "Выпечка выбрана."
                            case pastryType of
                                WithFilling -> do
                                    fillings <- chooseFilling menu
                                    updatePastryWithFillingsAndSize selectedPastry fillings
                                WithoutFilling -> return selectedPastry
                        Nothing -> do
                            putStrLn "Неверный выбор. Пожалуйста, выберите существующую выпечку."
                            choosePastryWithSize menu
        _ -> do
            putStrLn "Неверный выбор. Пожалуйста, выберите существующую выпечку."
            choosePastryWithSize menu

-- Основная функция заказа
main :: IO ()
main = do
    putStrLn "Добро пожаловать в нашу кофейню!"
    contents <- B.readFile "src/data.json"
    --putStrLn "Содержимое файла data.json:"
    --print contents
    case eitherDecode contents of
        Left err -> print err
        Right parsedMenu -> do
            --print parsedMenu         
            order <- makeOrder parsedMenu []
            printOrder order
            --putStrLn $ "Итого: " ++ show (calculateTotal order) ++ " руб."


-- Функция совершения заказа
makeOrder :: [Product] -> [Product] -> IO [Product]
makeOrder menu currentOrder = do
    putStrLn "Желаете заказать напиток или выпечку?"
    putStrLn "1. Напиток"
    putStrLn "2. Выпечка"
    putStrLn "3. Завершить заказ"
    choice <- getLine
    case choice of
        "1" -> do
            drink <- chooseDrink menu
            let newOrder = currentOrder ++ [drink]
            putStrLn $ "Стоимость напитка: " ++ show (calculateTotal newOrder) ++ " руб."
            makeOrder menu newOrder
        "2" -> do
            pastry <- choosePastryWithSize menu
            let newOrder = currentOrder ++ [pastry]
            putStrLn $ "Стоимость выпечки: " ++ show (calculateTotal newOrder) ++ " руб."
            makeOrder menu newOrder
        "3" -> return currentOrder
        _   -> do
            putStrLn "Неверный выбор. Пожалуйста, выберите 1, 2 или 3."
            makeOrder menu currentOrder

-- Функция вывода заказа с деталями
printOrder :: [Product] -> IO ()
printOrder order = do
    putStrLn "Ваш заказ:"
    mapM_ printProductDetails order
    putStrLn $ "Итого: " ++ show (calculateTotal order) ++ " руб."

-- Функция вывода деталей продукта
printProductDetails :: Product -> IO ()
printProductDetails product = case product of
    (Drink drinkType name price) ->
        putStrLn $ "Напиток: " ++ name ++ " (" ++ show drinkType ++ ") - " ++ show price ++ " руб."
    (Pastry pastryType name price) ->
        putStrLn $ "Выпечка: " ++ name ++ " (" ++ show pastryType ++ ") - " ++ show price ++ " руб."
    _ -> putStrLn "Неизвестный продукт"


-- Функция расчета общей стоимости заказа
calculateTotal :: [Product] -> Int
calculateTotal = sum . map calculateItemTotal

-- Функция расчета стоимости отдельного продукта
calculateItemTotal :: Product -> Int
calculateItemTotal (Drink _ _ price) = price
calculateItemTotal (Pastry _ _ price) = price
calculateItemTotal (FillingProduct filling) = fillPrice filling