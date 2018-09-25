# Лаборатория Касперского и Haskell

Вопросы можно задавать с места в любое время. Ссылка на этот документ — **[bit.ly/rif-18](https://bit.ly/rif-18)**.

## Подготовка

1. Установить **`stack`** по инструкции **[haskellstack.org](https://haskellstack.org)**

   ```sh
   brew install haskell-stack
   ```

2. Установить дополнительные инструменты (не обязательно)

   ```sh
   stack install yesod-bin
   ```

3. Забрать и собрать демо [github.com/**cblp/rif18**](https://github.com/cblp/rif18)

   ```sh
   git clone https://github.com/cblp/rif18.git
   cd rif18/webserver
   stack build
   stack exec webserver
   ```

4. Потыкать палочкой [localhost:3000](http://localhost:3000)


## Обо мне

Юрий Сыровецкий, старший разработчик/эксперт

1. НИИ, 5 лет, С++
2. Яндекс, 4 года, Python, С++...
3. Лаборатория Касперского, 2,5 года, Haskell, C...

[cblp.su](http://cblp.su)
[@cblp_su](https://twitter.com/cblp_su)

## Сообщество RuHaskell

1. [**RuHaskell**.org](https://ruhaskell.org)

2. [t.me/**haskellru**](https://t.me/haskellru)

## Что мы делаем в ЛК

### [**os**.kaspersky.com](https://os.kaspersky.com)&nbsp;— Kaspersky&nbsp;Security&nbsp;System&nbsp;(KSS), KasperskyOS, Kaspersky&nbsp;Secure&nbsp;Hypervisor, Kaspersky&nbsp;IoT&nbsp;Secure&nbsp;Gateway

#### Kaspersky Security System (KSS)

1. Язык описания конфигурации безопасности, неполный по Тьюрингу язык с системой типов
   - Компилятор в С
   - Инструментарий разработчика
2. Реализация конкретных моделей безопасности (со своими языками)
   - Плагины к основному компилятору

#### KasperskyOS

1. Микроядро
2. Интеграция KSS
3. Каждый процесс — домен безопасности

#### KSS on Linux

Домен безопасности — Linux container.

#### Kaspersky&nbsp;Secure&nbsp;Hypervisor

KOS, внутри которой запущены паравиртуалки с Windows, Linux и т. п.

### Почему мы выбрали Haskell

1. Сначала был С++
2. Haskell проще
3. Haskell безопаснее
4. Развитый инструментарий для разработки
5. Библиотеки для парсеров, компиляторов, кодогенерации

## Опрос аудитории. Знаете ли вы _?

1. λ-исчисление
2. λ-абстракция (безымянная функция как выражение)
3. Тип (в языках программирования)
4. Параметрический полиморфизм (generics)
5. Функциональное программирование
6. Языки семейства Lisp — CL, Clojure, Scheme (Guile, Racket)
7. Языки семейства ML — F#, Haskell, ISWYM, OCaml (ReasonML), Rust, StandardML
8. Языки с мощной поддержкой ФП — все перечисленные + Scala
9. Haskell

## Что хотите вынести сегодня?

1. Познакомиться с Haskell
2. Улучшить своё понимание ФП и навыки ФП в других языках
3. Создание компиляторов
4. Безопасное программирование

# λ-исчисление

## переменная

```haskell
x

y

--

sin

--

+

*

--

<<<

>=>

--

integral

∫
```

## аппликация (применение функции)

```haskell
(f x)

((f x) y)

(((f x) y) z)

...

--

(f (g x))

(f (g (h x)))

...
```

## абстракция (функция)

```haskell
square x = mul x x

square y = mul y y

...
```

# Haskell

## Базовый синтаксис

### аппликация

```haskell
f x

f x y

f x y z

--

2 + 2 * 2

--

sqrt (square x + square y)

--

HTTP.post (server ++ path) body

--

   f(x, y)  g(z)

--

   │ ╰──┬─╯ │ │
функция 1   2 3

--

       ╭───────────────1───────────────╮ ╭2─╮
filter (\c -> elem (toUpper c) "ABCDEF") word
                   ╰────1────╯ ╰──2───╯
```

### определение констант и функций

```haskell
π = 3.1415926

norm (x, y) = sqrt (square x + square y)

isPalindrome xs = xs == reverse xs
```
## Типы

```haskell
значение :: тип
```

### определение типа

```haskell
data Тип = Конструктор Тип₁ Тип₂ ...

--

data Point2D = Point2D Double Double

Point2D   0    0  :: Point2D
Point2D  10   20  :: Point2D
Point2D (-1) (-2) :: Point2D

--

data Point3D = Point3D Double Double Double

Point3D 1 2 3 :: Point3D
```

### синоним типа (type alias)

(как `typedef` в С)

```haskell
type Имя = Тип

--

type Age = Int

let age = 42 :: Int
age :: Age

type String = [Char]
("РИФ" :: String) == ['Р', 'И', 'Ф' :: Char]

type ParserState = (Position, Input)
```

## Тип-сумма

### (вариант [variant], объединение [union], тип-альтернатива)

```haskell
data Тип = Конструктор₁ Типы...
         | Конструктор₂ Типы...
         | ...

data Bool = False | True
-- False :: Bool
-- True  :: Bool

-- | Извлекает значение по ключу из словаря
lookup key dict = ...

let (found, value) = lookup key dict
case found of
	True  -> use value
	False -> handleError  -- но что в value?
	
data LookupResult = Found Value | NotFound
-- Found 34 :: LookupResult
-- NotFound :: LookupResult

case lookup key dict of
	Found value -> use value    -- value только здесь
	NotFound    -> handleError  -- а здесь нет
```

## Параметрический полиморфизм

```haskell
-- | aka Option/None/Some
-- (стандартная библиотека base)
data Maybe a = Nothing | Just a

--

-- | Универсальная сумма
-- (стандартная библиотека base)
data Either a b = Left a | Right b

--

-- | Связный список
-- (аналог [a] из стандартной библиотеки base)
data List a = Nil | Cons a (List a)

--

⁉️

--

-- | Двоичное дерево
data BinTree a = Empty | Node (Tree a) a (Tree a)

singletonTree x = Node Empty x Empty

--

-- | Просто дерево
-- (стандартная библиотека containers, модуль Data.Tree)
data Tree a = Node a (Forest a)
type Forest a = [Tree a]

--

⁉️
```

## `do`-нотация

### (императивный стиль)

```haskell
main = do
	name <- getLine  -- почти присваивание
	print ("Hello " ++ name)
```

## Сигнатуры типов

```haskell
x :: a
f :: a -> b
f :: a -> b -> c
f :: a -> b -> c -> d

--

norm :: (Double, Double) -> Double
norm (x, y) = sqrt (square x + square y)

--

lookup ::
    key ->
    Map key value ->
    Maybe value

--

main :: IO ()
main = do
	name <- getLine
	print ("Hello " ++ name)
```

### ограничения (constraints, свойства типа)

```haskell
λ> 2 + 2
    4

λ> 2.0 + 2.0
    4.0

--

λ> :info +
class Num a where
	(+) :: a -> a -> a
	...
	-- Defined in ‘GHC.Num’
infixl 6 +

λ> :info Num
class Num a where
	(+) :: a -> a -> a
	(-) :: a -> a -> a
	(*) :: a -> a -> a
	negate :: a -> a
	abs :: a -> a
	signum :: a -> a
	fromInteger :: Integer -> a
	-- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’

--

import Data.Aeson

encode :: ToJSON a => a -> ByteString
decode :: FromJSON a => ByteString -> Maybe a
```

## Выведение типов (в обе стороны)

```haskell
λ> 1 / 3^9 * 3^9
    0.9999999999999999

λ> 1 / 3^9 * 3^9 :: Rational
    1 % 1
```

### Подсказки от компилятора

```haskell
case cmd of
	...
    CmdServe -> cmdServe _ ui
    ...

Main.hs:144:30: error:
    • Found hole: _ :: Storage.Handle
    • In the first argument of ‘cmdServe’, namely ‘_’
      In the expression: cmdServe _ ui
      In a case alternative: CmdServe -> cmdServe _ ui
    • Relevant bindings include
        today :: Day (bound at Main.hs:120:5)
        brief :: Bool (bound at Main.hs:119:23)
        cmd :: CmdAction (bound at Main.hs:119:19)
        ui :: ConfigUI (bound at Main.hs:119:16)
        h :: Storage.Handle (bound at Main.hs:119:14)
        runCmdAction :: Storage.Handle -> ConfigUI ->
                        CmdAction -> Bool -> Storage ()
          (bound at Main.hs:119:1)
      Valid substitutions include
        undefined :: forall (a :: TYPE r).
                     GHC.Stack.Types.HasCallStack => a
          (imported from ‘Prelude’ at Main.hs:7:8-11
            (and originally defined in ‘GHC.Err’))
    |
144 |         CmdServe -> cmdServe _ ui
    |                              ^
```

# Безопасное программирование в Haskell

## 1. Сильная типизация

```c++
// g++ -Wall -Wextra -Werror -pedantic

#include <cstdlib>
#include <iostream>
using namespace std;

int main() {
    uint16_t x = 65535;
    cout << x << endl;  // 65535

    uint64_t y = x * x;
    cout << y << endl;  // ?

    return 0;
}
```

Проверим-ка! (файл «`weaktypes.cpp`»)

```haskell
import Data.Word

main = do
    let x = 65535 :: Word16
    print x
    -- 65535

    let y = x * x :: Word64
    {-
    a.hs:8:13: error:
        • Couldn't match expected type ‘Word64’
                      with actual type ‘Word16’
        • In the expression: x * x :: Word64
          In an equation for ‘y’: y = x * x :: Word64
          In the expression:
            do let x = ...
               print x
               let y = ...
               print y
               ....
      |
    8 |     let y = x * x :: Word64
      |             ^^^^^
    -}

    let y1 = fromIntegral (x * x) :: Word64
    print y1  -- 1

    let y2 = fromIntegral x * fromIntegral x :: Word64
    print y2  -- 4294836225
```

## 2. Типы выражают контракты

## `NULL` — ошибка на миллиард долларов

- [en.wikipedia.org/wiki/**Tony_Hoare**](https://en.wikipedia.org/wiki/Tony_Hoare)

Пример сигнатуры функции на Java:

```java
public static <T> String join(Collection<T> from,
                              String separator)
```

Она может вернуть `null`? Легко!

Аналог на C:

```c
char * join(const void ** from, const char * separator)
```

Аналог на Haskell:

```haskell
join :: collection t -> String -> String
```

В языке просто нет `null` и нет способа вернуть его из функции. Функция обязана вернуть `String`.

### Альтернатива `NULL` — Maybe

```haskell
data Maybe a = Nothing | Just a

lookup ::
	key ->
	Map key value ->  -- ^ словарь с ключами типа key
					  -- и значенями типа value
	Maybe value

case lookup "ex" ample of
	Nothing -> _
		-- нет способа получить несуществующее значение
	Just value -> _
		-- обязан проверить результат, чтобы извлечь значение
```

## Проверка исчерпания образцов (exhaustive patterns)

```haskell
data Lang = En | Ru

loginButtonText lang =
    case lang of
        En -> "Log in"
        Ru -> "Войти"
```

Добавляем новый язык

```haskell
data Lang = En | Fr | Ru
---------------- ^^

loginButtonText lang =
    case lang of
        En -> "Log in"
        Ru -> "Войти"
```

Ошибка!

```haskell
a.hs:5:24: error: [-Wincomplete-patterns, -Werror=incomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: Fr
  |
5 | loginButtonText lang = case lang of
  |                        ^^^^^^^^^^^^...
```

## Чистый язык

```haskell
import Data.Time
fromGregorian :: Integer -> Int -> Int -> Day
getCurrentTime :: IO UTCTime

--

import Control.Concurrent.STM
atomically :: STM a -> IO a

atomically $ do
	x <- readTChan ch1
	writeTChan ch2 x
	print x  -- error:
			 -- Couldn't match expected type ‘STM a’
			 --             with actual type ‘IO ()’
```

## 3. Типы пишут код

```haskell
data Person =
	Person{
		name    :: Text,
		age     :: Int,
		address :: [Text]
	}
	deriving (Show, Eq, Generic, FromJSON, ToJSON)

λ> me = Person{name = "Yuri", age = 33, address = ["Moscow","Russia"]}
λ> putStrLn $ encode me
{"address":["Moscow","Russia"],"age":33,"name":"Yuri"}

encode :: ToJSON a => a -> ByteString

decode       :: FromJSON a => ByteString -> Maybe a
eitherDecode :: FromJSON a => ByteString -> Either String a
```

### Типы пишут тесты

```haskell
λ> quickCheck $ \xs -> reverse (reverse xs) == xs
+++ OK, passed 100 tests.

--

deriving instance Arbitrary Person

λ> quickCheck $ \person@Person{age} -> validate person ==> age > 0
+++ OK, passed 100 tests.
```

### Легко создавать типы ad hoc

```haskell
createSymLink :: FilePath -> FilePath -> IO ()

createSymLink "link_name"   "target_file"
createSymLink "target_file" "link_name"


--

newtype LinkName = LinkName FilePath

newtype TargetFile = TargetFile FilePath

createSymLink :: TargetFile -> LinkName -> IO ()

createSymLink (LinkName "name") (TargetFile "target_file")
```

# Почему Haskell безопасный?

1. Кодирование контрактов в типах позволяет программе продолжать работать при рефакторинге.
2. Легко создавать типы, легко описывать логику и контракты на типах.
3. Чистота защищает от нежелательных побочных эффектов, сохраняя логику.
4. Богатые возможности для построения абстракций и управления сложностью.

# практика

1. Экранирование HTML в комментариях
2. Работа с базой данных
   1. `NOT NULL`
