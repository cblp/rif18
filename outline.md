# Лаборатория Касперского и Haskell

Вопросы можно задавать с места в любое время.

## Начало

1. https://haskellstack.org/

   - `brew install haskell-stack`

2. https://github.com/cblp/rif18

   ```sh
   $ git clone https://github.com/cblp/rif18.git
   $ cd rif18
   $ stack build
   ```


## Обо мне

Юрий Сыровецкий, старший разработчик

1. НИИ, 5 лет, С++
2. Яндекс, 4 года, Python, С++...
3. Лаборатория Касперского, 2,5 года, Haskell, C...

## RuHaskell

1. https://ruhaskell.org

2. https://t.me/haskellru, @haskellru

## Что мы делаем в ЛК

### Kaspersky Security System (KSS)

1. Язык описания конфигурации безопасности
2. Реализация конкретных моделей безопасности (со своими языками)

### Kaspersky OS

1. https://os.kaspersky.com
2. Микроядро
3. Интеграция KSS
4. Гипервизор

### Почему мы выбрали Haskell

1. Сначала был С++
2. Haskell проще
3. Haskell безопаснее
4. Развитый инструментарий для парсеров, компиляторов, кодогенерации

## Опрос аудитории. Знаете ли вы _?

1. λ-исчисление
2. λ-абстракция (безымянная функция как выражение)
3. Параметрический полиморфизм (generics)
4. Функциональное программирование
5. Языки семейства ML (SML, OCaml, Haskell, Rust, ReasonML)
6. Haskell

## Что хотите вынести сегодня?

1. Познакомиться с Haskell
2. Улучшить своё понимание ФП и навыки ФП в других языках
3. Создание компиляторов
4. Безопасное программирование

# λ-исчисление

## переменная

```
x

y



sin



+

*



<<<

>=>



integral

∫
```

## аппликация (применение функции)

```haskell
(f x)

((f x) y)

(((f x) y) z)

...



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

## Синтаксис

### аппликация

```haskell
f x

f x y

f x y z



2 + 2 * 2



sqrt (square x + square y)



HTTP.post (server ++ path) body



	f(x, y)  g(z)



	│╰──┬─╯  │╰┬╯
функция 1    2 3
```

### определение констант и функций

```haskell
π = 3.1415926

norm (x, y) = sqrt (square x + square y)
```

### определение типа

```haskell
data TypeName = ConstructorName ValueTypes...

data Point2D = Point2D Double Double

data Point3D = Point3D Double Double Double
```

### сопоставление с образцом (pattern matching)

```haskell
norm (Point2D x y) = sqrt (square x + square y)

norm point = case point of
	Point2D x y -> sqrt (square x + square y)
```

### ограничения (свойства типа)

```haskell
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
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
	-- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```

## Тип-сумма

### (вариант [variant], объединение [union], тип-альтернатива)

```haskell
data Bool = False | True

-- | Извлекает значение по ключу из словаря
lookup key dict = ...

let (found, value) = lookup key dict
case found of
	True  -> use value
	False -> handleError  -- но что в value?
	
data LookupResult = Found Value | NotFound

case lookup key dict of
	Found value -> use value    -- value только здесь
	NotFound    -> handleError  -- а здесь нет
```

## Параметрический полиморфизм

```haskell
-- | aka Option/None/Some
-- (стандартная библиотека base)
data Maybe a = Nothing | Just a



-- | Универсальная сумма
-- (стандартная библиотека base)
data Either a b = Left a | Right b



-- | Связный список
-- (аналог [a] из стандартной библиотеки base)
data List a = Nil | Cons a (List a)



-- | Двоичное дерево
data BinTree a = Empty | Node (Tree a) a (Tree a)

singletonTree x = Node Empty x Empty



-- | Просто дерево
-- (стандартная библиотека containers, модуль Data.Tree)
data Tree a = Node a (Forest a)
type Forest a = [Tree a]  -- синоним типа (как `typedef` в С)
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
norm :: (Double, Double) -> Double
norm (x, y) = sqrt (square x + square y)


lookup :: k -> Map k v -> Maybe v


main :: IO ()
main = do
	name <- getLine  -- почти присваивание
	print ("Hello " ++ name)
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
        runCmdAction :: Storage.Handle
                        -> ConfigUI -> CmdAction -> Bool -> Storage ()
          (bound at Main.hs:119:1)
      Valid substitutions include
        undefined :: forall (a :: TYPE r).
                      GHC.Stack.Types.HasCallStack =>
                      a
          (imported from ‘Prelude’ at Main.hs:7:8-11
            (and originally defined in ‘GHC.Err’))
    |
144 |         CmdServe -> cmdServe _ ui
    |                              ^
```

# Безопасное программирование в Haskell

## Сильная типизация

```c++
// g++ -Wall -Wextra -Werror -pedantic

#include <cstdlib>
#include <iostream>
using namespace std;

int main() {
    uint16_t x = 65535;
    cout << x << endl;  // 65535

    uint64_t y = x * x;
    cout << y << endl;  // 18446744073709420545

    return 0;
}
```

```haskell
import Data.Word

main = do
    let x = 65535 :: Word16
    print x
    -- 65535

    let y = x * x :: Word64
    {-
    a.hs:8:13: error:
        • Couldn't match expected type ‘Word64’ with actual type ‘Word16’
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

## Тип выражает контракт

## `NULL` — ошибка на миллиард долларов

- https://en.wikipedia.org/wiki/Tony_Hoare

Пример сигнатуры функции на Java:

```java
public static <T> String join(Collection<T> from, String separator) {
```

Она может вернуть `null`? Легко!

Аналог на C:

```c
char * join(void ** from, const char * separator) {
```

Аналог на Haskell:

```haskell
join :: collection t -> String -> String
```

В языке просто нет `null` и нет способа вернуть его из функции. Функция обязана вернуть `String`.

### Альтернатива `NULL` — Maybe

```haskell
lookup :: key -> Map key value -> Maybe value

case lookup "ex" ample of
	Nothing -> _
		-- нет способа получить несуществующее значение
	Just value -> _
		-- обязан проверить результат, чтобы извлечь значение
```

## Проверка исчерпания образцов (exhaustive patterns)

```haskell
data Lang = En | Ru

loginButtonText lang = case lang of
	En -> "Log in"
	Ru -> "Войти"
```

Добавляем новый язык

```haskell
data Lang = En | Fr | Ru

loginButtonText lang = case lang of
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



import Control.Concurrent.STM
atomically :: STM a -> IO a

atomically $ do
	x <- readTChan ch1
	writeTChan ch2 x
	print x  -- error:
			 -- Couldn't match expected type ‘STM a’
			 --             with actual type ‘IO ()’
```

# практика