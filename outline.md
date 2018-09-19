# Лаборатория Касперского и Haskell

Вопросы можно задавать с места в любое время.

## Начало

1. https://haskellstack.org/

   - `brew install haskell-stack`

2. https://github.com/cblp/rif18

   ```sh
   $ git clone https://github.com/cblp/rif18.git
   $ cd rif18
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
3. Параметрический полиморфизм (дженерики)
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

### определение функции

```haskell
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

## Тип-сумма

### (вариант [variant], объединение [union], альтернатива)

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
type Forest a = [Tree a]
```

## `do`-нотация

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

# Безопасное программирование в Haskell