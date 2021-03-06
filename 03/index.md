# 3 Strings

**string**: a sequence of characters, usually representing text. The words
you are reading now are strings.

**Haskell's `String` type**: A particular way of representing strings as
lists of `Char` types. It is very important to notice that even though
the name of this type is `String`, it is not the only, or even the best way to
represent sequences of characters and text. (see
[Data.Text](https://hackage.haskell.org/package/text-1.2.2.2/docs/Data-Text.html)
and
[Data.ByteString](https://hackage.haskell.org/package/bytestring-0.10.8.2/docs/Data-ByteString.html)

The main advantage of `String` is that it is conceptually very simple: a
`String` is a `List` of `Char` (in the abstract, that is. The acutual
implementation of `String` is a linked list of `Char`, which is sometimes not
that simple from a performance perspective.)

### Lists

A list (with a lowercase l) is some ordered collection of information. The order
may not be important, but if there is no order, it's not a list, it's a set
("unordered list" is a bit of a misnomer).

Suppose you buy groceries for your family every week. You want to know what
everyone else wants from the store without having to talk to them each
individually, so you put a piece of paper on the fridge and tell everyone to
write down what they want you to get. This is an everyday example of a list: A
grocery list.

```
Grocery List
------------
- Milk
- Eggs
- Flour
- Chicken Noodle Soup
- Mushrooms
- Beef tenderloin
- Puff pastry
```

A grocery list is an everyday example of a list. The order of your grocery list
probably isn't important but there definitely is an order: The groceries aren't
written randomly all over the page, but in parallel lines starting from the top
of the page downward.

The order of the list could indicate the chronological order in which people
added their requests. That's probably not important information most of the
time, but occasionally it might be.

In the above example, notice that the first item on the list is `Milk`. If the
list is chronological, it means that `Milk` was the first thing added since you
went to the store last week. If you know you bought a half-gallon of `Milk` last
week, that probably means your family drank all the `Milk` right after you
bought it. And that might make you decide to buy a whole gallon this time.

Another interesting thing you can see in the ordering of a grocery list is that
items that are next to each other are likely to be connected to each other
conceptually. The last three items: `mushrooms, Beef tenderloin, Puff pastry`
are all ingredients for a Beef Wellington recipe. If the store happens to be all
out of puff pastry, it might make sense to skip the mushrooms and the beef,
since you can't make the recipe.

Okay, so a grocery list has an ordering that's sometimes relevant.

Here's a slightly philosophical question: Was the grocery list a list
before someone added the first item (`Milk`) to it?

In common language, we probably wouldn't call a blank piece of paper a list.
But we could perhaps call a piece of paper with the title `Grocery List` at the
top a "blank grocery list", or an "empty grocery list", although that might
sound strange.

In Haskell-land (and CS-country in general), an empty list is definitely a
list. `List` in Haskell is defined as follows:

```haskell
data List a = Nil | Cons a (List a)
```

`Nil` is the empty list. We build lists by connecting the items in the list to
each other and ultimately to the empty list. So the above definition says
roughly: "A list of `a` is either an empty list, or is the connection of an `a`
to a `List` of `a`.

Since `a` is just a stand-in for some type of thing, a list of groceries in
Haskell would be:

```haskell
data List Grocery = Nil | Cons Grocery (List Grocery)
```

"A list of groceries is either an empty list, or is the connection of a grocery
item to a list of groceries."

If `Milk`, `Eggs` and `Flour` are all of the type `Grocery`, then we can
make the following Grocery list:

```haskell
aGroceryList = Cons Milk (Cons Eggs (Cons Flour Nil))
```

This looks a little complicated though, so in Haskell, `Cons` represented
as the `:` operator and `Nil` is `[]`.

```haskell
aNicerGroceryList = Milk:Eggs:Flour:[]
```

But the syntactic sugar goes even a step further.

```haskell
theNicestGroceryList = [Milk, Eggs, Flour]
```

At the REPL:

```haskell
Prelude> data Grocery = Milk | Eggs | Flour deriving Show
Prelude> Milk:Eggs:Flour:[]
[Milk,Eggs,Flour]
Prelude> :type [Milk,Eggs,Flour]
[Milk,Eggs,Flour] :: [Grocery]
```

That last line means that the type of `[Milk,Eggs,Flour]` is a `[Grocery]`,
which means the same as `List Grocery`.

`[]` is the list constructor in Haskell, which we can see at the REPL

```haskell
Prelude> :info []
data [] a = [] | a : [a] -- Defined in ‘GHC.Types’
```

There's some overloading of `[]` here, since `[]` is representing both `List`
and `Nil`, but in practice this is nice since you build a list of `a`, `[a]`,
by consing `a` to `nil`, `a:[]`.


### Char

A `Char` is an ASCII character ([American Standard Code for Information
Interchange]( https://en.wikipedia.org/wiki/ASCII)).

[NB. `Char` actually represents Unicode characters in general, but I don't want
to bring in Unicode as a conceptual dependency here]

ASCII is a way to represent text characters as numbers.  Here's a table of all
128 ASCII characters:

```
Dec  Char                           Dec  Char     Dec  Char     Dec  Char
---------                           ---------     ---------     ----------
  0  NUL (null)                      32  SPACE     64  @         96  `
  1  SOH (start of heading)          33  !         65  A         97  a
  2  STX (start of text)             34  "         66  B         98  b
  3  ETX (end of text)               35  #         67  C         99  c
  4  EOT (end of transmission)       36  $         68  D        100  d
  5  ENQ (enquiry)                   37  %         69  E        101  e
  6  ACK (acknowledge)               38  &         70  F        102  f
  7  BEL (bell)                      39  '         71  G        103  g
  8  BS  (backspace)                 40  (         72  H        104  h
  9  TAB (horizontal tab)            41  )         73  I        105  i
 10  LF  (NL line feed, new line)    42  *         74  J        106  j
 11  VT  (vertical tab)              43  +         75  K        107  k
 12  FF  (NP form feed, new page)    44  ,         76  L        108  l
 13  CR  (carriage return)           45  -         77  M        109  m
 14  SO  (shift out)                 46  .         78  N        110  n
 15  SI  (shift in)                  47  /         79  O        111  o
 16  DLE (data link escape)          48  0         80  P        112  p
 17  DC1 (device control 1)          49  1         81  Q        113  q
 18  DC2 (device control 2)          50  2         82  R        114  r
 19  DC3 (device control 3)          51  3         83  S        115  s
 20  DC4 (device control 4)          52  4         84  T        116  t
 21  NAK (negative acknowledge)      53  5         85  U        117  u
 22  SYN (synchronous idle)          54  6         86  V        118  v
 23  ETB (end of trans. block)       55  7         87  W        119  w
 24  CAN (cancel)                    56  8         88  X        120  x
 25  EM  (end of medium)             57  9         89  Y        121  y
 26  SUB (substitute)                58  :         90  Z        122  z
 27  ESC (escape)                    59  ;         91  [        123  {
 28  FS  (file separator)            60  <         92  \        124  |
 29  GS  (group separator)           61  =         93  ]        125  }
 30  RS  (record separator)          62  >         94  ^        126  ~
 31  US  (unit separator)            63  ?         95  _        127  DEL
```

Most of the first 32 are control characters intended for use by teletype
machines and magnetic tape readers, so you don't have to worry about them. A few
are still used though, `\n` in Haskell means newline and is the same as ASCII 10
`\LF`.

We can see the correspondence at the REPL:

```haskell
Prelude> import Data.Char
Prelude Data.Char> ord 'a'
97
Prelude Data.Char> chr 97
'a'
Prelude Data.Char> :type ord
ord :: Char -> Int
Prelude Data.Char> :type chr
chr :: Int -> Char
Prelude Data.Char> map ord "foobar"
[102,111,111,98,97,114]
Prelude Data.Char> map chr [102,111,111,98,97,114]
"foobar"
```

### Strings II

A String is just a list of `Char`, or `[Char]`:

```haskell
Prelude> :info String
type String = [Char] 	-- Defined in ‘GHC.Base’
```

The double quotes in "foobar" are just syntactic sugar for:

```haskell
Prelude> ['f','o','o','b','a','r']
"foobar"
```

## 3.4 Top-level versus local definitions

### Exercises: Scope

1. `y` is in scope for `z`
2. `h` is not in scope for `g`
3. `pi` is in scope, since it's in Prelude, but `d` is not in scope for
the definition of `r`. `d` is an argument in `area` but `r` can't see into
`area`'s definition.
4. This fixes what was wrong in the previous question. The `where` clause means
that `r` can access `area`'s arguments. However, `r` is now locally defined to
`area`, so another top level function wouldn't be able to see it.

## 3.5 Types of concatenation functions

### Exercises: Syntax Errors
1. Need `(++)` not `++`
2. Single quotes are type `Char` not type `String`
3. This one works.

## Chapter Exercises

### Reading syntax:

1.  a. correct
    b. not correct, need `(++)` not `++`
    c. correct
    d. incorrect, string not closed
    e. incorrect, wrong arg order
    f. correct
    g. incorrect, no integer argument
    h. correct

2.  a. c
    c. e
    d. a
    e. b

### Building Functions:

1.  a. `(++ "!")`
    b. `(!! 4)`
    c. `(drop 6)`

2. [See `Exercise2.hs`](/03/Exercise2.hs)
3. [See `Exercise3.hs`](/03/Exercise3.hs)
4. [See `Exercise4.hs`](/03/Exercise4.hs)
5. [See `Exercise5.hs`](/03/Exercise5.hs)
6. [See `Exercise5.hs`](/03/Exercise5.hs)
