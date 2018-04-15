# Синтаксис языка Coral


## Добавление новых конструкций и возможностей языка
Чтобы можно было более или менее естественным образом описывать процесс компиляции сразу для типов приёмов, решили добавить несколько элементов синтаксиса в язык coral:
1. В операторе case … of несколько шаблонов могут перечисляться подряд перед    оператором do:
  ``` coral
  case x of
    Pattern1 where cond1
    Pattern2 where cond2
    ...
    PatternN where condN
      where cond_common
    do
      <commands>
      done
  ```
   Конструкция эквивалентна нескольким шаблонам с одинаковым блоком do:
  ``` coral
  case x of
    Pattern1 where cond1, cond_common
    do
      <commands>
      done
    Pattern2 where cond1, cond_common
    do
      <commands>
      done
    ...
    PatternN where cond1, cond_common
    do
      <commands>
      done
  ```
2. В шаблоне в качестве шаблона подтерма может быть указано выражение `operation* x`, где `x` – переменная, `operation` – логический символ. Шаблон работает следующим образом:
  * Если соответствующий подтерм `t` имеет вид operation [args1,…,argN], то `x` становится равно [args1,…,argN].
  * Иначе `x` становится равно [t].

  В любом случае, в переменную x записывается список. Имеет смысл использовать конструкцию только для тех операций, для которых t эквивалентно operation [t].
В качестве шаблона можно указывать [p1,…,pN]++Y. В этом случае, если с шаблоном сопоставляется список L, то p1, …, pN сопоставляются с некоторыми элементами L, а Y становится равным списку оставшихся элементов L.

  При этом есть 2  записи:
  * pattern = L, тогда p1,…,pN обязательно должны встретиться в L в том же порядке.
  * Pattern ~=L, тогда p1,…,pN могут встретиться в L в произвольном порядке.

3. Запись фрагментов генерируемой программы (опять же на языке coral) в глобальные переменные-накопители:
  * Identification – блок идентификации
  * Action – действия при применении приёма

  По мере написания компилятора сюда могут добавляться ещё переменные.
Фрагмент программы добавляется оператором
``` coral
<< { добавляемый фрагмент программы  }
```
В добавляемом фрагменте можно использовать специальные символы:
  * Символ $. `$var` означает вставку на это место значение переменной `var`. Если var – терм, то ко всем переменным этого терма (которые становятся программными переменными) добавляется отличительный знак, что эта переменная была взята из терма. Например, подчёркивание спереди.
  * Символ @. `@Var` означает отложенную подстановку глобальной переменной `Var`. Отложенная подстановка выполняется перед окончательной сборкой программы. К этому времени все подставляемые переменные уже должны быть определены. Следует также добавить в язык coral оператор рекурсивной подстановки таких фрагментов.

4. Естественный синтаксис оператора замены какого-то атрибута в данном контексте. Например, при решении задачи поменять список посылок:
```coral
premises := new_premices
```
вместо более громоздкой записи в Haskell:
```haskell
  put (\s -> s{premices = new_premices})
```