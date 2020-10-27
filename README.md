# Команды ВПП
## Скалярные
#### Арифметические
| Название     | Описание                 | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------      | :----------------- | :------------------ |
| СЛЦ          | Сложение целочисленное   |  {int, int}        | int x2              |
| ВЧЦ          | Вычитание целочисленное  |  {int, int}        | int x2              |
| УМЦ          | Умножение целочисленное  |  {int, int}        | int x2              |
| ДЦ           | Деление целочисленное    |  {int, int}        | int x2              |
| МД           | Модуль                   |  {int, int}        | int x2              |
| СДА          | Сдвиг арифметический     |  {int, int}        | int x2              |
| СЛ           | Сложение                 |  {float, float}    | float x2            |
| ВЧ           | Вычитание                |  {float, float}    | float x2            |
| УМ           | Умножение                |  {float, float}    | float x2            |
| ДЛ           | Деление                  |  {float, float}    | float x2            |
| ОКР          | Округление               |  float             | float x2            |
| ОТБ          | Отбрасывание остатка     |  float             | float x2            |
| Ц->Д         | Из целого в дробное      |  int               | float x2            |

#### Логические
| Название     | Описание                 | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------      | :----------------- | :------------------ |
| ИЛИ          | Логическое ИЛИ           |  {int, int}        | int x2              |
| И            | Логические И             |  {int, int}        | int x2              |
| СР           | Логическое сравнение     |  {int, int}        | int x2              |
| ЛО           | Логическое отрицание     |  int               | int x2              |
| ЛСД          | Логический сдвиг         |  {int, int}        | int x2              |

#### Определение отношений
| Название     | Описание                 | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------      | :----------------- | :------------------ |
| БЛЦ          | Больше целочисленное     |  {int, int}        | int x2              |
| БРЦ          | Больше или равно целочисленное|  {int, int}   | int x2              |
| РВЦ          | Равно целочисленное      |  {int, int}        | int x2              |
| НРЦ          | Не равно целочисленное   |  {int, int}        | int x2              |
| МРЦ          | Меньше или равно целочисленное|  {int, int}   | int x2              |
| БЛ           | Больше                   |  {float, float}    | float x2            |
| БР           | Больше или равно         |  {float, float}    | float x2            |
| РВ           | Равно                    |  {float, float}    | float x2            |
| НР           | Не равно                 |  {float, float}    | float x2            |
| МР           | Меньше или равно         |  {float, float}    | float x2            |
| МН           | Меньше                   |  {float, float}    | float x2            |


#### Передача
| Название     | Описание                 | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------      | :----------------- | :------------------ |
| ПУ           | Передача по условию      |  {token, token}    | {none, token|}|{token, none}|
| ПСХ          | Передача синхронная      |  {token, token}    | {token, token}      |
| ПДБ          | Передача с дублированием |  token             | token x2            |

#### Установка
| Название     | Описание                 | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------      | :----------------- | :------------------ |
| УП           | Установка поколения      |  {token, int}      | token x2            |
| УТ           | Установка итерации       |  {token, int}      | token x2            |
| УИ           | Установка индекса        |  {token, int}      | token x2            |
| УС           | Установка состояния      |  {token, int}      | token x2            |
| УДВ          | Установка длины вектора  |  {addr, int}       | addr x2             |
| УНЭ          | Установка номера элемента|  {addr, int}       | {addr, int} x2      |
| НПП          | Начало подпрограммы      |                    |                     |

#### Выдача
| Название     | Описание                 | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------      | :----------------- | :------------------ |
| ВП           | Выдача поколения         |  token             | int x2              |
| ВТ           | Выдача итерации          |  token             | int x2              |
| ВИ           | Выдача индекса           |  token             | int x2              |
| ВС           | Выдача состояния         |  token             | int x2              |
| ВДВ          | Выдача длины вектора     |  {addr, int}       | int x2              |

#### Организация циклов
| Название     | Описание                                      | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------                           | :----------------- | :------------------ |
| КЦТ          | Конец цикла по интераци                       |                    |                     |
| КЦТ1         | Конец цикла по итерации с 1 отличием          |                    |                     |
| КЦИ          | Конец цикла по индексу                        |                    |                     |
| КЦИ1         | Конец цикла по индексу с 1 отличием           |                    |                     |
| КЦИВ         | Конец цикла по индексу вектора                |                    |                     |
| КЦИВ1        | Конец цикла по индексу вектора с 1 отличием   |                    |                     |
| КЦТВ         | Конец цикла по итерации вектора               |                    |                     |
| КЦТВ1        | Конец цикла по итерации вектора с 1 отличием  |                    |                     |
| ГНЦТ         | Групповое начало цикла по итерации            |                    |                     |
| ГНЦИ         | Групповое начало цикла по индексу             |                    |                     |
| ГНЦИ1        | Групповое начало цикла по индексу с 1 отличием|                    |                     |

## Векторные
#### Арифметические
| Название     | Описание                          | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------               | :----------------- | :------------------ |
| ВСЛ          | Векторное сложение                |  {addr, addr}      | addr x2             |
| ВВЧ          | Векторное вычитание               |  {addr, addr}      | addr x2             |
| ВУМ          | Векторное умножение               |  {addr, addr}      | addr x2             |
| ВДЛ          | Векторное деление                 |  {addr, addr}      | addr x2             |
| ВМД          | Векторный модуль                  |  {addr, addr}      | addr x2             |
| ВСЛЦ         | Векторное сложение целочисленное  |  {addr, addr}      | addr x2             |
| ВВЧЦ         | Векторное вычитание целочисленное |  {addr, addr}      | addr x2             |
| ВУМЦ         | Векторное умножение целочисленное |  {addr, addr}      | addr x2             |
| ВДЛЦ         | Векторное деление целочисленное   |  {addr, addr}      | addr x2             |


#### Логические
| Название     | Описание                 | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------      | :----------------- | :------------------ |
| ВИЛИ         | Векторное логическое ИЛИ |  {addr, addr}      | addr x2             |
| ВИ           | Векторное логические И   |  {addr, addr}      | addr x2             |
| ВСР          | Векторное логическое сравнение |  {addr, addr}| addr x2             |
| ВЛО          | Векторное логическое отрицание |  addr        | addr x2             |

#### Определение отношений
| Название     | Описание                 | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------      | :----------------- | :------------------ |
| ВБЛЦ         | Векторное больше целочисленное     |  {addr, addr}        | addr x2 |
| ВБРЦ         | Векторное больше или равно целочисленное|  {addr, addr}   | addr x2 |
| ВРВЦ         | Векторное равно целочисленное      |  {addr, addr}        | addr x2 |
| ВНРЦ         | Векторное не равно целочисленное   |  {addr, addr}        | addr x2 |
| ВМРЦ         | Векторное меньше или равно целочисленное|  {addr, addr}   | addr x2 |
| ВБЛ          | Векторное больше                   |  {addr, addr}    | addr x2 |
| ВБР          | Векторное больше или равно         |  {addr, addr}    | addr x2 |
| ВРВ          | Векторное равно                    |  {addr, addr}    | addr x2 |
| ВНР          | Векторное не равно                 |  {addr, addr}    | addr x2 |
| ВМР          | Векторное меньше или равно         |  {addr, addr}    | addr x2 |
| ВМН          | Векторное меньше                   |  {addr, addr}    | addr x2 |


#### Работа с массивами в памяти векторов
| Название     | Описание                 | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------      | :----------------- | :------------------ |
| СВ           | Создать вектор           |  {int, token}      | addr x2             |
| СВ0          | Создать вектор из 0      |  int               | addr x2             |
| УВ           | Удалить вектор           |  addr              | none                |
| ВЭВ          | Выдать элемент вектора   |  {addr, int}       | int x2              |
| ++           | Выдать элемент вектор, прибавить к нему 1 и записать обратно  |  {addr, int}        | addr x2              |
| УЭВ          | Установить элемент вектора| {{addr, int}, token} | addr x2          |
| ВСДЛ         | Векторный сдвиг влево    |  {addr, token}     | addr x2             |
| ВСДП         | Векторный сдвиг вправо   |  {addr, token}     | addr x2             |
| СБВ          | Собрать вектор           |  {addr, int }      | addr x2             |
| СБВП         | Сборка вектора произвольная |  {addr, addr }  | addr x2             |
| УПВ          | Упорядочить вектор       |  {addr, addr}      | addr x2             |
| ФП           | Формировать поток        |  addr              | {[token], addr}     |
| ФВ           | Формировать вектор       |  token             | addr x2             |
| ФВВМ         | Формировать вектор из внешней машины |  token | addr x2             |

#### Операции редукции
| Название     | Описание                 | Тип входных данных | Тип выходных данных |
| ------------ |:-------------------      | :----------------- | :------------------ |
| СМВ          | Суммирование элементов вектора |  addr        | token x2            |
| MAX          | Максимальный элемент вектора   |  addr        | token x2            |
| MIN          | Минимальный элемент вектора    |  addr        | token x2            |
