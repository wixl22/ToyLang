ToyLang — краткая шпаргалка (RU-синтаксис)

Мини-язык для обучения: типы, указатели, куча, структуры, методы, if/while, минимум сахара.

Фичи

Примитивы: int, float, bool, string, void
bool ведёт себя как число 0/1 (в арифметике и печати).

Структуры только в куче: переменные с типом *Struct + нов Struct.

Массивы: нов T[n] → *T (базовый адрес).

Методы — только pointer-receiver: создФунк (влад *Type x) name(...).

Управление: если/или, пока, прервать, продолжить.

Логика: &&, ||, ! — с коротким замыканием; условие принимает int/bool.

Встроенные: печать(...)/print(...), освободи(p)/free(p).

Импорт: импорт "lib.tl" (относительно текущего файла).

Базовый синтаксис
// --- переменные и присваивание ---
создПер int a;               // объявление
создПер int b := 42;         // инициализация
a := a + b;

// --- примитивы ---
создПер float pi := 3.14;
создПер bool ok := 1;        // bool как 0/1
создПер string s := "привет";

// --- условия и циклы (int/bool как условие) ---
если (a > 0) { печать("pos"); } или { печать("neg/zero"); }
пока (a > 0) { a := a - 1; }
если (2) { печать("truthy"); }     // 2 → true

// --- логика (short-circuit) ---
если (a != 0 && (10 / a) > 1) { печать("ok"); }
если (a == 0 || b == 0) { печать("zero inside"); }
печать(!ok);                         // 0 → 1, 1 → 0

// --- функции ---
создФунк add(арг int x, арг int y) возвр int { вернуть x + y; }
// эквивалент:  func add(x:int, y:int): int { return x + y; }

// --- структуры и методы (только *Struct) ---
создСтрук Person {
  атр string name;
  атр int    age;
}

создФунк (влад *Person p) birthday() возвр void {
  p.age := p.age + 1;
}

// --- создание и использование объектов в куче ---
создПер *Person p := нов Person;
p.name := "Alex";
p.age  := 30;
p.birthday();
печать(p.name, p.age);               // Alex 31

// --- указатели и массивы ---
создПер int x := 5;
создПер *int px := &x;   *px := 6;   печать(x);  // 6

создПер int n := 4;
создПер *int arr := нов int[n];
*(arr + 0) := 10; *(arr + 1) := 20; *(arr + 2) := 30; *(arr + 3) := 40;
печать(*(arr + 2));                  // 30
освободи(arr);                       // обязательно освобождаем

Карта соответствий (RU ⇄ EN)

объявления: создПер ⇄ let, создФунк ⇄ func, создСтрук ⇄ struct

ключевые: если/или/пока/прервать/продолжить/импорт/нов/вернуть/возвр/влад/арг/атр

операторы: присваивание := (временно = тоже ок); логика &&, ||, !

встроенные: печать ⇄ print, освободи ⇄ free

Правила типов (важно)

string — примитив (как int/float/bool), без нов.

Структуры нельзя объявлять как значение:
создПер Person v; // ❌
создПер *Person p := нов Person; // ✅

Сравнение указателей допускает == null и != null для любого *T.

Арифметика над числами (int/float/bool), % — только для int.

Мини-справка по ошибкам рантайма

dereference null — разыменование null (например, p.field при p == null).

store to null — запись по *null.

method receiver must be *Struct — метод объявлен не на *Type.

struct variables must be pointers — попытка создать Person без *.

void function cannot return a value / non-void ... must return a value — несоответствие return.

bad operands for ... — неверные типы для оператора.

Ошибки печатаются с координатами (line, col).

Полноценный пример: Linked List (int)
импорт "lib.tl";  // опционально; здесь не требуется

создСтрук Node { атр int value; атр *Node next; }
создСтрук List { атр *Node head; }

создФунк (влад *List l) push_front(арг int v) возвр void {
  создПер *Node n := нов Node;
  n.value := v;
  n.next  := l.head;
  l.head  := n;
  вернуть;
}

создФунк (влад *List l) push_back(арг int v) возвр void {
  создПер *Node n := нов Node;
  n.value := v;
  n.next  := null;

  если (l.head == null) { l.head := n; вернуть; }

  создПер *Node cur := l.head;
  пока (cur.next != null) { cur := cur.next; }
  cur.next := n;
  вернуть;
}

создФунк (влад *List l) sum() возвр int {
  создПер int s := 0;
  создПер *Node cur := l.head;
  пока (cur != null) { s := s + cur.value; cur := cur.next; }
  вернуть s;
}

создФунк (влад *List l) remove_even() возвр void {
  // снимаем подряд чётные головы
  пока (l.head != null && (l.head.value % 2) == 0) {
    создПер *Node n := l.head;
    l.head := n.next;
    освободи(n);
  }
  если (l.head == null) { вернуть; }

  создПер *Node prev := l.head;
  создПер *Node cur  := prev.next;
  пока (cur != null) {
    если ((cur.value % 2) == 0) {
      prev.next := cur.next;
      освободи(cur);
      cur := prev.next;
    } или {
      prev := cur;
      cur  := cur.next;
    }
  }
  вернуть;
}

создФунк (влад *List l) length() возвр int {
  создПер int k := 0;
  создПер *Node cur := l.head;
  пока (cur != null) { k := k + 1; cur := cur.next; }
  вернуть k;
}

создФунк (влад *List l) print() возвр void {
  создПер *Node cur := l.head;
  пока (cur != null) { печать(cur.value); cur := cur.next; }
  вернуть;
}

создФунк (влад *List l) clear() возвр void {
  создПер *Node cur := l.head;
  пока (cur != null) {
    создПер *Node nxt := cur.next;
    освободи(cur);
    cur := nxt;
  }
  l.head := null;
  вернуть;
}

// -------- демо --------
создФунк main() возвр void {
  создПер *List lst := нов List;

  // вставки
  lst.push_back(10);
  lst.push_front(5);
  lst.push_back(20);
  lst.push_back(30);

  печать("len =", lst.length());   // 4
  lst.print();                      // 5 10 20 30

  печать("sum =", lst.sum());       // 65

  lst.remove_even();
  lst.print();                      // 5  15  25  (если заменить push_back на квадрат, будет иначе)

  lst.clear();
  печать("len after clear =", lst.length());  // 0
  вернуть;
}

main();

Советы по стилю

Для больших программ держите код в нескольких файлах и подключайте импорт "path.tl".

Не забывайте освободи(...) для массивов и вручную аллоцированных структур.

Имена — латиница или кириллица (лексер поддерживает оба варианта).

Для диагностики можно запускать интерпретатор с флагом -d, чтобы увидеть дамп хипа.