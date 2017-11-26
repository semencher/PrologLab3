implement main
    open core

domains
    % Структура описывающая дерево.
    charTree =
        binTree(char, charTree, charTree);
        nil.

class predicates
    % Вывод дерева.
    show : (char, charTree).
    % Добавление вершины.
    add : (char, char, charTree, charTree [out]).
    % Ввод дерева.
    inputTree : (charTree [out]).
    % Вспомогательный метод для ввода дерева.
    iterInputTree : (charTree, charTree [out]).
    % Метод для поиска вершины в дереве.
    findV : (char, charTree, boolean [out]).
    % Запрос вешин на поиск до введения точки.
    forFind : (char, charTree).
    % Определение высоты дерева.
    hight : (charTree, integer [out]).
    % Вспомогательная функция для определения максимума из двух чисел.
    max : (integer, integer, integer [out]).

clauses
    show(Pred, binTree(Node, Left, Right)) :-
        console::write(Pred),
        console::write(" -> "),
        console::write(Node),
        console::nl,
        show(Node, Left),
        show(Node, Right).

    show(_Pred, nil).

    % Добавление корня.
    add(Node, NewNode, nil, NewTree) :-
        Node = ' ',
        !,
        NewTree = binTree(NewNode, nil, nil).

    % Добавление влево.
    add(Node, NewNode, binTree(X, L, R), NewTree) :-
        X = Node,
        L = nil,
        !,
        NewTree = binTree(X, binTree(NewNode, nil, nil), R).

    % Добавление вправо.
    add(Node, NewNode, binTree(X, L, R), NewTree) :-
        X = Node,
        R = nil,
        !,
        NewTree = binTree(X, L, binTree(NewNode, nil, nil)).

    % В случае если добавляем к узлу где все занято.
    add(Node, _NewNode, binTree(X, L, R), NewTree) :-
        X = Node,
        !,
        console::write("Попытка добавить третий дочерний узел к "),
        console::write(Node),
        console::write("."),
        console::nl,
        NewTree = binTree(X, L, R).

    % Идем вниз по дереву ищем место для добавления.
    add(Node, NewNode, binTree(X, L, R), NewTree) :-
        add(Node, NewNode, L, NewL),
        add(Node, NewNode, R, NewR),
        NewTree = binTree(X, NewL, NewR).

    add(_Node, _NewNode, nil, nil).

    inputTree(ResTree) :-
        C = stdio::readChar(),
        C <> '.',
        !,
        add(' ', C, nil, Res),
        iterInputTree(Res, ResTree).

    % Когда введена точка.
    inputTree(ResTree) :-
        ResTree = nil.

    iterInputTree(Tree, ResTree) :-
        C = stdio::readChar(),
        C = '.',
        !,
        ResTree = Tree.

    iterInputTree(Tree, ResTree) :-
        C1 = stdio::readChar(),
        C2 = stdio::readChar(),
        % Проверка дуги на корректность.
        findV(C1, Tree, Flag1),
        findV(C2, Tree, Flag2),
        Flag1 = true,
        Flag2 = false,
        !,
        add(C1, C2, Tree, CurRes),
        iterInputTree(CurRes, ResTree).

    % Если дуга неккоректна то переходим к следующей.
    iterInputTree(Tree, ResTree) :-
        iterInputTree(Tree, ResTree).

    findV(Vertex, binTree(X, _L, _R), Flag) :-
        X = Vertex,
        !,
        Flag = true.

    % Ищем в левом поддереве.
    findV(Vertex, binTree(_X, L, _R), Flag) :-
        findV(Vertex, L, Flag1),
        Flag1 = true,
        !,
        Flag = true.

    % Ищем в правом поддереве.
    findV(Vertex, binTree(_X, _L, R), Flag) :-
        findV(Vertex, R, Flag1),
        Flag1 = true,
        !,
        Flag = true.

    findV(_Vertex, _, false).

    forFind(Sym, _Tree) :-
        Sym = '.',
        !,
        console::write("End."),
        console::nl.

    % Чтобы не считывался перенос.
    forFind(Sym, Tree) :-
        Sym = '\n',
        !,
        SymN = stdio::readChar(),
        forFind(SymN, Tree).

    forFind(Sym, Tree) :-
        findV(Sym, Tree, Flag),
        console::write(Flag),
        console::nl,
        SymN = stdio::readChar(),
        forFind(SymN, Tree).

    max(Op1, Op2, Res) :-
        Op1 >= Op2,
        !,
        Res = Op1.

    max(_Op1, Op2, Res) :-
        Res = Op2.

    hight(nil, 0).

    hight(binTree(_X, L, R), ResI) :-
        hight(L, ResIL),
        hight(R, ResIR),
        max(ResIL, ResIR, ResIN),
        ResI = ResIN + 1.

clauses
    run() :-
        console::write("Введите дерево (вначале корень, потом дуги через любые разделители, поставив точку в конце):"),
        console::nl,
        inputTree(ITree),
        console::nl,
        console::write("Вы ввели дерево:"),
        console::nl,
        show(' ', ITree),
        console::nl,
        console::write("Высота введенного дерева: "),
        hight(ITree, IRes),
        console::write(IRes),
        console::nl,
        console::nl,
        console::write("Введите вершины для поиска (точку когда закончите):"),
        console::nl,
        forFind('\n', ITree),
        _ = console::readLine(),
        _ = console::readLine(),
        succeed.
        % place your own code here

end implement main

goal
    console::runUtf8(main::run).
