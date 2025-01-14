from typing import List, Union, Optional, Set, Dict

# Тип для узлов AST
ASTNode = Union[
    Dict[str, Union[str, 'ASTNode']],  # Symbol
    Dict[str, 'ASTNode'],  # Iterat, Capture, NoCapture, Concat, Alternat, LookAhead
    int,  # GroupLink
]

def tokenize(regex: str) -> List[str]:
    tokens = []
    i = 0
    while i < len(regex):
        if regex[i] in {'(', ')', '|', '*', '?', ':', '='}:
            tokens.append(regex[i])
            i += 1
        elif regex[i].isalpha() or regex[i].isdigit():
            tokens.append(regex[i])
            i += 1
        else:
            raise ValueError(f"Недопустимый символ: {regex[i]}")
    tokens.append('$')  # Добавляем конец строки
    return tokens

# Парсер
def parse(regex: str) -> ASTNode:
    tokens = tokenize(regex)
    pos = 0
    group_count = 0

    def current_token() -> str:
        nonlocal pos
        if pos < len(tokens):
            return tokens[pos]
        return '$'

    def consume(expected: Optional[str] = None):
        nonlocal pos
        if expected and current_token() != expected:
            raise ValueError(f"Ожидался токен {expected}, но получен {current_token()}")
        pos += 1

    def parse_alternative() -> ASTNode:
        node = parse_concatination()
        while current_token() == '|':
            consume('|')
            right = parse_concatination()
            node = {'type': 'Alternat', 'left': node, 'right': right}
        return node

    def parse_concatination() -> ASTNode:
        node = parse_iteration()
        while current_token() not in {'|', ')', '$'}:
            right = parse_iteration()
            node = {'type': 'Concat', 'left': node, 'right': right}
        return node

    def parse_iteration() -> ASTNode:
        node = parse_low()
        while current_token() == '*':
            consume('*')
            node = {'type': 'Iterat', 'node': node}
        return node

    def parse_low() -> ASTNode:
        nonlocal group_count
        if current_token() == '(':
            consume('(')
            if current_token() == '?':
                consume('?')
                if current_token() == ':':
                    consume(':')
                    node = parse_alternative()
                    consume(')')
                    return {'type': 'NoCapture', 'node': node}
                elif current_token() == '=':
                    consume('=')
                    node = parse_alternative()
                    consume(')')
                    return {'type': 'LookAhead', 'node': node}
                elif current_token().isdigit():
                    group_index = int(current_token())
                    consume()
                    consume(')')
                    return {'type': 'GroupLink', 'group_index': group_index}
            else:
                group_count += 1
                group_index = group_count
                node = parse_alternative()
                consume(')')
                return {'type': 'Capture', 'node': node, 'index': group_index}
        elif current_token().isalpha() or current_token().isdigit():
            val = current_token()
            consume()
            return {'type': 'Symbol', 'val': val}
        else:
            raise ValueError(f"Недопустимый токен: {current_token()}")

    node = parse_alternative()
    if current_token() != '$':
        raise ValueError("Ожидался конец выражения")
    if group_count > 9:
        raise ValueError("Количество групп захвата превышает 9")
    return node

# Функция для печати AST
def print_ast(node: ASTNode, indent: str = "", is_last: bool = False, is_root: bool = True):
    """Рекурсивно печатает AST с ветками, убирая первую ветку."""
    branch = "" if is_root else ("└── " if is_last else "├── ")
    next_indent = "" if is_root else ("    " if is_last else "│   ")

    if node['type'] == 'Symbol':
        print(f"{indent}{branch}Symbol: {node['val']}")
    elif node['type'] == 'Iterat':
        print(f"{indent}{branch}Iterat:")
        print_ast(node['node'], indent + next_indent, True, False)
    elif node['type'] == 'Capture':
        print(f"{indent}{branch}Capture ({node['index']}):")
        print_ast(node['node'], indent + next_indent, True, False)
    elif node['type'] == 'NoCapture':
        print(f"{indent}{branch}NoCapture:")
        print_ast(node['node'], indent + next_indent, True, False)
    elif node['type'] == 'Concat':
        print(f"{indent}{branch}Concat:")
        print_ast(node['left'], indent + next_indent, False, False)
        print_ast(node['right'], indent + next_indent, True, False)
    elif node['type'] == 'Alternat':
        print(f"{indent}{branch}Alternat:")
        print_ast(node['left'], indent + next_indent, False, False)
        print_ast(node['right'], indent + next_indent, True, False)
    elif node['type'] == 'GroupLink':
        print(f"{indent}{branch}GroupLink (for {node['group_index']})")
    elif node['type'] == 'LookAhead':
        print(f"{indent}{branch}LookAhead:")
        print_ast(node['node'], indent + next_indent, True, False)
    else:
        raise ValueError(f"Неизвестный тип узла: {node['type']}")

class ParseError(Exception):
    pass

# Типы для узлов AST
ASTNode = dict

def validate_ast(node: ASTNode, initialized_groups: Set[int], inside_lookahead: bool = False):
    if node['type'] == 'Symbol':
        # Символы всегда корректны
        pass
    elif node['type'] == 'Iterat':
        # Проверяем узел внутри итерации
        validate_ast(node['node'], initialized_groups, inside_lookahead)
    elif node['type'] == 'Capture':
        # Проверяем, что группа захвата не находится внутри опережающей проверки
        if inside_lookahead:
            raise ValueError("Группы захвата недопустимы внутри опережающей проверки.")
        # Добавляем группу в множество инициализированных
        initialized_groups.add(node['index'])
        # Проверяем узел внутри группы
        validate_ast(node['node'], initialized_groups, inside_lookahead)
    elif node['type'] == 'NoCapture':
        # Проверяем узел внутри незахватывающей группы
        validate_ast(node['node'], initialized_groups, inside_lookahead)
    elif node['type'] == 'Concat':
        # Проверяем левую и правую части конкатенации
        validate_ast(node['left'], initialized_groups, inside_lookahead)
        validate_ast(node['right'], initialized_groups, inside_lookahead)
    elif node['type'] == 'Alternat':
        # Проверяем левую и правую части альтернативы
        validate_ast(node['left'], initialized_groups, inside_lookahead)
        validate_ast(node['right'], initialized_groups, inside_lookahead)
    elif node['type'] == 'GroupLink':
        # Проверяем, что ссылка на группу инициализирована
        if node['group_index'] not in initialized_groups:
            raise ValueError(f"Ссылка на неинициализированную группу: {node['group_index']}")
    elif node['type'] == 'LookAhead':
        # Проверяем, что внутри опережающей проверки нет групп захвата или других опережающих проверок
        if inside_lookahead:
            raise ValueError("Вложенные опережающие проверки недопустимы.")
        validate_ast(node['node'], initialized_groups, inside_lookahead=True)
    else:
        raise ValueError(f"Неизвестный тип узла: {node['type']}")

def check_correctness(ast: ASTNode) -> bool:
    try:
        # Сначала собираем все группы захвата
        initialized_groups = collect_groups(ast)
        # Затем проверяем AST
        validate_ast(ast, initialized_groups)
        print("Регулярное выражение корректно.")
        return True
    except ValueError as e:
        print(f"Ошибка: {e}")
        return False

def collect_groups(node: ASTNode) -> Set[int]:
    # Рекурсивно собирает индексы всех групп захвата в AST.
    groups = set()
    if node['type'] == 'Capture':
        groups.add(node['index'])
        groups.update(collect_groups(node['node']))
    elif node['type'] in {'Iterat', 'NoCapture', 'LookAhead'}:
        groups.update(collect_groups(node['node']))
    elif node['type'] in {'Concat', 'Alternat'}:
        groups.update(collect_groups(node['left']))
        groups.update(collect_groups(node['right']))
    return groups

# Тип для узлов AST
ASTNode = Dict[str, Union[str, int, 'ASTNode']]

def build_cfg(ast: ASTNode) -> Dict[str, List[str]]:
    grammar = {}
    non_terminal_counter = [0]
    group_inner_nt = {}  # mapping from group_index to inner non-terminal
    unresolved_links = []  # список нерешенных ссылок (для отложенной обработки)

    def get_non_terminal() -> str:
        non_terminal_counter[0] += 1
        return f"N{non_terminal_counter[0]}"

    def build(node: ASTNode) -> str:
        nt = get_non_terminal()
        if node['type'] == 'Symbol':
            grammar[nt] = [node['val']]
        elif node['type'] == 'Iterat':
            child_nt = build(node['node'])
            grammar[nt] = [f"{child_nt} {nt}", "epsilon"]
        elif node['type'] == 'Capture':
            inner_nt = build(node['node'])
            grammar[nt] = [inner_nt]
            index = node['index']
            if index in group_inner_nt:
                raise ValueError(f"Duplicate capture index: {index}")
            group_inner_nt[index] = inner_nt
        elif node['type'] == 'NoCapture':
            child_nt = build(node['node'])
            grammar[nt] = [child_nt]
        elif node['type'] == 'Concat':
            left_nt = build(node['left'])
            right_nt = build(node['right'])
            grammar[nt] = [f"{left_nt} {right_nt}"]
        elif node['type'] == 'Alternat':
            left_nt = build(node['left'])
            right_nt = build(node['right'])
            grammar[nt] = [left_nt, right_nt]
        elif node['type'] == 'GroupLink':
            group_index = node['group_index']
            if group_index in group_inner_nt:
                grammar[nt] = [group_inner_nt[group_index]]
            else:
                # Добавляем нерешенную ссылку
                unresolved_links.append((nt, group_index))
        elif node['type'] == 'LookAhead':
            child_nt = build(node['node'])
            grammar[nt] = [child_nt]
        else:
            raise ValueError(f"Unknown node type: {node['type']}")
        return nt

    start_nt = build(ast)

    for nt, group_index in unresolved_links:
        if group_index in group_inner_nt:
            grammar[nt] = [group_inner_nt[group_index]]
        else:
            raise ValueError(f"Unresolved group link: {group_index}")

    return grammar

regex = "(?=a)"
ast = parse(regex.replace(" ", ""))
print_ast(ast)

if check_correctness(ast):
    cfg = build_cfg(ast)

    for nt, rules in cfg.items():
        print(f"{nt} → {' | '.join([''.join(rule) for rule in rules])}")
