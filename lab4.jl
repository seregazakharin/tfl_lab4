using Printf

# Типы для узлов AST
mutable struct ASTNode
  type::String
  val::Union{Nothing, String, Int}
  left::Union{Nothing, ASTNode}
  right::Union{Nothing, ASTNode}
  group_index::Union{Nothing, Int}
end

function tokenize(regex::String)
    tokens = String[]
    i = 1
    while i <= length(regex)
        if regex[i] in ['(', ')', '|', '*', '?', ':', '=']
            push!(tokens, string(regex[i]))
            i += 1
        elseif regex[i] >= 'a' && regex[i] <= 'z' || isdigit(regex[i])
            push!(tokens, string(regex[i]))
            i += 1
        else
            throw(ArgumentError("Недопустимый символ: " * string(regex[i])))
        end
    end
    push!(tokens, "+")  # Добавляем конец строки
    return tokens
end

  function parce_regex(regex::String)
    tokens = tokenize(regex)
    global pos = 1
    global group_count = 0

    function current_token()
        return pos <= length(tokens) ? tokens[pos] : "+"
    end

    function consume(expected::Union{Nothing, String} = nothing)
        global pos
        if expected != nothing && current_token() != expected
            throw(ArgumentError("Ожидался токен " * expected * ", но получен " * current_token()))
        end
        pos += 1
    end

    function parce_regex_alternative()
        node = parce_regex_concatenation()
        savednode = node
        while current_token() == "|"
            consume("|")
            right = parce_regex_concatenation()

            new_node = ASTNode("Alternat", nothing, savednode, right, nothing)
            savednode = new_node
        end
        return savednode
    end

    function parce_regex_concatenation()
        node = parce_regex_iteration()
        savednode = node
    
        # Обрабатываем оставшиеся токены в цикле
        while !(current_token() in ["|", ")", "+"])
            right = parce_regex_iteration()
    
            new_node = ASTNode("Concat", nothing, savednode, right, nothing)
            savednode = new_node
        end
        return savednode
    end

    function parce_regex_iteration()
        node = parce_regex_low()
        while current_token() == "*"
            consume("*")
            node = ASTNode("Iterat", nothing, node, nothing, nothing)
        end
        return node
    end

    function parce_regex_low()
        if current_token() == "("
            consume("(")
            if current_token() == "?"
                consume("?")
                if current_token() == ":"
                    consume(":")
                    node = parce_regex_alternative()
                    consume(")")
                    return ASTNode("NoCapture", nothing, node, nothing, nothing)
                elseif current_token() == "="
                    consume("=")
                    if isdigit(current_token()[1])
                        throw(ArgumentError("Недопустимый токен: " * current_token()))
                    end
                    node = parce_regex_alternative()
                    consume(")")
                    return ASTNode("LookAhead", nothing, node, nothing, nothing)
                elseif isdigit(current_token()[1])
                    try
                      tok = current_token()
                      group_index= parse(Int, tok)
                    catch e
                      println("Ошибка: не удалось преобразовать строку в число.", )
                    end
                    consume()  # Потребляем токен
                    consume(")")  # Закрываем скобку
                    return ASTNode("GroupLink", nothing, nothing, nothing, group_index)
                end
            else
                if isdigit(current_token()[1])
                    throw(ArgumentError("Недопустимый токен: " * current_token()))
                end
                group_count += 1
                group_index = group_count
                node = parce_regex_alternative()
                consume(")")
                return ASTNode("Capture", nothing, node, nothing, group_index)
            end
        elseif current_token() == "?" && isdigit(current_token()[2])  # Обрабатываем ссылку на группу
            group_index = parce_regex(Int, current_token()[2:end])  # Пропускаем "?" и парсим число
            consume()
            return ASTNode("GroupLink", nothing, nothing, nothing, group_index)
        elseif isletter(current_token()[1])
            val = current_token()
            consume()
            return ASTNode("Symbol", val, nothing, nothing, nothing)
        else
            throw(ArgumentError("Недопустимый токен: " * current_token()))
        end
    end

    node = parce_regex_alternative()
    if current_token() != "+"
        throw(ArgumentError("Ожидался конец выражения"))
    end
    if group_count > 9
        throw(ArgumentError("Количество групп захвата превышает 9"))
    end
    return node
  end

# Функция для печати AST
function print_ast(node::ASTNode, indent::String = "", is_last::Bool = false, is_root::Bool = true)
  branch = is_root ? "" : (is_last ? " └── " : " ├── ")
  next_indent = is_root ? "" : (is_last ? "    " : " │   ")

  if node.type == "Symbol"
      println("$indent$branch Symbol: $(node.val)")
  elseif node.type == "Iterat"
      println("$indent$branch Iterat:")
      print_ast(node.left, indent * next_indent, true, false)
  elseif node.type == "Capture"
      println("$indent$branch Capture ($(node.group_index)):")
      print_ast(node.left, indent * next_indent, true, false)
  elseif node.type == "NoCapture"
      println("$indent$branch NoCapture:")
      print_ast(node.left, indent * next_indent, true, false)
  elseif node.type == "Concat"
      println("$indent$branch Concat:")
      print_ast(node.left, indent * next_indent, false, false)
      print_ast(node.right, indent * next_indent, true, false)
  elseif node.type == "Alternat"
      println("$indent$branch Alternat:")
      print_ast(node.left, indent * next_indent, false, false)
      print_ast(node.right, indent * next_indent, true, false)
  elseif node.type == "GroupLink"
      println("$indent$branch GroupLink (for $(node.group_index))")
  elseif node.type == "LookAhead"
      println("$indent$branch LookAhead:")
      print_ast(node.left, indent * next_indent, true, false)
  else
      throw(ArgumentError("Неизвестный тип узла: $(node.type)"))
  end
end

function collect_groups(node::ASTNode)::Set{Int}
    groups = Set{Int}()
    if node.type == "Capture"
        push!(groups, node.group_index)
        union!(groups, collect_groups(node.left))
    elseif node.type in ["Iterat", "NoCapture", "LookAhead"]
        union!(groups, collect_groups(node.left))
    elseif node.type in ["Concat", "Alternat"]
        union!(groups, collect_groups(node.left))
        union!(groups, collect_groups(node.right))
    end
    return groups
end

# Функция для проверки корректности AST
function validate_ast(node::ASTNode, initialized_groups::Set{Int}, inside_lookahead::Bool = false)
  if node.type == "Symbol"
    # Cимволы корректны
  elseif node.type == "Iterat"
      # Проверяем узел внутри итерации
      validate_ast(node.left, initialized_groups, inside_lookahead)
  elseif node.type == "Capture"
      if inside_lookahead
          throw(ArgumentError("Группы захвата недопустимы внутри опережающей проверки."))
      end
      push!(initialized_groups, node.group_index)
      validate_ast(node.left, initialized_groups, inside_lookahead)
  elseif node.type == "NoCapture"
      validate_ast(node.left, initialized_groups, inside_lookahead)
  elseif node.type == "Concat"
      validate_ast(node.left, initialized_groups, inside_lookahead)
      validate_ast(node.right, initialized_groups, inside_lookahead)
  elseif node.type == "Alternat"
      validate_ast(node.left, initialized_groups, inside_lookahead)
      validate_ast(node.right, initialized_groups, inside_lookahead)
  elseif node.type == "GroupLink"
      if !(node.group_index in initialized_groups)
          throw(ArgumentError("Ссылка на неинициализированную группу: $(node.group_index)"))
      end
  elseif node.type == "LookAhead"
      if inside_lookahead
          throw(ArgumentError("Вложенные опережающие проверки недопустимы."))
      end
      validate_ast(node.left, initialized_groups, true)
  else
      throw(ArgumentError("Неизвестный тип узла: $(node.type)"))
  end
end

# Функция для проверки корректности
function check_correctness(ast::ASTNode)
  try
      initialized_groups = collect_groups(ast)
      validate_ast(ast, initialized_groups)
      println("Регулярное выражение корректно.")
      return true
  catch e
      println("Ошибка: ", e)
      return false
  end
end

# Функция для построения КС-грамматики
function build_cfg(ast::ASTNode)
    grammar = Dict{String, Vector{String}}()
    non_terminal_counter = Ref(0)
    group_inner_nt = Dict{Int, String}()
    unresolved_links = Vector{Tuple{String, Int}}()
    non_terminal_order = Vector{String}()

    function get_non_terminal()
        non_terminal_counter[] += 1
        nt = "N$(non_terminal_counter[])"
        push!(non_terminal_order, nt)
        return nt
    end

    function build(node::ASTNode)::String
        nt = get_non_terminal()
        if node.type == "Symbol"
            grammar[nt] = [node.val]
        elseif node.type == "Iterat"
            child_nt = build(node.left)
            grammar[nt] = ["$child_nt $nt", "epsilon"]
        elseif node.type == "Capture"
            inner_nt = build(node.left)
            grammar[nt] = [inner_nt]
            index = node.group_index
            if haskey(group_inner_nt, index)
                throw(ArgumentError("Индекс группы захвата дублируется: $index"))
            end
            group_inner_nt[index] = inner_nt
        elseif node.type == "NoCapture"
            child_nt = build(node.left)
            grammar[nt] = [child_nt]
        elseif node.type == "Concat"
            left_nt = build(node.left)
            right_nt = build(node.right)
            grammar[nt] = ["$left_nt $right_nt"]
        elseif node.type == "Alternat"
            left_nt = build(node.left)
            right_nt = build(node.right)
            grammar[nt] = [left_nt, right_nt]
        elseif node.type == "GroupLink"
            group_index = node.group_index
            if haskey(group_inner_nt, group_index)
                grammar[nt] = [group_inner_nt[group_index]]
            else
                push!(unresolved_links, (nt, group_index))
            end
        elseif node.type == "LookAhead"
            grammar[nt] = ["epsilon"]
        else
            throw(ArgumentError("Неизвестный тип: $(node.type)"))
        end
        return nt
    end

    start_nt = build(ast)

    for (nt, group_index) in unresolved_links
        if haskey(group_inner_nt, group_index)
            grammar[nt] = [group_inner_nt[group_index]]
        else
            throw(ArgumentError("Группы с данным индексом нет: $group_index"))
        end
    end

    return grammar, non_terminal_order
end

function print_grammar(grammar::Dict{String, Vector{String}}, order::Vector{String})
    for nt in order
        if haskey(grammar, nt)
            rules = grammar[nt]
            println("$nt → ", join(rules, " | "))
        end
    end
end

function main()
    try
        print("Введите регулярное выражение: ")
        regex = readline()
        ast = parce_regex(replace(regex, " " => ""))

        if check_correctness(ast)
            #print_ast(ast)
            grammar, order = build_cfg(ast)
            println("\nКС-грамматика:")
            print_grammar(grammar, order)
        else
            println("Выражение некорректно.")
        end
    catch e
        println("Ошибка: ", e)
        exit(1)
    end
end

main()
