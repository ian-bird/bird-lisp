
def do_read str
  if str.nil? or str.empty? 
    ""
  elsif str.chars.first.match?(/\s/)
    do_read(str[1..])
  elsif str.chars.first == '"'
    [str.match(/\"(\\.|[^"\\])*\"/)[0][1...-1].gsub('\"','"'), str.sub(/\"(?:\\.|[^"\\])*\"/,"")]
  elsif str.chars.first.match?(/\d/)
    [str.match(/\d+/)[0].to_f, str.sub(/\d+/,"")]
  elsif str.chars.first == '('
    list = []
    result = [nil, str[1..]]
    while(result[0] != ')')
      result = do_read(result[1])
      list << result[0]
    end
    [list[...-1], result[1][1..]]
  elsif str.chars.first == ')'
    [')', str[1..]]
  elsif str.chars.first.match?(/[a-zA-Z]/)
    [str.match(/\w+/)[0].to_sym, str.sub(/\w+/,"")]
  elsif str.start_with?("#t")
    [true, str[2..]]
  elsif str.start_with?("#f")
    [false, str[2..]]
  else
    str
  end
end

def read str
  do_read(str.gsub("(", " ( ").gsub(")", " ) ")).first
end

$env = {}

def base_eval list
  return list unless list.is_a? Array
  case list.first
  when :quote
    if list.count != 2
      throw "ill-formed special form quote"
    else
      list[1]
    end
  when :atom
    if list.count != 2
      throw "ill-formed special form atom"
    else
      !list[1].is_a?(Array)
    end
  when :eq
    list[1..].all? {|e| base_eval(e) == base_eval(list[1]) }
  when :car
    if list.count != 2
      throw "ill-formed special form car"
    else
      list[1].first
    end
  when :cdr
    if list.count != 2
      throw "ill-formed special form cdr"
    else
      list[1][1..]
    end
  when :cons
    if list.count != 3
      throw "ill-formed special form cons"
    elsif !list[2].is_a?(Array)
      throw "tuples not supported!"
    else
      [list[1]] + list[2]
    end
  when :if
    if list.count != 4
      throw "ill-formed special form if"
    elsif base_eval(list[1])
      base_eval(list[2])
    else
      base_eval(list[3])
    end
  when :lambda
    Fn.new list[1], list[2]
  when :label
    throw "expected symbol for lvalue" unless list[1].is_a? Symbol
    $env[list[1]] = base_eval(list[2])
  when Fn
    list[0].run list[1..]
  when :plus
    base_eval(list[1]) + base_eval(list[2])
  when :sub
    base_eval(list[1]) - base_eval(list[2])
  when :gt
    base_eval(list[1]) > base_eval(list[2])
  when :lt
    base_eval(list[1]) < base_eval(list[2])
  when Array
    base_eval([base_eval(list[0])] + list[1..])
  when Symbol
    $env[list[0]].run list[1..]
  else
    throw "unknown form #{list}"
  end
end

class Fn
  def initialize params, body
    @params = params
    @body = body
  end

  def run args
    throw "arity mismatch: expected #{@params.count}, got #{args.count}" if args.count != @params.count
    evaluated_args = args.map{|arg| base_eval(arg) }
    param_to_arg = @params.zip(evaluated_args).to_h
    base_eval(substitute(@body, param_to_arg))
  end

  def substitute body, param_to_arg_map
    if body.is_a? Array
      body.map{|val| substitute(val, param_to_arg_map)}
    elsif  param_to_arg_map.has_key? body
      param_to_arg_map[body]
    else
      body
    end
  end
end
