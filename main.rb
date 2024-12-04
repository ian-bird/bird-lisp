# read takes a lisp string and converts it into a list data structure
# do_eval takes a list data structure and evaluates it as code.
# There is a single environment and data can be saved to it using label.
def do_read str
  if str.nil? or str.empty? 
    nil
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

def do_eval list
  ($env[list].nil? ? list : $env[list]) if list.is_a? Symbol
  return list unless list.is_a? Array
  case list.first
  when :quote
      list[1]
  when :atom
      !list[1].is_a?(Array)
  when :eq
    list[1..].all? {|e| do_eval(e) == do_eval(list[1]) }
  when :car
      do_eval(list[1]).first
  when :cdr
      do_eval(list[1])[1..]
  when :cons
      [do_eval(list[1])] + do_eval(list[2])
  when :if
    do_eval(list[1]) ? do_eval(list[2]) : do_eval(list[3])
  when :lambda
    list
  when :label
    $env[list[1]] = do_eval(list[2])
  when :apply
    do_eval([list[1]] + do_eval(list[2]))

  when :sub
    do_eval(list[1]) - do_eval(list[2])
  when :lt
    do_eval(list[1]) < do_eval(list[2])
  when Array
    list.first.first == :lambda ? apply(list[0], list[1..]) : do_eval([do_eval(list[0])] + list[1..])
  when Symbol
    apply $env[list[0]], list[1..]
  else
    throw "unknown form #{list}"
  end
end

def apply lambda_s, args
  params = lambda_s[1]
  body = lambda_s[2]
  evaluated_args = args.map{|arg| do_eval(arg)}
  if params.is_a? Symbol
    s = substitute(body, {params => evaluated_args})
    do_eval(s)
  else
    throw "arity mismatch: expected #{params.count}, got #{args.count}" if args.count != params.count
    param_to_arg = params.zip(evaluated_args).to_h
    do_eval(substitute(body, param_to_arg))
  end
end

def substitute body, param_to_arg_map
  if body.is_a? Array
    body.map{|val| substitute(val, param_to_arg_map)}
  elsif  param_to_arg_map.has_key? body
    [:quote, param_to_arg_map[body]]
  else
    body
  end
end

# core library
[
  "(label foldl (lambda 
                  (f v coll) 
                  (if (eq coll (quote ())) 
                    v 
                    (foldl f (f v (car coll)) (cdr coll)))))",

  "(label plus (lambda(a b)(sub a (sub 0 b))))",

  "(label add (lambda args (foldl plus 0 args)))",

  "(label not (lambda(a)(if a #f #t)))",

  "(label bor (lambda(a b)(if a #t b)))",

  "(label or (lambda args (foldl bor #f args)))",

  "(label band (lambda(a b)(if a b #f)))",

  "(label and (lambda args (foldl band #t args)))",

  "(label gt (lambda(a b)(not (or (lt a b) (eq a b)))))",

  "(label gte (lambda(a b)(or (gt a b) (eq a b))))",

  "(label lte (lambda(a b)(or (lt a b) (eq a b))))",

  "(label reverse (lambda(coll)(foldl (lambda(a b)(cons b a)) (quote ()) coll)))",

  "(label foldr (lambda(f v coll)(foldl (lambda(a b)(f b a)) v (reverse coll))))",

  "(label map (lambda(f coll)(foldr (lambda(e acc)(cons (f e) acc)) (quote ()) coll)))",

  "(label cat (lambda(l1 l2)(foldr cons l2 l1)))",

  "(label papply (lambda args1 (lambda args2 (apply (car args1) (cat (cdr args1) args2)))))"


                  ].map {|lisp_code| do_eval(read(lisp_code))}

                  
                  