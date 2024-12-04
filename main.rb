# do_read takes a string as input and converts it into a 2 element array.
# the first element contains the parsed data structure,
# the second contains the remaining unparsed string.
def do_read str
  # this generates an error if we run out of string to parse and haven't done the first item yet
  if str.nil? or str.empty?
    nil
  # skip over whitespace
  elsif str.chars.first.match?(/\s/)
    do_read(str[1..])
  # convert the quoted chars into a ruby string and return that in the tuple
  elsif str.chars.first == '"'
    [str.match(/\"(\\.|[^"\\])*\"/)[0][1...-1].gsub('\"','"'), str.sub(/\"(?:\\.|[^"\\])*\"/,"")]
  # convert the numeric chars into a ruby float and return that in the tuple
  elsif str.chars.first.match?(/\d/)
    [str.match(/\d+/)[0].to_f, str.sub(/\d+/,"")]
  # if an open paren is encountered,
  # evaluate the next do reads in the special case
  # keep appending parsed data structures into the list until a closing paren
  # is reached, then return the tuple
  elsif str.chars.first == '('
    list = []
    result = [nil, str[1..]]
    while(result[0] != ')')
      result = do_read(result[1])
      list << result[0]
    end
    [list[...-1], result[1][1..]]
  # end case for recursion of list creation
  elsif str.chars.first == ')'
    [')', str[1..]]
  # convert plain text into symbols
  elsif str.chars.first.match?(/[a-zA-Z]/)
    [str.match(/\w+/)[0].to_sym, str.sub(/\w+/,"")]
  # convert #t/f into boolean
  elsif str.start_with?("#t")
    [true, str[2..]]
  elsif str.start_with?("#f")
    [false, str[2..]]
  else
    str
  end
end

# use do_read to parse as much as possible and discard the unparsed data
def read str
  do_read(str.gsub("(", " ( ").gsub(")", " ) ")).first
end

# this holds a map of symbols to replacements for them when evaluating
$env = {}

# do eval takes a list as input and returns its evaluation, either a value or another list. It'll throw
# an error if the semantics are incorrect.
def do_eval list
  ($env[list].nil? ? list : $env[list]) if list.is_a? Symbol
  return list unless list.is_a? Array
  case list.first
  when :quote # get the value without evaluating it
      list[1]
  when :atom # check if the item is not a list
      !list[1].is_a?(Array)
  when :eq 
    list[1..].all? {|e| do_eval(e) == do_eval(list[1]) }
  when :car # get the first element in a list after evaluating it
      do_eval(list[1]).first
  when :cdr # get everything but the first element in a list after evaluating it
      do_eval(list[1])[1..]
  when :cons # evaluate two lists and then attach left to the front of right
      [do_eval(list[1])] + do_eval(list[2])
  when :if # if the condition is true, evaluate the consequent, otherwise evaluate the alternative
    do_eval(list[1]) ? do_eval(list[2]) : do_eval(list[3])
  when :lambda # lambdas are evaluated elsewhere, just pass it through
    list
  when :label # associates a symbol to another value or list for replacement during evaluation
    $env[list[1]] = do_eval(list[2])
  when :apply # passes the list of arguments to the specified function 
    do_eval([list[1]] + do_eval(list[2]))

  when :sub # not strictly necessary primative definition to simplify encoding arithmetic
    do_eval(list[1]) - do_eval(list[2])
  when :lt # not strictly necessary primative definition to simplify encoding arithmetic
    do_eval(list[1]) < do_eval(list[2])
  when Array 
    # if the first element is a lambda expression, then <<apply>> the rest of the list to it
    # otherwise, evaluate the first argument and call evaluate again over the whole list
    list.first.first == :lambda ? apply(list[0], list[1..]) : do_eval([do_eval(list[0])] + list[1..])
  when Symbol
    # if a symbol is encountered then try to replace it with the labeled defintion stored in the environment
    apply $env[list[0]], list[1..]
  else
    throw "unknown form #{list}"
  end
end

# apply takes a lambda expression and a list of arguments
# and determines the output of the function, returning it.
def apply lambda_s, args
  params = lambda_s[1]
  body = lambda_s[2]
  evaluated_args = args.map{|arg| do_eval(arg)}
  if params.is_a? Symbol # handle var args case
    s = substitute(body, {params => evaluated_args})
    do_eval(s)
  else # evaluate each arg, create the map of params to evaluated args, substitute into macro body, and evaluate
    throw "arity mismatch: expected #{params.count}, got #{args.count}" if args.count != params.count
    param_to_arg = params.zip(evaluated_args).to_h
    do_eval(substitute(body, param_to_arg))
  end
end

# recursively descend body, replacing elements using the map
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

                  
                  