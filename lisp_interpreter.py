import re

class LispError(Exception):
    pass

# --- Lexer ---
def tokenize(chars):
    """Convert a string of characters into a list of tokens."""
    return chars.replace('(', ' ( ').replace(')', ' ) ').split()

# --- Parser ---
class Symbol(str):
    pass

class Number(float):
    pass

def parse(program):
    """Read a Lisp expression from a string."""
    return read_from_tokens(tokenize(program))

def read_from_tokens(tokens):
    """Read an expression from a sequence of tokens."""
    if not tokens:
        raise LispError("unexpected EOF while reading")
    token = tokens.pop(0)
    if token == '(':
        L = []
        while tokens and tokens[0] != ')':
            L.append(read_from_tokens(tokens))
        if not tokens or tokens.pop(0) != ')':
            raise LispError("unexpected EOF or missing ')'")
        return L
    elif token == ')':
        raise LispError("unexpected ')'")
    else:
        return atom(token)

def atom(token):
    """Numbers become numbers; every other token is a Symbol."""
    try:
        return Number(token)
    except ValueError:
        return Symbol(token)

# --- Environment ---
class Env(dict):
    """An environment: a dict of {'var': val} pairs, with an outer Env."""
    def __init__(self, parms=(), args=(), outer=None):
        self.update(zip(parms, args))
        self.outer = outer

    def find(self, var):
        """Find the innermost Env where var appears."""
        if var in self:
            return self
        elif self.outer:
            return self.outer.find(var)
        else:
            raise LispError(f"unbound symbol: {var}")

def standard_env():
    """An environment with some standard procedures."""
    import math, operator
    env = Env()
    env.update({
        '+': operator.add, '-': operator.sub, '*': operator.mul, '/': operator.truediv,
        '>': operator.gt, '<': operator.lt, '>=': operator.ge, '<=': operator.le, '=': operator.eq,
        'abs': abs,
        'append': lambda a, b: list(a) + list(b),
        'apply': lambda proc, args: proc(*args),
        'begin': lambda *x: x[-1],
        'car': lambda x: x[0],
        'cdr': lambda x: x[1:],
        'cons': lambda x, y: [x] + list(y),
        'eq?': operator.is_,
        'expt': pow,
        'equal?': operator.eq,
        'length': len,
        'list': lambda *x: list(x),
        'list?': lambda x: isinstance(x, list),
        'map': map,
        'max': max,
        'min': min,
        'not': operator.not_,
        'null?': lambda x: x == [],
        'number?': lambda x: isinstance(x, Number),
        'procedure?': callable,
        'round': round,
        'symbol?': lambda x: isinstance(x, Symbol),
        'display': print,
    })
    env.update(vars(math)) # sin, cos, sqrt, pi, etc.
    return env

GLOBAL_ENV = standard_env()

# --- Evaluator ---
def eval_lisp(x, env=GLOBAL_ENV):
    """Evaluate an expression in an environment."""
    if isinstance(x, Symbol):    # variable reference
        return env.find(x)[x]
    elif not isinstance(x, list): # constant literal
        return x
    elif x[0] == Symbol('quote'): # (quote exp)
        (_, exp) = x
        return exp
    elif x[0] == Symbol('if'):    # (if test conseq alt)
        (_, test, conseq, alt) = x
        exp = (conseq if eval_lisp(test, env) else alt)
        return eval_lisp(exp, env)
    elif x[0] == Symbol('define'): # (define var exp)
        (_, var, exp) = x
        env[var] = eval_lisp(exp, env)
    elif x[0] == Symbol('set!'):   # (set! var exp)
        (_, var, exp) = x
        env.find(var)[var] = eval_lisp(exp, env)
    elif x[0] == Symbol('lambda'): # (lambda (var...) exp)
        (_, parms, exp) = x
        return Lambda(parms, exp, env)
    elif x[0] == Symbol('let'):    # (let ((var val)...) exp)
        # syntactic sugar for (lambda (var...) exp) (val...)
        _, bindings, body = x
        params = [b[0] for b in bindings]
        args = [eval_lisp(b[1], env) for b in bindings]
        return eval_lisp([Symbol('lambda'), params, body], Env(params, args, env))
    else:                          # (proc exp...)
        proc = eval_lisp(x[0], env)
        args = [eval_lisp(arg, env) for arg in x[1:]]
        if isinstance(proc, Lambda):
            return eval_lisp(proc.exp, Env(proc.parms, args, proc.env))
        else:
            return proc(*args)

class Lambda:
    """A user-defined Lisp procedure."""
    def __init__(self, parms, exp, env):
        self.parms = parms
        self.exp = exp
        self.env = env

    def __call__(self, *args):
        # This __call__ method is not strictly necessary as eval_lisp handles Lambda directly.
        # It could be used for a more Pythonic direct call if Lambda objects were passed around.
        raise NotImplementedError("Lambda objects are evaluated by eval_lisp directly.")
        # For a Python-callable lambda:
        # return eval_lisp(self.exp, Env(self.parms, args, self.env))

# --- Read-Eval-Print Loop (REPL) ---
def repl(prompt='lisp> '):
    """A minimal Lisp Read-Eval-Print Loop."""
    while True:
        try:
            val = eval_lisp(parse(input(prompt)))
            if val is not None:
                print(lispstr(val))
        except LispError as e:
            print(f"LispError: {e}")
        except EOFError:
            break
        except Exception as e:
            print(f"Error: {e}")

def lispstr(exp):
    """Convert a Python object back into a Lisp-readable string."""
    if isinstance(exp, list):
        return '(' + ' '.join(map(lispstr, exp)) + ')'
    else:
        return str(exp)

if __name__ == '__main__':
    print("Welcome to Simple Lisp Interpreter!")
    print("Enter 'exit' or Ctrl+D to quit.")

    test_program = """
    (begin
      (define r 10)
      (define pi 3.14159)
      (define circle-area (lambda (r) (* pi (* r r))))
      (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))

      (display "Circle area with r=10: ")
      (display (circle-area r))

      (display "Factorial of 5: ")
      (display (fact 5))

      (define x 10)
      (set! x (+ x 5))
      (display "x after set!: ")
      (display x)

      (display "Conditional test (if): ")
      (if (> x 10) (display "x is greater than 10") (display "x is not greater than 10"))

      (let ((a 1) (b 2)) (+ a b))
    )
    """
    print("\n--- Running a test program ---")
    try:
        eval_lisp(parse(test_program))
    except LispError as e:
        print(f"LispError in test program: {e}")
    except Exception as e:
        print(f"Error in test program: {e}")

    print("\n--- Starting REPL ---")
    repl()