import re
import sys # Added for file loading

class LispError(Exception):
    pass

# --- Lexer ---
def tokenize(chars):
    """Convert a string of characters into a list of tokens."""
    # Ensure tokens like '(', ')', and '\'' are separated by spaces
    # Added backtick ` and comma , for future quasiquote support
    return chars.replace('(', ' ( ').replace(')', ' ) ').replace("'", " ' ").replace('`', ' ` ').replace(',', ' , ').split()

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
    elif token == "'": # Handle ' (quote ...) syntax
        return [Symbol('quote'), read_from_tokens(tokens)]
    elif token == '`': # Handle ` (quasiquote ...) syntax (placeholder for now)
        return [Symbol('quasiquote'), read_from_tokens(tokens)]
    elif token == ',': # Handle , (unquote ...) syntax (placeholder for now)
        return [Symbol('unquote'), read_from_tokens(tokens)]
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
        self.macros = {} # Store macros specific to this environment scope

    def find(self, var):
        """Find the innermost Env where var appears."""
        if var in self:
            return self
        elif self.outer:
            return self.outer.find(var)
        else:
            raise LispError(f"unbound symbol: {var}")

    def find_macro(self, var):
        """Find the innermost Env where a macro named var appears."""
        if var in self.macros:
            return self.macros
        elif self.outer:
            return self.outer.find_macro(var)
        else:
            return None # Macro not found

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
        'begin': lambda *x: x[-1] if x else None, # begin can be empty
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
        'exit': lambda: sys.exit(), # Use sys.exit() for cleaner exit
        'load': lambda filename: load_file(filename, env), # Load function
    })
    env.update(vars(math)) # sin, cos, sqrt, pi, etc.
    return env

GLOBAL_ENV = standard_env()

# --- Macro Expander ---
class Macro:
    """A user-defined Lisp macro."""
    def __init__(self, parms, exp, env):
        self.parms = parms
        self.exp = exp
        self.env = env # Macro definition environment (lexical scope for expansion)

    def __str__(self):
        return f"(macro {lispstr(self.parms)} {lispstr(self.exp)})"

    def __repr__(self):
        return self.__str__()

def macroexpand(exp, env):
    """
    Recursively expand macros in an expression.
    If exp is a macro call, expand it and then re-expand the result.
    Otherwise, return exp.
    """
    if isinstance(exp, list) and exp and isinstance(exp[0], Symbol):
        mac_env = env.find_macro(exp[0])
        if mac_env and exp[0] in mac_env.macros:
            macro_proc = mac_env.macros[exp[0]]
            if not isinstance(macro_proc, Macro):
                raise LispError(f"'{exp[0]}' is defined as a macro but is not a Macro object.")

            # Create a temporary environment for macro expansion
            # This environment binds macro arguments to their literal forms (not evaluated)
            macro_env = Env(macro_proc.parms, exp[1:], macro_proc.env)
            
            # Evaluate the macro body in its definition environment
            # Note: eval_lisp is used here, which means macro bodies can contain other Lisp forms
            # including calls to functions. The result of the macro is an S-expression.
            expanded_exp = eval_lisp(macro_proc.exp, macro_env)
            
            # Recursively expand the result of the macro, in case it expands to another macro call
            return macroexpand(expanded_exp, env)
    return exp # Not a macro call, or not expandable

# --- Evaluator ---
class Lambda:
    """A user-defined Lisp procedure."""
    def __init__(self, parms, exp, env):
        self.parms = parms
        self.exp = exp
        self.env = env

    def __str__(self):
        return f"(lambda {lispstr(self.parms)} {lispstr(self.exp)})"

    def __repr__(self):
        return self.__str__()

def eval_lisp(x, env=GLOBAL_ENV):
    """Evaluate an expression in an environment (with TCO and macro expansion)."""
    # Macro expansion happens before evaluation.
    # The expression x might be modified by macroexpand.
    x = macroexpand(x, env)

    while True: # Loop for Tail Call Optimization
        if isinstance(x, Symbol):    # variable reference
            return env.find(x)[x]
        elif not isinstance(x, list): # constant literal
            return x
        elif x[0] == Symbol('quote'): # (quote exp) or 'exp
            (_, exp) = x
            return exp
        elif x[0] == Symbol('if'):    # (if test conseq alt)
            (_, test, conseq, alt) = x
            x = (conseq if eval_lisp(test, env) else alt) # Evaluate test, then prepare for tail call
            # Note: The result of the conditional branch (conseq or alt) is then passed
            # to the next iteration of the while loop, which will re-run macroexpand on it.
            # This is important for macros expanding to conditionals.
        elif x[0] == Symbol('define'): # (define var exp)
            (_, var, exp) = x
            env[var] = eval_lisp(exp, env)
            return None # Define doesn't return a value in Scheme, often returns unspecified/None
        elif x[0] == Symbol('define-macro'): # (define-macro var (parms) exp)
            (_, var, parms, exp) = x
            env.macros[var] = Macro(parms, exp, env) # Macro env is its definition environment
            return None
        elif x[0] == Symbol('set!'):   # (set! var exp)
            (_, var, exp) = x
            env.find(var)[var] = eval_lisp(exp, env)
            return None
        elif x[0] == Symbol('lambda'): # (lambda (var...) exp)
            (_, parms, exp) = x
            return Lambda(parms, exp, env)
        elif x[0] == Symbol('let'):    # (let ((var val)...) exp)
            # syntactic sugar for (lambda (var...) exp) (val...)
            _, bindings, body = x
            params = [b[0] for b in bindings]
            args = [eval_lisp(b[1], env) for b in bindings]
            # Transform 'let' into a lambda application, which is a tail call
            x = [Symbol('lambda'), params, body]
            env = Env(params, args, env) # New environment for the lambda
            # Macro expansion will run on the new 'x' in the next iteration.
        elif x[0] == Symbol('cond'): # (cond (test exp)... (else exp))
            for clause in x[1:]:
                if clause[0] == Symbol('else'):
                    x = clause[1] # 'else' clause is a tail call
                    break
                test_result = eval_lisp(clause[0], env)
                if test_result:
                    x = clause[1] # Consequent expression is a tail call
                    break
            else: # No condition was true, and no else clause. Returns None.
                return None
            # Macro expansion will run on the new 'x' in the next iteration.
        else:                          # (proc exp...)
            proc = eval_lisp(x[0], env)
            args = [eval_lisp(arg, env) for arg in x[1:]]
            if isinstance(proc, Lambda):
                # Tail call: update x and env, then loop
                x = proc.exp
                env = Env(proc.parms, args, proc.env)
                # Macro expansion will run on the new 'x' in the next iteration.
            else:
                return proc(*args)

# --- File Loading ---
def load_file(filename, env):
    """Load and evaluate a Lisp file."""
    try:
        with open(filename, 'r') as f:
            program_text = f.read()
        
        # Tokenize and parse the entire file content
        tokens = tokenize(program_text)
        expressions = []
        while tokens:
            expressions.append(read_from_tokens(tokens))
            
        # Evaluate each top-level expression
        results = []
        for exp in expressions:
            results.append(eval_lisp(exp, env))
        return results[-1] if results else None # Return last result, or None if empty file
    except FileNotFoundError:
        raise LispError(f"File not found: {filename}")
    except Exception as e:
        raise LispError(f"Error loading file {filename}: {e}")

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
            print("\nExiting Lisp REPL.")
            break
        except SystemExit: # Catch exit() from Lisp 'exit'
            print("Exiting Lisp REPL.")
            break
        except Exception as e:
            print(f"Error: {e}")

def lispstr(exp):
    """Convert a Python object back into a Lisp-readable string."""
    if isinstance(exp, list):
        return '(' + ' '.join(map(lispstr, exp)) + ')'
    elif isinstance(exp, Number):
        if exp == int(exp):
            return str(int(exp))
        return str(exp)
    elif exp is None:
        return "unspecified"
    else:
        return str(exp)

if __name__ == '__main__':
    print("Welcome to Simple Lisp Interpreter with TCO, COND, Macros, and File Loading!")
    print("Enter 'exit' or Ctrl+D to quit.")

    # Create a test file for the `load` function
    with open("test_macros.scm", "w") as f:
        f.write("""
        (display "Loading test_macros.scm...")

        ; Define a simple 'unless' macro
        (define-macro unless (test then)
          `(if (not ,test) ,then)) ; Placeholder ` and , for future quasiquote

        (display "Testing 'unless' macro:")
        (unless (= 1 2) (display "1 is not equal to 2 (as expected)."))
        (unless (= 1 1) (display "1 is equal to 1 (should not display)."))

        ; Define a 'for' loop like macro
        ; (for i from 0 to 5 (display i))
        (define-macro for (var from start to end body)
            ; This is a very basic macro, actual for loops are more complex
            ; This manually expands a fixed range, not flexible iteration
            (if (> start end)
                (quote (begin)) ; Empty body if start > end
                (list 'begin
                      (list 'define var start)
                      (list 'display var) ; For simple demo
                      (list (Symbol 'for) var 'from (+ start 1) 'to end body)
                )
            )
        )

        ; This macro definition is flawed, as macro bodies are evaluated in their
        ; definition environment, and direct recursion within the macro body for expansion
        ; is not how typical 'for' macros are written in Scheme (they often use helper
        ; functions or direct expansion into explicit recursion/loop forms).
        ; For now, a simpler, non-recursive macro is better to demonstrate the concept.
        
        ; Let's make a simpler 'when' macro for demonstration
        (define-macro when (test . body)
          `(if ,test (begin ,@body))) ; Placeholder for quasiquote and unquote-splicing

        (display "Testing 'when' macro:")
        (when (> 5 3) (display "5 is greater than 3.") (display "This should also show."))
        (when (< 5 3) (display "5 is NOT greater than 3.")) ; Should not display
        
        (display "test_macros.scm loaded.")
        (define loaded-var "Hello from file!")
        """)

    test_program = """
    (begin
      (define r 10)
      (define pi 3.14159)
      (define circle-area (lambda (r) (* pi (* r r))))
      (display "Circle area with r=10: ")
      (display (circle-area r))

      (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
      (display "Factorial of 5: ")
      (display (fact 5))

      (define sum-up-to (lambda (n acc)
                          (if (= n 0)
                              acc
                              (sum-up-to (- n 1) (+ n acc)))))
      (display "Sum up to 10000 (TCO): ")
      (display (sum-up-to 10000 0))

      (define x 10)
      (set! x (+ x 5))
      (display "x after set!: ")
      (display x)

      (display "Conditional test (if): ")
      (if (> x 10) (display "x is greater than 10") (display "x is not greater than 10"))

      (display "Conditional test (cond): ")
      (define grade (lambda (score)
        (cond
          ((>= score 90) "A")
          ((>= score 80) "B")
          ((>= score 70) "C")
          ((>= score 60) "D")
          (else "F"))))
      (display (grade 95))
      (display (grade 72))
      (display (grade 55))

      (display "Local binding (let): ")
      (let ((a 1) (b 2)) (+ a b))

      'this-is-quoted-symbol ; Test ' sugar
      
      (display "Loading external file test_macros.scm...")
      (load "test_macros.scm")
      (display loaded-var)
      (display "Finished main test program.")
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