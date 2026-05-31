Base is a small imperative language with static typing and CBV/R/N/Need variants.
  • cbv-Base    (call-by-value)
  • cbr-Base    (call-by-reference; procedure arguments are restricted to variables)
  • cbna-Base   (call-by-name)
  • cbne-Base   (call-by-need)
If the restriction on cbr-Base is respected, the same program file will generally execute successfully under all four variants.

Base is very much limited in what it can achieve in computation. For one, it lacks loops, recursion, or user-defined datatypes.
It is rather meant to serve as a minimal, distraction-less environment in which one can play around with different parameter passing strategies.

----

Syntax:

x, y, f    ∈   Var                       (An member of {a, …, z} followed by zero or more members of {a, …, z, A, …, Z, _, '})
c          ∈   Con                       (0, 1, ..., True, False)
Π          ∈   Prim                      (Int, Bool)
⊕         ∈   Bin                       (_+_, _-_, _*_, _||_, _&&_)

Prog  P   ::=  {s; … ; s;}

Stmt  s   ::=  x : Π                     (declaration)
           |   x = e                     (assignment)
           |   proc f(x : Π, …, x : Π)Q  (procedure)
           |   P                         (lexical block)

Body  Q   ::=  {s; … ; s; return e;}     (procedure body)

Expr  e   ::=  x                         (variable)
           |   c                         (constant)
           |   e ⊕ e                    (binary operation)
           |   f(e, …, e)                (procedure invocation)

Type  A,B ::=  Π                         (primitive type)
           |   (A, …, A) -> B            (procedure type)

TEnv  Γ   ::=  •                         (empty type environment)
           |   Γ, x:A                    (extend with "x has type A")

¹ Base programs are parsed agnostic of indentation or layout, relying solely on the structure provided by the nonterminals.
² The return type B of a procedure type can be inferred from the body. Note that all procedures must return a value.

----

The value language (i.e., what programs evaluate to) is not preserved between each variant of Base.
For instance, cbr-Base uniquely has pointers while cbna- and cbne-Base uniquely have thunks.

cbv-Base:

Val   v   ::=  c                         (constant)
           |   ?                         (unknown)
           |   (𝜌,(x, …, x)Q)            (closure)

Env   𝜌   ::=  •                         (empty environment)
           |   𝜌, x=v                    (extend with "x is v")

cbr-Base:

Val   v   ::=  c                         (constant)
           |   ?                         (unknown)
           |   (𝜌,(x, …, x)Q)            (closure)

Env   𝜌   ::=  •                         (empty environment)
           |   𝜌, x=v                    (extend with "x is v")
           |   𝜌, x=>y                   (extend with "x points to y")

cbna-Base, cbne-Base:

Val   v   ::=  c                         (constant)
           |   ?                         (unknown)
           |   (𝜌,(x, …, x)Q)            (closure)

Env   𝜌   ::=  •                         (empty environment)
           |   𝜌, x=v                    (extend with "x is v")
           |   𝜌, x≡e                    (extend with "x is e")

----
