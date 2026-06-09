Base is a small imperative language with static typing and CBV/R/N/Need variants.
  • cbv-Base    (call-by-value)
  • cbr-Base    (call-by-reference; procedure arguments are restricted to variables)
  • cbna-Base   (call-by-name)
  • cbne-Base   (call-by-need)
If the restriction on cbr-Base is respected, the same program file will generally execute without errors under all four variants.

Base is very much limited in what it can achieve in computation. For one, it lacks loops, recursion, or user-defined datatypes. It is rather meant to serve as a minimal, distraction-less environment in which one can play around with different parameter passing strategies.

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

The value language (i.e., what programs evaluate to) is the same for all variants of Base. Static scoping is implemented through closures, which takes a "snapshot" of the environment at the time of the definition of a procedure.

Val   v   ::=  c                         (constant)
           |   (𝜌,(x, …, x)Q)            (closure)

Environments map variables to locations, which generalize memory addresses. In the current implementation, Loc is an alias for Int.

l          ∈   Loc                       (memory locations)

Env   𝜌   ::=  •                         (empty environment)
           |   𝜌, x=>l                   (extend with "x points to l")

----

Memory stores, which generally map locations to values, are more variant-specific. In particular, cbn- and cbneed-Base also map locations to thunks, which are pairs of environments and expressions.

cbv-Base, cbr-Base:

Mem   μ   ::=  •                         (empty memory store)
           |   μ, l=v                    (extend with "l is v")

³ In the evaluation rule for procedure invocation, cbv-Base copies the argument values into fresh locations, then points all parameter names to those locations. cbr-Base instead points all parameter names to the same locations pointed to by the argument names; any change to a parameter becomes a change to the corresponding argument. This indirection is why there exists a stipulation that all arguments to procedures be variables in cbr-Base.

cbn-Base, cbneed-Base:

Mem   μ   ::=  •                         (empty memory store)
           |   μ, l=v                    (extend with "l is v")
           |   μ, l=(𝜌,e)                (extend with "l is (𝜌,e)")

⁴ The evaluation rule for procedure invocation is the same between cbn- and cbneed-Base. Each argument is first packaged into a thunk. Then, it is placed into a fresh location to be pointed to by the corresponding parameter name. The only difference between the two systems is whether the result of a forced thunk is memoized (i.e., updated in the memory store).

----
