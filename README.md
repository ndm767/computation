# Models of computation

## general_recursive

General recursive functions, implementing the base functions as well as composition, primitive recursion, minimization.  
Uses the `.grf` file extension  
Syntax:  

- `E(x)` is the erase function. It outputs the empty string.  
- `P_i^n(x_1, x_2, ..., x_n)` is the projection function. It selects the `i`th element (1-indexed) out of `n` inputs.
- `S_#c(x)` is the successor function where `c` is any symbol. The output of this will be `xc`.  
- `f(x, y, ...) := [body]` is a function definition. `f` may be any identifier as specified in the lexer.  
- Composition works as expected (`f(g(x))`).  
- Primitive recursion assumes that the recursive variable is the first argument. `f(n_y, x) := [base case when n = ""] | [otherwise]`. Note the `n_y` to denote that we are reducing `n`.
- Minimization works as follows: `f(x) = min_z[g(x, z) = "target"]`. Note the square brackets and quotation marks.  

## turing_instr

Instruction-based Turing machine  
uses `.trn` file extension  
Output is `(cons tape head-position)`  
Note that input follows the convention of adding one. I.e., 0 = "1", 1 = "1 1", 2 = "1 1 1", etc.  

## turing_table

Table-based Turing machine  
uses `.ttn` file extension  
Input format is `(state, read symbol, write symbol, move, next_state)`  
In the examples folder are implementations of the Busy Beaver machines for n = 2, 3, 4, 5.  

## markov_algo  

Markov algorithm.  
Uses `.mkv` file extension.  
Markov algorithms contain three statements that must be in this order:  

- `var "x"` where `x` is any single character declares a variable that can be any symbol other than a "special" character  
- `special "y"` where `y` is any single character declares that any occurance of `y` cannot be treated a normal symbol in the alphabet and be replaced by a variable  
- `"input" -> "output"` is a production as usual. Note that if the production is terminal, the arrow must be written `->t`.  

See `examples/markov_algo/reverse_str.mkv` for an example that utilizes all features.  

## register_machine  

Register machines.  
Uses `.rgm` file extension.  
Note that the registers are 1-indexed.  
