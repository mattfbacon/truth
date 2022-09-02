# Truth

A truth table generator. Useful for Math 19 students.

## Usage

Pass in the expression, in a Rust-like form, as a CLI argument to the program. The program will output a truth table in LaTeX format.

Example usage:

```shell
cargo run "(a & b) | c"
```

Output:

```latex
\begin{tabular}{c c c c c }
$a$ & $b$ & $c$ & $a \wedge b$ & $\left( a \wedge b \right) \vee c$\\
T & T & T & T & T\\
T & T & F & T & T\\
T & F & T & F & T\\
T & F & F & F & F\\
F & T & T & F & T\\
F & T & F & F & F\\
F & F & T & F & T\\
F & F & F & F & F\\
\end{tabular}
```

## License

AGPL-3.0-or-later
