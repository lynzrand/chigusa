# 对 C0 的语法不等价改写说明

不等价改写的原因是为了实现简便，主要途径是扩充。主要改写内容包括：

- 允许在作用域内的任何地方声明变量
- 函数声明也按照变量声明解析
- 允许关系运算符出现在任何表达式内，以非 0 值表示真
- 允许字符串中间出现大于 1 字节的字符，以 UTF-8 格式存储
- 允许字符串字面量使用 `\u{X...X}` 和 `\uXXXX` 表示 Unicode 字符，以 UTF-8 格式存储
- 解析时允许 `&`, `&&`, `|`, `||`, `>>`, `<<` 作为二元运算符使用，允许 `~`, `!`, `&`, `*`, `++`, `--` 作为一元运算符使用，允许出现 `ident[x]` 数组语法，直到编译时才会因不支持报错。

<!-- - 允许函数以任何顺序被声明和引用 -->

完整语法见 c0_grammar.txt（可能不全），有时间会去更新一下。下面对于每个语法内容详细说明：

## 任意地方声明变量

简单来说就是跟现在大多数编程语言一样。具体操作就是把变量声明变成了一个 Statement。

```
SingleVarDecl: Identifier ("=" Expr)?
VarDecl: "const"? TypeDeclaration SingleVarDecl ("," SingleVarDecl)?
VarDeclStmt: VarDecl ";"

Stmt: VarDeclStmt | ...
```

这个操作引入了一个 UB：既然变量声明是一个 Statement，他就可以被放进 `if` 和 `while` 的分支里。在这个编译器实现中，这个行为的结果是不确定的。具体来说，这个变量被声明进了 `if` 所在的作用域，所以它可以被同一层级的代码访问。但是，由于它的初始化代码可能没有运行，所以它的值是不确定的。

## 函数声明也按照变量声明解析

没有体现在语法中。编译器在解析的时候会把函数声明解析成以函数名为名、以函数类型为类型的变量装进作用域的符号表里。但是因为 ~~禁止套娃~~ 禁止函数嵌套，所以到了编译期会照常报错。

> 所以顶级作用域里面就是一堆变量声明。

## 允许关系运算符作为二元运算符出现

这一条和最后一条一起说。简单来说就是扩充了运算符的种类。

```
// Unary operators are applied according to their order of appearance.
PrecedingUnaryOperator: "+" | "-" | "*" | "&" | "!" | "~" | "++" | "--"
ProcedingUnaryOperator: "++" | "--"

// Binary operators are applied according to precedence rules.
// Operators with the same precedence value are left associative, except for assignment ops.
// i.e. `x = z = a + b - c` -> `(x = (z = ((a + b) - c)))`
BinaryOperator: "+" | "-" | "*" | "/" | "%" | "^" | "="
    | "&" | "|" | "&&" | "||" | ">>" | "<<" | "<" | "<=" | "==" | "!=" | ">=" | ">"

ProceedingUnaryOperation: Expr ProceedingUnaryOperator

PrecedingUnaryOperation: PrecedingUnaryOperator ProceedingUnaryOperation

BinaryOperation: PrecedingUnaryOperation (BinaryOperator PrecedingUnaryOperation)*
```

### 关于符号优先算法

这里用的不是传统的 OPG 分析法，而是一个以算符优先级为参数的递归下降法。原始算法抄自 [`librustc_parse` 的递归下降算法][librustc]。算法的大意是对于每一个新出现的优先级往里递归一次，如果新符号的优先级跟当前一样就在同一层迭代，如果更小就回到上一层。实现见 `src/c0/parser.rs:Parser::p_base_expr`。

[librustc]: https://github.com/rust-lang/rust/blob/b5f265eeed23ac87ec6b4a7e6bc7cb4ea3e67c31/src/librustc_parse/parser/expr.rs#L778

### 关于关系运算符

由于 c0 虚拟机并没有关系运算指令，实现中采用了多指令组合的方式模拟关系运算符。其中得出 0 是假，其他结果都是真。

模拟方式：

```
Eq => TCmp, Dup, IMul, IPush(1), ICmp
Neq => TCmp
Gt => TCmp, IPush(1), ISub, IPush(0), ICmp, IPush(1), ICmp
Lt => TCmp, IPush(1), IAdd, IPush(0), ICmp, IPush(1), ICmp
Gte => TCmp, IPush(1), IAdd
Lte => TCmp, IPush(1), ISub
```

## 允许字符串使用非 ASCII 字符

这里把剩下的两条一起说了。第一条是简化字符串解析，大概就是只要你不在字符串里换行什么话都好说。因为 Rust 的字符串是 UTF-8 比特数组，所以信息都是 UTF-8 存储的。第二条是因为支持 UTF-8 了所以就放飞自我写了个四位 unicode 解析，无视掉就好。

```
EscapedChar: "\" ([ntr\\'"] | "x" [0-9a-fA-F]{2} | "u" ( [0-9a-fA-F]{4} | '{' [0-9a-fA-F]+ '}' ) )
Char: [^\\'"\n\r] | EscapedChar

CharLiteral: "'" Char "'"
StringLiteral: "\"" Char* "\""
```

> 谁管你字符串里存的什么呢，哼

---

## 一些碎碎念

> 我们的口号是：没有回溯！没有回填！ <- 这个人最后还是回填了

> Cranelift 杀我.jpg

> 要不是 Windows （似乎）跑不起来 inkwell 咱早用 LLVM 来干了

> 前！置！类！型！转！换！

> 隐！式！类！型！转！换！

> ^ 上面两个是本次实验中比较坑人的东西

> 基本块大法好。
