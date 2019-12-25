# Chigusa

Chigusa 是一个用 Rust 写成的 C0 编译器。使用了手写的递归下降分析。

## 编译与使用

### 编译

```sh
cargo build --release
```

编译产物路径是 `target/release/chigusa`。

### 使用

```
chigusa 0.1.0

A compiler that parses c0 grammar and compiles it into o0 binary format.

C0: https://github.com/BUAA-SE-Compiling/c0-handbook
O0: https://github.com/BUAA-SE-Compiling/c0-vm-standards

USAGE:
    chigusa [FLAGS] [OPTIONS] [file]

FLAGS:
    -h, --help       Prints help information
    -s, --s0         Emit C0 assembly file, same as `--emit s0`
    -c, --o0         Emit C0 binary file, same as `--emit o0`
        --stdout     Write result to stdout. Overwrites `output-file`.
    -V, --version    Prints version information

OPTIONS:
        --emit <emit>              The type of code to emit. Allowed are: token, ast, s0, o0 [default: o0]
    -o, --out <output-file>        Output file. [default: out]
    -v, --verbosity <verbosity>    Verbossity. Allowed values are: debug, trace, info, warn, error, off. [default: warn]

ARGS:
    <file>    Input file. Defaults to stdin if no file were supplied.
```

> 与预期的不同的东西：使用的 `clap` 不支持在没有参数的情况下默认输出帮助。

## 完成的实验内容

本实验完成的内容包括：

- 基础实验 ~~（废话）~~
- 注释
- 字符与字符串 char
- 双精度浮点数 double
- 作用域
- 类型转换
- `break` 语句

## 语法的改写

### 等价改写

为了使得编译器实现更简便，实验中进行了如下的语法等价改写：

- 不区分二元运算符语句和一元运算符语句中的各个种类（如将 A + B 和 A * B 都解析为 `BinaryOp`），内部使用运算符优先级确定解析顺序
- 对所有语句提取相同前缀，使得各语句的前缀不重合；这个不细讲了
- （编译器内）将可被赋值的非终结符类型由 `Identifier` 提升至 `LValue`，以提高可扩展性（虽然大概率不会扩展）

### 不等价改写

见 [grammar-changes.md](./grammar-changes.md).

## 未定义行为

对于实验指导书中提到的的未定义行为，本次实验中处理如下：

- `UB 3.2.2.4`: 数字字面量在溢出时不会报错，且会被从允许的最高位截断。字面量本身的承载类型是支持无限精度的。
- `UB 3.2.2.5`: 关系表达式以所有非 0 值为真，以 0 为假
- `UB 3.2.2.7`: 任何未初始化的 const 变量会引起编译时错误 (`ParseErrVariant::ConstTypeNeedExplicitInitialization`)
- `UB 3.2.2.8`: 非 void 函数中的所有控制流都必须有返回语句，没有返回语句的控制流是编译时错误（`CompileErrorVar::ControlReachesEndOfNonVoidFunction`)
- ~~`UB 3.2.2.11`: 不归我管）~~
- `UB 3.2.3.2`: `char` 字面量与 `int` 相同
- ~~`UB 3.2.3.4`: 没做~~
