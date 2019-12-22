# Chigusa

Chigusa 是一个用 Rust 写成的 C0 编译器。

## 编译与使用

编译

```sh
cargo build --release
```

使用

```
chigusa 0.1.0

USAGE:
    chigusa.exe [FLAGS] [OPTIONS] [file]

FLAGS:
    -h, --help       Prints help information
        --jit        Use JIT compilation and run immediately.
        --stdout     Write result to stdout. Overwrites `output-file`.
    -V, --version    Prints version information

OPTIONS:
        --emit <emit>              The type of code to emit. Allowed are: token, ast, ir, asm, obj, exe [default: exe]  
    -o, --out <output-file>        Output file. [default: a.out]
    -v, --verbosity <verbosity>    Verbossity. Allowed values are: debug, trace, info, warn, error, off. [default: warn]
ARGS:
    <file>    Input file. Defaults to stdin if no file were supplied.
```

## 完成的实验内容

本实验完成的内容包括：

- 基础实验 ~~（废话）~~
- 注释
- 字符与字符串 char
- 双精度浮点数 double
- 作用域
- 类型转换

## 语法的改写

### 等价改写

为了使得编译器实现更简便，实验中进行了如下的语法等价改写：

- 不区分二元运算符语句和一元运算符语句中的各个种类（如 A + B 和 A * B），内部使用运算符优先级确定解析顺序
- 对所有语句提取相同前缀，使得各语句的前缀不重合。

### 不等价改写

不等价改写的原因也是为了实现简便，主要途径是扩充。

完整语法见 c0_grammar.txt

> 我们的口号是：没有回溯！没有回填！

## 未定义行为
