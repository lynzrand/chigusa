# Chigusa 中的文法&语法解析器设计

## 设计思路

### 整体流程流水线化

```
String                      // 输入的原始代码
-> Iter<Item=char>          // 字符迭代器
-> CharIndices              // 给每个字符标号的迭代器
-> Peekable<CharIndices>    // 允许查看下一个字符的迭代器
-> Iter<Item=Token>         // Lexer 分析生成的 token 迭代器
-> AstNode::Program         // Parser 生成的抽象语法树
                            // 程序在这里等待 AST 生成完毕
                            // 到这里应当保证所有错误都报告完毕了

-> Vec<Operation>           // Compiler 生成的即将转换成虚拟机汇编的中间产物
-> Vec<u8>                  // Compiler 生成的虚拟机指令集的汇编
                            // 到这里可以保存成二进制文件了
                            // 也可以直接丢进虚拟机运行
```

### 文法解析器

```
Stack<>
```

