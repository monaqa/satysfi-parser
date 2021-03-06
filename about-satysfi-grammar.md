# SATySFi の文法について

ここでは [`v0.0.6`](https://github.com/gfngfn/SATySFi/releases/tag/v0.0.6) 時点での SATySFi の文法について述べる。

## SATySFi のファイルの種類

SATySFi の構文が記されるファイルは以下の2種類である。

* 文書ファイル (document file)
* パッケージファイル (package file)

文書ファイルはその名の通り文書本体を記述するために用いるファイルであるのに対し、
パッケージファイルは文書ファイルの記述のために必要な変数やコマンドなどを定義するために存在する補助的なファイルである。
目的の差から、この2種類のファイルでは全体的な構文に違いがある。

文書ファイルは以下のような構造からなっている。

```
program_saty = { SOI ~ header_stage? ~ headers ~ (preamble ~ "in")? ~ expr ~ EOI }
```

それに対して

```
program_satyh = { SOI ~ header_stage? ~ headers ~ preamble ~ EOI }
```

