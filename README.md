# fp_search — мини-поисковик по файлам (OCaml)

Это **полностью готовый проект** под заливку в репозиторий: библиотека + CLI + тесты + CI.

Идея: текстовый “поисковый движок”:
- индексирует директорию с файлами
- строит **инвертированный индекс** (term → doc → позиции)
- выполняет поиск по запросам с **AND/OR/NOT**, скобками и **фразами** `"like this"`
- ранжирует результаты (TF‑IDF + бонус за фразы)
- даёт `repl` режим

## Быстрый старт

Требования: OCaml + opam + dune.

```bash
opam switch create . 5.1.0 --deps-only --yes
eval $(opam env)
opam install . --deps-only -y
dune build
dune test
```

## Использование

### 1) Индексация директории

```bash
dune exec -- fp-search build --root ./data --out ./index.bin
```

Полезные флаги:
- `--ext txt,md,log` — какие расширения индексировать (по умолчанию: txt,md,log,csv)
- `--max-bytes 5000000` — ограничение размера файла
- `--follow-symlinks` — идти по симлинкам (по умолчанию нет)

### 2) Поиск

```bash
dune exec -- fp-search search --index ./index.bin --query 'postgres AND "query cache" NOT deprecated' --top 10 --snippets
```

Поддерживается:
- `AND`, `OR`, `NOT` (регистр не важен)
- скобки: `(a OR b) AND c`
- фразы: `"hello world"`
- неявный AND: `foo bar` == `foo AND bar`

### 3) REPL

```bash
dune exec -- fp-search repl --index ./index.bin
```

### 4) Статистика

```bash
dune exec -- fp-search stats --index ./index.bin
```

## Архитектура

- `Tokenizer` — разбор текста в токены + позиции
- `Index` — инвертированный индекс + обход файлов
- `Query` — токенизация запроса и парсер (приоритеты NOT > AND > OR)
- `Engine` — выполнение запроса, фразовый матчинг по позициям, ранжирование
- `Store` — сохранение/загрузка индекса
- `bin/fp_search_cli.ml` — CLI: build/search/stats/repl
- `test/` — Alcotest

## CI

GitHub Actions: `dune build` + `dune test` на Ubuntu.

## Лицензия

MIT.
