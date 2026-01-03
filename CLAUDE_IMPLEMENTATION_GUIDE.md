# OmicsLake v2.0 実装ガイド

## 概要

OmicsLakeは「Data Lineage中核思想を保持しつつ、既存オミクス解析ワークフローに最小限の変更で導入できる透過的アドオンパッケージ」として設計されている。

### 設計原則

1. **Lineage First**: すべての操作でデータ系譜を自動追跡
2. **Zero Friction**: 既存パイプラインへの導入障壁を最小化
3. **R Native**: SQL不要、R構文でクエリ記述
4. **Progressive Adoption**: 段階的に機能を導入可能
5. **Extensible**: アダプター/プラグインによる拡張性

---

## 実装済みファイル構造

```
OmicsLake/
├── R/
│   ├── Lake.R              # メインR6クラス + DependencyTracker
│   ├── QueryBuilder.R      # フルエントクエリビルダー
│   ├── shortcuts.R         # グローバルショートカット関数
│   ├── operators.R         # カスタム演算子 (%like%, %between%等)
│   ├── dplyr_compat.R      # dplyr統合 + lake_tbl S3メソッド
│   ├── observe.R           # 観察モード (軽量リネージ追跡)
│   ├── wrap.R              # 関数ラッピング + Pipeline
│   ├── compat.R            # 後方互換レイヤー (ol_* → v2.0)
│   ├── adapters/
│   │   ├── base.R          # LakeAdapter基底クラス
│   │   └── se_adapter.R    # SummarizedExperimentアダプター
│   ├── backend.R           # DuckDBバックエンド
│   ├── io.R                # Legacy I/O関数 (ol_read, ol_write等)
│   ├── init.R              # プロジェクト初期化 (ol_init)
│   ├── versions.R          # バージョン管理 (tags, snapshots)
│   ├── query.R             # SQL実行
│   ├── views.R             # ビュー管理
│   ├── parquet.R           # Parquetインポート/エクスポート
│   ├── bioc.R              # Bioconductorサポート
│   ├── visualization.R     # リネージ可視化
│   ├── aggregation.R       # 集計関数
│   ├── utils.R             # 内部ユーティリティ
│   └── zzz.R               # パッケージロードフック
├── tests/testthat/         # テストファイル
├── NAMESPACE               # エクスポート宣言
└── DESCRIPTION             # パッケージメタデータ
```

---

## Phase 1: コアAPI (実装済み)

### 1.1 Lake R6クラス

**ファイル**: `R/Lake.R`

```r
#' @export
Lake <- R6::R6Class("Lake",
  cloneable = FALSE,

  public = list(
    # 初期化
    initialize = function(project = NULL,
                          backend = "duckdb",
                          auto_track = TRUE,
                          root = NULL),

    # ========== Core I/O ==========
    put = function(name, data, depends_on = NULL, tags = NULL),
    get = function(name, ref = "@latest", where = NULL, select = NULL, collect = TRUE),
    ref = function(name),  # dplyr用遅延参照

    # ========== Versioning ==========
    snap = function(label, note = "", params = list()),
    tag = function(name, tag),
    restore = function(label),
    diff = function(name, ref1 = "@latest", ref2 = "@first"),

    # ========== Lineage ==========
    tree = function(name = NULL, direction = "up", depth = 10),
    plot = function(name = NULL, direction = "both"),
    deps = function(name, direction = "up"),
    impact = function(name),

    # ========== Query Builder ==========
    query = function(),       # QueryBuilder開始
    from = function(table),   # テーブルからクエリ開始
    join = function(left, right, by = NULL, type = "left"),

    # ========== Aggregation Shortcuts ==========
    count = function(table, ...),
    mean = function(table, col, ...),

    # ========== Listing ==========
    tables = function(),
    objects = function(),
    ls = function(),
    snaps = function(),
    log = function(name = NULL, n = 20),
    history = function(name = NULL, n = 20),

    # ========== Data Management ==========
    drop = function(name, force = FALSE),
    rm = function(name, force = FALSE),

    # ========== Import/Export ==========
    export = function(name, path, format = NULL),
    import = function(path, name, format = NULL),

    # ========== SQL (Escape Hatch) ==========
    sql = function(query, collect = TRUE),
    q = function(query, collect = TRUE),

    # ========== Bracket Notation ==========
    `[` = function(name, i, j),
    `[<-` = function(name, value),

    print = function()
  ),

  private = list(
    .project = NULL,
    .backend_type = NULL,
    .state = NULL,
    .auto_track = TRUE,
    .tracker = NULL,  # DependencyTrackerインスタンス
    # ... 内部メソッド
  )
)
```

### 1.2 DependencyTracker (Lake.R内)

```r
#' @keywords internal
DependencyTracker <- R6::R6Class("DependencyTracker",
  public = list(
    initialize = function(lake),
    track_read = function(name, ref = "@latest"),
    start_write = function(name),
    end_write = function(),
    current_reads = function(),
    clear = function()
  ),
  private = list(
    .lake = NULL,
    .read_stack = list()
  )
)
```

---

## Phase 2: グローバルショートカット (実装済み)

**ファイル**: `R/shortcuts.R`

```r
# グローバルデフォルトLake環境
.lake_env <- new.env(parent = emptyenv())

#' @export
use_lake <- function(project = NULL, ...)

#' @export
lake <- function()

#' @export
put <- function(name, data, ...)

#' Note: fetch()を使用 (base::get()との衝突回避)
#' @export
fetch <- function(name, ...)

#' @export
ref <- function(name)

#' @export
snap <- function(label, ...)

#' @export
tag <- function(name, tag)

#' @export
tree <- function(name = NULL, ...)

#' @export
history <- function(name = NULL, ...)

#' @export
tables <- function()

#' @export
objects <- function()

#' @export
drop <- function(name, ...)

#' @export
sql <- function(query, ...)

#' @export
restore <- function(label)

#' @export
deps <- function(name, direction = "up")

#' @export
import_data <- function(path, name, ...)

#' @export
export_data <- function(name, path, ...)

#' @export
query <- function()

#' @export
from <- function(table)
```

**注意**: 当初は `get()` を使用予定だったが、`base::get()` との衝突を避けるため `fetch()` に変更された。

---

## Phase 3: QueryBuilder (実装済み)

**ファイル**: `R/QueryBuilder.R`

```r
#' @export
QueryBuilder <- R6::R6Class("QueryBuilder",
  cloneable = TRUE,

  public = list(
    initialize = function(lake),

    # テーブル指定
    from = function(table, alias = NULL),

    # JOIN
    join = function(table, on = NULL, type = "left", alias = NULL),
    left_join = function(table, on = NULL, alias = NULL),
    inner_join = function(table, on = NULL, alias = NULL),
    right_join = function(table, on = NULL, alias = NULL),
    full_join = function(table, on = NULL, alias = NULL),

    # フィルタリング
    where = function(...),
    filter = function(...),

    # 選択
    select = function(...),
    pick = function(...),
    mutate = function(...),

    # グループ化・集計
    group_by = function(...),
    summarize = function(...),
    summarise = function(...),
    having = function(...),

    # ソート
    order_by = function(...),
    arrange = function(...),

    # 制限
    limit = function(n),
    take = function(n),
    top = function(n, by, desc = TRUE),
    offset = function(n),

    # ユニーク
    distinct = function(...),

    # 実行
    run = function(),
    collect = function(),
    as = function(name),
    save_as = function(name),

    # デバッグ
    show_sql = function(),
    explain = function(),
    print = function()
  )
)
```

---

## Phase 4: カスタム演算子 (実装済み)

**ファイル**: `R/operators.R`

```r
# パターンマッチング
#' @export
`%like%` <- function(x, pattern)    # SQL LIKE (% = .*, _ = .)

#' @export
`%ilike%` <- function(x, pattern)   # 大文字小文字無視

# 範囲フィルタ
#' @export
`%between%` <- function(x, range)   # x >= range[1] & x <= range[2]

#' @export
`%!between%` <- function(x, range)  # NOT BETWEEN

# 集合演算
#' @export
`%!in%` <- function(x, table)       # NOT IN

# 正規表現
#' @export
`%regex%` <- function(x, pattern)   # 正規表現マッチ

#' @export
`%iregex%` <- function(x, pattern)  # 大文字小文字無視の正規表現

# NULL/NAチェック
#' @export
is_null <- function(x)

#' @export
is_not_null <- function(x)

# 文字列関数
#' @export
starts_with_str <- function(x, prefix)

#' @export
ends_with_str <- function(x, suffix)

#' @export
contains_str <- function(x, substring)

# ユーティリティ
#' @export
coalesce <- function(...)           # 最初の非NA値を返す (dplyr::coalesceラッパー)

#' @export
if_else_na <- function(condition, true, false, na = NA)
```

---

## Phase 5: dplyr統合 (実装済み)

**ファイル**: `R/dplyr_compat.R`

### パイプ終端関数

```r
#' パイプ結果をLakeに保存
#' @export
save_as <- function(.data, name, lake = NULL)

#' パイプ対応の書き込み関数を生成
#' @export
into <- function(lake)

#' パイプ演算子 (data %>>% "name")
#' @export
`%>>%` <- function(.data, target)
```

### lake_tbl S3メソッド

`lake$ref()` が返す `lake_tbl` クラスに対して、すべての主要dplyr動詞でリネージ属性を保持：

```r
# 実装済みS3メソッド
S3method(filter, lake_tbl)
S3method(select, lake_tbl)
S3method(mutate, lake_tbl)
S3method(summarise, lake_tbl)
S3method(summarize, lake_tbl)
S3method(group_by, lake_tbl)
S3method(ungroup, lake_tbl)
S3method(arrange, lake_tbl)
S3method(distinct, lake_tbl)
S3method(slice, lake_tbl)
S3method(rename, lake_tbl)
S3method(relocate, lake_tbl)
S3method(left_join, lake_tbl)
S3method(inner_join, lake_tbl)
S3method(right_join, lake_tbl)
S3method(full_join, lake_tbl)
S3method(semi_join, lake_tbl)
S3method(anti_join, lake_tbl)
S3method(collect, lake_tbl)
S3method(print, lake_tbl)
```

---

## Phase 6: 軽量モード (実装済み)

**ファイル**: `R/observe.R`

```r
#' ファイルI/O操作を観察
#' @export
observe <- function(expr, track_functions = NULL)
# 戻り値: list(result, reads, writes, lineage)

#' 観察結果をLakeに記録
#' @export
observe_to_lake <- function(expr, lake, prefix = "file:")

#' Lake操作を追跡するコードブロック
#' @export
with_tracking <- function(lake, name, expr)

#' 観察セッション開始
#' @export
observe_session <- function(lake)

#' @export
ObserveSession <- R6::R6Class("ObserveSession", ...)
```

**注意**: 当初は `assignInNamespace` によるファイルI/Oフックを計画していたが、実装では外部影響を避けるためより軽量なアプローチを採用。

---

## Phase 7: 関数ラッピング (実装済み)

**ファイル**: `R/wrap.R`

```r
#' 関数をリネージ追跡付きでラップ
#' @export
wrap_fn <- function(fn, lake, output_name, input_names = NULL, save_output = TRUE)

#' インラインで関数呼び出しをラップ
#' @export
wrap_call <- function(lake, fn, ..., output = NULL, save = TRUE)

#' データを保存せずリネージにマーク
#' @export
mark <- function(name, data = NULL, lake = NULL)

#' 明示的な依存関係リンクを作成
#' @export
link <- function(from, to, lake = NULL, relationship = "linked")

#' 依存関係リンクを削除
#' @export
unlink_dep <- function(from, to, lake = NULL)

#' パイプライン作成
#' @export
create_pipeline <- function(lake, name)

#' @export
Pipeline <- R6::R6Class("Pipeline",
  public = list(
    initialize = function(lake, name),
    step = function(name, fn),
    run = function(input = NULL),
    print = function()
  )
)

#' 関数呼び出しをトレース
#' @export
trace_calls <- function(functions, expr, lake = NULL)
```

---

## Phase 8: アダプター (実装済み)

**ファイル**: `R/adapters/base.R`

```r
#' @export
LakeAdapter <- R6::R6Class("LakeAdapter",
  public = list(
    name = function(),
    can_handle = function(data),
    priority = function(),
    put = function(lake, name, data),
    get = function(lake, name, ref = "@latest"),
    components = function(lake, name),
    exists = function(lake, name)
  )
)

#' アダプター登録
#' @export
register_adapter <- function(adapter)

#' 登録済みアダプター取得
#' @export
get_adapters <- function()

#' データに適したアダプター検索
#' @export
find_adapter <- function(data)
```

**ファイル**: `R/adapters/se_adapter.R`

```r
#' @export
SEAdapter <- R6::R6Class("SEAdapter",
  inherit = LakeAdapter,
  public = list(
    name = function() "SummarizedExperiment",
    can_handle = function(data),  # SummarizedExperiment判定
    priority = function() 100,
    put = function(lake, name, data),  # assays, colData, rowData, metadata分解保存
    get = function(lake, name, ref),   # SE再構築
    components = function(lake, name),
    exists = function(lake, name)
  ),
  private = list(
    .matrix_to_long = function(mat),  # 疎行列対応
    .long_to_matrix = function(long_df)
  )
)
```

### 未実装アダプター

- **SeuratAdapter**: 計画のみ、未実装

---

## Phase 9: 後方互換レイヤー (実装済み)

**ファイル**: `R/compat.R`

```r
#' マイグレーションガイド表示
#' @export
show_migration_guide <- function()
```

**注意**: `ol_*` 関数への非推奨警告は将来のバージョンで追加予定。現在は両API（`ol_*` と `Lake`）が並行して動作。

### APIマッピング

| Legacy API | New API |
|------------|---------|
| `ol_init("project")` | `Lake$new("project")` / `use_lake("project")` |
| `ol_write("name", data)` | `lake$put("name", data)` / `put("name", data)` |
| `ol_read("name")` | `lake$get("name")` / `fetch("name")` |
| `ol_save("name", obj)` | `lake$put("name", obj)` |
| `ol_label("v1.0")` | `lake$snap("v1.0")` / `snap("v1.0")` |
| `ol_tag("name", "tag")` | `lake$tag("name", "tag")` / `tag("name", "tag")` |
| `ol_checkout("v1.0")` | `lake$restore("v1.0")` / `restore("v1.0")` |
| `ol_show_lineage("name")` | `lake$tree("name")` / `tree("name")` |
| `ol_get_dependencies("name")` | `lake$deps("name")` / `deps("name")` |
| `ol_query("SELECT ...")` | `lake$sql("SELECT ...")` / `sql("SELECT ...")` |
| `ol_list_tables()` | `lake$tables()` / `tables()` |
| `ol_list_objects()` | `lake$objects()` / `objects()` |

---

## NAMESPACEエクスポート一覧

### v2.0 コアクラス
- `Lake`, `QueryBuilder`, `DependencyTracker`

### v2.0 ショートカット
- `use_lake`, `lake`, `put`, `fetch`, `ref`
- `snap`, `tag`, `tree`, `history`
- `tables`, `objects`, `drop`, `sql`, `restore`
- `deps`, `import_data`, `export_data`, `query`, `from`

### v2.0 演算子
- `%like%`, `%ilike%`, `%between%`, `%!between%`
- `%!in%`, `%regex%`, `%iregex%`
- `is_null`, `is_not_null`
- `starts_with_str`, `ends_with_str`, `contains_str`
- `coalesce`, `if_else_na`

### v2.0 dplyr互換
- `save_as`, `into`, `%>>%`

### v2.0 軽量モード
- `observe`, `observe_to_lake`, `with_tracking`, `observe_session`
- `wrap_fn`, `wrap_call`, `mark`, `link`, `unlink_dep`
- `create_pipeline`, `Pipeline`, `trace_calls`
- `ObserveSession`

### v2.0 アダプター
- `LakeAdapter`, `SEAdapter`
- `register_adapter`, `get_adapters`, `find_adapter`

### Legacy API
- すべての `ol_*` 関数（後方互換のため維持）

---

## 使用例

### 基本的な使用法

```r
library(OmicsLake)

# Lake初期化
lake <- Lake$new("my_analysis")

# データ保存
lake$put("counts", counts_df)
lake$put("metadata", sample_info)

# データ取得
data <- lake$get("counts")

# Formula構文でフィルタ
filtered <- lake$get("counts", where = ~ expression > 100)

# カラム選択
subset <- lake$get("counts", select = c("gene_id", "sample_1", "sample_2"))
```

### dplyrパイプライン

```r
# 依存関係は自動追跡される
lake$ref("counts") |>
  dplyr::left_join(lake$ref("metadata"), by = "sample_id") |>
  dplyr::filter(quality > 0.8) |>
  dplyr::group_by(condition) |>
  dplyr::summarize(mean_expr = mean(expression)) |>
  save_as("summary", lake)

# リネージ確認
lake$tree("summary")
```

### QueryBuilder

```r
result <- lake$from("counts")$
  join("annotations", on = "gene_id")$
  where(biotype == "protein_coding")$
  where(expression > 100)$
  select(gene_id, gene_name, expression)$
  top(100, by = expression)$
  run()
```

### カスタム演算子

```r
# パターンマッチング
mito_genes <- lake$get("counts", where = ~ gene_id %like% "MT-%")

# 範囲フィルタ
mid_expressed <- lake$get("counts", where = ~ expression %between% c(10, 1000))

# 複合条件
filtered <- lake$get("counts",
  where = ~ gene_id %like% "ENS%" & expression > 100 & is_not_null(annotation)
)
```

### バージョン管理

```r
# スナップショット作成
lake$snap("raw_data", note = "Initial data import")

# タグ付け
lake$tag("counts", "before_normalization")

# 時間を超えてデータ取得
old_counts <- lake$get("counts", ref = "@tag(before_normalization)")

# 復元
lake$restore("raw_data")
```

### Bioconductorサポート

```r
library(SummarizedExperiment)

# SE保存（アダプターが自動処理）
se <- SummarizedExperiment(...)
lake$put("rna_seq", se)

# SE取得（完全復元）
se_restored <- lake$get("rna_seq")
```

---

## 注意事項

1. **base::get() との衝突**: ショートカット関数は `fetch()` を使用（`get()` ではない）
2. **パフォーマンス**: 大規模データでは遅延評価 (`collect = FALSE`) を推奨
3. **スレッドセーフティ**: DuckDB接続は単一スレッド想定
4. **後方互換**: `ol_*` 関数は非推奨警告なしで維持（将来バージョンで警告追加予定）
5. **Seuratアダプター**: 未実装（SEAdapterのみ実装済み）

---

## 更新履歴

- **2024-01**: 初版作成
- **2025-01**: 実装状況に合わせて更新
  - ファイル構造を実際のレイアウトに修正
  - `get()` → `fetch()` への変更を反映
  - 追加演算子 (`%iregex%`, `coalesce`, `if_else_na`) を記載
  - `observe.R` の実装方式説明を更新
  - `wrap.R` の追加機能 (`Pipeline`, `trace_calls`等) を記載
  - NAMESPACEエクスポート一覧を追加
