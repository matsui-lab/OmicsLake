# OmicsLake ユーザービニエット

このビニエットでは、開発版の OmicsLake を手元にセットアップし、デモ用データを生成してパッケージが提供する主要 API を利用するまでの流れを通しで紹介します。

## 1. 環境セットアップ

### 1.1 必要なソフトウェア

- R (4.3 以降を推奨)
- C++ ビルドツール (duckdb パッケージのビルドに必要な場合があります)
- Git (開発版を取得する場合)

### 1.2 依存パッケージのインストール

OmicsLake は DuckDB と Apache Arrow をバックエンドとして利用するため、以下のパッケージを事前にインストールしてください。

```r
install.packages(c(
  "arrow",
  "duckdb",
  "DBI",
  "dplyr" # データベース参照を dplyr で扱いたい場合に便利
))
```

開発中のパッケージ本体は `remotes` などを使ってインストールできます。

```r
install.packages("remotes")
remotes::install_local("path/to/OmicsLake")
```

ソースから作業する場合は、`devtools::load_all()` を利用してロードしても構いません。

## 2. プロジェクトの初期化

OmicsLake ではプロジェクトごとに DuckDB のカタログを管理します。`ol_init()` を実行すると、プロジェクト固有の作業ディレクトリと DuckDB データベースが作成され、以降の操作がそのプロジェクトに紐付きます。【F:R/init.R†L1-L8】

```r
library(OmicsLake)

# "atlas" というプロジェクトを作成して接続
ol_init("atlas")
```

内部的には DuckDB に接続し、カタログとスキーマを準備しています。【F:R/backend.R†L85-L140】

> **ヒント:** プロジェクトのルートパスは `options(ol.root = "/path/to/root")` で変更できます。【F:R/utils.R†L1-L5】

## 3. デモデータの生成

以下のコードは、擬似的な RNA-seq カウントテーブルを生成し、後続のデモに利用するものです。

```r
set.seed(42)
samples <- paste0("sample", sprintf("%02d", 1:6))
genes <- paste0("gene", sprintf("%03d", 1:500))
counts <- matrix(rpois(length(samples) * length(genes), lambda = 50),
                 nrow = length(genes), dimnames = list(genes, samples))
counts_df <- as.data.frame(counts, stringsAsFactors = FALSE)
counts_df$gene_id <- rownames(counts_df)
counts_df <- counts_df[, c("gene_id", samples)]
```

## 4. データベーステーブルへの書き込み

生成したデータフレームをデータベーステーブルとして保存するには `ol_write()` を利用します。この関数は Arrow テーブルとして一時登録し、DuckDB テーブルへ書き込みます。【F:R/io.R†L1-L23】

```r
# 初回は create、再実行時に上書きしたい場合は mode = "overwrite"
ol_write("counts", counts_df, mode = "create")
```

> **注意:** `mode = "append"` を選ぶと既存テーブルの末尾にデータが追記されます。【F:R/io.R†L14-L23】

## 5. スナップショット管理とラベル付け

OmicsLake では書き込みのたびにスナップショットが作成され、`ol_commit()` はその ID を返します (実質的に現在時刻の文字列)。【F:R/io.R†L49-L55】

```r
snapshot_id <- ol_commit("counts import")
```

ヒューマンリーダブルなラベルを付けたい場合は `ol_label()` を使います。内部でメタデータテーブルにタグ情報が保存されます。【F:R/init.R†L10-L34】【F:R/backend.R†L142-L173】

```r
ol_label("baseline")
```

任意のテーブルスナップショットに対して、`ol_tag()` でタグを設定することも可能です。【F:R/backend.R†L205-L234】

```r
ol_tag("counts", tag = "qc-passed", ref = snapshot_id)
```

## 6. テーブルの読み出し

最新スナップショットを取得する場合は `ol_read()` を利用します。参照構文 (`@latest`, `@tag(name)`, `@version(timestamp)` など) を解釈し、該当するスナップショットの SQL を生成して結果を返します。【F:R/io.R†L57-L90】【F:R/backend.R†L146-L204】

```r
counts_latest <- ol_read("counts")                 # 最新
counts_tagged <- ol_read("counts", ref = "@tag(qc-passed)")  # タグ経由
```

`collect = FALSE` を指定すると DB 接続越しの `dplyr::tbl` として返るので、遅延評価でのクエリも可能です。【F:R/io.R†L70-L78】

スナップショット履歴は `ol_log()` で確認できます。【F:R/io.R†L92-L110】

```r
ol_log("counts")
```

## 7. R オブジェクトの保存と読み込み

テーブルではなく R オブジェクトを管理したい場合は `ol_save()` と `ol_read_object()` を利用します。既定ではデータベーステーブル内にシリアライズされたバイト列として保存されますが、外部ストレージモードの場合は RDS ファイルとして保存し、そのパスを記録します。【F:R/io.R†L25-L48】【F:R/backend.R†L97-L118】【F:R/backend.R†L235-L271】

```r
metadata <- list(species = "human", build = "GRCh38")
ol_save("counts_metadata", metadata)
restored <- ol_read_object("counts_metadata")
```

保存済みオブジェクトの最初のバージョンを取得したい場合は `when = "first"` を指定します。【F:R/backend.R†L235-L271】

```r
initial_copy <- ol_read_object("counts_metadata", when = "first")
```

## 8. セッション終了時の後片付け

不要になったプロジェクトは DuckDB 接続を閉じることでクリーンアップできます。

```r
ol_disconnect <- function(project = getOption("ol.project")) {
  OmicsLake:::.ol_disconnect_backend(project)
}

ol_disconnect()
```

`.ol_disconnect_backend()` は内部関数ですが、DuckDB 接続を安全にクローズしレジストリから状態を削除します。【F:R/backend.R†L17-L27】

---

以上で、OmicsLake を用いたプロジェクト初期化からデータ書き込み・参照、メタデータ管理までの一連の流れが把握できるはずです。適宜ご自身のデータやワークフローに置き換えてご活用ください。
