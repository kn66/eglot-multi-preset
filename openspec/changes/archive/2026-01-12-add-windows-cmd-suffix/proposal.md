# Change: Add .cmd suffix for Windows compatibility

## Why
Windows環境では、npm/npxでインストールされたNode.jsコマンド（`rass`、`typescript-language-server`、`tailwindcss-language-server`等）は`.cmd`拡張子付きのバッチファイルとして提供される。現在の実装ではUnix系OS向けの拡張子なしコマンド名を使用しているため、Windows環境で動作しない。

## What Changes
- 新しいヘルパー関数`eglot-multi-preset--executable-name`を追加し、`system-type`に基づいてWindows環境では`.cmd`拡張子を付与
- `eglot-multi-preset-alist`のデフォルト値で、コマンド名にこのヘルパー関数を適用
- カスタムプリセット登録時にも自動的に拡張子が適用されるよう設計

## Impact
- Affected specs: 新規capability `windows-compatibility`
- Affected code: `eglot-multi-preset.el` - ヘルパー関数追加、デフォルトalist修正
