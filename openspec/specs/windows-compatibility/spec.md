# windows-compatibility Specification

## Purpose
TBD - created by archiving change add-windows-cmd-suffix. Update Purpose after archive.
## Requirements
### Requirement: Windows Executable Suffix
Windows環境（`system-type`が`windows-nt`または`ms-dos`）では、LSPサーバーコマンド名に`.cmd`拡張子を自動的に付与しなければならない（SHALL）。

#### Scenario: Windows環境でのコマンド名
- **WHEN** `system-type`が`windows-nt`である
- **AND** コマンド名が`rass`である
- **THEN** 実際に使用されるコマンド名は`rass.cmd`となる

#### Scenario: Unix環境でのコマンド名
- **WHEN** `system-type`が`gnu/linux`または`darwin`である
- **AND** コマンド名が`rass`である
- **THEN** 実際に使用されるコマンド名は`rass`のまま変更されない

#### Scenario: 既に拡張子がある場合
- **WHEN** コマンド名に既に`.cmd`または`.exe`拡張子がある
- **THEN** 追加の拡張子は付与されない

### Requirement: Cross-Platform Default Presets
デフォルトの`eglot-multi-preset-alist`に含まれるプリセットは、Windows/Unix両環境で適切なコマンド名を使用しなければならない（SHALL）。

#### Scenario: デフォルトプリセットのWindows対応
- **WHEN** ユーザーがWindows環境でデフォルトプリセットを使用する
- **THEN** `rass.cmd`、`typescript-language-server.cmd`等の適切な拡張子付きコマンドが使用される

#### Scenario: カスタムプリセットの登録
- **WHEN** ユーザーが`eglot-multi-preset-register`でプリセットを登録する
- **THEN** 登録されたコマンド名はそのまま使用される（ユーザー責任）

