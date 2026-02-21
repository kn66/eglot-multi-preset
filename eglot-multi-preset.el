;;; eglot-multi-preset.el --- Multi-server LSP preset selection for eglot -*- lexical-binding: t; -*-

;; Copyright (c) 2026 kn66

;; Author: kn66
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (eglot "1.20"))
;; Keywords: languages, tools
;; URL: https://github.com/Nobuyuki/.emacs.d

;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; eglot-multi-preset provides a mechanism for selecting LSP server presets
;; when starting eglot.  This is particularly useful when working with
;; LSP multiplexers like rassumfrassum (rass) or lspx where different
;; projects may require different server combinations.
;;
;; This package is designed for MULTI-SERVER configurations only.
;; Single-server setups should use `eglot-server-programs' directly.
;;
;; Default presets included:
;;   - Python: "rass: ..." and "lspx: ..."
;;   - TypeScript/JS: "rass: ..." and "lspx: ..."
;;
;; Usage:
;;
;; 1. Enable eglot-multi-preset-mode:
;;
;;    (eglot-multi-preset-mode 1)
;;
;; 2. Now when you run M-x eglot in Python or TypeScript buffers,
;;    you will be prompted to select which preset to use.
;;    "eglot default" uses the standard eglot-server-programs configuration.
;;
;; 3. After selecting a preset, you will be asked whether to save it
;;    to `.dir-locals.el'.  If saved, the preset will be automatically
;;    used next time without prompting.
;;
;; 4. To force preset selection when a saved config exists:
;;
;;    C-u M-x eglot
;;
;; 5. To customize where `.dir-locals.el' is saved:
;;
;;    (setq eglot-multi-preset-dir-locals-directory "/path/to/dir")
;;
;; 6. To add custom presets:
;;
;;    (eglot-multi-preset-register 'vue-mode "rass: vue + tailwind"
;;      '("rass" "--" "vue-language-server" "--stdio"
;;        "--" "tailwindcss-language-server" "--stdio"))

;;; Code:

(require 'eglot)
(require 'cl-lib)

;; Silence byte-compiler warning while dynamically binding this special var.
(defvar read-eval)

;;; Customization group

(defgroup eglot-multi-preset nil
  "Multi-server LSP preset selection for eglot."
  :group 'eglot
  :prefix "eglot-multi-preset-")

;;; Windows compatibility helpers

(defconst eglot-multi-preset--windows-p
  (memq system-type '(windows-nt ms-dos))
  "Non-nil if running on Windows.")

(defun eglot-multi-preset--executable-name (name)
  "Return NAME with `.cmd' suffix on Windows if needed.
On Windows, Node.js CLI tools installed via npm are provided as
.cmd batch files.  This function adds the suffix automatically
unless NAME already ends with `.cmd' or `.exe'."
  (if (and eglot-multi-preset--windows-p
           (not (string-match-p "\\.\\(cmd\\|exe\\)\\'" name)))
      (concat name ".cmd")
    name))

(defconst eglot-multi-preset--eslint-workspace-config
  '(:eslint (:validate "probe"
             :workingDirectory (:mode "auto")
             :workingDirectories [(:mode "auto")]
             :packageManager "npm"))
  "Default workspace configuration for ESLint language server.
Based on lsp-mode defaults for vscode-eslint-language-server.")

(defconst eglot-multi-preset--tailwind-workspace-config
  '(:tailwindCSS (:emmetCompletions :json-false
                  :showPixelEquivalents t
                  :rootFontSize 16
                  :validate t
                  :hovers t
                  :suggestions t
                  :codeActions t
                  :classAttributes ["class" "className" "ngClass" "class:list"]))
  "Default workspace configuration for tailwindcss-language-server.
Mirrors the baseline settings from lsp-tailwindcss.")

(defconst eglot-multi-preset--tailwind-initialization-options
  '(:configuration (:tailwindCSS (:emmetCompletions :json-false
                     :showPixelEquivalents t
                     :rootFontSize 16
                     :validate t
                     :hovers t
                     :suggestions t
                     :codeActions t
                     :classAttributes ["class" "className" "ngClass" "class:list"])))
  "Initialization options for tailwindcss-language-server.
lsp-tailwindcss sends a non-null `configuration' object at initialize time.")

(defun eglot-multi-preset--make-default-alist ()
  "Generate the default preset alist with platform-appropriate executable names."
  (let ((rass (eglot-multi-preset--executable-name "rass"))
        (lspx (eglot-multi-preset--executable-name "lspx"))
        (pyright (eglot-multi-preset--executable-name "pyright-langserver"))
        (ts-ls (eglot-multi-preset--executable-name "typescript-language-server"))
        (eslint (eglot-multi-preset--executable-name "vscode-eslint-language-server"))
        (tailwind (eglot-multi-preset--executable-name "tailwindcss-language-server")))
    `(;; Python: rass/lspx presets combining type checker + ruff
      ((python-mode python-ts-mode)
       . (("rass: ty + ruff" . (,rass "python"))
          ("rass: pyright + ruff"
           . (,rass "--" ,pyright "--stdio"
                    "--" "ruff" "server"))
          ("lspx: ty + ruff"
           . (,lspx "--lsp" "ty server"
                    "--lsp" "ruff server"))
          ("lspx: pyright + ruff"
           . (,lspx "--lsp" ,(format "%s --stdio" pyright)
                    "--lsp" "ruff server"))))
      ;; TypeScript/JavaScript: rass/lspx presets for multi-server configurations
      ((typescript-mode typescript-ts-mode typescript-tsx-mode tsx-ts-mode js-mode js-ts-mode)
       . (("rass: ts-ls + eslint"
           . ( :contact (,rass "--" ,ts-ls "--stdio"
                               "--" ,eslint "--stdio")
               :workspace-config ,eglot-multi-preset--eslint-workspace-config))
          ("rass: ts-ls + eslint + tailwind"
           . ( :contact (,rass "--" ,ts-ls "--stdio"
                               "--" ,eslint "--stdio"
                               "--" ,tailwind "--stdio"
                               :initializationOptions
                               ,eglot-multi-preset--tailwind-initialization-options)
               :workspace-config
               ,(append eglot-multi-preset--eslint-workspace-config
                        eglot-multi-preset--tailwind-workspace-config)))
          ("lspx: ts-ls + eslint"
           . ( :contact (,lspx "--lsp" ,(format "%s --stdio" ts-ls)
                               "--lsp" ,(format "%s --stdio" eslint))
               :workspace-config ,eglot-multi-preset--eslint-workspace-config))
          ("lspx: ts-ls + eslint + tailwind"
           . ( :contact (,lspx "--lsp" ,(format "%s --stdio" ts-ls)
                               "--lsp" ,(format "%s --stdio" eslint)
                               "--lsp" ,(format "%s --stdio" tailwind)
                               :initializationOptions
                               ,eglot-multi-preset--tailwind-initialization-options)
               :workspace-config
               ,(append eglot-multi-preset--eslint-workspace-config
                        eglot-multi-preset--tailwind-workspace-config))))))))

;;; Core data structure

(defcustom eglot-multi-preset-alist
  (eglot-multi-preset--make-default-alist)
  "Alist mapping major modes to their LSP server presets.
Each entry is (MODE-OR-MODES . PRESETS) where:

MODE-OR-MODES is:
  - A single major mode symbol (e.g., `python-mode')
  - A list of major mode symbols (e.g., (typescript-mode tsx-ts-mode))

PRESETS is a list of (PRESET-NAME . CONTACT) pairs where:
  - PRESET-NAME is a string identifying the preset
  - CONTACT is a list (PROGRAM ARGS...) suitable for eglot

Note: Single-server configurations should use `eglot-server-programs'
directly.  This package is for multi-server configurations using
LSP multiplexers like rass (rassumfrassum) and lspx.

The default value includes:
  - Python: rass/lspx presets with ty + ruff, and pyright + ruff
  - TypeScript/JS: rass/lspx presets with ts-ls + eslint, and ts-ls + eslint + tailwind"
  :type '(alist :key-type (choice symbol (repeat symbol))
                :value-type (alist :key-type string :value-type sexp))
  :group 'eglot-multi-preset)

(defcustom eglot-multi-preset-default-label "eglot default"
  "Label for the default eglot server option in preset selection."
  :type 'string
  :group 'eglot-multi-preset)

(defcustom eglot-multi-preset-dir-locals-directory nil
  "Directory where `.dir-locals.el' should be saved.
If nil, uses the project root directory.
If set to a path, uses that directory instead."
  :type '(choice (const :tag "Project root" nil)
                 (directory :tag "Custom directory"))
  :group 'eglot-multi-preset)

(defcustom eglot-multi-preset-auto-save 'ask
  "Whether to automatically save selected presets to `.dir-locals.el'.
- `ask': Prompt before saving (default)
- `always': Always save without prompting
- `never': Never save to .dir-locals.el"
  :type '(choice (const :tag "Ask before saving" ask)
                 (const :tag "Always save" always)
                 (const :tag "Never save" never))
  :group 'eglot-multi-preset)

(defcustom eglot-multi-preset-language-id-overrides
  '((tsx-ts-mode . "typescriptreact")
    (typescript-tsx-mode . "typescriptreact")
    (typescript-ts-mode . "typescript"))
  "Alist mapping major modes to explicit LSP language IDs.
When a mode is present here, eglot server entries written by this package
use `(MODE :language-id ID)' instead of bare MODE.  This is useful for
tree-sitter modes like `tsx-ts-mode' whose default language ID may not be
understood by some language servers."
  :type '(alist :key-type symbol :value-type string)
  :group 'eglot-multi-preset)

;;; Project workspace configuration storage

(defvar eglot-multi-preset--project-workspace-configs (make-hash-table :test 'equal)
  "Hash table mapping project roots to workspace configurations.
Used to provide workspace configuration to eglot without requiring
.dir-locals.el to be saved first.")

(defun eglot-multi-preset--normalize-project-root (root)
  "Normalize ROOT so it can be used as a stable hash-table key."
  (when root
    (file-name-as-directory
     (expand-file-name
      (if (file-exists-p root)
          (file-truename root)
        root)))))

(defun eglot-multi-preset--current-project-root ()
  "Return current project root, including Eglot transient project fallback."
  (when-let ((project (or (project-current) (eglot--current-project))))
    (eglot-multi-preset--normalize-project-root (project-root project))))

(defun eglot-multi-preset--server-mode-spec (mode)
  "Build eglot server mode spec for MODE with optional language-id override."
  (if-let ((language-id (cdr (assq mode eglot-multi-preset-language-id-overrides))))
      (list mode :language-id language-id)
    (list mode)))

(defun eglot-multi-preset--workspace-section-name (section)
  "Convert SECTION key to a workspace/configuration section name string."
  (if (keywordp section)
      (substring (symbol-name section) 1)
    section))

(defun eglot-multi-preset--workspace-config-has-empty-section-p (config)
  "Return non-nil if CONFIG already defines the empty section key."
  (cond
   ;; Alist: ((SECTION . VALUE) ...)
   ((and (listp config) (consp (car-safe config)))
    (cl-loop for (section . _value) in config
             thereis (equal (eglot-multi-preset--workspace-section-name section)
                            "")))
   ;; Plist: (SECTION VALUE ...)
   ((listp config)
    (cl-loop for (section _value) on config by #'cddr
             thereis (equal (eglot-multi-preset--workspace-section-name section)
                            "")))
   (t nil)))

(defun eglot-multi-preset--workspace-config-section-value (config section-name)
  "Return SECTION-NAME value from CONFIG, or nil if missing.
SECTION-NAME is matched after normalizing keys via
`eglot-multi-preset--workspace-section-name'."
  (cond
   ((not (listp config)) nil)
   ;; Alist: ((SECTION . VALUE) ...)
   ((consp (car-safe config))
    (when-let ((entry
                (cl-find-if
                 (lambda (item)
                   (equal (eglot-multi-preset--workspace-section-name (car item))
                          section-name))
                 config)))
      (copy-tree (cdr entry))))
   ;; Plist: (SECTION VALUE ...)
   (t
    (cl-loop for (section value) on config by #'cddr
             when (equal (eglot-multi-preset--workspace-section-name section)
                         section-name)
             return (copy-tree value)))))

(defun eglot-multi-preset--workspace-config-has-eslint-section-p (config)
  "Return non-nil if CONFIG defines an ESLint workspace section."
  (and (eglot-multi-preset--workspace-config-section-value config "eslint")
       t))

(defun eglot-multi-preset--workspace-config-empty-section-value (config)
  "Return fallback value for workspace/configuration section \"\" from CONFIG.
If CONFIG defines an ESLint section, return only that section value.
Otherwise return a copy of CONFIG."
  (cond
   ((not (listp config)) config)
   ((eglot-multi-preset--workspace-config-section-value config "eslint"))
   (t (copy-tree config))))

(defun eglot-multi-preset--workspace-config-remove-empty-section (config)
  "Return CONFIG with any section \"\" entries removed."
  (cond
   ((not (listp config)) config)
   ;; Alist: ((SECTION . VALUE) ...)
   ((consp (car-safe config))
    (cl-remove-if
     (lambda (entry)
       (equal (eglot-multi-preset--workspace-section-name (car entry)) ""))
     config))
   ;; Plist: (SECTION VALUE ...)
   (t
    (let (result)
      (cl-loop for (section value) on config by #'cddr
               unless (equal (eglot-multi-preset--workspace-section-name section) "")
               do (setq result (append result (list section value))))
      result))))

(defun eglot-multi-preset--workspace-config-with-empty-section (config)
  "Ensure CONFIG can satisfy workspace/configuration requests for section \"\"."
  (let* ((has-empty (eglot-multi-preset--workspace-config-has-empty-section-p config))
         (has-eslint (eglot-multi-preset--workspace-config-has-eslint-section-p config))
         (base-config (if (and has-empty has-eslint)
                          (eglot-multi-preset--workspace-config-remove-empty-section config)
                        config))
         (empty-section-value
          (eglot-multi-preset--workspace-config-empty-section-value base-config)))
    (cond
     ((not (listp config)) config)
     ;; Keep existing empty-section entries unless ESLint normalization is needed.
     ((and has-empty (not has-eslint)) config)
     ;; Alist: append (\"\" . VALUE)
     ((consp (car-safe config))
      (append base-config (list (cons "" empty-section-value))))
     ;; Plist: append \"\" VALUE
     (t
      (append base-config (list "" empty-section-value))))))

(defun eglot-multi-preset--workspace-configuration-function (server)
  "Return workspace configuration for SERVER.
First checks the preset-specific config stored in
`eglot-multi-preset--project-workspace-configs', then falls back
to reading from .dir-locals.el."
  (let* ((project-root
          (eglot-multi-preset--normalize-project-root
           (project-root (eglot--project server))))
         (preset-config (gethash project-root
                                 eglot-multi-preset--project-workspace-configs))
         (dir-local-config
          ;; Fallback: read from dir-locals (existing behavior)
          (when project-root
            (with-temp-buffer
              (setq default-directory project-root)
              (setq major-mode (car (eglot--major-modes server)))
              (hack-dir-local-variables-non-file-buffer)
              eglot-workspace-configuration))))
    (eglot-multi-preset--workspace-config-with-empty-section
     (or preset-config
         ;; If no dir-local value is set, this can still be the default
         ;; function value.  In that case, return nil to avoid sending a
         ;; function object as workspace configuration.
         (unless (functionp dir-local-config)
           dir-local-config)))))

;;; Internal functions

(defun eglot-multi-preset--get-dir-locals-directory ()
  "Get the directory for `.dir-locals.el' operations.
Returns `eglot-multi-preset-dir-locals-directory' if set,
otherwise returns the project root directory."
  (or eglot-multi-preset-dir-locals-directory
      (when-let ((project (project-current)))
        (project-root project))
      default-directory))

(defun eglot-multi-preset--dir-locals-file ()
  "Get the path to `.dir-locals.el' for the current context."
  (expand-file-name ".dir-locals.el"
                    (eglot-multi-preset--get-dir-locals-directory)))

(defun eglot-multi-preset--choose-save-directory ()
  "Choose target directory for saving `.dir-locals.el'."
  (if eglot-multi-preset-dir-locals-directory
      (expand-file-name eglot-multi-preset-dir-locals-directory)
    (read-directory-name "Save .dir-locals.el to: "
                         (eglot-multi-preset--get-dir-locals-directory)
                         nil t)))

(defun eglot-multi-preset--read-dir-locals ()
  "Read and parse `.dir-locals.el' content.
Returns the parsed alist, or nil if file doesn't exist or can't be parsed."
  (let ((dir-locals-file (eglot-multi-preset--dir-locals-file)))
    (when (file-exists-p dir-locals-file)
      (with-temp-buffer
        (insert-file-contents dir-locals-file)
        (goto-char (point-min))
        (condition-case nil
            (eglot-multi-preset--safe-read (current-buffer))
          (error nil))))))

(defun eglot-multi-preset--safe-read (buffer)
  "Read one Lisp form from BUFFER with read-time evaluation disabled."
  (let ((read-eval nil))
    (read buffer)))

(defun eglot-multi-preset--dir-locals-find-mode-entry (dir-locals-content mode)
  "Find MODE entry in DIR-LOCALS-CONTENT with related-mode fallback.
Lookup order is MODE, then related modes from
`eglot-multi-preset--related-modes', then nil."
  (or (assq mode dir-locals-content)
      (cl-loop for related-mode in (eglot-multi-preset--related-modes mode)
               unless (eq related-mode mode)
               thereis (assq related-mode dir-locals-content))
      (assq nil dir-locals-content)))

(defun eglot-multi-preset--dir-locals-has-eglot-config-p ()
  "Check if `.dir-locals.el' has `eglot-server-programs' for current mode.
Returns the eglot-server-programs value if found, nil otherwise."
  (let ((dir-locals-content (eglot-multi-preset--read-dir-locals)))
    (when dir-locals-content
      (let ((mode-entry
             (eglot-multi-preset--dir-locals-find-mode-entry
              dir-locals-content
              major-mode)))
        (when mode-entry
          (cdr (assq 'eglot-server-programs (cdr mode-entry))))))))

(defun eglot-multi-preset--dir-locals-get-workspace-config ()
  "Get `eglot-workspace-configuration' from `.dir-locals.el' for current mode.
Returns the workspace configuration plist if found, nil otherwise."
  (let ((dir-locals-content (eglot-multi-preset--read-dir-locals)))
    (when dir-locals-content
      (let ((mode-entry
             (eglot-multi-preset--dir-locals-find-mode-entry
              dir-locals-content
              major-mode)))
        (when mode-entry
          (cdr (assq 'eglot-workspace-configuration (cdr mode-entry))))))))

(defun eglot-multi-preset--save-to-dir-locals (contact workspace-config dir mode)
  "Save CONTACT and WORKSPACE-CONFIG to `.dir-locals.el'.
CONTACT is the server contact specification to save.
WORKSPACE-CONFIG is the eglot-workspace-configuration plist (can be nil).
DIR is the directory where `.dir-locals.el' will be saved.
MODE is the major mode to associate with the configuration.

When MODE belongs to a grouped preset (e.g. TS/JS modes), this writes
entries for all related modes so reopening another mode in the same
group still uses the selected preset."
  (condition-case err
      (let* ((dir-locals-file (expand-file-name ".dir-locals.el" dir))
             (target-modes (eglot-multi-preset--related-modes mode))
             (existing-buffer (get-file-buffer dir-locals-file)))
        ;; Ensure directory exists
        (unless (file-exists-p dir)
          (make-directory dir t))
        ;; Use add-dir-local-variable approach by manipulating the file directly
        (with-current-buffer (find-file-noselect dir-locals-file)
          (goto-char (point-min))
          (let ((content (condition-case nil
                             (eglot-multi-preset--safe-read (current-buffer))
                           (error nil))))
            ;; Find or create mode entries for all related modes.
            (dolist (target-mode target-modes)
              (let* ((mode-entry (assq target-mode content))
                     (server-entry
                      (list (cons (eglot-multi-preset--server-mode-spec target-mode)
                                  contact))))
                (if mode-entry
                    ;; Update existing mode entry
                    (progn
                      ;; Update eglot-server-programs
                      (let ((var-entry (assq 'eglot-server-programs (cdr mode-entry))))
                        (if var-entry
                            (setcdr var-entry server-entry)
                          (setcdr mode-entry
                                  (cons (cons 'eglot-server-programs server-entry)
                                        (cdr mode-entry)))))
                      ;; Update eglot-workspace-configuration if provided
                      (when workspace-config
                        (let ((ws-entry (assq 'eglot-workspace-configuration (cdr mode-entry))))
                          (if ws-entry
                              (setcdr ws-entry workspace-config)
                            (setcdr mode-entry
                                    (cons (cons 'eglot-workspace-configuration workspace-config)
                                          (cdr mode-entry)))))))
                  ;; Add new mode entry
                  (let ((new-entry (list (cons 'eglot-server-programs server-entry))))
                    (when workspace-config
                      (push (cons 'eglot-workspace-configuration workspace-config) new-entry))
                    (setq content (cons (cons target-mode new-entry) content))))))
            ;; Write back
            (erase-buffer)
            (insert ";;; Directory Local Variables -*- no-byte-compile: t -*-\n")
            (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
            (pp content (current-buffer))
            (save-buffer)))
        (unless existing-buffer
          (kill-buffer (get-file-buffer dir-locals-file)))
        (message "Saved eglot preset to %s" dir-locals-file))
    (error
     (message "Failed to save eglot preset: %s" (error-message-string err)))))

(defun eglot-multi-preset--normalize-modes (mode-spec)
  "Normalize MODE-SPEC to a list of mode symbols.
MODE-SPEC can be a single symbol or a list of symbols."
  (if (listp mode-spec)
      mode-spec
    (list mode-spec)))

(defun eglot-multi-preset--related-modes (mode)
  "Return preset-related mode symbols for MODE.
If MODE belongs to a grouped mode spec in `eglot-multi-preset-alist',
return all symbols in that group.  Otherwise return a list with MODE only."
  (if-let ((entry
            (cl-find-if
             (lambda (item)
               (eglot-multi-preset--mode-matches-p mode (car item)))
             eglot-multi-preset-alist)))
      (let ((modes (eglot-multi-preset--normalize-modes (car entry))))
        (cl-remove-duplicates (cl-remove-if-not #'symbolp modes)))
    (list mode)))

(defun eglot-multi-preset--mode-matches-p (mode mode-spec)
  "Check if MODE matches MODE-SPEC.
MODE-SPEC can be a single mode symbol or a list of mode symbols.
Also checks parent modes for inheritance."
  (let ((modes (eglot-multi-preset--normalize-modes mode-spec)))
    (or (memq mode modes)
        ;; Check parent mode
        (let ((parent (get mode 'derived-mode-parent)))
          (and parent
               (eglot-multi-preset--mode-matches-p parent mode-spec))))))

(defun eglot-multi-preset--lookup-mode-presets (mode)
  "Look up presets for MODE from `eglot-multi-preset-alist'.
Returns the presets alist for MODE, or nil if no presets are registered.
Also checks parent modes for inheritance."
  (catch 'found
    (dolist (entry eglot-multi-preset-alist)
      (let ((mode-spec (car entry))
            (presets (cdr entry)))
        (when (eglot-multi-preset--mode-matches-p mode mode-spec)
          (throw 'found presets))))
    nil))

(defun eglot-multi-preset--find-mode-entry (mode)
  "Find the first preset entry matching MODE.
MODE can be a symbol or mode list.  For symbols, this also matches
grouped mode entries containing MODE."
  (or (assoc mode eglot-multi-preset-alist)
      (when (symbolp mode)
        (cl-find-if (lambda (entry)
                      (memq mode
                            (eglot-multi-preset--normalize-modes (car entry))))
                    eglot-multi-preset-alist))))

(defun eglot-multi-preset--extended-format-p (preset-value)
  "Check if PRESET-VALUE uses the extended plist format.
Extended format has :contact key, legacy format is just a list."
  (and (listp preset-value)
       (plist-member preset-value :contact)))

(defun eglot-multi-preset--contact-executables (contact)
  "Extract executable names from CONTACT when it is a command list.
Supports multiplexed commands like:
  (\"rass\" \"--\" \"typescript-language-server\" \"--stdio\" ...)
or
  (\"lspx\" \"--lsp\" \"typescript-language-server --stdio\" ...)."
  (when (and (listp contact) (stringp (car contact)))
    (let ((executables (list (car contact)))
          (rest (cdr contact)))
      (while rest
        (let ((token (car rest))
              (next (cadr rest)))
          (cond
           ((and (stringp token)
                 (string= token "--")
                 (stringp next)
                 (not (string-prefix-p "-" next)))
            (push next executables))
           ((and (stringp token)
                 (string= token "--lsp")
                 (stringp next))
            (when-let* ((parts (split-string-and-unquote next))
                        (exe (car parts)))
              (unless (or (string= exe "")
                          (string-prefix-p "-" exe))
                (push exe executables)))))
          (setq rest (cdr rest))))
      (delete-dups (nreverse executables)))))

(defun eglot-multi-preset--missing-executables (contact)
  "Return missing executable names in CONTACT."
  (cl-remove-if #'executable-find
                (eglot-multi-preset--contact-executables contact)))

(defun eglot-multi-preset--contact-from-server-programs (server-programs mode)
  "Extract CONTACT for MODE from SERVER-PROGRAMS.
SERVER-PROGRAMS should be a value compatible with `eglot-server-programs'."
  (when (listp server-programs)
    (when-let* ((entry
                 (cl-find-if
                  (lambda (item)
                    (let ((modes (car-safe item)))
                      (or (eq modes mode)
                          (and (listp modes) (memq mode modes)))))
                  server-programs)))
      (cdr entry))))

(defun eglot-multi-preset--get-contact (preset-name mode)
  "Get contact specification for PRESET-NAME in MODE.
Returns the contact list for the preset, or nil if PRESET-NAME
is the default label (`eglot-multi-preset-default-label').
Supports both legacy format (PROGRAM ARGS...) and extended format
(:contact (PROGRAM ARGS...) :workspace-config ...)."
  (unless (string= preset-name eglot-multi-preset-default-label)
    (let* ((presets (eglot-multi-preset--lookup-mode-presets mode))
           (preset-value (cdr (assoc preset-name presets))))
      (if (eglot-multi-preset--extended-format-p preset-value)
          (plist-get preset-value :contact)
        preset-value))))

(defun eglot-multi-preset--get-workspace-config (preset-name mode)
  "Get workspace configuration for PRESET-NAME in MODE.
Returns the workspace-config plist for the preset, or nil if not defined.
Only extended format presets can have workspace configuration."
  (unless (string= preset-name eglot-multi-preset-default-label)
    (let* ((presets (eglot-multi-preset--lookup-mode-presets mode))
           (preset-value (cdr (assoc preset-name presets))))
      (when (eglot-multi-preset--extended-format-p preset-value)
        (plist-get preset-value :workspace-config)))))

(defun eglot-multi-preset--build-candidates (mode)
  "Build completion candidates for MODE.
Returns list starting with the default label followed by mode-specific presets."
  (let ((presets (eglot-multi-preset--lookup-mode-presets mode)))
    (cons eglot-multi-preset-default-label (mapcar #'car presets))))

(defun eglot-multi-preset--apply-workspace-config (workspace-config)
  "Apply WORKSPACE-CONFIG for the current project.
WORKSPACE-CONFIG is a plist like (:eslint (:validate \"probe\" ...)).
This stores the configuration in `eglot-multi-preset--project-workspace-configs'
using the project root as key, so it can be retrieved by the
workspace-configuration function when eglot requests it."
  (when-let ((project-root (eglot-multi-preset--current-project-root)))
    (if workspace-config
        (puthash project-root workspace-config
                 eglot-multi-preset--project-workspace-configs)
      (remhash project-root eglot-multi-preset--project-workspace-configs))))

;;; Eglot integration (advice)

(defvar eglot-multi-preset--in-progress nil
  "Non-nil when preset selection is in progress.
Used to prevent recursive advice calls.")

(defun eglot-multi-preset--refresh-eglot-args-if-interactive (args)
  "Refresh eglot ARGS for interactive calls after preset injection.
When this advice changes `eglot-server-programs' dynamically, ARGS may
still point to contacts guessed before the change.  Re-guess in that
case so `eglot' starts the intended server contact."
  (if (and (listp args)
           (>= (length args) 6)
           (nth 5 args))
      (append (eglot--guess-contact nil) (list t))
    args))

(defun eglot-multi-preset--maybe-select-preset (orig-fun &rest args)
  "Advice for `eglot' to prompt for preset selection.
If presets are registered for the current mode and no server is
running, show selection UI.  Otherwise, call ORIG-FUN with ARGS as-is.
With prefix argument, force preset selection even if dir-locals exists."
  (let ((presets (eglot-multi-preset--lookup-mode-presets major-mode))
        (existing-config (eglot-multi-preset--dir-locals-has-eglot-config-p))
        (force-selection current-prefix-arg))
    (cond
     ;; Already in progress or server running - normal behavior
     ((or eglot-multi-preset--in-progress (eglot-current-server))
      (apply orig-fun args))
     ;; No presets registered - normal behavior
     ((not presets)
      (apply orig-fun args))
     ;; Dir-locals config exists and not forcing - use it
     ((and existing-config (not force-selection))
      (message "Using eglot preset from .dir-locals.el")
      (let* ((workspace-config (eglot-multi-preset--dir-locals-get-workspace-config))
             (contact-from-args (nth 3 args))
             (saved-contact (eglot-multi-preset--contact-from-server-programs
                             existing-config major-mode))
             (contact
              (if (and (listp contact-from-args) (stringp (car contact-from-args)))
                  contact-from-args
                saved-contact)))
        ;; Always inject saved server programs while calling `eglot'.  This
        ;; covers both custom dir-locals directories and related-mode fallback
        ;; cases where Emacs did not apply mode-local variables directly.
        (let ((eglot-server-programs (append existing-config eglot-server-programs)))
          (when-let ((missing (eglot-multi-preset--missing-executables contact)))
            (user-error "Missing LSP executables: %s" (mapconcat #'identity missing ", ")))
          (eglot-multi-preset--apply-workspace-config workspace-config)
          (apply orig-fun
                 (eglot-multi-preset--refresh-eglot-args-if-interactive args)))))
     ;; Show preset selection
     (t
      (let* ((candidates (eglot-multi-preset--build-candidates major-mode))
             (selected (completing-read "LSP preset: " candidates nil t)))
        (if (string= selected eglot-multi-preset-default-label)
            ;; "eglot default" selected - use standard eglot
            (progn
              (eglot-multi-preset--apply-workspace-config nil)
              (apply orig-fun args))
          ;; Custom preset selected
          (let* ((eglot-multi-preset--in-progress t)
                 (contact (eglot-multi-preset--get-contact selected major-mode))
                 (workspace-config (eglot-multi-preset--get-workspace-config selected major-mode))
                 (eglot-server-programs
                  (cons (cons (eglot-multi-preset--server-mode-spec major-mode)
                              contact)
                        eglot-server-programs)))
            (when-let ((missing (eglot-multi-preset--missing-executables contact)))
              (user-error "Missing LSP executables: %s" (mapconcat #'identity missing ", ")))
            ;; Apply workspace configuration buffer-locally
            (eglot-multi-preset--apply-workspace-config workspace-config)
            ;; Save based on eglot-multi-preset-auto-save setting
            (when (and contact
                       (or (not existing-config) force-selection)
                       (not (eq eglot-multi-preset-auto-save 'never))
                       (or (eq eglot-multi-preset-auto-save 'always)
                           (y-or-n-p "Save this preset to .dir-locals.el? ")))
              (let ((save-dir (eglot-multi-preset--choose-save-directory)))
                (eglot-multi-preset--save-to-dir-locals contact workspace-config save-dir major-mode)))
            (apply orig-fun
                   (eglot-multi-preset--refresh-eglot-args-if-interactive args)))))))))

(defvar eglot-multi-preset--saved-workspace-configuration nil
  "Saved value of `eglot-workspace-configuration' default before mode activation.")

;;;###autoload
(define-minor-mode eglot-multi-preset-mode
  "Global minor mode to enable preset selection on eglot startup.
When enabled, running `eglot' in a buffer with registered presets
will prompt for preset selection via `completing-read'.
The first option is always \"eglot default\" which uses the standard
`eglot-server-programs' configuration."
  :global t
  :group 'eglot-multi-preset
  :lighter " EgMP"
  (if eglot-multi-preset-mode
      (progn
        ;; Save current default and set our lookup function
        (setq eglot-multi-preset--saved-workspace-configuration
              (default-value 'eglot-workspace-configuration))
        (setq-default eglot-workspace-configuration
                      #'eglot-multi-preset--workspace-configuration-function)
        (advice-add 'eglot :around #'eglot-multi-preset--maybe-select-preset))
    ;; Restore original default and remove advice
    (setq-default eglot-workspace-configuration
                  eglot-multi-preset--saved-workspace-configuration)
    (clrhash eglot-multi-preset--project-workspace-configs)
    (advice-remove 'eglot #'eglot-multi-preset--maybe-select-preset)))

;;; Programmatic API

;;;###autoload
(defun eglot-multi-preset-register (mode preset-name contact)
  "Register a preset for MODE.
MODE is a major mode symbol.
PRESET-NAME is a string identifying the preset.
CONTACT is a list (PROGRAM ARGS...) for eglot.

If MODE belongs to a grouped mode entry in
`eglot-multi-preset-alist', that entry is updated.

Example:
  (eglot-multi-preset-register \\='python-mode \"my-preset\" \\='(\"my-server\" \"--stdio\"))"
  (let ((existing (eglot-multi-preset--find-mode-entry mode)))
    (if existing
        ;; Mode already has presets - add or update
        (let ((presets (cdr existing)))
          (if (assoc preset-name presets)
              ;; Update existing preset
              (setcdr (assoc preset-name presets) contact)
            ;; Add new preset
            (setcdr existing (cons (cons preset-name contact) presets))))
      ;; New mode entry
      (push (cons mode (list (cons preset-name contact))) eglot-multi-preset-alist))))

;;;###autoload
(defun eglot-multi-preset-unregister (mode preset-name)
  "Unregister a preset from MODE.
MODE is a major mode symbol.
PRESET-NAME is the string identifying the preset to remove."
  (let ((existing (eglot-multi-preset--find-mode-entry mode)))
    (when existing
      (let ((presets (cdr existing)))
        (setcdr existing (assoc-delete-all preset-name presets))
        ;; Remove mode entry if no presets remain
        (unless (cdr existing)
          (setq eglot-multi-preset-alist
                (delete existing eglot-multi-preset-alist)))))))

;;;###autoload
(defun eglot-multi-preset-clear-dir-locals ()
  "Remove eglot-server-programs from `.dir-locals.el' for current mode.
This allows you to reset the saved preset and be prompted again."
  (interactive)
  (let ((dir-locals-file (eglot-multi-preset--dir-locals-file)))
    (if (not (file-exists-p dir-locals-file))
        (message "No .dir-locals.el found at %s" dir-locals-file)
      (let ((existing-buffer (get-file-buffer dir-locals-file)))
        (with-current-buffer (find-file-noselect dir-locals-file)
          (goto-char (point-min))
          (let ((content (condition-case nil
                             (eglot-multi-preset--safe-read (current-buffer))
                           (error nil))))
            (when content
              (let ((mode-entry (assq major-mode content)))
                (if (not mode-entry)
                    (message "No eglot preset found for %s in .dir-locals.el" major-mode)
                  ;; Remove eglot-server-programs from mode entry
                  (setcdr mode-entry
                          (assq-delete-all 'eglot-server-programs (cdr mode-entry)))
                  ;; Remove mode entry if empty
                  (unless (cdr mode-entry)
                    (setq content (assq-delete-all major-mode content)))
                  ;; Write back
                  (erase-buffer)
                  (if content
                      (progn
                        (insert ";;; Directory Local Variables -*- no-byte-compile: t -*-\n")
                        (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
                        (pp content (current-buffer)))
                    (insert ";;; Directory Local Variables -*- no-byte-compile: t -*-\n")
                    (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
                    (insert "()\n"))
                  (save-buffer)
                  (message "Cleared eglot preset for %s from %s" major-mode dir-locals-file)))))
          (unless existing-buffer
            (kill-buffer (get-file-buffer dir-locals-file))))))))

(provide 'eglot-multi-preset)
;;; eglot-multi-preset.el ends here
