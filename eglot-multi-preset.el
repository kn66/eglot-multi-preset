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
;; LSP multiplexers like rassumfrassum (rass) where different projects
;; may require different server combinations.
;;
;; This package is designed for MULTI-SERVER configurations only.
;; Single-server setups should use `eglot-server-programs' directly.
;;
;; Default presets included:
;;   - Python: "rass: ty + ruff" (type checking + linting/formatting)
;;   - TypeScript/JS: "rass: ts-ls + eslint" and "rass: ts-ls + eslint + tailwind"
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

(defun eglot-multi-preset--make-default-alist ()
  "Generate the default preset alist with platform-appropriate executable names."
  (let ((rass (eglot-multi-preset--executable-name "rass"))
        (ts-ls (eglot-multi-preset--executable-name "typescript-language-server"))
        (eslint (eglot-multi-preset--executable-name "vscode-eslint-language-server"))
        (tailwind (eglot-multi-preset--executable-name "tailwindcss-language-server")))
    `(;; Python: rass preset combining ty (type checker) + ruff (linter/formatter)
      ((python-mode python-ts-mode)
       . (("rass: ty + ruff" . (,rass "python"))))
      ;; TypeScript/JavaScript: rass presets for multi-server configurations
      ((typescript-mode typescript-ts-mode tsx-ts-mode js-mode js-ts-mode)
       . (("rass: ts-ls + eslint"
           . (,rass "--" ,ts-ls "--stdio"
              "--" ,eslint "--stdio"))
          ("rass: ts-ls + eslint + tailwind"
           . (,rass "--" ,ts-ls "--stdio"
              "--" ,eslint "--stdio"
              "--" ,tailwind "--stdio")))))))

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
LSP multiplexers like rass (rassumfrassum).

The default value includes:
  - Python: rass preset with ty + ruff
  - TypeScript/JS: rass presets with ts-ls + eslint, and ts-ls + eslint + tailwind"
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

(defun eglot-multi-preset--dir-locals-has-eglot-config-p ()
  "Check if `.dir-locals.el' has `eglot-server-programs' for current mode.
Returns the eglot-server-programs value if found, nil otherwise."
  (let ((dir-locals-file (eglot-multi-preset--dir-locals-file)))
    (when (file-exists-p dir-locals-file)
      (let ((dir-locals-content
             (with-temp-buffer
               (insert-file-contents dir-locals-file)
               (goto-char (point-min))
               (condition-case nil
                   (read (current-buffer))
                 (error nil)))))
        ;; Look for eglot-server-programs in dir-locals
        (when dir-locals-content
          (let ((mode-entry (or (assq major-mode dir-locals-content)
                                (assq nil dir-locals-content))))
            (when mode-entry
              (cdr (assq 'eglot-server-programs (cdr mode-entry))))))))))

(defun eglot-multi-preset--save-to-dir-locals (contact dir mode)
  "Save CONTACT as `eglot-server-programs' to `.dir-locals.el'.
CONTACT is the server contact specification to save.
DIR is the directory where `.dir-locals.el' will be saved.
MODE is the major mode to associate with the configuration."
  (condition-case err
      (let* ((dir-locals-file (expand-file-name ".dir-locals.el" dir))
             (server-entry (list (cons (list mode) contact))))
        ;; Ensure directory exists
        (unless (file-exists-p dir)
          (make-directory dir t))
        ;; Use add-dir-local-variable approach by manipulating the file directly
        (with-current-buffer (find-file-noselect dir-locals-file)
          (goto-char (point-min))
          (let ((content (condition-case nil
                             (read (current-buffer))
                           (error nil))))
            ;; Find or create mode entry
            (let ((mode-entry (assq mode content)))
              (if mode-entry
                  ;; Update existing mode entry
                  (let ((var-entry (assq 'eglot-server-programs (cdr mode-entry))))
                    (if var-entry
                        (setcdr var-entry server-entry)
                      (setcdr mode-entry
                              (cons (cons 'eglot-server-programs server-entry)
                                    (cdr mode-entry)))))
                ;; Add new mode entry
                (setq content
                      (cons (cons mode
                                  (list (cons 'eglot-server-programs server-entry)))
                            content))))
            ;; Write back
            (erase-buffer)
            (insert ";;; Directory Local Variables -*- no-byte-compile: t -*-\n")
            (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
            (pp content (current-buffer))
            (save-buffer)
            (kill-buffer)))
        (message "Saved eglot preset to %s" dir-locals-file))
    (error
     (message "Failed to save eglot preset: %s" (error-message-string err)))))

(defun eglot-multi-preset--normalize-modes (mode-spec)
  "Normalize MODE-SPEC to a list of mode symbols.
MODE-SPEC can be a single symbol or a list of symbols."
  (if (listp mode-spec)
      mode-spec
    (list mode-spec)))

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

(defun eglot-multi-preset--get-contact (preset-name mode)
  "Get contact specification for PRESET-NAME in MODE.
Returns the contact list for the preset, or nil if PRESET-NAME
is the default label (`eglot-multi-preset-default-label')."
  (unless (string= preset-name eglot-multi-preset-default-label)
    (let ((presets (eglot-multi-preset--lookup-mode-presets mode)))
      (cdr (assoc preset-name presets)))))

(defun eglot-multi-preset--build-candidates (mode)
  "Build completion candidates for MODE.
Returns list starting with the default label followed by mode-specific presets."
  (let ((presets (eglot-multi-preset--lookup-mode-presets mode)))
    (cons eglot-multi-preset-default-label (mapcar #'car presets))))

;;; Eglot integration (advice)

(defvar eglot-multi-preset--in-progress nil
  "Non-nil when preset selection is in progress.
Used to prevent recursive advice calls.")

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
      (let ((eglot-server-programs
             (append existing-config eglot-server-programs)))
        (apply orig-fun args)))
     ;; Show preset selection
     (t
      (let* ((candidates (eglot-multi-preset--build-candidates major-mode))
             (selected (completing-read "LSP preset: " candidates nil t)))
        (if (string= selected eglot-multi-preset-default-label)
            ;; "eglot default" selected - use standard eglot
            (apply orig-fun args)
          ;; Custom preset selected
          (let* ((eglot-multi-preset--in-progress t)
                 (contact (eglot-multi-preset--get-contact selected major-mode))
                 (eglot-server-programs
                  (cons (cons (list major-mode) contact)
                        eglot-server-programs)))
            ;; Save based on eglot-multi-preset-auto-save setting
            (when (and contact
                       (or (not existing-config) force-selection)
                       (not (eq eglot-multi-preset-auto-save 'never))
                       (or (eq eglot-multi-preset-auto-save 'always)
                           (y-or-n-p "Save this preset to .dir-locals.el? ")))
              (let ((save-dir (read-directory-name
                               "Save .dir-locals.el to: "
                               (eglot-multi-preset--get-dir-locals-directory)
                               nil t)))
                (eglot-multi-preset--save-to-dir-locals contact save-dir major-mode)))
            (apply orig-fun args))))))))

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
      (advice-add 'eglot :around #'eglot-multi-preset--maybe-select-preset)
    (advice-remove 'eglot #'eglot-multi-preset--maybe-select-preset)))

;;; Programmatic API

;;;###autoload
(defun eglot-multi-preset-register (mode preset-name contact)
  "Register a preset for MODE.
MODE is a major mode symbol.
PRESET-NAME is a string identifying the preset.
CONTACT is a list (PROGRAM ARGS...) for eglot.

Example:
  (eglot-multi-preset-register \\='python-mode \"my-preset\" \\='(\"my-server\" \"--stdio\"))"
  (let ((existing (assoc mode eglot-multi-preset-alist)))
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
  (let ((existing (assoc mode eglot-multi-preset-alist)))
    (when existing
      (let ((presets (cdr existing)))
        (setcdr existing (assoc-delete-all preset-name presets))
        ;; Remove mode entry if no presets remain
        (unless (cdr existing)
          (setq eglot-multi-preset-alist (assq-delete-all mode eglot-multi-preset-alist)))))))

;;;###autoload
(defun eglot-multi-preset-clear-dir-locals ()
  "Remove eglot-server-programs from `.dir-locals.el' for current mode.
This allows you to reset the saved preset and be prompted again."
  (interactive)
  (let ((dir-locals-file (eglot-multi-preset--dir-locals-file)))
    (if (not (file-exists-p dir-locals-file))
        (message "No .dir-locals.el found at %s" dir-locals-file)
      (with-current-buffer (find-file-noselect dir-locals-file)
        (goto-char (point-min))
        (let ((content (condition-case nil
                           (read (current-buffer))
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
        (kill-buffer)))))

(provide 'eglot-multi-preset)
;;; eglot-multi-preset.el ends here
