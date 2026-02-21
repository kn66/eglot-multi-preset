;;; eglot-multi-preset-tests.el --- Tests for eglot-multi-preset -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Regression tests for mode activation/deactivation behavior and
;; contact executable validation.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'eglot-multi-preset)

(defun eglot-multi-preset-tests--read-form (file)
  "Read first Lisp form from FILE with read-time evaluation disabled."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (eglot-multi-preset--safe-read (current-buffer))))

(ert-deftest eglot-multi-preset-mode-reenable-preserves-workspace-default ()
  "Re-enabling the mode should not overwrite the saved default value."
  (let ((original-default (default-value 'eglot-workspace-configuration))
        (sentinel 'eglot-multi-preset-test-sentinel))
    (unwind-protect
        (progn
          (eglot-multi-preset-mode 0)
          (setq-default eglot-workspace-configuration sentinel)
          (eglot-multi-preset-mode 1)
          (eglot-multi-preset-mode 1)
          (eglot-multi-preset-mode 0)
          (should (eq (default-value 'eglot-workspace-configuration)
                      sentinel)))
      (ignore-errors (eglot-multi-preset-mode 0))
      (setq-default eglot-workspace-configuration original-default))))

(ert-deftest eglot-multi-preset-mode-disable-when-inactive-keeps-default ()
  "Disabling an inactive mode must not mutate workspace defaults."
  (let ((original-default (default-value 'eglot-workspace-configuration))
        (sentinel 'eglot-multi-preset-test-sentinel-disabled))
    (unwind-protect
        (progn
          (eglot-multi-preset-mode 0)
          (setq-default eglot-workspace-configuration sentinel)
          (eglot-multi-preset-mode 0)
          (should (eq (default-value 'eglot-workspace-configuration)
                      sentinel)))
      (ignore-errors (eglot-multi-preset-mode 0))
      (setq-default eglot-workspace-configuration original-default))))

(ert-deftest eglot-multi-preset-missing-executables-ignores-tcp-contact ()
  "TCP contacts of the form (HOST PORT) should skip executable checks."
  (should-not (eglot-multi-preset--missing-executables '("127.0.0.1" 2087))))

(ert-deftest eglot-multi-preset-missing-executables-checks-command-contact ()
  "Command contacts should still report missing executables."
  (let ((program "eglot-multi-preset-not-a-real-executable-for-test"))
    (should (equal (eglot-multi-preset--missing-executables (list program))
                   (list program)))))

(ert-deftest eglot-multi-preset-save-to-dir-locals-keeps-malformed-file ()
  "Saving a preset must not overwrite malformed existing .dir-locals.el."
  (let* ((tmp-dir (make-temp-file "eglot-multi-preset-tests-" t))
         (dir-locals-file (expand-file-name ".dir-locals.el" tmp-dir))
         (broken-content "((python-mode . (\n"))
    (unwind-protect
        (progn
          (with-temp-file dir-locals-file
            (insert broken-content))
          (should-not
           (eglot-multi-preset--save-to-dir-locals
            '("rass" "python") nil tmp-dir 'python-mode))
          (should
           (equal (with-temp-buffer
                    (insert-file-contents dir-locals-file)
                    (buffer-string))
                  broken-content)))
      (ignore-errors (delete-directory tmp-dir t)))))

(ert-deftest eglot-multi-preset-save-to-dir-locals-preserves-existing-workspace-config ()
  "Saving a no-workspace preset keeps existing workspace config entries."
  (let* ((tmp-dir (make-temp-file "eglot-multi-preset-tests-" t))
         (dir-locals-file (expand-file-name ".dir-locals.el" tmp-dir))
         (workspace-config '(:eslint (:validate "off")))
         (initial-content
          `((python-mode
             . ((eglot-server-programs
                 . (((python-mode) . ("old-server"))))
                (eglot-workspace-configuration . ,workspace-config))))))
    (unwind-protect
        (progn
          (with-temp-file dir-locals-file
            (insert ";;; Directory Local Variables -*- no-byte-compile: t -*-\n")
            (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
            (pp initial-content (current-buffer)))
          (should
           (eglot-multi-preset--save-to-dir-locals
            '("rass" "python") nil tmp-dir 'python-mode))
          (let* ((saved (eglot-multi-preset-tests--read-form dir-locals-file))
                 (python-entry (assq 'python-mode saved))
                 (saved-workspace
                  (cdr (assq 'eglot-workspace-configuration (cdr python-entry)))))
            (should (equal saved-workspace workspace-config))))
      (ignore-errors (delete-directory tmp-dir t)))))

(ert-deftest eglot-multi-preset-refresh-eglot-args-falls-back-when-guess-missing ()
  "Interactive arg refresh should keep ARGS when contact guess is unavailable."
  (let ((args '(nil nil nil nil nil t)))
    (cl-letf (((symbol-function 'eglot-multi-preset--guess-contact)
               (lambda () nil)))
      (should (equal (eglot-multi-preset--refresh-eglot-args-if-interactive args)
                     args)))))

(ert-deftest eglot-multi-preset-refresh-eglot-args-rebuilds-interactive-contact ()
  "Interactive arg refresh should use guessed contact when available."
  (let ((args '(old-a old-b old-c old-d old-e t)))
    (cl-letf (((symbol-function 'eglot-multi-preset--guess-contact)
               (lambda () '("rass" "--" "server" "--stdio"))))
      (should (equal (eglot-multi-preset--refresh-eglot-args-if-interactive args)
                     '(old-a old-b old-c
                       ("rass" "--" "server" "--stdio")
                       old-e t))))))

(ert-deftest eglot-multi-preset-refresh-eglot-args-respects-dynamic-indexes ()
  "Interactive arg refresh should respect resolved eglot argument indexes."
  (let ((args '(a b c d e f g)))
    (cl-letf (((symbol-function 'eglot-multi-preset--eglot-contact-arg-index)
               (lambda () 1))
              ((symbol-function 'eglot-multi-preset--eglot-interactive-arg-index)
               (lambda () 6))
              ((symbol-function 'eglot-multi-preset--guess-contact)
               (lambda () '("rass" "python"))))
      (should (equal (eglot-multi-preset--refresh-eglot-args-if-interactive args)
                     '(a ("rass" "python") c d e f t))))))

(ert-deftest eglot-multi-preset-read-dir-locals-reports-parse-errors ()
  "Malformed .dir-locals content should emit an explanatory message."
  (let* ((tmp-dir (make-temp-file "eglot-multi-preset-tests-" t))
         (dir-locals-file (expand-file-name ".dir-locals.el" tmp-dir))
         (messages nil))
    (unwind-protect
        (progn
          (with-temp-file dir-locals-file
            (insert "((python-mode . (\n"))
          (let ((eglot-multi-preset-dir-locals-directory tmp-dir)
                (default-directory tmp-dir))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (should-not (eglot-multi-preset--read-dir-locals))
              (should (cl-some
                       (lambda (msg)
                         (string-match-p "Ignoring malformed .*\\.dir-locals\\.el" msg))
                       messages)))))
      (ignore-errors (delete-directory tmp-dir t)))))

(ert-deftest eglot-multi-preset-clear-dir-locals-errors-on-malformed-file ()
  "Clearing presets should fail loudly for malformed .dir-locals files."
  (let* ((tmp-dir (make-temp-file "eglot-multi-preset-tests-" t))
         (dir-locals-file (expand-file-name ".dir-locals.el" tmp-dir)))
    (unwind-protect
        (progn
          (with-temp-file dir-locals-file
            (insert "((python-mode . (\n"))
          (let ((eglot-multi-preset-dir-locals-directory tmp-dir)
                (default-directory tmp-dir)
                (major-mode 'python-mode))
            (should-error (eglot-multi-preset-clear-dir-locals)
                          :type 'user-error)))
      (ignore-errors (delete-directory tmp-dir t)))))

(provide 'eglot-multi-preset-tests)
;;; eglot-multi-preset-tests.el ends here
