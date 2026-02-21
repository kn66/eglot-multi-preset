;;; eglot-multi-preset-tests.el --- Tests for eglot-multi-preset -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Regression tests for mode activation/deactivation behavior and
;; contact executable validation.

;;; Code:

(require 'ert)
(require 'eglot-multi-preset)

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

(provide 'eglot-multi-preset-tests)
;;; eglot-multi-preset-tests.el ends here
