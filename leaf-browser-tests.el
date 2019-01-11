;;; ctmbrowser-tests.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords: custom

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'leaf-browser)
(require 'cort)

(setq httpd-show-backtrace-when-error t)
(defun httpd-error (proc status &optional info)
  "Send an error page appropriate for STATUS to the client,
optionally inserting object INFO into page. If PROC is T use the
`httpd-current-proc' as the process."
  (httpd-discard-buffer)
  (httpd-log `(error ,status ,info))
  (with-temp-buffer
    (let ((html (or (cdr (assq status httpd-html)) ""))
          (erro (url-insert-entities-in-string (format "error: %s\n"  info)))
          (bt   (format "backtrace: %s\n"
                        (with-temp-buffer
                          (let ((standard-output (current-buffer)))
                            (backtrace))
                          (buffer-string)))))
      (insert (format html (concat
                            (when info erro)
                            (when httpd-show-backtrace-when-error bt)))))
                            
    (httpd-send-header proc "text/html" status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Auto Refresh
;;

(defvar lbrowser-auto-refresh-timer nil)
(defvar lbrowser-auto-refresh-baffer "")
(defvar lbrowser-auto-refresh-prev-sexp-history nil)

(defservlet* leaf-browser/debug/:path "text/html" ()
  (message path)
  (insert (seml-decode-html
           (with-current-buffer lbrowser-auto-refresh-baffer
             (eval
              (read
               (buffer-substring-no-properties (point-min) (point-max)))))
           "<!DOCTYPE html>")))

(defun lbrowser-auto-refresh-start ()
  "auto refresh from buffer-string (without saving)"
  (interactive)
  (unless lbrowser-auto-refresh-timer
    (setq lbrowser-auto-refresh-baffer (buffer-name))
    (setq lbrowser-auto-refresh-timer
          (run-with-idle-timer 0.3 t 'lbrowser-auto-refresh-func))))

(defun lbrowser-auto-refresh-stop ()
  "auto refresh from buffer-string (without saving)"
  (interactive)
  (when lbrowser-auto-refresh-timer
    (setq lbrowser-auto-refresh-baffer "")
    (cancel-timer lbrowser-auto-refresh-timer)
    (setq lbrowser-auto-refresh-timer nil)))

(defun lbrowser-auto-refresh-func ()
  "auto refresh from buffer-string (without saving)"
  (let ((fn (lambda (x)
              (save-excursion
                (with-current-buffer (get-buffer-create "*leaf-auto-refresh*")
                  (goto-char (point-max))
                  (when (< 10 (line-number-at-pos))
                    (erase-buffer))
                  (insert x)))))
        (url) (sexp))
    (condition-case err
        (progn
          (setq sexp (eval
                      (read
                       (with-current-buffer lbrowser-auto-refresh-baffer
                         (buffer-substring-no-properties (point-min) (point-max))))))
          (setq url (replace-regexp-in-string
                     "\n" ""
                     (shell-command-to-string
                      (mapconcat 'identity
                                 '("osascript -e"
                                   "'tell application \"/Applications/Google Chrome.app\""
                                   "to URL of active tab of window 1'") " "))))

          (cond ((equal sexp lbrowser-auto-refresh-prev-sexp-history)
                 (funcall fn (format "%s, Nothing to change, Abort\n"
                                     lbrowser-auto-refresh-baffer)))
                ((string-match "localhost.*leaf-browser/debug" url)
                 (setq lbrowser-auto-refresh-prev-sexp-history
                       (eval (read
                              (with-current-buffer lbrowser-auto-refresh-baffer
                                (buffer-substring-no-properties (point-min) (point-max))))))

                 (shell-command-to-string
                  (mapconcat 'identity
                             '("osascript -e"
                               "'tell application \"/Applications/Google Chrome.app\""
                               "to reload active tab of window 1'") " "))

                 (setq lbrowser-auto-refresh-prev-sexp-history sexp)
                 (funcall fn (format "%s, Success.\n"
                                     lbrowser-auto-refresh-baffer)))
                (t (funcall fn (format "%s, URL is %s, Abort.\n"
                                       lbrowser-auto-refresh-baffer url)))))
      
      (error (funcall fn (format "%s, Cannot eval, Abort. (Err msg: %s)\n"
                                 lbrowser-auto-refresh-baffer err))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test settings
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  test definition
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  simple test
;;

(provide 'ctmbrowser-tests)
;;; ctmbrowser-tests.el ends here
