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

(defservlet* leaf-browser/debug-/:path "text/html" ()
    (message path)
    (insert (seml-decode-html
             (with-temp-buffer
               (insert-file-contents
                (format "~/.emacs.d/site-lisp/leaf-browser.el/seml/%s.seml" path))
               (eval
                (read
                 (buffer-substring-no-properties (point-min) (point-max)))))
             "<!DOCTYPE html>")))

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
