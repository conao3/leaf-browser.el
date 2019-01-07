;;; leaf-browser.el --- Web frontend of custom-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao@conao-air.local>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: settings
;; Version: 0.0.1
;; URL: https://github.com/conao3/leaf-browser.el
;; Keywords: 
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'leaf)
(require 'simple-httpd)
(require 'htmlize)

(defgroup leaf-browser nil
  "Web frontend of custom-mode and generate `leaf' configuration."
  :group 'lisp)

(defconst lbrowser-version "0.0.1"
  "leaf-browser version")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Serve function
;;

(defun lbrowser-encode-html ()
  "encode sexp to html"
  (let ((fn (lambda (dom)
              (let ((tag  (pop dom))
                    (prop (pop dom))
                    (val  (if (= 1 (length dom)) (car dom) dom)))
                (if (memq tag html-parse-single-tags)
                    (format "%s\n"
                            (format "<%s%s>"
                                    tag
                                    (mapconcat (lambda (x)
                                                 (format " %s=\"%s\"" (car x) (cdr x)))
                                               prop "")))
                  (format "%s%s%s\n"
                          (format "<%s%s>"
                                  tag
                                  (mapconcat (lambda (x)
                                               (format " %s=\"%s\"" (car x) (cdr x)))
                                             prop ""))
                          (if (consp val) (mapconcat fn val "") val)
                          (format "</%s>" tag)))))))
    (funcall fn (with-current-buffer "*html*"
                  (libxml-parse-html-region (point-min) (point-max))))))

(defun lbrowser-servlet-home (path query req)
  "Generate page"
  (insert "home!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main function
;;

(defun lbrowser-open ()
  "Open leaf-browser session."
  (interactive)
  (unless (httpd-running-p)
    (httpd-start))

  (defservlet leaf-browser/home text/json (path query req)
    (lbrowser-servlet-home path query req))

  (message "Open leaf-browser session."))

(defun lbrowser-close ()
  "Close leaf-browser session"
  (interactive)
  (when (httpd-running-p)
    (httpd-stop))
  (message "Close leaf-browser session."))

(provide 'leaf-browser)
;;; leaf-browser.el ends here
