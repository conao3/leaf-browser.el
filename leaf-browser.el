;;; leaf-browser.el --- Web frontend of custom-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Naoya Yamashita <conao3@gmail.com>
;; Keywords: lisp settings
;; Version: 0.0.1
;; URL: https://github.com/conao3/leaf-browser.el
;; Package-Requires: ((emacs "24.3") (leaf "3.5.0") (simple-httpd "1.5.1") (seml-mode "1.5.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'custom)
(require 'cus-edit)

(require 'leaf)
(require 'simple-httpd)
(require 'seml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Declare
;;

;; seml-mode
(declare-function 'seml-decode-seml-from-file "seml-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Customizable variables
;;

(defgroup leaf-browser nil
  "Web frontend of custom-mode and generate `leaf' configuration."
  :group 'lisp)

(defconst lbrowser-version "0.0.1"
  "leaf-browser version")

(defcustom lbrowser-root-dir (file-name-directory
                              (or load-file-name (buffer-file-name)))
  "lbrowser-root-dir"
  :type 'directory
  :group 'leaf-browser)

(defcustom lbrowser-debugp nil
  "Show additional debug information in browser."
  :type 'boolean
  :group 'leaf-browser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Internal variables
;;

(defvar lbrowser-httpd-query nil
  "Last browser query data.  Refresh this variable every fetch seml.")

(defvar lbrowser-loaded-custom-group nil
  "Manate already loaded custom group.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Support functions
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Serve functions
;;

(defun lbrowser-define-servlet ()
  "Serve define"
  ;; declare lexical-binding variables
  (defvar type "")
  (defvar path "")

  ;; (mapc (lambda (x)
  ;;         (eval `(defvar ,x nil)))
  ;;       '(type var1 var2 targetpath targetfile debug))

  (defservlet* leaf-browser/:type "text/html" ()
    ;; httpd-query
    (setq lbrowser-httpd-query httpd-query)

    ;; serve data
    (insert (seml-decode-seml-from-file
             (expand-file-name "seml/home.seml" lbrowser-root-dir))))

  (defservlet* leaf-browser/sources/:path "text/html" ()
    (insert-file-contents (concat lbrowser-root-dir "sources/" path))

    ;; when insert file success, send header.
    ;; (when error occur, abort this function automatically.)
    (httpd-send-header t
                       (httpd-get-mime (file-name-extension path))
                       200)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main function
;;

(defun lbrowser-open ()
  "Open leaf-browser session."
  (interactive)
  (unless (httpd-running-p)
      (httpd-start))

  (lbrowser-define-servlet)

  (message "Open leaf-browser session."))

(defun lbrowser-close ()
  "Close leaf-browser session"
  (interactive)
  (when (httpd-running-p)
    (httpd-stop))
  (message "Close leaf-browser session."))

(provide 'leaf-browser)
;;; leaf-browser.el ends here
