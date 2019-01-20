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

(defvar lbrowser-breadcrumbs nil
  "Manage breadcrubs on top nav bar.")

(defvar lbrowser-httpd-query nil
  "Last browser query data.  Refresh this variable every fetch seml.")

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
  (defvar path "")
  
  (defservlet* leaf-browser/:type/:var1/:var2 "text/html" (targetpath targetfile debug)
    (if (string= type "")
        (setq lbrowser-breadcrumbs nil)
      (if (member var1 lbrowser-breadcrumbs)
          (while (not (string= var1 (car lbrowser-breadcrumbs)))
            (pop lbrowser-breadcrumbs))
        (push var1 lbrowser-breadcrumbs)))
    (setq lbrowser-httpd-query httpd-query)
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
