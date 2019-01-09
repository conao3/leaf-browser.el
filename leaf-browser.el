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
(require 'seml-mode)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Page
;;

;; (mapcar (lambda (tag)
;;         (eval `(defmacro ,(intern (format "lbrowser-%s" tag)) ()
;;                  (declare (indent 1)))))
;;       (cdr '(:dammy-symbol
;;              html head body
;;              section nav article header footer
;;              div form input)))

(defvar lbrowser-contents-home
  '(html nil
     (head nil
       (link ((rel . "stylesheet") (href . "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css")))
       (meta ((charset . "utf-8")))
       (meta ((name . "viewport") (content . "width=device-width, initial-scale=1.0")))
       (title nil "home"))
     (style ((type . "text/css"))
            "body {color: #D7DAE0; background-color: #292D34;}")
     (body nil
       (div ((class . "container"))
         (div ((class . "center-align"))
           (img ((class . "responsive-img") (src . "/leaf-browser/imgs/splash.svg"))))
         (div ((class . "row"))
           (div ((class . "col s3"))
             (comment nil "Grey navigation panel"))
           (div ((class . "col s9"))
             (comment nil "Teal page content"))))
       (script ((src . "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js")))))
  "leaf-browser contesnts serve <leaf-browser/home>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Serve function
;;

(defun lbrowser-servlet-define ()
  "Serve define"
  (defservlet* leaf-browser/home "text/html" ()
    (insert (seml-decode-html lbrowser-contents-home)))

  (defservlet* leaf-browser/imgs/:name "image/svg+xml" ()
    (insert-file-contents (concat lbrowser-root-dir "imgs/" name)))

  (defservlet* leaf-browser/debug/:path "text/html" ()
    (message path)
    (insert (seml-decode-html
             (with-current-buffer "*leaf-debug-sexp*"
               (read (buffer-substring-no-properties (point-min) (point-max))))
             "<!DOCTYPE html>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main function
;;

(defun lbrowser-open ()
  "Open leaf-browser session."
  (interactive)
  (unless (httpd-running-p)
    (let ((httpd-port "8088")
          (httpd-root ))
      (httpd-start)))

  (lbrowser-servlet-define)

  (message "Open leaf-browser session."))

(defun lbrowser-close ()
  "Close leaf-browser session"
  (interactive)
  (when (httpd-running-p)
    (httpd-stop))
  (message "Close leaf-browser session."))

(provide 'leaf-browser)
;;; leaf-browser.el ends here
