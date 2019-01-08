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

(defcustom lbrowser-root-dir (file-name-directory
                              (or load-file-name (buffer-file-name)))
  "lbrowser-root-dir")

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

(defcustom contents-home
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

(defvar lbrowser-html-single-tags
  '(base link meta img br area param hr col option input wbr)
  "List of empty element tags.")

(defun lbrowser-encode-html (domsexp &optional doctype)
  "encode sexp to html"
  (concat
   (if doctype doctype "")
   (let* ((prop--fn) (encode-fn))
     (setq prop--fn
           (lambda (x)
             (format " %s=\"%s\"" (car x) (cdr x))))
     (setq encode-fn
           (lambda (dom)
             (if (listp dom)
                 (let* ((tag  (pop dom))
                        (prop (pop dom))
                        (rest dom)
                        (tagname (symbol-name tag)))
                   (if (memq tag html-parse-single-tags)
                       (format "%s\n"
                               (format "<%s%s>" tagname (mapconcat prop--fn prop "")))
                     (format "\n%s%s%s\n"
                             (format "<%s%s>" tagname (mapconcat prop--fn prop ""))
                             (mapconcat encode-fn rest "")
                             (format "</%s>" tagname))))
               dom)))
     (funcall encode-fn domsexp))))

(defun lbrowser-servlet-define ()
  "Serve define"
  (defservlet* leaf-browser/home "text/html" ()
    (insert (lbrowser-encode-html lbrowser-contents-home)))

  (defservlet* leaf-browser/imgs/:name "image/svg+xml" ()
    (insert-file (concat lbrowser-root-dir "imgs/" name)))

  (defservlet* leaf-browser/debug/:path "text/html" ()
    (insert (lbrowser-encode-html
             (with-current-buffer "*leaf-debug-sexp*"
               (read (buffer-string)))
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
