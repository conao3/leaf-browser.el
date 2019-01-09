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

(defvar lbrowser-contents
  (mapcar (lambda (path)
            (file-name-sans-extension
             (file-name-nondirectory path)))
          (file-expand-wildcards
           (concat lbrowser-root-dir "seml/*.sml"))))

(mapc (lambda (name)
        (eval
         `(defvar ,(intern (format "lbrowser-contents-%s" name))
            ',(read
               (with-temp-buffer
                 (insert-file-contents
                  (format "%sseml/%s.sml" lbrowser-root-dir name))
                 (buffer-substring-no-properties (point-min) (point-max)))))))
      lbrowser-contents)

;; for bytecompiler
(defvar lbrowser-contents-home)

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
             (with-temp-buffer
               (insert-buffer-substring "home.sml" (point-min) (point-max))
               (eval
                (read
                 (buffer-substring-no-properties (point-min) (point-max)))))
             "<!DOCTYPE html>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main function
;;

(defun lbrowser-open ()
  "Open leaf-browser session."
  (interactive)
  (unless (httpd-running-p)
      (httpd-start))

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
