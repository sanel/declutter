;;; -*- indent-tabs-mode: nil -*-
;;; declutter.el --- Read html content and paywall sites without clutter

;; Copyright (c) 2018 Sanel Zukan
;;
;; Author: Sanel Zukan <sanelz@gmail.com>
;; URL: http://www.github.com/sanel/declutter
;; Version: 0.2.0
;; Keywords: html, web browser 

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Allows reading paywall sites without clutter. Uses outline.com service for actual work.

;;; Installation:

;; Copy it to your load-path and run with:
;; M-: (require 'declutter)

;;; Usage:

;; M-x declutter

;;; Code:

(require 'json)
(require 'shr)

(defgroup declutter nil
  "Declutter web sites."
  :prefix "declutter-"
  :group 'applications)

(defcustom outline-api "https://outlineapi.com/v3/parse_article?source_url="
  "Outline service, used to get cleaned content."
  :type 'string
  :group 'declutter)

(defun declutter-fetch-json (url)
  "Tries to get json from given url."
  (with-temp-buffer
    (url-insert-file-contents url)
    (let ((json-false :false))
      (json-read))))

(defun declutter-get-html (url)
  "Construct properl url and call outline.com service. Expects json response
and retrieve html part from it."
  (let* ((full-url (concat outline-api (url-hexify-string url)))
         (response (declutter-fetch-json full-url)))
    (cdr
     (assoc 'html (assoc 'data response)))))

(defun declutter-url (url)
  "Calls (declutter-get-html) inside new buffer and parses it as html.
Creates temporary buffer just to get html, as (shr-render-buffer) will create
own dedicated *html* buffer with parsed content."
  (with-temp-buffer
    (let ((content (declutter-get-html url)))
      (if (not content)
          (message "Zero reply from outline.com. This usually means it wasn't able to render the article.")
          (progn
            (insert content)
            (shr-render-buffer (current-buffer)))))))

(defun declutter-get-url-under-point ()
  "Tries to figure out is there any url under point. Returns nil if none."
  (let ((url (get-text-property (point) 'shr-url)))
       (if url
           url
           (browse-url-url-at-point))))

(defun declutter-under-point ()
  "declutters url under point."
  (interactive)
  (let ((url (declutter-get-url-under-point)))
    (if url
        (declutter-url url)
        (message "No URL under point"))))

;;;###autoload
(defun declutter (url)
  "Reads url and declutter it, using outline.com service."
  (interactive
   (let* ((url    (declutter-get-url-under-point))
          (prompt (if url
                      (format "URL (default: %s): " url)
                      "URL: ")))
     (list
      (read-string prompt nil nil url))))
  (declutter-url url))

(provide 'declutter)

;;; declutter.el ends here
