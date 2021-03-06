;;; -*- indent-tabs-mode: nil -*-
;;; declutter.el --- Read html content and paywall sites without clutter

;; Copyright (c) 2019-2020 Sanel Zukan
;;
;; Author: Sanel Zukan <sanelz@gmail.com>
;; URL: http://www.github.com/sanel/declutter
;; Version: 0.4.0
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

;; Allows reading sites without clutter. Uses outline.com service or lynx for actual work.

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

(defcustom outline-api "https://api.outline.com/v3/parse_article?source_url="
  "Outline service, used to get cleaned content."
  :type 'string
  :group 'declutter)

(defcustom declutter-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/73.0.3683.75 Safari/537.36"
  "Custom user agent."
  :type 'string
  :group 'declutter)

(defcustom declutter-engine 'outline
  "Engine used to visit and render url. Values are 'outline for
using outline.com service, 'lynx for using local lynx installation or
'rdrview for https://github.com/eafer/rdrview."
  :type 'symbol
  :group 'declutter)

(defcustom declutter-engine-path nil
  "If lynx or rdrview was used as rendering engine, this will be
the path to the application binary. If set to nil but the engine is lynx or rdrview,
it will call them as is, assuming they are in PATH."
  :type 'string
  :group 'declutter)

(defun declutter-fetch-json (url)
  "Tries to get json from given url."
  (with-temp-buffer
    (let ((url-request-extra-headers '(("Referer" . "https://outline.com/")))
          (url-user-agent (concat "User-Agent: " declutter-user-agent "\r\n")))
      (url-insert-file-contents url)
      (let ((json-false :false))
        (json-read)))))

(defun declutter-get-html (url)
  "Construct properl url and call outline.com service. Expects json response
and retrieve html part from it."
  (let* ((full-url (concat outline-api (url-hexify-string url)))
         (response (declutter-fetch-json full-url)))
    (cdr
     (assoc 'html (assoc 'data response)))))

(defun declutter-render-content (content htmlp)
  "Assuming content is html string, render it in *declutter* buffer as html
or just display it, depending if htmlp was set to true."
  (with-current-buffer (pop-to-buffer "*declutter*")
    (save-excursion
      (erase-buffer)
      (insert content)
      ;; shr-render-buffer will start own *html* buffer, so use shr-render-region
      (when htmlp
        (shr-render-region (point-min) (point-max))))))

(defun declutter-url-outline (url)
  "Use Outline API to declutter url."
  (let ((content (declutter-get-html url)))
    (if (not content)
      (message "Zero reply from outline.com. This usually means it wasn't able to render the article.")
      (declutter-render-content content t))))

(defun declutter-url-lynx (url)
  "Use lynx to declutter url."
  (let* ((agent (if declutter-user-agent
                    (concat "-useragent=\"" declutter-user-agent "\"")))
         (path (or declutter-engine-path "lynx"))
         (content (shell-command-to-string (concat path agent " -dump " url))))
    (if (not content)
      (message "No content from lynx")
      (declutter-render-content content nil))))

(defun declutter-url-rdrview (url)
  "Use rdrview to declutter url."
  (let* ((path (or declutter-engine-path "rdrview"))
         (content (shell-command-to-string (concat path " -H " url))))
    (if (not content)
      (message "No content from rdrview")
      (declutter-render-content content t))))

(defun declutter-url (url)
  "Depending on declutter-engine variable, call appropriate declutter-url-* functions."
  (cond
   ((eq 'outline declutter-engine) (declutter-url-outline url))
   ((eq 'lynx declutter-engine) (declutter-url-lynx url))
   ((eq 'rdrview declutter-engine) (declutter-url-rdrview url))
   (t (message "Unknown decluttering engine. Use 'outline, 'lynx or 'rdrview."))))

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
