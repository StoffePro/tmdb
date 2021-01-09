;;; tmdb.el --- querying The Movie Databse (TMDb)
;; Copyright (C) 2021 Gunnar Horrigmo

;; Author: Gunnar Horrigmo <gnunar@stoffe-pro.net>
;; Keywords: extensions, entertainment, vegetation, movies 

;; This file is not part of GNU Emacs.

;; tmdb.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; tmdb.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'url)
(require 'url-parse)
(require 'auth-source)
(require 'url-util)

(defvar tmdb-api-base-url "https://api.themoviedb.org/3/")

(defvar tmdb-auth-apiv4-key-username "tmdbelapiv4key"
  "Not really used except to search auth-source, and we apparently can't _not_ use it, so, there...")

(defun tmdb-make-url (path &optional query)
  (let ((url (url-generic-parse-url tmdb-api-base-url))
	(querystring (if query
			 (concat "?" (url-build-query-string query)))))
    (setf (url-filename url) (concat (url-filename url) path querystring))
    (url-recreate-url url)))

(defun tmdb-url-get (url &optional bearer)
  (let ((url-request-method "GET")
	(url-request-extra-headers
	 (list
	  (cons "Content-Type" "application/json;charset=utf-8")
	  (cons "Authorization"
		(concat "Bearer " (or bearer (tmdb-get-apiv4-key)))))))
    (url-retrieve-synchronously url)))

(defun tmdb-get-apiv4-key ()
  "Get TMDB API key using the auth-source-library."
  (let* ((auth-source-creation-prompts
	  '((secret . "Enter your TMDB API Read Access Token (v4 auth): ")))
	 (host (url-host (url-generic-parse-url tmdb-api-base-url)))
	 (port (url-port (url-generic-parse-url tmdb-api-base-url)))
	 (found (first (auth-source-search :max 1
					   :host host 
					   :port port
					   :require '(:secret)
					   :create t
					   :user tmdb-auth-apiv4-key-username)))
	 (secret (plist-get found :secret))
	 (key (if (functionp secret)
		  (funcall secret)
		secret))
	 (save-function (plist-get found :save-function))
	 (resbuf)
	 (resp 0))
    (if found
	(progn
	  (if (functionp save-function)
	      (unwind-protect
		  (condition-case err
		      (with-current-buffer
			  (tmdb-url-get tmdb-api-base-url key)
			(if (= (setq resp (url-http-parse-response)) 200)
			    (funcall save-function))
			(kill-buffer (current-buffer)))
		    (error (user-error "%s" (err cadr))))
		(unless (= resp 200)
		  (auth-source-forget+
		   '(:host host :port port tmdb-auth-apiv4-key-username)))))
	  key))))

(defvar tmdb-configuration
  (let ((buf (tmdb-url-get (tmdb-make-url "configuration")))
	(config))
    (unwind-protect
	(with-current-buffer buf
	  (if (= (url-http-parse-response) 200)
	      (progn
		(re-search-forward "\n\n")
		(setq config (json-parse-buffer)))))
      (kill-buffer buf))
    config))

(provide 'tmdb)

;;; tmdb.el ends here
