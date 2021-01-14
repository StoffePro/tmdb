;;; tmdb.el --- querying The Movie Databse (TMDb)
;; Copyright (C) 2021 Gunnar Horrigmo

;; Author: Gunnar Horrigmo <gnunar@stoffe-pro.net>
;; Keywords: extensions, external, communication, entertainment, movies

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
(require 'url-http)
(require 'url-util)
(require 'url-parse)
(require 'auth-source)

(defvar tmdb-api-base-url "https://api.themoviedb.org/3/")

(defvar tmdb-auth-apiv4-key-username "tmdbelapiv4key"
  "Not really used except to search auth-source, and we apparently can't _not_ use it, so, there...")

(defgroup tmdb nil
  "Querying The Movie Databse (TMDb)"
  :link '(url-link "https://developers.themoviedb.org/")
  :group 'comm)

(defcustom tmdb-search-include-adult t
  "Whether to include adult titles in searches"
  :type '(choice (const :tag "no" nil)
		 (const :tag "yes" t))
  :group 'tmdb)

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
	(config)
	(confighash)
	(images))
    (unwind-protect
	(with-current-buffer buf
	  (when (= (url-http-parse-response) 200)
	    (re-search-forward "\n\n")
	    (setq confighash (json-parse-buffer))
	    (dolist (key '("base_url"
			   "secure_base_url"
			   "backdrop_sizes"
			   "logo_sizes"
			   "poster_sizes"
			   "profile_sizes"
			   "still_sizes"))
	      (setq images (plist-put images
				      (make-symbol (concat ":" key))
				      (gethash key (gethash "images" confighash)))))
	    (setq config (plist-put config :images images))
	    (setq config (plist-put config :change_keys (gethash "change_keys" confighash)))))
      (kill-buffer buf))
    config))

(defvar tmdb-api-path-search-movie "search/movie")
(defvar tmdb-search-more-results nil)

(defun tmdb-search-movie (query &optional page include-adult region year primary-release-year)
  (let ((params)
	(buf)
	(data))
    (loop for param in `(("query" ,query)
			 ("page" ,page)
			 ("include_adult" ,include-adult)
			 ("region" ,region)
			 ("year" ,year)
			 ("primary_release_year" ,primary-release-year))
	  do (if (second param)
		 (add-to-list 'params param)))
    (setq buf (tmdb-url-get (tmdb-make-url tmdb-api-path-search-movie params)))
    (unwind-protect
	(with-current-buffer buf
	  (when (= (url-http-parse-response) 200)
	    (re-search-forward "\n\n")
	    (setq data (json-parse-buffer))))
      (kill-buffer buf)
      (if (> (gethash "total_pages" data) (gethash "page" data))
	  (setq tmdb-search-more-results `(tmdb-search-movie . (,query ,(1+ (gethash "page" data)) ,include-adult ,region ,year ,primary-release-year)))
	(setq tmdb-search-more-results nil)))
    data))

(defun tmdb-search-get-more-results ()
  (when tmdb-search-more-results
    (apply (car tmdb-search-more-results) (cdr tmdb-search-more-results))))



(provide 'tmdb)

;;; tmdb.el ends here
