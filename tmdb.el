;;; tmdb.el --- querying The Movie Databse (TMDb)
;; Copyright (C) 2021 Gunnar Horrigmo

;; Author: Gunnar Horrigmo <gnunar@stoffe-pro.net>
;; Keywords: extensions

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

(defvar tmdb-api-base-url "https://api.themoviedb.org/3/")

(defvar tmdb-auth-token-username "tmdbeltoken"
  "Not really used except to search auth-source, and we apparently can't _not_ use it, so, there...")

(defun tmdb-get-api-key ()
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
					   :user tmdb-auth-token-username)))
	 (secret (plist-get found :secret))
	 (token (if (functionp secret)
		    (funcall secret)
		  secret))
	 (save-function (plist-get found :save-function))
	 (url-request-method "GET")
	 (url-request-extra-headers
	  (list
	   '("Content-Type" . "application/json;charset=utf-8")
	   (cons "Authorization" (concat "Bearer " token))))
	 (resbuf)
	 (authok))
    (unwind-protect
	(if (and found
		 (setq resbuf (url-retrieve-synchronously
			       tmdb-api-base-url nil t 2)))
	    (with-current-buffer resbuf
	      (if (= (url-http-parse-response) 200)
		  (progn
		    (setq authok t)
		    (if (functionp save-function)
			(funcall save-function))
		    token))))
      (unless authok
	(auth-source-forget+ '(:host host :port port tmdb-auth-token-username))))))
