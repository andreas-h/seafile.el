;;; seafile.el --- Interact with Seafile server

;; Copyright(C) 2015 Andreas Hilboll

;; Author: Andreas Hilboll <andreas@hilboll.de>
;; URL: https://github.com/andreas-h/seafile.el
;; Version: not released
;; Package-Requires: ((request))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


; ======================================================================
; ----------------------------------------------------------------------
; Code
; ----------------------------------------------------------------------
; ======================================================================

 (setq request-log-level 'info)
 (setq request-message-level 'info)

; ----------------------------------------------------------------------
; Configuration options
; ----------------------------------------------------------------------
 (setq host_user "xxx@uni-bremen.de")
 (setq host_pass "XXX")  ; TODO read password from encrypted file
 (setq host_proto "https")
 (setq host_name "seafile.zfn.uni-bremen.de")
 (setq library_name "Attachments")
 (setq seafile_key "")

; ----------------------------------------------------------------------
; Parameter preparation
; ----------------------------------------------------------------------
 (setq host_url (concat host_proto "://" host_name))


; ----------------------------------------------------------------------
; Load needed packages
; ----------------------------------------------------------------------
 (require 'request)

; ----------------------------------------------------------------------
; Get authentication token via API
; ----------------------------------------------------------------------

 (defun seafile/get-auth-token (host user pass)
   "Get authentication token from Seafile instance at HOST for USER with PASS"
   (progn
     (request
      (concat host "/api2/auth-token/")
      :type "POST"
      :data `(("username" . ,user)
	      ("password" . ,pass))
      :parser 'buffer-string
      :sync t
      :success (function*
		(lambda (&key data &allow-other-keys)
		  (when data
		    (let ((json-object-type 'plist))
		      (setq seafile-auth-token
			    (plist-get (json-read-from-string data) :token)))))))
     seafile-auth-token))

(seafile/get-auth-token host_url host_user host_pass)


; ----------------------------------------------------------------------
; Get library id
; ----------------------------------------------------------------------
(defun seafile/get-library-id (library host user pass)
  "Get the id of LIBRARY from Seafile HOST using USER and PASS"
  (progn
    (setq library_id nil)
    (let ((auth_header (concat "Token " (seafile/get-auth-token host user pass)))
	  (json-object-type 'plist))
      (request
       (concat host_url "/api2/repos/")
       :type "GET"
       :headers `(("Authorization" . ,auth_header)
      	      ("Accept" . "application/json; indent=4"))
       :parser 'buffer-string
       :sync t
       :success (function*
		 (lambda (&key data &allow-other-keys)
		   (when data
		     (let ((json-object-type 'plist))
		       (setq repo_list (mapcar 'identity (json-read-from-string data)))
		       (dolist (repo repo_list)
			 (when (equal library (plist-get repo :name))
			   (setq library_id (plist-get repo :id))))))))))
    library_id))

(setq my-attachments-library-id (seafile/get-library-id "Attachments" host_url host_user host_pass))

;     :status-code '((200 . (lambda (&rest _) (message "Got 200 (OK).")))
;	             (201 . (lambda (&rest _) (message "Got 201 (CREATED).")))
;                    (202 . (lambda (&rest _) (message "Got 202 (ACCEPTED).")))
;                    (301 . (lambda (&rest _) (message "Got 301 (MOVED_PERMANENTLY).")))
;                    (400 . (lambda (&rest _) (message "Got 400 (BAD_request).")))
;                    (403 . (lambda (&rest _) (message "Got 403 (FORBIDDEN).")))
;                    (404 . (lambda (&rest _) (message "Got 404 (NOT_FOUND).")))
;                    (409 . (lambda (&rest _) (message "Got 409 (CONFLICT).")))
;                    (429 . (lambda (&rest _) (message "Got 429 (TOO_MANY_REQUESTS).")))
;                    (440 . (lambda (&rest _) (message "Got 440 (REPO_PASSWD_REQUIRED).")))
;                    (441 . (lambda (&rest _) (message "Got 441 (REPO_PASSWD_MAGIC_REQUIRED).")))
;                    (500 . (lambda (&rest _) (message "Got 500 (INTERNAL_SERVER_ERROR).")))
;                    (520 . (lambda (&rest _) (message "Got 520 (OPERATION_FAILED).")))))

; ----------------------------------------------------------------------
; Check if directory for today exists
; ----------------------------------------------------------------------
(defun seafile/lsdir (path library_id host user pass)
  "List entries of directory PATH in LIBRARY_ID"
  (progn
    (setq dirlist nil)
    (let ((auth_header (concat "Token " (seafile/get-auth-token host user pass)))
	  (json-object-type 'plist))
      (request
       (concat host_url "/api2/repos/" library_id "/dir/?p=" path)  ; TODO check leading slash
       :type "GET"
       :headers `(("Authorization" . ,auth_header)
		  ("Accept" . "application/json; indent=4"))
       :parser 'buffer-string
       :sync t
       :success (function*
		 (lambda (&key data &allow-other-keys)
		   (when data
		     (setq dirlist (mapcar 'identity (json-read-from-string data))))))
       :error
       (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                    (message "Got error: %S" error-thrown)))
       :complete (lambda (&rest _) (message "Finished (get daydirs)!"))
       :status-code '((200 . (lambda (&rest _) (message "Got 200 (OK).")))
                      (201 . (lambda (&rest _) (message "Got 201 (CREATED).")))
                      (202 . (lambda (&rest _) (message "Got 202 (ACCEPTED).")))
                      (301 . (lambda (&rest _) (message "Got 301 (MOVED_PERMANENTLY).")))
                      (400 . (lambda (&rest _) (message "Got 400 (BAD_request).")))
                      (403 . (lambda (&rest _) (message "Got 403 (FORBIDDEN).")))
                      (404 . (lambda (&rest _) (message "Got 404 (NOT_FOUND).")))
                      (409 . (lambda (&rest _) (message "Got 409 (CONFLICT).")))
                      (429 . (lambda (&rest _) (message "Got 429 (TOO_MANY_REQUESTS).")))
                      (440 . (lambda (&rest _) (message "Got 440 (REPO_PASSWD_REQUIRED).")))
                      (441 . (lambda (&rest _) (message "Got 441 (REPO_PASSWD_MAGIC_REQUIRED).")))
                      (500 . (lambda (&rest _) (message "Got 500 (INTERNAL_SERVER_ERROR).")))
                      (520 . (lambda (&rest _) (message "Got 520 (OPERATION_FAILED)."))))))
       dirlist))

(defun seafile/check-if-directory-exists (target library_id host user pass)
  ; TODO: allow for directories not in '/'
  (progn
    (setq found_directory nil)
    (setq days_list (seafile/lsdir "/" library_id host user pass))
    (dolist (day days_list)
      (when (and (equal target (plist-get day :name))
		 (equal "dir" (plist-get day :type)))
	(setq found_directory target)))
    found_directory))

(setq my-today-directory-path (seafile/check-if-directory-exists (format-time-string "%Y-%m-%d") my-attachments-library-id host_url host_user host_pass))

(defun filelink/create-daydir (folder library_id host user pass)
  ()
  )


; ----------------------------------------------------------------------
; Create directory for today
; ----------------------------------------------------------------------
(defun seafile/mkdir (target library_id host user pass)
  "Creates directory TARGET in LIBRARY_ID"
  (progn
    (setq found_directory nil)
    (let ((auth_header (concat "Token " (seafile/get-auth-token host user pass)))
	  (json-object-type 'plist))
      (request
       (concat host_url "/api2/repos/" library_id "/dir/?p=" target)  ; TODO check for leading slash
       :type "POST"
       :headers `(("Authorization" . ,auth_header)
		  ("Accept" . "application/json; indent=4"))
       :data `(("operation" . "mkdir"))
       :parser 'buffer-string
       :sync t
       :success (function*
		 (lambda (&key response &allow-other-keys)
		   (setq created_directory (request-response-header response "Location"))))
       :error
       (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                    (message "Got error: %S" error-thrown)))
       :complete (lambda (&rest _) (message "Finished (get mkdir)!"))
       :status-code '((200 . (lambda (&rest _) (message "Got 200 (OK).")))
                      (201 . (lambda (&rest _) (message "Got 201 (CREATED).")))
                      (202 . (lambda (&rest _) (message "Got 202 (ACCEPTED).")))
                      (301 . (lambda (&rest _) (message "Got 301 (MOVED_PERMANENTLY).")))
                      (400 . (lambda (&rest _) (message "Got 400 (BAD_request).")))
                      (403 . (lambda (&rest _) (message "Got 403 (FORBIDDEN).")))
                      (404 . (lambda (&rest _) (message "Got 404 (NOT_FOUND).")))
                      (409 . (lambda (&rest _) (message "Got 409 (CONFLICT).")))
                      (429 . (lambda (&rest _) (message "Got 429 (TOO_MANY_REQUESTS).")))
                      (440 . (lambda (&rest _) (message "Got 440 (REPO_PASSWD_REQUIRED).")))
                      (441 . (lambda (&rest _) (message "Got 441 (REPO_PASSWD_MAGIC_REQUIRED).")))
                      (500 . (lambda (&rest _) (message "Got 500 (INTERNAL_SERVER_ERROR).")))
                      (520 . (lambda (&rest _) (message "Got 520 (OPERATION_FAILED)."))))))
       created_directory))

(unless my-today-directory-path
  (setq my-today-directory-path (seafile/mkdir (format-time-string "/%Y-%m-%d") my-attachments-library-id host_url host_user host_pass)))

; ----------------------------------------------------------------------
; Check if file exists in directory for today
; ----------------------------------------------------------------------
(defun seafile/check-if-file-exists (name directory library_id host user pass)
  (progn
    (setq found_file nil)
    (setq dir_list (seafile/lsdir directory library_id host user pass))
    (dolist (file dir_list)
      (when (and (equal name (plist-get file :name))
		 (equal "file" (plist-get file :type)))
	(setq found_file name)))
    found_file))

(seafile/check-if-file-exists "asdf" (format-time-string "/%Y-%m-%d") my-attachments-library-id host_url host_user host_pass)

; ----------------------------------------------------------------------
; Get upload link
; ----------------------------------------------------------------------
; curl -H "Authorization: Token f2210dacd9c6ccb8133606d94ff8e61d99b477fd" https://cloud.seafile.com/api2/repos/99b758e6-91ab-4265-b705-925367374cf0/upload-link/
; "http://cloud.seafile.com:8082/upload-api/ef881b22"


; ----------------------------------------------------------------------
; Upload file
; ----------------------------------------------------------------------
; curl -H "Authorization: Token f2210dacd9c6ccb8133606d94ff8e61d99b477fd" -F file=@test.txt -F filename=test.txt -F parent_dir=/ http://cloud.seafile.com:8082/upload-api/ef881b22
; "adc83b19e793491b1c6ea0fd8b46cd9f32e592fc"


; ----------------------------------------------------------------------
; Create share link
; ----------------------------------------------------------------------
; curl -v  -X PUT -d "p=/foo.md" -H 'Authorization: Token f2210dacd9c6ccb8133606d94ff8e61d99b477fd' -H 'Accept: application/json; indent=4' https://cloud.seafile.com/api2/repos/afc3b694-7d4c-4b8a-86a4-89c9f3261b12/file/shared-link/
; curl -v  -X PUT -d "password=password&expire=6&p=/123/" -H 'Authorization: Token f2210dacd9c6ccb8133606d94ff8e61d99b477fd' -H 'Accept: application/json; indent=4' https://cloud.seafile.com/api2/repos/afc3b694-7d4c-4b8a-86a4-89c9f3261b12/file/shared-link/

; Response (Location header contains link)
; ...
; < HTTP/1.0 201 CREATED
; < Location: https://cloud.seafile.com/f/9b437a7e55/
; ...

