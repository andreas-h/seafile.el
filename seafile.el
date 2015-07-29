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
 (request
  (concat host_url "/api2/auth-token/")
  :type "POST"
  ; TODO read username and password from variables
  :data `(("username" . ,host_user)
 	 ("password" . ,host_pass))
 
  :parser 'buffer-string
  :success
  (function* (lambda (&key data &allow-other-keys)
               (when data
 		(let ((json-object-type 'plist))
 		  (setq auth_token (plist-get (json-read-from-string data) :token))))))
 
  :error
  (function* (lambda (&key error-thrown &allow-other-keys&rest _)
               (message "Got error: %S" error-thrown)))
 
  :complete (lambda (&rest _) (message "Finished!"))
  :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                 (418 . (lambda (&rest _) (message "Got 418."))))
 
  )


; ----------------------------------------------------------------------
; Get library id
; ----------------------------------------------------------------------
(setq auth_header (concat "Token " auth_token))

(request
 (concat host_url "/api2/repos/")
 :type "GET"
 :headers `(("Authorization" . ,auth_header)
	    ("Accept" . "application/json; indent=4"))

 :parser 'buffer-string
 :success
 (function* (lambda (&key data &allow-other-keys)
              (when data
		(let ((json-object-type 'plist))
		  (setq repo_list (mapcar 'identity (json-read-from-string data)))
		  (dolist (repo  repo_list)
		    (message (plist-get repo :name))
		    (when (equal library_name (plist-get repo :name))
		      (setq library_id (plist-get repo :id)))
		    )
		  )
		)
	      )
	    )

 :error
 (function* (lambda (&key error-thrown &allow-other-keys&rest _)
              (message "Got error: %S" error-thrown)))

 :complete (lambda (&rest _) (message "Finished (get repos)!"))
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
                (520 . (lambda (&rest _) (message "Got 520 (OPERATION_FAILED)."))))

 )


; ----------------------------------------------------------------------
; Check if directory for today exists
; ----------------------------------------------------------------------
; curl -H "Authorization: Token f2210dacd9c6ccb8133606d94ff8e61d9b477fd" -H 'Accept: application/json; indent=4' https://cloud.seafile.com/api2/repos/99b758e6-91ab-4265-b705-925367374cf0/dir/?p=/
; (format-time-string "%Y-%m-%d") gives the current date
; Response is vector of plist; "type" == "dir", "name" == (format-time-string ...), "id" = ...


; ----------------------------------------------------------------------
; Create directory for today
; ----------------------------------------------------------------------
; curl -d  "operation=mkdir" -v  -H 'Authorization: Token f2210dacd9c6ccb8133606d94ff8e61d99b477fd' -H 'Accept: application/json; charset=utf-8; indent=4' https://cloud.seafile.com/api2/repos/dae8cecc-2359-4d33-aa42-01b7846c4b32/dir/?p=/foo
; /foo is the to-be-created directory


; ----------------------------------------------------------------------
; Check if file exists in directory for today
; ----------------------------------------------------------------------
; curl -H "Authorization: Token f2210dacd9c6ccb8133606d94ff8e61d9b477fd" -H 'Accept: application/json; indent=4' https://cloud.seafile.com/api2/repos/99b758e6-91ab-4265-b705-925367374cf0/dir/?p=/foo

; ----------------------------------------------------------------------
; Get upload link
; ----------------------------------------------------------------------
; curl -H "Authorization: Token f2210dacd9c6ccb8133606d94ff8e61d99b477fd" https://cloud.seafile.com/api2/repos/99b758e6-91ab-4265-b705-925367374cf0/upload-link/


; ----------------------------------------------------------------------
; Upload file
; ----------------------------------------------------------------------
; curl -H "Authorization: Token f2210dacd9c6ccb8133606d94ff8e61d99b477fd" -F file=@test.txt -F filename=test.txt -F parent_dir=/ http://cloud.seafile.com:8082/upload-api/ef881b22


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
