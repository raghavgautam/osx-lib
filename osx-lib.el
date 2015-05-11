;;; osx-lib.el --- Basic function for Apple/OSX.
;;
;; Copyright (C) 2015 OSX Lib authors
;;
;; Author: Raghav Kumar Gautam <raghav@apache.org>
;; Keywords: Apple, AppleScript, OSX, Finder, Emacs, Elisp, VPN
;;; Commentary:
;; Provides functions for:
;;   1. Running Apple Script / osascript
;;   2. Notification functions
;;   3. Copying to/from clipboard
;;   4. Show the current file in Finder. Works with dired.
;;   5. VPN Connect/Disconnect
;;
;;; Code:
;;running apple script

(require 'dired)

;;;###autoload
(defun osx-lib-run-osascript (script-content)
  "Run an SCRIPT-CONTENT as AppleScript/osascipt."
  (interactive "sContent of AppleScript/osascript:")
  (let  ((file (make-temp-file "osx-lib-" nil ".scpt")))
    (with-temp-buffer
      (insert script-content)
      (write-file file)
      (start-process "OsaScript" "*OsaScript*" "osascript" file))))

(defalias 'osx-lib-run-applescript 'osx-lib-run-osascript)

;;notification fuctions
;;;###autoload
(defun osx-lib-notify2 (title message)
  "Create a notification with title as TITLE and message as MESSAGE."
  (osx-lib-run-osascript
   (concat "display notification \""
	   message
	   "\" with title  \""
	   title
	   "\"")))

;;;###autoload
(defun osx-lib-notify3 (title subtitle message)
  "Create a notification with title as TITLE, subtitle as SUBTITLE and message as MESSAGE."
  (osx-lib-run-osascript
   (concat "display notification \""
	   message
	   "\" with title  \""
	   title
	   "\" subtitle \""
	   subtitle
	   "\"")))

;;clipboard functions
;;;###autoload
(defun osx-lib-copy-to-clipboard (text)
  "Copy the given TEXT to clipboard."
  (shell-command-to-string (concat "pbcopy < <(echo -n " (shell-quote-argument text) ")")))

(defun osx-lib-copy-from-clipboard ()
  "Get clipboard text."
  (shell-command-to-string "pbpaste"))

;;function to show file in finder
;;;###autoload
(defun osx-lib-reveal-in-finder-as (file)
  "Reveal the supplied file FILE in Finder.
This function runs the actual AppleScript."
  (let ((script (concat
		 "set thePath to POSIX file \"" (shell-quote-argument file) "\"\n"
		 "tell application \"Finder\"\n"
		 " set frontmost to true\n"
		 " reveal thePath \n"
		 "end tell\n")))
    (osx-lib-run-osascript script)))

;;;###autoload
(defun osx-lib-reveal-in-finder ()
  "Reveal the file associated with the current buffer in the OS X Finder.
In a dired buffer, it will open the current file."
  (interactive)
  (osx-lib-reveal-in-finder-as
   (or (buffer-file-name)
       (expand-file-name (or (dired-file-name-at-point) ".")))))

(defalias 'osx-lib-find-file-in-finder 'osx-lib-reveal-in-finder)

;;vpn connect/disconnect functions
;;;###autoload
(defun osx-lib-vpn-connect (vpn-name password)
  "Connect to vpn using given VPN-NAME and PASSWORD."
  (interactive "MPlease enter vpn-name:\nMPlease enter vpn password:")
  (let ((old-content (osx-lib-copy-from-clipboard)))
    (osx-lib-run-osascript
     (format
      "tell application \"System Events\"
        tell current location of network preferences
                set VPN to service \"%s\" -- your VPN name here
                if exists VPN then connect VPN
                repeat while (current configuration of VPN is not connected)
                    delay 1
                end repeat
        end tell
end tell" vpn-name))
    (osx-lib-copy-to-clipboard password)
    (osx-lib-notify2 "Please paste" "Password has been copied to clipboard")
    (sit-for 5)
    (osx-lib-copy-to-clipboard old-content)
    (osx-lib-notify2 "Clipboard restored" "")))

;;;###autoload
(defun osx-lib-vpn-disconnect (vpn-name)
  "Disconnect from VPN-NAME vpn."
  (interactive "MEnter the vpn that you want to connect to:")
  (osx-lib-run-osascript
   (format
"tell application \"System Events\"
        tell current location of network preferences
                set VPN to service \"%s\" -- your VPN name here
                if exists VPN then disconnect VPN
                repeat while (current configuration of VPN is connected)
                    delay 1
                end repeat
        end tell
end tell" vpn-name))
  (osx-lib-notify2 "VPN Disconnected" ""))

(provide 'osx-lib)
;;; osx-lib ends here
