;;; osx-essential.el --- Basic function for Apple/OSX. This functions for running apple script, notification, vpn etc.
;;
;; Copyright (C) 2015 OSX Essentials authors
;;
;; Author: Raghav Kumar Gautam <raghav@apach.org>
;; Keywords: Apple, AppleScript, OSX, Finder, Emacs, Elisp, VPN
;;; Commentary:
;; Provides osx functions for:
;;   1. Running Apple Script / osascript
;;   2. Notification functions
;;   3. Copying to/from clipboard
;;   4. Show the current file in Finder. Works with dired.
;;   5. VPN Connect/Disconnect
;;
;;; Code:
;;running apple script

(require 'dired)
(defun osx-run-osascript (script-content)
  "Run an SCRIPT-CONTENT as AppleScript/osascipt."
  (interactive "sContent of AppleScript/osascript:")
  (start-process "OsaScript" "*OsaScript*" "osascript" "-e" script-content))

(defalias 'osx-run-applescript 'osx-run-osascript)

;;notification fuctions
(defun osx-notify2 (title message)
  "Create a notification with title as TITLE and message as MESSAGE."
  (osx-run-osascript
   (concat "display notification \"" message "\" with title  \"" title "\"")))

(defun osx-notify3 (title subtitle message)
  "Create a notification with title as TITLE, subtitle as SUBTITLE and message as MESSAGE."
  (osx-run-osascript
   (concat "display notification \"" message "\" with title  \"" title "\"" " subtitle \"" subtitle "\"")))

;;clipboard functions
(defun osx-copy-to-clipboard (text)
  "Copy the given TEXT to clipboard."
  (shell-command-to-string (concat "pbcopy < <(echo -n " (shell-quote-argument text) ")")))

(defun osx-copy-from-clipboard ()
  "Get clipboard text."
  (shell-command-to-string "pbpaste"))

;;function to show file in finder
(defun osx-reveal-in-finder-as (file)
  "Reveal the supplied file FILE in Finder.
This function runs the actual AppleScript."
  (let* ((script (concat
		  "set thePath to POSIX file \"" file "\"\n"
		  "tell application \"Finder\"\n"
		  " set frontmost to true\n"
		  " reveal thePath \n"
		  "end tell\n")))
    (osx-run-osascript script)))

(defun osx-reveal-in-finder ()
  "Reveal the file associated with the current buffer in the OS X Finder.
In a dired buffer, it will open the current file."
  (interactive)
  (osx-reveal-in-finder-as
   (or (buffer-file-name)
       (expand-file-name (dired-file-name-at-point)))))

(defalias 'osx-find-file-in-finder 'osx-reveal-in-finder)

;;vpn connect/disconnect functions
(defun osx-vpn-connect (vpn-name password)
  "Connect to vpn using given VPN-NAME and PASSWORD."
  (interactive "MPlease enter vpn-name:\nMPlease enter vpn password:")
  (let ((old-content (osx-copy-from-clipboard)))
    (osx-run-osascript
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
    (osx-copy-to-clipboard password)
    (osx-notify2 "Please paste" "Password has been copied to clipboard")
    (sit-for 5)
    (osx-copy-to-clipboard old-content)
    (osx-notify2 "Clipboard restored" "")))

(defun osx-vpn-disconnect (vpn-name)
  "Disconnect from VPN-NAME vpn."
  (interactive "MEnter the vpn that you want to connect to:")
  (osx-run-osascript
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
  (osx-notify2 "VPN Disconnected" ""))

(provide 'osx-essential)
;;; osx-essential ends here
