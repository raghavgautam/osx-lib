;;; osx-lib.el --- Basic function for Apple/OSX.
;;
;; Copyright (C) 2015 OSX Lib authors
;;
;; Author: Raghav Kumar Gautam <raghav@apache.org>
;; Keywords: Apple, AppleScript, OSX, Finder, Emacs, Elisp, VPN, Speech
;; Package-Requires: ((emacs "24.4"))
;;; Commentary:
;; Provides functions for:
;;   1. Running Apple Script / osascript
;;   2. Play beep
;;      (setq ring-bell-function #'osx-lib-do-beep)
;;   3. Notification functions
;;      (osx-lib-notify2 "Emacs" "Text Editor")
;;   4. Copying to/from clipboard
;;   5. Show the current file in Finder. Works with dired.
;;   6. VPN Connect/Disconnect
;;      (defun work-vpn-connect ()
;;        "Connect to Work VPN."
;;        (interactive)
;;        (osx-lib-vpn-connect "WorkVPN" "VPN_Password"))
;;   7. Use speech
;;      (osx-lib-say "Emacs")
;;
;;; Code:
;;running apple script

(require 'dired)
(require 'subr-x)

(defcustom osx-lib-say-voice nil
  "Speech voice to use for osx-lib-say.  Nil/empty means default speech voice."
  :group 'osx-lib)

(defun osx-lib-escape (str)
  "Escape STR to make it suitable for using is applescripts."
  (replace-regexp-in-string "\"" "\\\\\"" str))

;;;###autoload
(defun osx-lib-run-osascript (script-content)
  "Run an SCRIPT-CONTENT as AppleScript/osascipt."
  (interactive "sContent of AppleScript/osascript:")
  (let  ((file (make-temp-file "osx-lib-" nil ".scpt")))
    (with-temp-file file
      (insert script-content)
      ;;delete the script after execution
      (insert "\ndo shell script \"rm -rf \" & the quoted form of POSIX path of (path to me)"))
    (osx-lib-run-file file)))

(defun osx-lib-run-file (file)
  "Run an AppleScript/osascipt FILE."
  (start-process "OsaScript" "*OsaScript*" "osascript" file))

;;;###autoload
(defun osx-lib-osx-version ()
  "Get OS version."
  (interactive)
  (string-trim (shell-command-to-string
		"sw_vers  -productVersion")))

(defun osx-lib-run-js-file (file)
  "Run an AppleScript/osascipt FILE."
  (start-process "OsaScript" "*OsaScript*" "osascript" "-l" "JavaScript" file))

;;;###autoload
(defun osx-lib-run-js (script-content)
  "Run an SCRIPT-CONTENT as JavaScript."
  (interactive "sContent of JavaScript AppleScript/osascript:")
  (let  ((file (make-temp-file "osx-lib-" nil ".js")))
    (with-temp-file file
      (insert script-content)
      ;;delete the script after execution
      ;;(insert "\ndo shell script \"rm -rf \" & the quoted form of POSIX path of (path to me)")
      )
    (osx-lib-run-js-file file)))

(defalias 'osx-lib-run-applescript 'osx-lib-run-osascript)

;;;###autoload
(defun osx-lib-do-beep ()
  "Play beep sound."
  (osx-lib-run-applescript "beep"))

;;notification fuctions
;;;###autoload
(defun osx-lib-notify2 (title message)
  "Create a notification with title as TITLE and message as MESSAGE."
  (osx-lib-run-osascript
   (concat "display notification \""
	   (osx-lib-escape message)
	   "\" with title  \""
	   (osx-lib-escape title)
	   "\"")))

;;;###autoload
(defun osx-lib-notify3 (title subtitle message)
  "Create a notification with title as TITLE, subtitle as SUBTITLE and message as MESSAGE."
  (osx-lib-run-osascript
   (concat "display notification \""
	   (osx-lib-escape message)
	   "\" with title  \""
	   (osx-lib-escape title)
	   "\" subtitle \""
	   (osx-lib-escape subtitle)
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
    (if (string-lessp (osx-lib-osx-version) "10.10")
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
end tell" (osx-lib-escape vpn-name)))
      (shell-command-to-string (shell-command-to-string (format "scutil --nc start \"%s\"" (shell-quote-argument vpn-name)))))
    (osx-lib-copy-to-clipboard password)
    (osx-lib-notify2 "Please paste" "Password has been copied to clipboard")
    (sit-for 5)
    (osx-lib-copy-to-clipboard old-content)
    (osx-lib-notify2 "Clipboard restored" "")))

;;;###autoload
(defun osx-lib-vpn-disconnect (vpn-name)
  "Disconnect from VPN-NAME vpn."
  (interactive "MEnter the vpn that you want to connect to:")
  (if (string-lessp (osx-lib-osx-version) "10.10")
      (osx-lib-run-osascript
       (format "
tell application \"System Events\"
        tell current location of network preferences
                set VPN to service \"%s\" -- your VPN name here
                if exists VPN then disconnect VPN
                repeat while (current configuration of VPN is connected)
                    delay 1
                end repeat
        end tell
end tell"
	       (osx-lib-escape vpn-name)))
    (shell-command-to-string (shell-command-to-string (format "scutil --nc stop \"%s\"" (shell-quote-argument vpn-name)))))
  (osx-lib-notify2 "VPN Disconnected" ""))

;;;###autoload
(defun osx-lib-say (message)
  "Speak the MESSAGE."
  (interactive "MEnter the name message: ")
  (osx-lib-run-osascript
   (format "
tell application \"System Events\"
	say \"%s\"%s
end tell
"
	   (osx-lib-escape message)
	   (if (and osx-lib-say-voice (stringp osx-lib-say-voice) (> (length (string-trim osx-lib-say-voice)) 1))
	       (format " using \"%s\"" osx-lib-say-voice)
	     ""))))

(defalias 'osx-lib-speak 'osx-lib-say)

(defun osx-open-url-at-point (url)
  "Open url at point using default browser."
  (interactive (list (read-from-minibuffer "Please enter the url: " (thing-at-point 'url))))
  (start-process "OsaScript" "*OsaScript*" "open" (eshell-escape-arg url)))

(provide 'osx-lib)
;;; osx-lib.el ends here
