;; desktop-autosave.el: An emacs package to automatically save desktop sessions
;; Copyright 2010 Victor Chudnovsky
;;
;; Author:  victor.chudnovsky+desktop-autosave@gmail.com
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Description:
;;
;; This emacs package will cause the emacs desktop session to be saved
;; to file periodically (when emacs autosaves) as well as whenever
;; significant events occur such as opening or saving a file.
;;
;; Usage:
;;  Starting desktop-autosave:
;;    M-x desktop-autosave-start RET <desktop name>
;;  Stopping desktop-autosave:
;;    M-x desktop-autosave-stop
;;  Showing previously-saved desktops:
;;    M-x desktop-autosave-show
;;  Deleting a previously-saved desktop:
;;    M-x desktop-autosave-delete-desktop RET <desktop name> RET <confirm> RET
;;
;; Main e-lisp functions:
;;  (desktop-autosave-start (&optional desktop-name force-proceed)
;;  (desktop-autosave-stop &optional save)
;;  (desktop-autosave-currently-saving)
;;  (desktop-autosave-show)
;;  (desktop-autosave-delete-desktop (&optional name)
;;
;; Installation:
;; You probably want to include this functionality in your ~/.emacs as follows:
;;  (setq load-path (cons "<path to this file>" load-path))
;;  (require 'desktop-autosave)
;;
;; You may also want to customize in your ~/.emacs what global
;; settings get saved with the desktop. See
;; http://www.xsteve.at/prg/emacs/power-user-tips.html for
;; suggestions.
;;
;; Configuration:
;;   EMACS_DESKTOP_REPOSITORY Environment variable that, if present and
;;   not empty, determines the directory where the desktops are saved. If
;;   not present, defaults to /home/$USER/.emacs.d/desktop-sessions
;;
;; The code below is based loosely on Joseph Brenner's "Desktop
;; Recover" package (http://www.emacswiki.org/emacs/DesktopRecover).
;;
;; Known issues:
;; * Saving on the desktop when exiting emacs leads to an infinite loop
;;   and is currently disabled. This should not affect the effectiveness
;;   of this package, since there are plenty of other places where the
;;   desktop is saved.
;;
;; Enhancements under consideration:
;; * Saving frame configurations along with the desktop

(provide 'desktop-autosave)
(require 'desktop)

;;; Configuration variables

(defvar desktop-autosave-desktop-repository
  (let ((repository-from-env (getenv "EMACS_DESKTOP_REPOSITORY")))
    (if (and repository-from-env
	     (> (length repository-from-env) 0))
	repository-from-env
      (format "/home/%s/.emacs.d/desktop-sessions" (user-login-name))))
  "Repository (directory) where the desktop session directories
  are saved")

(defvar desktop-autosave-directory-name nil
  "Directory for auto-saving the current desktop session")

(defvar desktop-autosave-save-period 1
  "Number of autosaves before an autosave-triggered desktop save occurs")

(defvar desktop-autosave-merge-desktop nil
  "Whether to merge the recovered desktop into the currently
  opened desktop (not needed if you start desktop-autosave
  before opening files)")

;;; Desktop name and name history

(defvar desktop-autosave-desktop-name "desktop-autosave"
  "Current desktop session name")

(defvar desktop-autosave-desktop-name-hist (list desktop-autosave-desktop-name)
  "Desktop session name history")

;; Debugging

(defvar desktop-autosave-debug-mode t
  "Debug mode for desktop-autosave")

(defun desktop-autosave-dbg (&rest args)
  "Prints args if desktop-autosave is in debug mode."
  (if desktop-autosave-debug-mode
      (apply 'message args)))

;;;  The various autosave hooks

(defun desktop-autosave-do-saves-automatically ()
  "Makes the desktop be saved automatically using the various event hooks."
  (message "Starting desktop-autosave for %s" desktop-autosave-directory-name)
  (setq desktop-autosave-auto-save-count 0)
  (add-hook 'auto-save-hook 'desktop-autosave-handle-auto-save)
  (add-hook 'after-save-hook 'desktop-autosave-save-desktop)
  (add-hook 'find-file-hook 'desktop-autosave-save-desktop)
  (add-hook 'kill-emacs-hook 'desktop-autosave-clean-up-for-exit)
  ; TODO(vchudnov): This causes an infinite loop:
  ;  (add-hook 'kill-buffer-hook 'desktop-autosave-handle-kill-file)
  (desktop-autosave-save-desktop)
  )

(defun desktop-autosave-stop-automatic-saves ()
  "Stops the desktop from being saved automatically via various event hooks."
  (message "Stopping desktop-autosave for %s" desktop-autosave-directory-name)
  (custom-set-variables '(desktop-save nil))
  (desktop-autosave-release-lock)
  (desktop-autosave-clear-location)
  (remove-hook 'auto-save-hook 'desktop-autosave-handle-auto-save)
  (remove-hook 'kill-emacs-hook 'desktop-autosave-clean-up-for-exit)
  (remove-hook 'after-save-hook 'desktop-autosave-save-desktop)
  (remove-hook 'find-file-hook 'desktop-autosave-save-desktop)
  ; TODO(vchudnov): This was never set because of the infinite loop issue:
  ;  (remove-hook 'kill-buffer-hook 'desktop-autosave-handle-kill-file)
  )

(defun desktop-autosave-save-desktop ()
  "Saves the desktop."
  (let ((location
	 (desktop-autosave-sync-params)))
    (desktop-autosave-release-lock)
    (desktop-remove)
    (desktop-save location)
    (custom-set-variables '(desktop-save t))
    (message "Saved desktop in %s" location)))

(defun desktop-autosave-clean-up-for-exit ()
  "For doing a 'clean' exit.
   Intended to be attached to the kill-emacs-hook.
   Saves the desktop and stops desktop-autosave."
  (desktop-autosave-stop nil))

(defun desktop-autosave-handle-auto-save ()
  "Takes the appropriate action if the auto-save-hook fires.
   Saves the desktop state if the auto-save counter exceeds the
   `desktop-autosave-save-period'."
  (cond ((>= desktop-autosave-auto-save-count desktop-autosave-save-period)
         (setq desktop-autosave-auto-save-count 1)
         (desktop-autosave-save-desktop))
        (t
         (setq desktop-autosave-auto-save-count (+ 1 desktop-autosave-auto-save-count)))))

; TODO(vchudnov): This leads to an infinite loop! Figure out why
(defun desktop-autosave-handle-kill-file ()
  "Updates the desktop when a buffer is killed."
  (desktop-autosave-dbg "kill file handler")
  (run-at-time "1 sec" nil 'desktop-autosave-save-desktop))


;;; Interfacing with the desktop package

(defun desktop-autosave-sync-params ()
  "Makes sure the desktop package knows about the desktop
   directory name for desktop-save. Returns this directory name."
  (let ((location
	 (file-name-as-directory (expand-file-name desktop-autosave-directory-name))))
    (setq desktop-dirname location)
    location))

(defun desktop-autosave-release-lock ()
  "Releases the desktop-lock, for cleanliness."
  (if (boundp 'desktop-release-lock)
      (desktop-release-lock)
    (progn
      (desktop-autosave-dbg "Symbol not bound: desktop-release-lock")
      (let ((file (desktop-full-lock-name desktop-autosave-directory-name)))
	(when (file-exists-p file) (delete-file file))))))

;;; Helper functions for managing desktop-autosave operation

(defun desktop-autosave-fixdir (location &optional root)
  "Fixes up the file directory LOCATION.
   Conditions directory paths for portability and robustness.
   If the directory does not yet exist, it will be created.\n
   Some examples (note that this always adds a trailing slash):
     '~/tmp'             =» '/home/doom/tmp/'
     '~/tmp/../bin/test' =» '/home/doom/bin/test/'\n
   Relative paths are converted to absolute, using the current
   `default-directory' setting, unless specified otherwise with the
   ROOT option. As a side-effect: this converts the empty string into
   `default-directory' or ROOT."
  (let ((location
         (substitute-in-file-name
          (convert-standard-filename
           (file-name-as-directory
            (expand-file-name location root))))))
    (unless (file-directory-p location)
      (make-directory location t)
      (message "Created directory: %s" location))
    location))

(defun desktop-autosave-currently-saving ()
  "If we're currently saving a desktop, returns the name of the
   desktop. Otherwise, returns nil. (This is done by returning
   desktop-autosave-directory-name, which must follow these
   semantics.)"
  (if desktop-autosave-directory-name
      (progn
	(desktop-autosave-dbg "Currently saving session %s in %s"
		 desktop-autosave-desktop-name
		 desktop-autosave-directory-name)
	desktop-autosave-desktop-name)
    (progn
      (desktop-autosave-dbg "Not currently saving")
      nil)))

(defun desktop-autosave-get-desktop-name (prompt initial)
  "Prompts for the name of a desktop using the given prompt and
   initial suggested value for the name. Applies history to the
   values entered here. Supplies autocompletion, prompting for
   confirmation if the name entered is not an autocompletion
   item."
  (completing-read prompt (desktop-autosave-get-session-names) nil
		   'confirm initial 'desktop-autosave-desktop-name-hist))

(defun desktop-autosave-get-directory-for-desktop (desktop-name)
  "Returns the desktop directory name for a desktop called desktop-name."
  (desktop-autosave-fixdir (concat (file-name-as-directory
				    desktop-autosave-desktop-repository)
				   desktop-name)))

(defun desktop-autosave-set-location (name)
  "Sets the location where the current desktop session is to be saved."
  (setq desktop-autosave-desktop-name name)
  (setq desktop-autosave-directory-name
	(desktop-autosave-get-directory-for-desktop name)))

(defun desktop-autosave-clear-location ()
  "Clears the location of the current desktop session, thus implicitly
   indicating that desktop saves are turned off."
  (setq desktop-autosave-desktop-name "")
  (setq desktop-autosave-directory-name nil))


(defun desktop-autosave-load-desktop (&optional force-proceed)
  "Loads a previously saved desktop, asking for confirmation
  unless force-proceed is true. When loading, clears the current
  desktop beforehand if desktop-autosave-merge-desktop is
  false. If there is no previous desktop to load, does
  nothing. Returns a bool indicating whether the whole
  desktop-autosave setup can continue (which is nil if the user
  chose to cancel a confirmation prompt)."
  (desktop-autosave-dbg "In desktop-autosave-load-desktop")
  (let ((desktop-exists
	 (file-exists-p (desktop-full-file-name desktop-autosave-directory-name)))
	(lock-exists
	 (file-exists-p (desktop-full-lock-name desktop-autosave-directory-name)))
	(proceed t))
    (if desktop-exists
	(if (or force-proceed
		(if lock-exists
		    (yes-or-no-p
		     "Desktop is locked, due to either being in use or a crashed session. Continue? ")
		  (y-or-n-p "Recover previously saved desktop? ")))
	    (progn
	      (if (not desktop-autosave-merge-desktop)
		  (progn
		    (message "Overwriting current desktop with new desktop %s"
			     desktop-autosave-directory-name)
		    (desktop-clear))
		(message "Merging current buffers into new desktop %s"
			 desktop-autosave-directory-name))  
	      (custom-set-variables '(desktop-load-locked-desktop t))
	      (desktop-read desktop-autosave-directory-name)
	      t)
	  nil)
      t)))

(defun desktop-autosave-get-session-names (&optional name-prefix)
  "Returns a list of all the desktops whose names start with name-prefix."
  (let ((name-match-expr (concat desktop-autosave-desktop-repository "/" name-prefix "*")))
    (condition-case nil
	(let* ((suffix  "/.emacs.desktop")
	       (all-desktops (file-expand-wildcards
			      (concat name-match-expr suffix)))
	       (prefix-length (+ 1 (length desktop-autosave-desktop-repository)))
	       (suffix-length (length suffix))
	       (result nil))
	  (dolist (desktop all-desktops result)
	    (setq result (cons (substring desktop prefix-length (- (length desktop) suffix-length))
			       result))))
      (error (progn
	       (message "No desktops found matching %s" name-match-expr)
	       nil)))))

;; Top-level user-facing functions

(defun desktop-autosave-show ()
  "Shows the desktops that are found on disk."
  (interactive)
  (message 
   (let ((result ""))
     (dolist (desktop (desktop-autosave-get-session-names) result)
       (setq result (concat result desktop " "))))))

(defun desktop-autosave-delete-desktop (&optional name)
  "Deletes the desktop directory <name>. If <name> is nil or not
specified, asks for the desktop directory name. Never allows the
current desktop session to be deleted. Always asks for
confirmation before deletion. Returns t if a deletion was
performed, or nil otherwise."
  (interactive)
  (if (not name)
      (setq name
	    (completing-read "Desktop to delete: " (desktop-autosave-get-session-names))))
  (if (string= name desktop-autosave-desktop-name)
      (progn
	(message "Cannot delete the current desktop-autosave session. Please use desktop-autosave-stop first.")
	nil)
    (let ((full-desktop-path (desktop-autosave-get-directory-for-desktop name)))
      (if (yes-or-no-p (format "Really delete desktop directory %s? " full-desktop-path))
	  (condition-case nil
	      (progn
		(delete-directory full-desktop-path t)
		(message "Deleted %s" full-desktop-path)
		t)
	    (error (progn
		     (message "Error: could not delete %s" full-desktop-path)
		     nil)))
	nil))))
	  

(defun desktop-autosave-start (&optional desktop-name force-proceed)
  "Starts desktop-autosave. If desktop-name is not provided, it is requested.

   If force-proceed is false or not set: does not do anything if
   we are already autosaving a desktop by the name. If we're not,
   and a previously autosaved desktop with this name is found,
   prompts as to whether to retrieve that saved desktop.

   If force-proceed is true: always stops any current
   desktop-autosave and starts autosaving desktop-name. If a
   previously autosaved desktop with desktop-name is found, it is
   automatically loaded."
  (interactive)

  (unless desktop-name
    (setq desktop-name
	  (desktop-autosave-get-desktop-name "Name of this desktop: "
					     desktop-autosave-desktop-name)))
  (if (or force-proceed
	  (not (string= (desktop-autosave-currently-saving)
			desktop-name)))
	  (progn
	    (desktop-autosave-stop)
	    (desktop-autosave-set-location  desktop-name)
	    (desktop-autosave-dbg "Autosave location: %s" desktop-autosave-directory-name)
	    (if (desktop-autosave-load-desktop force-proceed)
		(desktop-autosave-do-saves-automatically)))))

(defun desktop-autosave-stop (&optional dont-save)
  "Stops desktop-autosave. If desktop-autosave is not on, this is
a no-op. Otherwise, saves the desktop one last time unless
dont-save is non-nil."
  (interactive)
  (if (desktop-autosave-currently-saving)
      (progn
	(if (not dont-save)
	    (desktop-autosave-save-desktop))
	(desktop-autosave-stop-automatic-saves))))


(prefer-coding-system 'utf-8)