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
;;  M-x desktop-autosave-start RET <desktop name>
;;  M-x desktop-autosave-stop
;;
;; Main e-lisp functions:
;;  (desktop-autosave-start (&optional desktop-name force-proceed)
;;  (desktop-autosave-stop &optional save)
;;  (desktop-autosave-currently-saving)
;;
;; You may also want to customize in your ~/.emacs what global
;; settings get saved with the desktop. See
;; http://www.xsteve.at/prg/emacs/power-user-tips.html for
;; suggestions.
;;
;; The code below is based loosely on Joseph Brenner's "Desktop
;; Recover" package (http://www.emacswiki.org/emacs/DesktopRecover).

(provide 'desktop-autosave)
(require 'desktop)

;;; Configuration variables

; TODO: Make this location be read from an environment
; variable and then default to this if not set.
(defvar desktop-autosave-desktop-repository
  (format "/home/%s/.emacs.d/desktop-sessions" (user-login-name))
  "Directory where the desktop session directories are saved")

(defvar desktop-autosave-directory-name nil
  "Directory for auto-saving the current desktop session")

(defvar desktop-autosave-save-period 1
  "Number of autosaves before an autosave-triggered desktop save occurs")

(defvar desktop-autosave-merge-desktop nil
  "Whether to merge the recovered desktop into the currently
  opened desktop (not needed if you set up desktop-autosave before opening files)")



;;; Desktop name and name history

(defvar desktop-autosave-desktop-name "desktop-autosave"
  "Current desktop session name")

; TODO: Make this history work.
(defvar desktop-autosave-desktop-name-hist '(desktop-autosave-desktop-name)
  "Desktop session name history")


;;;  The various autosave hooks

(defun desktop-autosave-do-saves-automatically ()
  "Makes the desktop be saved automatically using the various event hooks."
  (message "Starting desktop-autosave for %s" desktop-autosave-directory-name)
  (setq desktop-autosave-auto-save-count 0)
  (add-hook 'auto-save-hook 'desktop-autosave-handle-auto-save)
  (add-hook 'after-save-hook 'desktop-autosave-save-desktop)
  (add-hook 'find-file-hook 'desktop-autosave-save-desktop)
  (add-hook 'kill-emacs-hook 'desktop-autosave-clean-up-for-exit)
  ; TODO: This causes an infinite loop:
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
  ; This was never set because of the infinite loop issue:
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
  (desktop-autosave-stop t))

(defun desktop-autosave-handle-auto-save ()
  "Takes the appropriate action if the auto-save-hook fires.
   Saves the desktop state if the auto-save counter exceeds the
   `desktop-autosave-save-period'."
  (cond ((>= desktop-autosave-auto-save-count desktop-autosave-save-period)
         (setq desktop-autosave-auto-save-count 1)
         (desktop-autosave-save-desktop))
        (t
         (setq desktop-autosave-auto-save-count (+ 1 desktop-autosave-auto-save-count)))))

; TODO: This leads to an infinite loop! Figure out why
(defun desktop-autosave-handle-kill-file ()
  "Updates the desktop when a buffer is killed.
  TODO: Currently, this leads to an infinite loop if
  involed as a handler. Figure out why."
  (message "kill file handler")
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
      (message "Symbol not bound: desktop-release-lock")
      (let ((file (desktop-full-lock-name desktop-autosave-directory-name)))
	(when (file-exists-p file) (delete-file file))))))

;;; Utilities for managing desktop-autosave operation

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
	(message "Currently saving session %s in %s"
		 desktop-autosave-desktop-name
		 desktop-autosave-directory-name)
	desktop-autosave-desktop-name)
    (progn
      (message "Not currently saving")
      nil)))

(defun desktop-autosave-get-desktop-name (prompt initial)
  "Prompts for the name of a desktop using the given prompt and
   initial suggested value for the name. Applies history to the
   values entered here."
  (read-string prompt initial desktop-autosave-desktop-name-hist initial nil))


(defun desktop-autosave-set-location (directory name)
  "Sets the location where the current desktop session is to be saved."
  (message "desktop-autosave-set-location: name=%s directory= %s"
	   name directory)
  (setq desktop-autosave-desktop-name name)
  (setq desktop-autosave-directory-name
	(desktop-autosave-fixdir (concat (file-name-as-directory directory)
					 name)))
 ; TODO: merge this with the other setq desktop-dirname
  (setq desktop-dirname desktop-autosave-directory-name))

(defun desktop-autosave-clear-location ()
  "Clears the location of the current desktop session, thus implicitly
   indicating that desktop saves are turned off."
  (setq desktop-autosave-directory-name nil))


(defun desktop-autosave-load-desktop (&optional force-proceed)
  "Loads a previously saved desktop, asking for confirmation
  unless force-proceed is true. When loading, clears the current
  desktop beforehand if desktop-autosave-merge-desktop is
  false. If there is no previous desktop to load, does
  nothing. Return a bool indicating whether the whole
  desktop-autosave setup can continue (which is nil if the user
  chose to cancel a confirmation prompt."
  (message "In desktop-autosave-load-desktop")
  (let ((desktop-exists
	 (file-exists-p (desktop-full-file-name desktop-autosave-directory-name)))
	(lock-exists
	 (file-exists-p (desktop-full-lock-name desktop-autosave-directory-name)))
	(proceed t))
    (if desktop-exists
	(if (or force-proceed
		(if lock-exists
		    (yes-or-no-p "Desktop is locked, due to either being in use or a crashed session. Continue? ")
		  (y-or-n-p "Recover previously saved desktop? ")))
	    (progn
	      (if (not desktop-autosave-merge-desktop)
		  (progn
		    (message "Overwriting current desktop with new desktop %s" desktop-autosave-directory-name)
		    (desktop-clear))
		(message "Merging current buffers into new desktop %s" desktop-autosave-directory-name))  
	      (custom-set-variables '(desktop-load-locked-desktop t))
	      (desktop-read desktop-autosave-directory-name)
	      t)
	  nil)
      t)))

(defun desktop-autosave-show ()
  "Shows the desktop repositories that are found on disk."
  (interactive)
  ; TODO: Handle the case where the directory whose name is
  ; in desktop-autosave-desktop-repository does not exist.
  (let ((alldesktops (directory-files desktop-autosave-desktop-repository nil  ".*/desktop\.el")))
    (dolist (desktop alldesktops nil)
      (message "Repository: %s" desktop))))

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
	    (if (desktop-autosave-currently-saving)
		(desktop-autosave-stop-automatic-saves))
	    (message "--Repository: %s\n--Name: %s" desktop-autosave-desktop-repository desktop-name)
	    (desktop-autosave-set-location  desktop-autosave-desktop-repository desktop-name)
	    (message "Autosave location: %s" desktop-autosave-directory-name)
	    (if (desktop-autosave-load-desktop force-proceed)
		(desktop-autosave-do-saves-automatically)))))

(defun desktop-autosave-stop (&optional save)
  "Stops desktop-autosave."
  (interactive)
  (if save
      (desktop-autosave-save-desktop))
  (desktop-autosave-stop-automatic-saves))


(prefer-coding-system 'utf-8)