;;; splash-screen.el --- An alternative splash screen -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Nicolas .P Rougier

;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; URL: https://github.com/rougier/emacs-splash
;; Keywords: startup
;; Version: 0.1
;; Package-Requires: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  An alternative splash screen:
;;
;;  +–––——————————––––––––––––––––––––––––––––———————————————————————+
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                         <www.gnu.org>                          |
;;  |                     GNU Emacs version XX.Y                     |
;;  |                      Type Ctrl-h for help                      |
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |           GNU Emacs comes with ABSOLUTELY NO WARRANTY          |
;;  |        Copyright (C) 2020 Free Software Foundation, Inc.       |
;;  +–––––––––––––––––––––––––––––––––––––––––———————————————————————+
;;
;; Features:
;;
;;  - No logo, not even in graphical mode.
;;  - Vertical and horizontal scroll bars and modeline are hidden
;;  - "C-h" shows the regular startup screen
;;  - " ", "x", "q", <esc>, <return> or <mouse-1> kills the splash screen
;;  - Splash screen is automatically killed after 3 seconds
;;  - With emacs-mac (Mituharu), splash screen is faded out after 3 seconds
;;
;; Note: The screen is not shown if there are opened file buffers. For
;;       example, if you start emacs with a filename on the command
;;       line, the splash is not shown.
;;
;; Usage:
;; 
;;  (require 'splash-screen)
;;
 
(require 'cl-lib)

;;; Code:
(defun splash-screen ()
  "Emacs splash screen"
  
  (interactive)
  (let* ((splash-buffer  (get-buffer-create "*splash*"))
         (height         (window-body-height nil))
         (width          (window-body-width nil))
         (padding-center (- (/ height 2) 2))
         (padding-bottom (- height (/ height 2) 3)))

    ;; If there are buffer associated with filenames,
    ;;  we don't show splash screen.
    (if (eq 0 (length (cl-loop for buf in (buffer-list)
                              if (buffer-file-name buf)
                              collect (buffer-file-name buf))))
        
        (with-current-buffer splash-buffer
          (erase-buffer)
          
          ;; Buffer local settings
          (setq mode-line-format nil)
          (setq cursor-type nil)
          (setq vertical-scroll-bar nil)
          (setq horizontal-scroll-bar nil)
          (setq fill-column width)
      
          ;; Vertical padding to center
          (insert-char ?\n padding-center)

          ;; Central text
          (insert-text-button "www.gnu.org"
                    'action (lambda (_) (browse-url "https://www.gnu.org"))
                    'follow-link t)
          (center-line) (insert "\n")
          (insert (concat (propertize "GNU Emacs"  'face 'bold)
                          " " "version "
                          (format "%d.%d"
                                  emacs-major-version emacs-minor-version)))
          (center-line) (insert "\n")
          (insert (propertize "Type Ctrl-h for help"
                              'face 'font-lock-comment-face))
          (center-line)

          ;; Vertical padding to bottom
          (insert-char ?\n padding-bottom)

          ;; Bottom text
          (insert (propertize "GNU Emacs comes with ABSOLUTELY NO WARRANTY"
                              'face 'font-lock-comment-face))
          (center-line) (insert "\n")
          (insert (propertize "Copyright (C) 2020 Free Software Foundation, Inc."
                          'face 'font-lock-comment-face))
          (center-line)
          (goto-char 0)

          (local-set-key " "               'splash-screen-kill)
          (local-set-key "x"               'splash-screen-kill)
          (local-set-key "q"               'splash-screen-kill)
          (local-set-key (kbd "<mouse-1>") 'splash-screen-kill)
          (local-set-key (kbd "<escape>")  'splash-screen-kill)
          (local-set-key (kbd "<return>")  'splash-screen-kill)
          (local-set-key (kbd "C-h")       'about-emacs)
          (read-only-mode t)
          (display-buffer-same-window splash-buffer nil)
          (run-with-idle-timer 3.0 nil 'splash-screen-fade)))))

;; Mac animation, only available from
;;  https://bitbucket.org/mituharu/emacs-mac/src/master/
;;  https://github.com/railwaycat/homebrew-emacsmacport
;; Could be replaced by dimmer.el for other distributions
;;  https://github.com/gonewest818/dimmer.el
(defvar mac-animation-duration 1.0)
(defvar mac-animation-locked-p nil)
(defun mac-animation-toggle-lock ()
  (setq mac-animation-locked-p (not mac-animation-locked-p)))
(defun mac-animation-frame-fade-out (&rest args)
  (unless mac-animation-locked-p
    (mac-animation-toggle-lock)
    (mac-start-animation nil :type 'fade-out :duration 1.0)
    (run-with-timer 1.0 nil 'mac-animation-toggle-lock)))
(defun splash-screen-fade ()
  "Kill the splash screen buffer (fade out)."
  (interactive)
  (if (get-buffer "*splash*")
      (progn (if (fboundp 'mac-start-animation)
                 (advice-add 'set-window-buffer
                             :before 'mac-animation-frame-fade-out))
             (kill-buffer "*splash*")
             (if (fboundp 'mac-start-animation)
                 (advice-remove 'set-window-buffer
                                'mac-animation-frame-fade-out)))))

(defun splash-screen-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (if (get-buffer "*splash*")
        (kill-buffer "*splash*")))

;; Suppress any startup message in the echo area
(run-with-idle-timer 0.05 nil (lambda() (message nil)))

;; Install hook after frame parameters have been applied
(if (and (not (member "-no-splash"  command-line-args))
         (not (member "--file"      command-line-args))
         (not (member "--insert"    command-line-args))
         (not (member "--find-file" command-line-args))
         (not inhibit-startup-screen))
    (progn
      (add-hook 'window-setup-hook 'splash-screen)
      (setq inhibit-startup-screen t 
            inhibit-startup-message t
            inhibit-startup-echo-area-message t)))

(provide 'splash-screen)
;;; splash-screen.el ends here

