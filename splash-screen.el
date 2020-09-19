;; -------------------------------------------------------------------
;; A less intrusive splash screen
;; Copyright 2020 Nicolas P. Rougier
;; -------------------------------------------------------------------
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;;
;;  +–––——————————––––––––––––––––––––––––––––———————————————————————+
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                                                                |
;;  |                         <www.gnu.org>                          |
;;  |                     GNU Emacs version 27.1                     |
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
;;  Mockup for a less intrusive startup screen:
;;
;; - No logo (not even in graphical mode)
;; - Vertical and horizontal scroll bars are hidden
;; - Modeline is hidden
;; - " ", "x", "q", <esc> or <return> kills the startup buffer
;; - <mouse-1< kills the startup buffer
;; - "C-h" shows the regular splash screen
;; - Splash screen is automatically killed after 5 seconds

(defun splash-screen ()
  "Emacs splash screen"
  
  (interactive)
  (let* ((splash-buffer  (get-buffer-create " *GNU Emacs* "))
         (height         (window-body-height))
         (width          (window-body-width))
         (padding-center (- (/ height 2) 2))
         (padding-bottom (- height (/ height 2) 2)))
    
    (with-current-buffer splash-buffer
      (erase-buffer)
      (setq mode-line-format nil)
      (setq cursor-type nil)
      (setq vertical-scroll-bar nil)
      (setq horizontal-scroll-bar nil)
      (setq fill-column width)
      ;; (fringe-mode '(0 . 0)) 
      
      ;; Vertical padding to center
      (insert-char ?\n padding-center)

      ;; Central text
      (insert-text-button "www.gnu.org"
                    'action (lambda (x) (browse-url "https://www.gnu.org"))
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
      (run-with-idle-timer 5 nil 'splash-screen-kill)
      )))

(defun splash-screen-kill ()
  "Kill the splash screen buffer."
  (interactive)
  (if (get-buffer " *GNU Emacs* ")
      (kill-buffer " *GNU Emacs* ")))
