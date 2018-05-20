;;; frame-purpose.el --- Purpose-specific frames that show only certain buffers

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: http://github.com/alphapapa/frame-purpose.el
;; Version: 1.0.0-pre
;; Package-Requires: ((emacs "25.1") (dash "2.12"))
;; Keywords: buffers, convenience, frames

;;; Commentary:

;; `frame-purpose' makes it easy to open purpose-specific frames that only show certain buffers,
;; e.g. by buffers' major mode, their filename or directory, etc, with custom frame/X-window
;; titles, icons, and other frame parameters.

;; Note that this does not lock buffers to certain frames, and it does not prevent frames from
;; showing any buffer.  It works by overriding the `buffer-list' function so that, within a
;; purpose-specific frame, any code that uses `buffer-list' (e.g. `list-buffers', `ibuffer', Helm
;; buffer-related commands) will only show buffers for that frame's purpose.  You could say that,
;; within a purpose-specific frame, you would have to "go out of your way" to show a buffer that's
;; not related to that frame's purpose.

;;;; Usage:

;; First, enable `frame-purpose-mode'.  This overrides `buffer-list' with a function that acts
;; appropriately on purpose-specific frames.  When the mode is disabled, `buffer-list' is returned
;; to its original definition, and purpose-specific frames will no longer act specially.

;; There are some built-in interactive commands:

;; + `frame-purpose-make-directory-frame': Make a purpose-specific frame for buffers associated with
;; DIRECTORY.  DIRECTORY defaults to the current buffer's directory.

;; + `frame-purpose-make-mode-frame': Make a purpose-specific frame for buffers in major MODE.  MODE
;; defaults to the current buffer's major mode.

;;  + `frame-purpose-show-sidebar': Show list of purpose-specific buffers on SIDE of this frame.
;;  When a buffer in the list is selected, the last-used window switches to that buffer.  Makes a
;;  new buffer if necessary.  SIDE is a symbol, one of left, right, top, or bottom.

;; For more fine-grained control, or for scripted use, the primary function is
;; `frame-purpose-make-frame'.  For example:

;; Make a frame to show only Org-mode buffers, with a custom frame (X window) title and icon:
;; (frame-purpose-make-frame :modes 'org-mode
;;                           :title "Org"
;;                           :icon-type "~/src/emacs/org-mode/logo.png")

;; Make a frame to show only `matrix-client' buffers, with a custom frame (X window) title and icon,
;; and showing a list of buffers in a sidebar window on the right side of the frame:
;; (frame-purpose-make-frame :modes 'matrix-client-mode
;;                           :title "Matrix"
;;                           :icon-type (expand-file-name "~/src/emacs/matrix-client-el/logo.png")
;;                           :sidebar 'right)

;;; License:

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

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'seq)

(require 'dash)

;;;; Customization

(defgroup frame-purpose nil
  "Group for `frame-purpose'."
  :link '(url-link "http://github.com/alphapapa/frame-purpose.el"))

(defcustom frame-purpose--initial-buffer-fn #'frame-purpose--initial-buffer
  "Called to choose the buffer to show in new frames."
  :type 'function)

;;;; Commands

(defun frame-purpose-make-directory-frame (&optional directory)
  "Make a purpose-specific frame for buffers associated with DIRECTORY.
DIRECTORY defaults to the current buffer's directory."
  (interactive (list (file-name-directory (buffer-file-name))))
  (frame-purpose-make-frame :filenames directory
                            :title (file-name-nondirectory (directory-file-name directory))))

(defun frame-purpose-make-mode-frame (&optional mode)
  "Make a purpose-specific frame for buffers in major MODE.
MODE defaults to the current buffer's major mode."
  (interactive (list major-mode))
  (frame-purpose-make-frame :modes mode
                            :title (symbol-name mode)))

;;;; Functions

(cl-defun frame-purpose-make-frame (&rest args)
  "Make a new, purpose-specific frame.
The new frame will only display buffers with the specified
attributes.

ARGS is a plist of keyword-value pairs, including:

`:modes': One or a list of major modes, matched against buffers'
major modes.

`:filenames': One or a list of strings, matched as regular
expressions against buffers' filenames.

`:buffer-predicate': A function which takes a single argument, a
buffer, and returns non-nil when the buffer matches the frame's
purpose.  When set, `:modes' and `:filenames' must not also be
set.

Remaining keywords are transformed to non-keyword symbols and
passed as frame parameters to `make-frame', which see."
  (unless frame-purpose-mode
    (user-error "Enable `frame-purpose-mode' first"))
  ;; Process args
  (let* ((parameters (cl-loop for (keyword value) on args by #'cddr
                              for symbol = (intern (substring (symbol-name keyword) 1))
                              collect (cons symbol value)))
         (modes (when-let (modes (map-elt parameters 'modes))
                  (cl-typecase modes
                    (symbol (list modes))
                    (list modes))))
         (filenames (when-let (filenames (map-elt parameters 'filenames))
                      (cl-typecase filenames
                        (string (list filenames))
                        (list filenames)))))
    ;; Validate args
    (unless (or modes filenames (map-elt parameters 'buffer-predicate))
      (user-error "One of `:modes', `:filenames', or `:buffer-predicate' must be set"))
    (when (and (map-elt parameters 'buffer-predicate)
               (or modes filenames))
      (user-error "When buffer-predicate is set, modes and filenames must be unspecified"))
    ;; Make predicate
    (map-put parameters 'buffer-predicate
             (byte-compile (or (map-elt parameters 'buffer-predicate)
                               `(lambda (buffer)
                                  (with-current-buffer buffer
                                    (or ,(when modes
                                           `(member major-mode ',modes))
                                        ,(when filenames
                                           `(cl-loop for filename in ',filenames
                                                     when (buffer-file-name)
                                                     thereis (string-match filename (buffer-file-name))))))))))
    ;; Make frame
    (with-selected-frame (make-frame parameters)
      (funcall frame-purpose--initial-buffer-fn)
      (when (frame-parameter nil 'sidebar)
        (frame-purpose-show-sidebar (frame-parameter nil 'sidebar)))
      (selected-frame))))

(defun frame-purpose--buffer-list (&optional frame)
  "Return list of buffers.  When FRAME has a buffer-predicate, only return frames passing it."
  ;; NOTE: Overriding `buffer-list' may have unexpected consequences because of interactions with
  ;; other code that calls it and may expect to receive an unfiltered list of buffers.  But...so
  ;; far, it seems okay...
  (cl-flet ((buffer-list (&optional frame)
                         (frame-purpose--buffer-list-original frame)))
    (if-let ((pred (frame-parameter frame 'buffer-predicate)))
        (cl-loop for buffer in (buffer-list frame)
                 when (funcall pred buffer)
                 collect buffer)
      (buffer-list frame))))

(defun frame-purpose--initial-buffer (&optional frame)
  "Switch to the first valid buffer in this frame."
  (when-let ((buffer (car (buffer-list frame))))
    (switch-to-buffer buffer)))

;;;; Mode

(define-minor-mode frame-purpose-mode
  "Toggle `frame-purpose-mode', allowing the easy creation of purpose-specific frames.
This works by overriding `buffer-list' in frames which have their
`buffer-predicate' parameter set.  If any unusual behavior is
noticed in Emacs as a result of the override, disabling this mode
should restore correct behavior."
  :global t
  :init-value nil
  (if frame-purpose-mode
      (frame-purpose--enable)
    (frame-purpose--disable)))

(defun frame-purpose--enable ()
  "Store original `buffer-list' definition and override it.
Called by `frame-purpose-mode'.  Do not call this function
manually."
  (fset 'frame-purpose--buffer-list-original (symbol-function #'buffer-list))
  (advice-add #'buffer-list :override #'frame-purpose--buffer-list))

(defun frame-purpose--disable ()
  "Restore original `buffer-list' definition.
Called by `frame-purpose-mode'.  Do not call this function
manually."
  (advice-remove #'buffer-list #'frame-purpose--buffer-list)
  (fmakunbound 'frame-purpose--buffer-list-original))

;;;; Sidebar

(cl-defun frame-purpose-show-sidebar (&optional (side 'right))
  "Show list of purpose-specific buffers on SIDE of this frame.
When a buffer in the list is selected, the last-used window
switches to that buffer.  Makes a new buffer if necessary.  SIDE
is a symbol, one of left, right, top, or bottom."
  (interactive)
  (frame-purpose--update-purpose-buffer-list)
  (setq side (cl-case side
               ;; Invert the side for `split-window'
               ('left 'right)
               ('right 'left)))
  (split-window nil (apply #'max (--map (+ 3 (length (buffer-name)))
                                        (buffer-list)))
                side)
  (switch-to-buffer (frame-purpose--list-buffer-name)))

(defun frame-purpose--update-purpose-buffer-list ()
  "Update purpose-specific buffer list for this frame."
  (with-current-buffer (frame-purpose--get-list-buffer)
    (let* ((inhibit-read-only t)
           (sorted-buffers (sort (buffer-list)
                                 (lambda (b1 b2)
                                   (cl-labels ((name (buffer)
                                                     (or (buffer-name buffer)
                                                         (buffer-file-name buffer))))
                                     (string< (name b1) (name b2))))))
           (grouped-buffers (mapcar #'cdr (seq-group-by #'buffer-modified-p sorted-buffers))))
      (erase-buffer)
      (dolist (group grouped-buffers)
        (cl-loop for buffer in group
                 for face = (if (buffer-modified-p buffer)
                                'highlight
                              'default)
                 do (insert (propertize (buffer-name buffer)
                                        'buffer buffer
                                        'face face)
                            "\n"))))))

(defun frame-purpose--get-list-buffer ()
  "Return the current frame's purpose-specific, buffer-listing sidebar buffer.
Creates the buffer if necessary."
  (unless (frame-parameter nil 'buffer-predicate)
    (user-error "This frame has no specific purpose"))
  (let ((buffer-name (frame-purpose--list-buffer-name)))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (setq buffer-read-only t)
          (use-local-map (make-sparse-keymap))
          (mapc (lambda (key) (local-set-key (kbd key) #'frame-purpose--sidebar-switch-to-buffer))
                '("<mouse-1>" "RET"))
          (setq mode-line-format nil)
          (current-buffer)))))

(defun frame-purpose--list-buffer-name (&optional frame)
  "Return name of purpose-specific buffer list buffer for FRAME (or current frame)."
  (when-let ((frame-title (frame-parameter frame 'title)))
    (format "*frame-purpose-buffers for frame: %s*" frame-title)))

(defun frame-purpose--sidebar-switch-to-buffer ()
  "Switch previously used window to the buffer chosen in the sidebar.
The previously used window is typically the one that was active
when the user clicked in the sidebar."
  (interactive)
  (when-let ((buffer (get-text-property (point-at-bol) 'buffer)))
    (select-window (get-mru-window nil nil 'not-selected))
    (switch-to-buffer buffer)))

;;;; Footer

(provide 'frame-purpose)

;;; frame-purpose.el ends here
