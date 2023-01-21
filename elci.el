;;; elci.el --- Interact with LXC from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 jvdydev

;; Author: Judy (jvdydev)
;; Version: 0.1
;; Keywords: containers
;; URL: https://github.com/jvdydev/elci.git

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU/Emacs.

;;; Commentary:
;; Elci is a small package to interface with Linux Containers (LXC) from Emacs.

;; This file works with outline-minor-mode.

;;; Code:
;;; Dependencies
(require 'ewoc)
(require 'cl-lib)

;;; defcustoms
(defgroup elci nil
  "elci: Interactive LXC management."
  :prefix "elci-")

(defcustom elci-mode-display-options '(name state autostart groups ipv4 ipv6 unprivileged)
  "List of columns displayed when creating an elci-mode buffer.
Valid Options: name, state, pid, ram, swap, autostart, groups, interface, ipv4, ipv6, unprivileged."
  :group 'elci)

;;; elci-conf: Configuration forms
;; Inspired by org-capture's select template buffer
;; Form to query user for multiple data points to pass as arguments
;;;; Input readers
(defun elci-conf--read-value (prompt &optional test default)
  "Read user input using PROMPT.
If user input is empty, return DEFAULT.
User input is passed as single argument to TEST.
TEST should raise an appropriate `error' if user input is not valid."
  (let ((user-input
         (read-string (concat prompt (when default (concat " (default: " default ")")) ": ")
                      "" nil default)))
    (when (fboundp test)
      (condition-case err
          (funcall test user-input)
        (error (user-error "Error: %s" (cadr err)))))
    user-input))

;;;; Popup Buffer / Abstract Display (ewoc)
;;;;; Helper Macros
(defmacro elci-conf--with-popup-buffer (buffer-or-name &rest body)
  "Popup a buffer using BUFFER-OR-NAME.
Run BODY in the context of the popup buffer.
After BODY exists (or on-error), kill popup buffer and restore window configuration."
  `(let ((popup-buffer (get-buffer-create ,buffer-or-name)))
     (unwind-protect
         (save-window-excursion
           (with-current-buffer (switch-to-buffer-other-window popup-buffer)
             ,@body))
       (kill-buffer popup-buffer))))

(defmacro elci-conf--foreach-ewoc-node (ewoc node &rest body)
  "Iterate over all nodes in EWOC.
Each iteration will bind the current node to NODE.
BODY is run for each iteration."
  `(let ((,node (ewoc-nth ,ewoc 0)))
     (while ,node
       ,@body
       (setq ,node (ewoc-next ,ewoc ,node)))))

;;;;; ewoc painter
(cl-defgeneric elci-conf--paint (type data)
  "Paint ewoc node data DATA for type TYPE."
  (message "[ELCI] Uknown render type: %s (buffer: %s)" type (current-buffer)))

(cl-defmethod elci-conf--paint ((type (eql 'title)) data)
  (let ((title (cl-getf data :string)))
    (insert "\n")
    (insert title)
    (insert "\n")
    (dotimes (i (length title))
      (insert "="))
    (insert "\n")))

(cl-defmethod elci-conf--paint ((type (eql 'group)) data)
  (let ((name (cl-getf data :gname)))
    (insert "\n")
    (insert name)
    (insert "\n")
    (dotimes (i (length name))
      (insert "-"))))

(cl-defmethod elci-conf--paint ((type (eql 'option)) data)
  (let ((name (cl-getf data :name))
        (key (char-to-string (cl-getf data :key)))
        (value (cl-getf data :value "")))
    (insert (format "[%s] %s: %s" key name value))))

(cl-defmethod elci-conf--paint ((type (eql 'action)) data)
  (let ((name (cl-getf data :name))
        (key (char-to-string (cl-getf data :key))))
    (insert (format "[%s] %s" key name))))

(defun elci-conf--ewoc-painter (data)
  "Ewoc painter called by ewoc.
Calls generic to paint DATA."
  (elci-conf--paint (cl-getf data :type 'no-type-symbol-in-data) data))

;;;;; Buffer setup
(defun elci-conf--fill-buffer (title
                               options)
  "Fill a buffer from TITLE and OPTIONS using EWOC.
Return ewoc instance.

OPTIONS is a list of (grouped) options.
Multiple options may be wrapped in a group plist, containing the keys :gname (group name) and :options (the options list).

Each option is a plist and should at least contain the keys '(:name :key :argument).
name (string) - Name of the option as it's displayed in the UI
key (char) - Key to press
argument (string) - Argument prefix. Will be concat to the user-provided value (if present).

Other (optional) keys:
value (string) - Allows setting the initial value (default value).
required (bool) - Sets the option to be required (currently unused)."
  (let ((ewoc-instance (ewoc-create #'elci-conf--ewoc-painter
                                    "Press the key in [?] to fill in option or run action."
                                    "")))
    ;; title
    (ewoc-enter-last ewoc-instance (list :type 'title
                                         :string title))
    ;; groups and options
    (dolist (option-or-group options)
      (if (cl-getf option-or-group :gname nil)
          (progn
            (ewoc-enter-last ewoc-instance (list :type 'group :gname (cl-getf option-or-group :gname)))
            (dolist (option (cl-getf option-or-group :options))
              (ewoc-enter-last ewoc-instance `(:type option ,@option))))
        (ewoc-enter-last ewoc-instance `(:type option ,@option-or-group))))

    ;; user-actions (quit / run)
    (ewoc-enter-last ewoc-instance (list :type 'group :gname "Actions"))
    (ewoc-enter-last ewoc-instance (list :type 'action :act 'quit :name "Quit" :key ?q))
    (ewoc-enter-last ewoc-instance (list :type 'action :act 'run :name "Run" :key ?r))

    ewoc-instance))

;;;;; EWOC helpers
(defun elci-conf--extract-keys (ewoc)
  "Extract all keys from EWOC.
Return three values, the quit and run action keys as well as all allowed keys (including action keys)."
  (let ((allowed-keys nil)
        (quit-key nil)
        (run-key nil))
    (elci-conf--foreach-ewoc-node ewoc current-node
                                  (let* ((data (ewoc-data current-node))
                                         (node-type (cl-getf data :type nil))
                                         (node-act (cl-getf data :act nil))
                                         (node-key (cl-getf data :key nil)))
                                    (when (member node-type '(option action))
                                      (push node-key allowed-keys)
                                      (when (eql node-type 'action)
                                        (cond ((eql node-act 'quit) (setq quit-key node-key))
                                              ((eql node-act 'run) (setq run-key node-key))
                                              (t (error "Unkown action: %s" node-act)))))))
    (cl-values quit-key run-key allowed-keys)))

(defun elci-conf--extract-result (ewoc)
  "Extract result (appended argument + value).
May in the future raise error on required args missing."
  (let ((result nil))
    (elci-conf--foreach-ewoc-node ewoc current-node
                                  ;; extract result
                                  (let ((data (ewoc-data current-node)))
                                    (when (and (eql (cl-getf data :type nil) 'option)
                                               (cl-getf data :value nil))
                                      (push (concat (cl-getf data :argument)
                                                    (cl-getf data :value))
                                            result))))
    (mapconcat #'identity result " ")))

(defun elci-conf--update-option-value (ewoc key)
  "Interactively update option value with key KEY in EWOC.
Update appropriate ewoc data structure and call invalidate for the updated node."
  (message "Called update for key `%s'" key)
  (elci-conf--foreach-ewoc-node ewoc current-node
                                (let* ((data (ewoc-data current-node))
                                       (node-type (cl-getf data :type nil))
                                       (node-key (cl-getf data :key nil))
                                       (node-name (cl-getf data :name nil))
                                       (node-old-value (cl-getf data :value nil)))
                                  (when (and (eql node-type 'option)
                                             (eql node-key key))
                                    (let ((new-value (elci-conf--read-value node-name nil node-old-value)))
                                      (setf (cl-getf data :value) new-value)
                                      (ewoc-set-data current-node data)
                                      (message "New value: %s" new-value)
                                      (ewoc-invalidate ewoc current-node))))))

;;;;; Query function (call this from other places)
(defun elci-conf--query-configuration (buffer-name title options &optional required-options)
  "Query user to fill in data in a popup buffer named BUFFER-NAME.
Data to be queried is added using OPTIONS (see `elci-conf--fill-buffer' for details).
TITLE will be placed over-top.

Each options' argument will be returned with the associated user-data concatenated.
If no user-data was provided, option will not be present.

If REQUIRED-OPTIONS is non-nil, it may be a list of strings matching names defined in OPTIONS."
  (let ((result nil)
        (ewoc-instance nil))
    (elci-conf--with-popup-buffer
     buffer-name
     (read-only-mode)
     (let ((inhibit-read-only t))
       ;; Fill buffer
       (erase-buffer)
       ;; TODO pass required-options here
       (setq ewoc-instance (elci-conf--fill-buffer title options)))

     ;; clear minibuffer
     (message " ")

     ;; Main loop, inline
     (cl-multiple-value-bind (quit-action-key run-action-key allowed-keys)
         (elci-conf--extract-keys ewoc-instance)

       (let ((quit nil))
         (while (not quit)
           (let ((key (read-char-exclusive)))
             (cond ((char-equal key quit-action-key) (setq quit t))
                   ((char-equal key run-action-key)
                    (progn
                      (setq result
                            (elci-conf--extract-result ewoc-instance))
                      (setq quit t)))
                   ((member key allowed-keys)
                    (elci-conf--update-option-value ewoc-instance key))
                   ((t (message "Invalid key: `%s'" key)))))))))
    result))

;;; elci-lxc: LXC abstraction layer
;;;; Helpers
(defun elci-lxc--join-strings (sep strings)
  "Join STRINGS using SEP.
Trim ends of joined strings."
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (string-trim (mapconcat #'identity strings sep))))

(defun elci-lxc--join-symbols (sep symbols)
  "Join SYMBOLS by name using SEP."
  (declare (pure t) (side-effect-free t))
  (elci-lxc--join-strings sep (mapcar #'symbol-name symbols)))

(defun elci-lxc--trim (line &optional startc endc)
  "Trim leading and trailing whitespace from LINE.
STARTC and ENDC may each be lists of additional characters to trim at start and end respectively."
  (let ((new-line line)
        (start-regex (concat "\\`[" (mapconcat #'identity startc "") " \t\n\r]+"))
        (end-regex (concat "[" (mapconcat #'identity endc "") " \t\n\r]+\\'")))
    (when (string-match start-regex new-line)
      (setq new-line (replace-match "" t t new-line)))

    (when (string-match end-regex new-line)
      (setq new-line (replace-match "" t t new-line)))
    new-line))

;;;; listing containers
(cl-defgeneric elci-lxc--propertize-entry (type entry)
  "Propertize ENTRY depending on TYPE.
If TYPE is unkown, return unporpertized ENTRY."
  entry)

(cl-defmethod elci-lxc--propertize-entry ((type (eql 'STATE)) entry)
  ;; error, success, vc-dir-directory
  (let ((foreground (pcase (intern entry)
                      ('RUNNING "green")
                      ('STOPPED "red")
                      ('FROZEN "blue"))))
    (when foreground
      (put-text-property 0 (length entry) 'face (cons 'foreground-color foreground) entry)))
  entry)


(defun elci-lxc--propertize-entries (header entries)
  "Propertize all items in ENTRIES given HEADER.
Return a new set of ENTRIES with the same data, but propertized."
  (let ((prop-entries '())
        (prop-entry '()))
    (dolist (entry entries prop-entries)
      (setq prop-entry '())
      (dotimes (i (length entry))
        (setq prop-entry (append prop-entry (list
                                             (elci-lxc--propertize-entry (nth i header)
                                                                         (nth i entry))))))
      (setq prop-entries (append prop-entries (list prop-entry))))
    prop-entries))

(defun elci-lxc--maybe-construct-arg (flag argument &optional test)
  "If ARGUMENT is non-nil (and TEST succeeds), construct a string \"FLAG ARGUMENT\", else return an empty string.
TEST may be an optional single-argument function that takes ARGUMENT and return a truthy value."
  (if (or (not test)
          (and (fboundp test) (funcall test argument)))
      (concat flag " " argument)
    ""))


(defun elci-lxc--list-containers (fancy-format-options &optional propertize-entries)
  "Run container listing with FANCY-FORMAT-OPTIONS.
Return two values, header (list of symbols) and list of entries (each entry is a list of strings).
If PROPERTIZE-ENTRIES is non-nil, apply properties using ELCI-LXC--PROPERTIZE-ENTRIES."
  (let* ((fancy-format (elci-lxc--join-symbols "," fancy-format-options))
         (ls (string-split
               (shell-command-to-string
                (elci-lxc--join-strings " "
                                        (list "lxc-ls -f"
                                              (elci-lxc--maybe-construct-arg "-F" fancy-format))))
               "[\n\r]+" t)))
    (when (and (eql 1 (length ls))
               (save-match-data
                 (string-match-p "^Invalid Key" (car ls))))
      (user-error (format "LXC Error: %s" (car ls))))

    (let* ((out (mapcar (lambda (l) (string-split
                                (replace-regexp-in-string ", *" "," l)
                                " " t))
                        ls))
           (header (mapcar #'intern (car out)))
           (entries (if propertize-entries
                        (elci-lxc--propertize-entries header (cdr out))
                      (cdr out))))
      (cl-values header entries))))

;;; elci: Interactive buffer
(defvar-local elci--header-list nil
  "Buffer-local storage for header to update fields appropriately.")

;;;; Internal buffer helpers
(defun elci--setup-buffer (format-options)
  (setq elci--header-list format-options)
  (cl-multiple-value-bind (header entries)
      (elci-lxc--list-containers format-options t)
    (setq tabulated-list-format (vconcat (mapcar (lambda (e) (list e (/ 100 (length header))))
                                                 (mapcar #'symbol-name header))))
    (setq tabulated-list-entries (mapcar (lambda (e) (list nil (vconcat e))) entries))
    (tabulated-list-init-header)
    (tabulated-list-print)))


(defun elci--update-buffer ()
  (when (eql major-mode 'elci-mode)
    (cl-multiple-value-bind (header entries)
        (elci-lxc--list-containers elci--header-list t)
      (setq tabulated-list-entries (mapcar (lambda (e) (list nil (vconcat e))) entries)))))

(defun elci--find-argument (string arg)
  "Find argument ARG in STRING.
Return substring or nil."
  (save-match-data
    (when-let ((start (string-match arg string)))
      (car (string-split (cadr (string-split string arg)) " ")))))

;;;; User-facing functions
;;;###autoload
(defun elci ()
  "Create elci-mode buffer."
  (interactive)
  (when (get-buffer "*elci*")
    (kill-buffer "*elci*"))
  (with-current-buffer (switch-to-buffer (get-buffer-create "*elci*"))
    (elci-mode)
    (elci--setup-buffer elci-mode-display-options)))


(defun elci-update-buffer ()
  "Update a buffer in elci-mode."
  (interactive)
  (if (not (eql major-mode 'elci-mode))
      (user-error "Not in an elci buffer.")
    (message "Updating elci-mode buffer.")
    (revert-buffer)))

;;;; Mode definition
(defvar elci-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Quit
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "Q") #'kill-this-buffer)

    ;; Update
    (define-key map (kbd "U") #'elci-update-buffer)

    ;; TODO Actions
    map))

(define-derived-mode elci-mode tabulated-list-mode "Elci Mode"
  "Major mode for interacting with LXC from a list of containers."
  (use-local-map elci-mode-map)
  (add-hook 'tabulated-list-revert-hook #'elci--update-buffer nil t))

;;; _
(provide 'elci)
;;; elci.el ends here
