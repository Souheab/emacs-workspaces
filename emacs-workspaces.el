;; HOOKS SECTION
(defvar workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be deleted.")

;;TODO use initial buffer choice to guess then use scratch
(defcustom workspace-fallback-buffer "*scratch*"
  "The BUFFER-OR-NAME of the default buffer to switch to.")



  ;;;; Create main workspace
  ;; The default perspective persp-mode creates is special and doesn't represent
  ;; a real persp object, so buffers can't really be assigned to it, among other
;; quirks, so I replace it with a "main" perspective.

(defun workspaces-ensure-no-nil-workspaces-h (&rest _)
    (when persp-mode
      (dolist (frame (frame-list))
        (when (string= (safe-persp-name (get-current-persp frame)) persp-nil-name)
          ;; Take extra steps to ensure no frame ends up in the nil perspective
          (persp-frame-switch (or (cadr (hash-table-keys *persp-hash*))
                                  workspaces-main)
                              frame)))))

(defun workspaces-init-first-workspace-h (&rest _)
  "Ensure a main workspace exists."
  (when persp-mode
    (let (persp-before-switch-functions)
      ;; Try our best to hide the nil perspective.
      (when (equal (car persp-names-cache) persp-nil-name)
        (pop persp-names-cache))
      ;; ...and create a *real* main workspace to fill this role.
      (unless (or (persp-get-by-name workspaces-main)
                  ;; Start from 2 b/c persp-mode counts the nil workspace
                  (> (hash-table-count *persp-hash*) 2))
        (persp-add-new workspaces-main))
      ;; HACK Fix #319: the warnings buffer gets swallowed when creating
      ;;      `workspaces-main', so display it ourselves, if it exists.
      (when-let (warnings (get-buffer "*Warnings*"))
        (save-excursion
          (display-buffer-in-side-window
           warnings '((window-height . shrink-window-if-larger-than-buffer))))))))




;; disable uniquify while persp-mode runs
(defun workspaces-init-persp-mode-h ()
  (cond (persp-mode
         ;; `uniquify' breaks persp-mode. It renames old buffers, which causes
         ;; errors when switching between perspective (their buffers are
         ;; serialized by name and persp-mode expects them to have the same
         ;; name when restored).
         (when uniquify-buffer-name-style
           (setq workspace--old-uniquify-style uniquify-buffer-name-style))
         (setq uniquify-buffer-name-style nil)
         ;; Ensure `persp-kill-buffer-query-function' is last
         (remove-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
         (add-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function t))
        (t
         (when workspace--old-uniquify-style
           (setq uniquify-buffer-name-style workspace--old-uniquify-style))
         )))

(add-hook 'persp-mode-hook #'workspaces-ensure-no-nil-workspaces-h)
(add-hook 'persp-after-load-state-functions #'workspaces-ensure-no-nil-workspaces-h)

(add-hook 'persp-mode-hook #'workspaces-init-first-workspace-h)
(add-hook 'persp-mode-hook #'workspaces-init-persp-mode-h)



;; Tabline stuff SECTION
(defalias #'workspace-current #'get-current-persp
  "Return the currently active workspace.")

(defalias #'workspace-contains-buffer-p #'persp-contain-buffer-p
  "Return t if PERSP contains BUFF-OR-NAME, otherwise return nil")

(defalias #'workspace-p #'perspective-p
  "Return t if OBJ is a perspective hash table.")

(defun workspace-list-names ()
  "Return the list of names of open workspaces."
  persp-names-cache)
(defun workspace-current-name ()
  "Get the name of the current workspace."
  (safe-persp-name (workspace-current)))

(defface workspace-tab-selected-face '((t (:inherit highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)
(defface workspace-tab-face '((t (:inherit default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

(defun workspace--tabline (&optional names)
  (let ((names (or names (workspace-list-names)))
        (current-name (workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (1+ i) name)
                          'face (if (equal current-name name)
                                    'workspace-tab-selected-face
                                  'workspace-tab-face)))
     " ")))


;; Workspace Control Functions LIBRARY SECTION

(defvar workspace--last nil)
(defvar workspace--index 0)

(defun workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (member name (workspace-list-names)))

(defun workspace--protected-p (name)
  (equal name persp-nil-name))


(defun workspace-new (name)
  "Create a new workspace named NAME. If one already exists, error.
Otherwise return the new persp on success."
  (when (workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (let ((persp (persp-add-new name)))
    (save-window-excursion
      (let ((ignore-window-parameters t)
            )
        (persp-delete-other-windows))
      ;; TODO consider if initial-buffer-choice not set IN ALL INSTANCE
      (switch-to-buffer workspace-default-buffer)
      (setf (persp-window-conf persp)
            (funcall persp-window-state-get-function (selected-frame))))
    persp))

(defun workspace-switch (name &optional auto-create-p)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE-P is non-nil, create the workspace if it doesn't exist, otherwise
throws an error."
  (unless (workspace-exists-p name)
    (if auto-create-p
        (workspace-new name)
      (error "%s is not an available workspace" name)))
  (let ((old-name (workspace-current-name)))
    (unless (equal old-name name)
      (setq workspace--last
            (or (and (not (string= old-name persp-nil-name))
                     old-name)
                workspaces-main))
      (persp-frame-switch name))
    (equal (workspace-current-name) name)))

(defun workspace--message-body (message &optional type)
  (concat (workspace--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))

(defun workspace-message (message &optional type)
  "Show an 'elegant' message in the echo area next to a listing of workspaces."
  (message "%s" (workspace--message-body message type)))

(defun workspace-error (message &optional noerror)
  "Show an 'elegant' error in the echo area next to a listing of workspaces."
  (funcall (if noerror #'message #'error)
           "%s" (workspace--message-body message 'error)))

(defun workspace--generate-id ()
  (or (cl-loop for name in (workspace-list-names)
               when (string-match-p "^#[0-9]+$" name)
               maximize (string-to-number (substring name 1)) into max
               finally return (if max (1+ max)))
      1))

(defun workspace-get (name &optional noerror)
  "Return a workspace named NAME. Unless NOERROR is non-nil, this throws an
error if NAME doesn't exist."
  (cl-check-type name string)
  (when-let (persp (persp-get-by-name name))
    (cond ((workspace-p persp) persp)
          ((not noerror)
           (error "No workspace called '%s' was found" name)))))


(defun workspace-delete (workspace &optional inhibit-kill-p)
  "Delete the workspace denoted by WORKSPACE, which can be the name of a perspective
or its hash table. If INHIBIT-KILL-P is non-nil, don't kill this workspace's
buffers."
  (unless (stringp workspace)
    (setq workspace (persp-name workspace)))
  (when (workspace--protected-p workspace)
    (error "Can't delete '%s' workspace" workspace))
  (workspace-get workspace) ; error checking
  (persp-kill workspace inhibit-kill-p)
  (not (workspace-exists-p workspace)))

(defun workspace-buffer-list (&optional persp)
  "Return a list of buffers in PERSP.

PERSP can be a string (name of a workspace) or a workspace (satisfies
`+workspace-p'). If nil or omitted, it defaults to the current workspace."
  (let ((persp (or persp (workspace-current))))
    (unless (workspace-p persp)
      (user-error "Not in a valid workspace (%s)" persp))
    (persp-buffers persp)))

(defun workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (workspace--tabline))))


(defun workspace/new (&optional name clone-p)
  "Create a new workspace named NAME. If CLONE-P is non-nil, clone the current
workspace, otherwise the new workspace is blank."
  (interactive (list nil current-prefix-arg))
  (unless name
    (setq name (format "#%s" (workspace--generate-id))))
  (condition-case e
      (cond ((workspace-exists-p name)
             (error "%s already exists" name))
            (clone-p (persp-copy name t))
            (t
             (workspace-switch name t)
             (workspace/display)))
    ((debug error) (workspace-error (cadr e) t))))


(defun workspace/switch-to (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive
   (list (or current-prefix-arg
               (completing-read "Switch to workspace: " (workspace-list-names)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (workspace-list-names))
            (old-name (workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (workspace-switch dest)))
              ((stringp index)
               (workspace-switch index t))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (workspace-current-name) old-name)
              (workspace-message (format "Already in %s" old-name) 'warn)
            (workspace/display))))
    ('error (workspace-error (cadr ex) t))))

(defun workspace/delete (name)
  "Delete this workspace. If called with C-u, prompts you for the name of the
workspace to delete."
  (interactive
   (let ((current-name (workspace-current-name)))
     (list
      (if current-prefix-arg
          (completing-read (format "Delete workspace (default: %s): " current-name)
                           (workspace-list-names)
                           nil nil nil nil current-name)
        current-name))))
  (condition-case-unless-debug ex
      ;; REVIEW refactor me
      (let ((workspaces (workspace-list-names)))
        (if (not (member name workspaces))
            (workspace-message (format "'%s' workspace doesn't exist" name) 'warn)
          (cond ((delq (selected-frame) (persp-frames-with-persp (get-frame-persp)))
                 (user-error "Can't close workspace, it's visible in another frame"))
                ((not (equal (workspace-current-name) name))
                 (workspace-delete name)) 
                ((cdr workspaces)
                 (workspace-delete name)
                 (workspace-switch
                  (if (workspace-exists-p workspace--last)
                      workspace--last
                    (car (workspace-list-names))))
                 (when (eq (workspace-buffer-list) nil)
                   (switch-to-buffer workspace-fallback-buffer)
                   )
                 )
                (t
                 (mapc 'kill-buffer (workspace-buffer-list)) ;; HACK KEEP WATCH ON THIS
                 (workspace-switch workspaces-main t)
                 (unless (string= (car workspaces) workspaces-main)
                   (workspace-delete name))
                 )) 
          (workspace-message (format "Deleted '%s' workspace" name) 'success)
          ;(switch-to-buffer workspace-default-buffer)
          ))
    ('error (workspace-error ex t))))


(defun workspace/switch-to-buffer ()
  "Switch to a buffer inside workspace"
  (interactive)
  (let ((buffers (mapcar #'buffer-name (workspace-buffer-list))))
    (switch-to-buffer (completing-read "Select buffer: " buffers))))

(provide 'emacs-workspaces)

