(in-package :pass-storage)

(defstruct app
  main-window
  data
  view
  column
  check-renderer
  current-icon
  current-title
  current-description
  current-view
  actions-common
  actions-edit
  actions-merge
  filter
  search-entry
  statusbar
  filename
  password
  changed)

(defun set-status (app message)
  (let ((statusbar (app-statusbar app)))
    (gtk-statusbar-pop statusbar 0)
    (gtk-statusbar-push statusbar 0 message)))

(defun ensure-data-is-saved (app)
  (if (app-changed app)
      (case (ask-save (app-main-window app)
                      (format nil "Save changes before closing? ~
                                   If you don't save, changes will be ~
                                   permanently lost."))
        (:ok (cb-save app) t)
        (:reject t)
        (:cancel nil))
      t))

(defun e-close (app)
  (if (ensure-data-is-saved app)
      (progn
        (gtk-clipboard-clear (gtk-clipboard-get "CLIPBOARD"))
        (gtk-widget-destroy (app-main-window app))
        nil)
      t))

(defun get-selected-iter (app)
  (let ((selection (gtk-tree-view-get-selection (app-view app))))
    (when selection
      (let ((current-model (gtk-tree-view-model (app-view app))))
        (cond
          ((eq current-model (app-data app))
           (let ((iter (gtk-tree-selection-get-selected selection)))
             (when iter
               (values iter
                       (gtk-tree-model-get-path (app-data app) iter)))))
          ((eq current-model (app-filter app))
           (let ((filter-iter (gtk-tree-selection-get-selected selection)))
             (when filter-iter
               (values
                 (gtk-tree-model-filter-convert-iter-to-child-iter
                     (app-filter app)
                     filter-iter)
                 (gtk-tree-model-get-path (app-filter app) filter-iter))))))))))

(defun get-selected-group-iter (app)
  (let* ((data (app-data app))
         (iter (get-selected-iter app)))
    (loop
      while (and iter
                 (not (is-group (gtk-tree-model-get-value data iter 0))))
      do (setf iter (gtk-tree-model-iter-parent data iter)))
    iter))

(defun listview-cursor-changed (app)
  (let ((s (get-selected-iter app)))
    (loop
      for action in '("copy-name"
                      "copy-password"
                      "edit"
                      "delete")
      do (setf (gtk-action-sensitive
                 (gtk-action-group-get-action (app-actions-edit app) action))
               s))
    (let ((entry (and s (gtk-tree-model-get-value (app-data app) s 0))))
      (loop
        for class in (list 'entry-generic
                           'entry-creditcard
                           'entry-cryptokey
                           'entry-database
                           'entry-door
                           'entry-email
                           'entry-ftp
                           'entry-phone
                           'entry-shell
                           'entry-website)
        do (setf (gtk-action-sensitive
                   (gtk-action-group-get-action (app-actions-edit app)
                                                (format nil "convert-to-~(~A~)"
                                                             class)))
                 (and s
                      (not (is-group entry))
                      (not (eql (find-class class) (class-of entry)))))))
    (if s
        (let ((entry (gtk-tree-model-get-value (app-data app) s 0)))
          (setf (gtk-image-stock (app-current-icon app))
                (entry-icon entry))
          (setf (gtk-label-label (app-current-title app))
                (format nil "<big><b>~A</b></big>"
                        (markup-escape-text (entry-name entry))))
          (setf (gtk-label-label (app-current-description app))
                (entry-description entry))
          (setf (gtk-label-label (app-current-view app))
                (entry-to-markup entry
                    :show-secrets
                    (config-show-secrets-on-preview *config*)))
        )
        ;; else
        (setf (gtk-image-stock (app-current-icon app)) ""
              (gtk-label-label (app-current-title app)) ""
              (gtk-label-label (app-current-description app)) ""
              (gtk-label-label (app-current-view app)) ""))))

(defun update-row (data iter entry)
  (gtk-tree-store-set-value data iter 0 entry)
  (gtk-tree-store-set-value data iter 1 (entry-name entry))
  (gtk-tree-store-set-value data iter 2 (entry-icon entry))
  (unless (is-group entry)
    (gtk-tree-store-set-value data iter 4
      (let ((e (password-entropy (entry-get-password entry))))
        (cond ((< e 28) "ps-very-weak")
              ((and (<= 28 e) (< e 36)) "ps-weak")
              ((and (<= 36 e) (< e 60)) "ps-reasonable")
              ((and (<= 60 e) (< e 128)) "ps-strong")
              ((<= 128 e) "ps-very-strong"))))))

(defun select-iter (app iter)
  (let* ((current-model (gtk-tree-view-model (app-view app)))
         (iter-to-select
          (cond
            ((eq current-model (app-data app))
             iter)
            ((eq current-model (app-filter app))
             (gtk-tree-model-filter-convert-child-iter-to-iter
                 (app-filter app)
                 iter)))))
    (when iter-to-select
      (let ((path-to-select (gtk-tree-model-get-path current-model
                                iter-to-select)))
        (gtk-tree-view-expand-to-path (app-view app) path-to-select)
        (gtk-tree-view-set-cursor (app-view app) path-to-select)))))

(defun get-usernames (app)
  (let ((data (app-data app)))
    (labels ((traverse (parent-iter)
               (iter (for i in-tree-model data children-of parent-iter)
                     (let ((entry (gtk-tree-model-get-value data i 0)))
                       (if (is-group entry)
                           (nunioning (traverse i) :test 'string=)
                           (adjoining (entry-username entry)
                                      :test 'string=))))))
      (let* ((names (traverse nil))
             (non-nil-names (remove nil names))
             (non-empty-names (remove "" non-nil-names :test 'string=))
             (sorted-names (sort non-empty-names 'string-lessp)))
        sorted-names))))

(defun cb-add-item (app type)
  (let ((entry (make-instance type)))
    (when (edit-entry entry (app-main-window app) "Add" (get-usernames app))
      (let ((iter (gtk-tree-store-append (app-data app)
                                         (get-selected-group-iter app))))
        (update-row (app-data app) iter entry)
        (gtk-tree-model-filter-refilter (app-filter app))
        (select-iter app iter)
        (set-status app "New entry was added")
        (setf (app-changed app) t)))))

(defun cb-edit-entry (app)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter
      (let ((entry (gtk-tree-model-get-value data iter 0)))
        (when (edit-entry entry (app-main-window app)
                                "Edit"
                                (get-usernames app))
          (update-row (app-data app) iter entry)
          (gtk-tree-model-filter-refilter (app-filter app))
          (listview-cursor-changed app)
          (set-status app "Entry was changed")
          (setf (app-changed app) t))))))

(defun cb-convert-entry (app dest-class)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter
      (let ((entry (gtk-tree-model-get-value data iter 0)))
        (update-row (app-data app) iter (copy-entry entry dest-class))
        (gtk-tree-model-filter-refilter (app-filter app))
        (set-status app "Entry has changed type")
        (setf (app-changed app) t)
        (listview-cursor-changed app)))))

(defun cb-del-entry (app)
  (let ((iter (get-selected-iter app)))
    (when (and iter
               (ask (app-main-window app)
                    "Do you really want to delete selected item?"))
      (let* ((data (app-data app)))
        (gtk-tree-store-remove data iter)
        (listview-cursor-changed app)
        (set-status app "Entry was deleted")
        (setf (app-changed app) t)))))

(defun cb-uncheck-all (app)
  (labels ((uncheck (model parent)
             (iter (for i in-tree-model model children-of parent)
                   (gtk-tree-store-set-value model i 3 nil)
                   (uncheck model i))))
    (uncheck (app-data app) nil)
    (set-status app "Unchecked all items")))

(defun cb-merge (app)
  (let ((checked
         (labels ((collect-checked (model parent path)
                    (iter (for i in-tree-model model children-of parent)
                          (let ((entry (gtk-tree-model-get-value model i 0)))
                            (appending (collect-checked model
                                                        i
                                                        (cons (entry-name entry)
                                                              path)))
                            (when (gtk-tree-model-get-value model i 3)
                              (collect (list entry (reverse path))))))))
           (collect-checked (app-data app) nil nil))))
    (when (< (length checked) 2)
      (say-info (app-main-window app)
                "Nothing to merge. Select few items and try again.")
      (return-from cb-merge))
    (unless (ask (app-main-window app)
                 (with-output-to-string (str)
                   (format str "Do you want to merge following items?~%")
                   (iter (for (entry path) in checked)
                         (format str "~%~{~A / ~}~A" path (entry-name entry)))))
      (return-from cb-merge))
    ;; delete entries
    (labels ((delete-checked (model parent)
               (let ((i (if parent
                            (gtk-tree-model-iter-children model parent)
                            (gtk-tree-model-get-iter-first model))))
                 (when i
                   (iter (while
                             (if (gtk-tree-model-get-value model i 3)
                                 (gtk-tree-store-remove model i)
                                 (progn
                                   (delete-checked model i)
                                   (setf i
                                         (gtk-tree-model-iter-next model i))))))))))
      (delete-checked (app-data app) nil))
    ;; create new entry
    (let ((result (make-instance (or (equal-together (mapcar (lambda (c) (class-of (first c))) checked))
                                     (find-class 'entry-generic)))))
      (iter (for (entry path) in checked)
            (join-entry result path entry))
      (setf (entry-name result)
            (let ((names (mapcar (lambda (c) (entry-name (first c))) checked)))
              (or (equal-together names)
                  (format nil "~A ~{ and ~A~}" (car names) (cdr names)))))
      ;; TODO: detect common path
      (let ((iter (gtk-tree-store-append (app-data app) nil)))
        (update-row (app-data app) iter result)
        (gtk-tree-model-filter-refilter (app-filter app))
        (select-iter app iter)
        (set-status app "New entry was created by merging")
        (setf (app-changed app) t)))))

(defun make-data ()
  (make-instance 'gtk-tree-store
                 :column-types '("GObject"
                                 "gchararray"
                                 "gchararray"
                                 "gboolean"
                                 "gchararray")))

(defun load-data (filename parent-window)
  (labels ((parse-entry (elem data parent-iter)
             (let ((type (intern (tag-get-attr elem :|type|) 'keyword))
                   (iter (gtk-tree-store-append data parent-iter)))
               (format t "in PARSE-ENTRY ~S~%" elem)
               (update-row data
                           iter
                           (load-entry type elem))
               (when (eql type :|folder|)
                 (iter (for ch in (tag-children elem))
                       (parse ch data iter)))))
           (parse (elem data parent-iter)
             (format t "in PARSE ~S~%" elem)
             (when (is-tag elem)
               (cond
                 ;; toplevel
                 ((eq (tag-name elem) :|revelationdata|)
                  (iter (for ch in (tag-children elem))
                        (parse ch data nil)))
                 ((eq (tag-name elem) :|entry|)
                  (parse-entry elem data parent-iter))))))
    (loop
       for password = (edit-object nil
                                   parent-window
                                   "Enter password" "ps-pass-storage"
                                   '((nil "Password" :entry :required :password)))
       while password
       do (handler-case
              (let ((xml (load-revelation-file filename (car password)))
                    (data (make-data)))
                (parse xml data nil)
                (return-from load-data (values data
                                               (car password))))
            (error (e)
              (declare (ignore e))
              (say-error parent-window "Can't open this file."))))))

(defmacro tree-foreach-collect (iter model parent-iter &body body)
  `(let ((,iter (if ,parent-iter
                    (gtk-tree-model-iter-children ,model ,parent-iter)
                    (gtk-tree-model-get-iter-first ,model))))
     (when ,iter
       (loop
          collect (progn ,@body)
          while (setf ,iter (gtk-tree-model-iter-next ,model ,iter))))))

(defun save-data (app filename)
  (let ((data (app-data app)))
    (labels ((traverse (parent-iter)
               (tree-foreach-collect iter
                                     data
                                     parent-iter
                                     (let ((entry (gtk-tree-model-get-value data iter 0)))
                                       (if (is-group entry)
                                           (append (save-entry entry) (traverse iter))
                                           (save-entry entry))))))
      (let ((xml (list* (list :|revelationdata| :|version| "0.4.11" :|dataversion| "1")
                        (traverse nil))))

        (unless (app-password app)
          (let ((password (edit-object nil (app-main-window app) "Enter password" "ps-pass-storage"
                                       '((nil "Password" :entry :required :password)))))
            (unless password
              (return-from save-data))
            (setf (app-password app) (car password))))

        (save-revelation-file filename (app-password app) xml)
        (set-status app "File was saved")
        (setf (app-changed app) nil)))))

(defun cb-new (app)
  (when (ensure-data-is-saved app)
    (setf (app-changed app) nil)
    (gtk-tree-store-clear (app-data app))
    (setf (gtk-toggle-action-active (gtk-action-group-get-action (app-actions-common app) "merge-mode"))
          nil)
    (setf (app-filename app) nil)
    (setf (app-password app) nil)
    (listview-cursor-changed app)
    (set-status app "New file was created")))

(defun set-data (app data)
  (setf (app-data app) data
        (gtk-tree-view-model (app-view app)) data
        (app-filter app) (make-instance 'gtk-tree-model-filter :child-model data))
  (set-filter-function app))

(defun open-file (app filename)
  (multiple-value-bind (data password)
      (load-data filename (app-main-window app))
    (when data
      (set-data app data)
      (setf (app-filename app) filename)
      (setf (app-password app) password)
      (setf (app-changed app) nil)
      (set-status app "File was opened"))))

(defun cb-open (app)
  (when (ensure-data-is-saved app)
    (let ((dlg (make-instance 'gtk-file-chooser-dialog
                              :action :open
                              :title "Open file"
                              :window-position :center-on-parent
                              :transient-for (app-main-window app))))
      (gtk-dialog-add-button dlg "gtk-cancel" :cancel)
      (gtk-dialog-add-button dlg "gtk-open" :ok)
      (gtk-dialog-set-alternative-button-order dlg (list :ok :cancel))
      (gtk-dialog-set-default-response dlg :ok)
      (when (std-dialog-run dlg)
        (open-file app (gtk-file-chooser-get-filename dlg))))))

(defun merge-file-by-name (app data)
  (labels ((find-entry (name parent-iter)
             (iter (for i in-tree-model (app-data app) children-of parent-iter)
                   (for entry = (gtk-tree-model-get-value (app-data app) i 0))
                   (if (is-group entry)
                       (appending (find-entry name i))
                       (when (string-equal (entry-name entry) name)
                         (collecting entry)))))
           (merge-entry (src-iter)
             (let ((entry (gtk-tree-model-get-value data src-iter 0)))
               (if (is-group entry)
                   (iter (for i in-tree-model data children-of src-iter)
                         (merge-entry i))
                   (progn
                     (let ((dst-entries (find-entry (entry-name entry) nil)))
                       (if dst-entries
                           (join-entry (first dst-entries) nil entry)
                           (update-row (app-data app)
                                       (gtk-tree-store-append (app-data app)
                                                              nil)
                                       entry))))))))
    (iter (for i in-tree-model data children-of nil)
          (merge-entry i))))

(defun append-file (app data)
  (labels ((append-entry (src-iter dst-iter)
             (let ((entry (gtk-tree-model-get-value data src-iter 0))
                   (iter (gtk-tree-store-append (app-data app) dst-iter)))
               (update-row (app-data app) iter entry)
               (when (is-group entry)
                 (iter (for i in-tree-model data children-of src-iter)
                       (append-entry i iter))))))
    (iter (for i in-tree-model data children-of nil)
          (append-entry i nil))))

(defun cb-merge-file (app)
  (let ((r (edit-object nil (app-main-window app) "Merge file" "ps-pass-storage"
                        '((nil ("Merge entries by name (ignore groups)" "Append file") :choice)
                          (nil "File to merge" :filename :required)))))
    (when r
      (destructuring-bind (mode filename) r
        (let ((data (load-data filename (app-main-window app))))
          (when data
            (ecase mode
              (0 (merge-file-by-name app data))
              (1 (append-file app data)))))))))

(defun cb-save-as (app)
  (let ((dlg (make-instance 'gtk-file-chooser-dialog
                            :action :save
                            :title "Save file"
                            :window-position :center-on-parent
                            :transient-for (app-main-window app))))

    (gtk-dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk-dialog-add-button dlg "gtk-save" :ok)
    (gtk-dialog-set-alternative-button-order dlg (list :ok :cancel))
    (gtk-dialog-set-default-response dlg :ok)

    (when (std-dialog-run dlg)
      (setf (app-filename app) (gtk-file-chooser-get-filename dlg))
      (save-data app (gtk-file-chooser-get-filename dlg)))))

(defun cb-save (app)
  (if (app-filename app)
      (save-data app (app-filename app))
      (cb-save-as app)))

(defun cb-change-password (app)
  (let ((parent-window (app-main-window app)))
    (loop
       (let ((passwords (edit-object nil parent-window "Change password" "ps-pass-storage"
                                     '((nil "Password" :entry :required :password)
                                       (nil "Confirm" :entry :required :password)))))
         (unless passwords
           (return-from cb-change-password))
         (when (string= (first passwords) (second passwords))
           (setf (app-password app) (first passwords))
           (setf (app-changed app) t)
           (return-from cb-change-password))
         (say-warning parent-window "Entered passwords are not identical")))))

(defun cb-preferences (app)
  (edit-object *config* (app-main-window app) "Preferences" "gtk-preferences"
               '((default-file "Default path" :filename)
                 (search-in-secrets "Search in secrets (passwords)" :boolean)
                 (show-secrets-on-preview "Show secrets (passwords) on preview panel" :boolean))))

(defun cb-copy-name (app)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter

      (let* ((entry (gtk-tree-model-get-value data iter 0))
             (name (entry-get-name entry)))
        (when name

          (gtk-clipboard-set-text
           (gtk-clipboard-get "CLIPBOARD")
           name)
          (set-status app "Name was copied to clipboard"))))))

(defun cb-copy-password (app)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter

      (let* ((entry (gtk-tree-model-get-value data iter 0))
             (password (entry-get-password entry)))
        (when password

          (gtk-clipboard-set-text
           (gtk-clipboard-get "CLIPBOARD")
           password)
          (set-status app "Secret (password) was copied to clipboard"))))))

(defun cb-about (app)
  (gtk-show-about-dialog (app-main-window app)
                         :title "About Dialog"
                         :program-name "PassStorage"
                         :version *ps-version*
                         :copyright "Copyright 2009-2011, Andrey Kutejko"
                         :website "http://andy128k.github.com/PassStorage"
                         :website-label "Project web site"
                         :authors '("Andrey Kutejko <andy128k@gmail.com>"
                                    "Dieter Kaiser <crategus@crategus.com>")
                         :logo
                         (gtk-widget-render-icon (app-main-window app)
                                                 "ps-pass-storage"
                                                 :dialog
                                                 "")
                         :wrap-license t))

(defun make-menu (group &rest actions)
  (let ((menu (make-instance 'gtk-menu)))
    (iter (for action in actions)
          (if action
              (gtk-menu-shell-append menu
                                     (gtk-action-create-menu-item
                                       (gtk-action-group-get-action group
                                                                    action)))
              (gtk-menu-shell-append menu (make-instance 'gtk-menu-item
                                                         :visible t))))
    menu))

(defmacro lambda-u (&body body)
  (let ((p (gensym)))
    `(lambda (&rest ,p)
       (declare (ignore ,p))
       #-nil
       (handler-case
           (progn ,@body)
         (error (c)
           ;; TODO: use parent-window
           (say-error nil (format nil "~A" c))))
       #+nil
       ,@body)))

(defmacro create-action (group action-params &optional accel func)
  `(let ((action (make-instance 'gtk-action ,@action-params)))
     (gtk-action-group-add-action ,group action ,accel)
     ,(when func
            `(g-signal-connect action "activate" ,func))))

(defmacro create-toggle-action (group action-params &optional accel func)
  `(let ((action (make-instance 'gtk-toggle-action ,@action-params)))
     (gtk-action-group-add-action ,group action ,accel)
     ,(when func
            `(g-signal-connect action "toggled" ,func))))

(defun entry-icon-by-class (class)
  (let ((entry (make-instance class)))
    (entry-icon entry)))

(defun entry-title-by-class (class)
  (let ((entry (make-instance class)))
    (entry-title entry)))

;; search
(defun entry-satisfies (entry model iter text)
  (or
   (entry-has-text entry
                   text
                   :look-at-secrets (config-search-in-secrets *config*))

   (iter (for i in-tree-model model children-of iter)
         (thereis (entry-satisfies (gtk-tree-model-get-value model i 0) model i text)))))

(defun set-filter-function (app)
  (gtk-tree-model-filter-set-visible-func
    (app-filter app)
    (lambda (model iter)
       (let ((text (gtk-entry-text (app-search-entry app)))
             (entry (gtk-tree-model-get-value model iter 0)))
         (and entry
              (or
                (entry-satisfies entry model iter text)
                (iter (for i in-tree-model model parents-of iter)
                      (thereis (entry-has-text (gtk-tree-model-get-value model i 0)
                                               text
                                               :look-at-secrets
                                               (config-search-in-secrets *config*))))))))))

(defun set-mode (app)
  (flet ((action-groups-visibility (e m)
           (setf (gtk-action-group-visible (app-actions-edit app)) e
                 (gtk-action-group-visible (app-actions-merge app)) m)))
    (if (gtk-toggle-action-active
          (gtk-action-group-get-action (app-actions-common app) "merge-mode"))
        (action-groups-visibility nil t)
        (action-groups-visibility t nil))
    (gtk-tree-view-column-queue-resize (app-column app))))

(defun on-start (app)
  (let ((default-file (or
                       (first (cli-options))
                       (config-default-file *config*))))
    (when (and default-file
               (probe-file default-file))
      (open-file app default-file)))
  (gtk-widget-grab-focus (app-search-entry app)))

(defun create-status-icon (app)
  (let ((window (app-main-window app))
        (status-icon (make-instance 'gtk-status-icon
                                    :stock "ps-pass-storage")))
    (g-signal-connect status-icon "activate"
       (lambda (status-icon)
         (declare (ignore status-icon))
         (if (gtk-widget-visible window)
             (gtk-widget-hide window)
             (gtk-widget-show window))))
    status-icon))

(defun make-icon-set (paths)
  (let ((icon-set (make-instance 'gtk-icon-set)))
    (iter (for path in paths)
          (let ((source (make-instance 'gtk-icon-source)))
            (gtk-icon-source-set-filename source (namestring path))
            (gtk-icon-set-add-source icon-set source)))
    icon-set))

(defun location-prefix ()
  (awhen (pathname-directory (directory-namestring (cl-binary-location:location)))
         (if (string-equal "bin" (car (last it)))
             (butlast it)
             it)))

(defun register-stock-icons ()
  ;; stock icons
  (let ((icons (make-hash-table :test 'equal))
        (icons-directory (make-pathname
                           :defaults
                           (directory-namestring (cl-binary-location:location))
                           :directory
                           (append '(:RELATIVE "") ; (location-prefix)
                                   '("share" "pixmaps" "pass-storage")))))
    ;; find all icons
    (fad:walk-directory
        icons-directory
        (lambda (fn)
          (push fn (gethash (pathname-name fn) icons))))

    ;; register stock icons
    (let ((factory (make-instance 'gtk-icon-factory)))
      (iter (for (icon-name files) in-hashtable icons)
            (gtk-icon-factory-add factory
                                  (concatenate 'string "ps-" icon-name)
                                  (make-icon-set files)))
      (gtk-icon-factory-add-default factory))))

(defun register-actions (app ui)
  (let ((actions-common (app-actions-common app))
        (actions-edit (app-actions-edit app))
        (actions-merge (app-actions-merge app)))

    (create-action actions-common
                   (:name "file-menu" :label "_File"))

    (create-action actions-common
                   (:name "new" :stock-id "gtk-new")
                   "<Control>n"
                   (lambda-u (cb-new app)))

    (create-action actions-common
                   (:name "open" :stock-id "gtk-open")
                   "<Control>o"
                   (lambda-u (cb-open app)))

    (create-action actions-common
                   (:name "merge-file" :label "_Merge file")
                   nil
                   (lambda-u (cb-merge-file app)))

    (create-action actions-common
                   (:name "save" :stock-id "gtk-save")
                   "<Control>s"
                   (lambda (action)
                     (declare (ignore action))
                     (cb-save app)))
    (create-action actions-common
                   (:name "save-as" :stock-id "gtk-save-as")
                   nil
                   (lambda (action)
                     (declare (ignore action))
                     (cb-save-as app)))

    (create-action actions-common
                   (:name "quit" :stock-id "gtk-quit")
                   "<Control>q"
                   (lambda (action)
                     (declare (ignorable action))
                     (format t "Action 'quit' for ~A~%" action)
                     (e-close app)))

    (create-action actions-common
                   (:name "edit-menu" :label "_Edit"))

    (create-action actions-common
                   (:name "find" :label "_Find") "<Control>f")

      (create-action actions-edit
                     (:name "copy-name" :label "Copy _name" :sensitive nil)
                     "<Control>c"
                     (lambda (action)
                       (declare (ignore action))
                       (cb-copy-name app)))

     (create-action actions-edit
                    (:name "copy-password" :label "Copy pass_word" :sensitive nil)
                    "<Control><Shift>c"
                    (lambda (action)
                      (declare (ignore action))
                      (cb-copy-password app)))

    (create-action actions-common
                   (:name "change-password" :label "Change _password")
                   nil
                   (lambda-u (cb-change-password app)))

    (create-toggle-action actions-common
                          (:name "merge-mode"
                           :label "_Merge mode"
                           :stock-id "ps-stock-merge-mode")
                          nil
                          (lambda (action)
                            (declare (ignore action))
                            (set-mode app)))

    (create-action actions-common
                   (:name "preferences" :stock-id "gtk-preferences")
                   nil
                   (lambda-u (cb-preferences app)))

    (create-action actions-common
                   (:name "entry-menu" :label "E_ntry"))

    (loop
      for class1 in (list 'entry-group
                          'entry-generic
                          'entry-creditcard
                          'entry-cryptokey
                          'entry-database
                          'entry-door
                          'entry-email
                          'entry-ftp
                          'entry-phone
                          'entry-shell
                          'entry-website)
      do (let ((class class1))
           (create-action actions-edit
                          (:name (format nil "add-~(~A~)" class)
                          :stock-id (entry-icon-by-class class)
                          :label (format nil "Add ~A"
                                         (entry-title-by-class class)))
                          nil
                          (lambda (action)
                            (declare (ignore action))
                            (cb-add-item app class)))))


    (create-action actions-edit
                   (:name "edit" :stock-id "gtk-edit" :sensitive nil)
                   nil
                   (lambda (action)
                     (declare (ignore action))
                     (cb-edit-entry app)))

    (create-action actions-edit
                   (:name "convert" :label "Convert"))

    (loop
       for class1 in (list 'entry-generic
                           'entry-creditcard
                           'entry-cryptokey
                           'entry-database
                           'entry-door
                           'entry-email
                           'entry-ftp
                           'entry-phone
                           'entry-shell
                           'entry-website)
       do
         (let ((class class1))
           (create-action actions-edit
                          (:name (format nil "convert-to-~(~A~)" class)
                                 :stock-id (entry-icon-by-class class)
                                 :label (format nil "to ~A" (entry-title-by-class class))
                                 :sensitive nil)
                          nil
                          (lambda (action)
                            (declare (ignore action))
                            (cb-convert-entry app class)))))

    (create-action actions-edit
                   (:name "delete" :stock-id "gtk-delete" :sensitive nil)
                   nil
                   (lambda (action)
                     (declare (ignore action))
                     (cb-del-entry app)))

    (create-action actions-merge
                   (:name "uncheck-all" :label "Uncheck all")
                   nil
                   (lambda-u (cb-uncheck-all app)))

    (create-action actions-merge
                   (:name "merge" :label "Merge" :stock-id "ps-stock-merge")
                   nil
                   (lambda-u (cb-merge app)))

    (create-action actions-common
                   (:name "help-menu" :label "_Help"))

    (create-action actions-common
                   (:name "about" :stock-id "gtk-about")
                   "<Control>h"
                   (lambda (action)
                     (format t "~&call cb-about  ~A~%" action)
                     (cb-about app)))

    (gtk-ui-manager-insert-action-group ui actions-common 0)
    (gtk-ui-manager-insert-action-group ui actions-edit 1)
    (gtk-ui-manager-insert-action-group ui actions-merge 2)))

(defvar *ui-info*
"<ui>
  <menubar name='menubar'>
    <menu name='file-menu' action='file-menu'>
      <menuitem name='new' action='new'/>
      <menuitem name='open' action='open'/>
      <menuitem name='save' action='save'/>
      <menuitem name='save-as' action='save-as'/>
      <separator/>
      <menuitem name='merge-file' action='merge-file'/>
      <separator/>
      <menuitem name='quit' action='quit'/>
    </menu>
    <menu name='edit-menu' action='edit-menu'>
      <menuitem name='find' action='find'/>
      <separator/>
      <menuitem name='copy-name' action='copy-name'/>
      <menuitem name='copy-password' action='copy-password'/>
      <separator/>
      <menuitem name='change-password' action='change-password'/>
      <separator/>
      <menuitem name='uncheck-all' action='uncheck-all'/>
      <menuitem name='merge-mode' action='merge-mode'/>
      <separator/>
      <menuitem name='preferences' action='preferences'/>
    </menu>
    <menu name='entry-menu' action='entry-menu'>
      <menuitem name='add-entry-group' action='add-entry-group'/>
      <separator/>
      <menuitem name='add-entry-generic' action='add-entry-generic'/>
      <menuitem name='add-entry-creditcard' action='add-entry-creditcard'/>
      <menuitem name='add-entry-cryptokey' action='add-entry-cryptokey'/>
      <menuitem name='add-entry-database' action='add-entry-database'/>
      <menuitem name='add-entry-door' action='add-entry-door'/>
      <menuitem name='add-entry-email' action='add-entry-email'/>
      <menuitem name='add-entry-ftp' action='add-entry-ftp'/>
      <menuitem name='add-entry-phone' action='add-entry-phone'/>
      <menuitem name='add-entry-shell' action='add-entry-shell'/>
      <menuitem name='add-entry-website' action='add-entry-website'/>
      <separator/>
      <menuitem name='edit' action='edit'/>
      <menu name='convert' action='convert'>
        <menuitem name='convert-to-entry-generic' action='convert-to-entry-generic'/>
        <menuitem name='convert-to-entry-creditcard' action='convert-to-entry-creditcard'/>
        <menuitem name='convert-to-entry-cryptokey' action='convert-to-entry-cryptokey'/>
        <menuitem name='convert-to-entry-database' action='convert-to-entry-database'/>
        <menuitem name='convert-to-entry-door' action='convert-to-entry-door'/>
        <menuitem name='convert-to-entry-email' action='convert-to-entry-email'/>
        <menuitem name='convert-to-entry-ftp' action='convert-to-entry-ftp'/>
        <menuitem name='convert-to-entry-phone' action='convert-to-entry-phone'/>
        <menuitem name='convert-to-entry-shell' action='convert-to-entry-shell'/>
        <menuitem name='convert-to-entry-website' action='convert-to-entry-website'/>
      </menu>
      <menuitem name='delete' action='delete'/>
      <menuitem name='merge' action='merge'/>
    </menu>
    <menu name='help-menu' action='help-menu'>
      <menuitem name='about' action='about'/>
    </menu>
  </menubar>
  <toolbar name ='toolbar'>
    <toolitem name='edit' action='edit'/>
    <toolitem name='delete' action='delete'/>
    <toolitem name='merge' action='merge'/>
    <toolitem name='merge-mode' action='merge-mode'/>
    <separator expand='true'/>
  </toolbar>
</ui>
")

(defun pass-storage ()
  (g-random-set-seed (get-universal-time))
  (load-config)
  (register-stock-icons)
  (setf (gtk-settings-gtk-shell-shows-app-menu (gtk-settings-get-default))
        nil)
  (setf (gtk-settings-gtk-shell-shows-menubar (gtk-settings-get-default))
        nil)

  (let* ((data (make-data))
         (app (make-app :data data
                        :filter
                        (make-instance 'gtk-tree-model-filter
                                       :child-model data)
                        :actions-common
                        (make-instance 'gtk-action-group
                                       :name "actions-common")
                        :actions-edit
                        (make-instance 'gtk-action-group
                                       :name "actions-edit")
                        :actions-merge
                        (make-instance 'gtk-action-group
                                       :name "actions-merge")))
         (ui (make-instance 'gtk-ui-manager)))
    ;; Register the actions
    (register-actions app ui)
    (gtk-ui-manager-add-ui-from-string ui *ui-info*)
    (gtk-ui-manager-add-ui-from-string ui
"<ui>
  <popup accelerators='true'>
    <menuitem action='copy-name'/>
    <menuitem action='copy-password'/>
    <separator/>
    <menuitem action='add-entry-group'/>
    <menuitem action='add-entry-generic'/>
    <menuitem action='add-entry-creditcard'/>
    <menuitem action='add-entry-cryptokey'/>
    <menuitem action='add-entry-database'/>
    <menuitem action='add-entry-door'/>
    <menuitem action='add-entry-email'/>
    <menuitem action='add-entry-ftp'/>
    <menuitem action='add-entry-phone'/>
    <menuitem action='add-entry-shell'/>
    <menuitem action='add-entry-website'/>
    <separator/>
    <menuitem action='edit'/>
    <menu action='convert'>
      <menuitem action='convert-to-entry-generic'/>
      <menuitem action='convert-to-entry-creditcard'/>
      <menuitem action='convert-to-entry-cryptokey'/>
      <menuitem action='convert-to-entry-database'/>
      <menuitem action='convert-to-entry-door'/>
      <menuitem action='convert-to-entry-email'/>
      <menuitem action='convert-to-entry-ftp'/>
      <menuitem action='convert-to-entry-phone'/>
      <menuitem action='convert-to-entry-shell'/>
      <menuitem action='convert-to-entry-website'/>
    </menu>
    <menuitem action='delete'/>
  </popup>
</ui>")

      (let ((main-window (make-instance 'gtk-window
                                        :title "PassStorage"
                                        :window-position :center
                                        :default-width 600
                                        :default-height 450))
            (content (make-instance 'gtk-box :orientation :vertical))
            (search-entry (make-instance 'gtk-entry
                                         :primary-icon-stock "gtk-find"
                                         :secondary-icon-stock "gtk-clear"))
            (paned (make-instance 'gtk-paned))
            (scrolled (make-instance 'gtk-scrolled-window
                                     :can-focus t
                                     :hscrollbar-policy :automatic
                                     :vscrollbar-policy :automatic
                                     :shadow-type :in))
            (view (make-instance 'gtk-tree-view
                                 :can-focus t
                                 :model (app-data app)
                                 :headers-visible nil
                                 :reorderable t
                                 :search-column 1))
            (col (make-instance 'gtk-tree-view-column
                                :sizing :autosize))
            (check-renderer (make-instance 'gtk-cell-renderer-toggle
                                           :visible nil))
            (description (make-instance 'gtk-box
                                        :orientation :vertical
                                        :width-request 40))
            (current-icon (make-instance 'gtk-image
                                         :yalign 1.0))
            (current-title (make-instance 'gtk-label
                                          :use-markup t))
            (current-description (make-instance 'gtk-label
                                                :ypad 10))
            (current-view (make-instance 'gtk-label
                                         :use-markup t
                                         :yalign 0.0))
            (statusbar (make-instance 'gtk-statusbar
                                      :has-resize-grip t)))
        ;; Add menubar from ui
        (let ((bar (gtk-ui-manager-get-widget ui "/menubar")))
          (gtk-widget-show bar)
          (gtk-widget-set-halign bar :fill)
          (gtk-box-pack-start content bar :expand nil :fill nil))
        ;; Add toolbar from ui
        (let ((bar (gtk-ui-manager-get-widget ui "/toolbar"))
              (item (make-instance 'gtk-tool-item))
              (toolbutton (make-instance 'gtk-menu-tool-button
                            :stock-id "gtk-add"
                            :label "Add entry"
                            :related-action
                            (gtk-action-group-get-action (app-actions-edit app)
                                                         "add-entry-generic")
                            :menu (make-menu (app-actions-edit app)
                                             "add-entry-group"
                                             nil
                                             "add-entry-generic"
                                             "add-entry-creditcard"
                                             "add-entry-cryptokey"
                                             "add-entry-database"
                                             "add-entry-door"
                                             "add-entry-email"
                                             "add-entry-ftp"
                                             "add-entry-phone"
                                             "add-entry-shell"
                                             "add-entry-website"))))
          (gtk-container-add item search-entry)
          (gtk-toolbar-insert bar item -1)
          (gtk-toolbar-insert bar toolbutton 0)
          (gtk-widget-show bar)
          (gtk-widget-set-halign bar :fill)
          (gtk-box-pack-start content bar :expand nil :fill nil))

        ;; Add paned to content
        (gtk-box-pack-start content paned :expand t)
        ;; Add scrolled to paned
        (gtk-paned-pack1 paned scrolled :resize nil)
        ;; Add tree view to scrolled
        (gtk-container-add scrolled view)
        ;; Add cell renderer
        (gtk-tree-view-column-pack-start col check-renderer :expand nil)
        (gtk-tree-view-append-column view col)
        (let* ((renderer (make-instance 'gtk-cell-renderer-pixbuf
                                        :stock-size 1))
               (column (gtk-tree-view-column-new-with-attributes "Name"
                                                                 renderer
                                                                 "stock-id" 2)))
          (gtk-tree-view-append-column view column))
        (let* ((renderer (make-instance 'gtk-cell-renderer-text))
               (column (gtk-tree-view-column-new-with-attributes "Name"
                                                                 renderer
                                                                 "text" 1)))
          (gtk-tree-view-column-set-expand column t)
          (gtk-tree-view-append-column view column))
        (let* ((renderer (make-instance 'gtk-cell-renderer-pixbuf
                                        :stock-size 1))
               (column (gtk-tree-view-column-new-with-attributes "Name"
                                                                 renderer
                                                                 "stock-id" 4)))
          (gtk-tree-view-append-column view column))
        ;; Add description to paned
        (gtk-paned-pack2 paned description :resize nil)
        ;; Add image and labels to the description
        (gtk-box-pack-start description current-icon)
        (gtk-box-pack-start description current-title :expand nil)
        (gtk-box-pack-start description current-description :expand nil)
        (gtk-box-pack-start description current-view)
        ;; Add statusbar to content
        (gtk-box-pack-start content statusbar :expand nil)

;     (set-filter-function app)

        (g-signal-connect (gtk-action-group-get-action (app-actions-common app) "find") "activate"
                             (lambda-u
                              (gtk-widget-grab-focus search-entry)))

        (g-signal-connect search-entry "changed"
                             (lambda-u
                              (if (string= "" (gtk-entry-text search-entry))
                                  (progn
                                    (set-status app "View filter was reset.")
                                    (setf (gtk-tree-view-model view) (app-data app))
                                    (setf (gtk-tree-view-reorderable view) t)
                                    (gtk-tree-view-collapse-all view))
                                  (progn
                                    (set-status app "View is filtered.")
                                    (setf (gtk-tree-view-model view) (app-filter app))
                                    (setf (gtk-tree-view-reorderable view) nil)
                                    (gtk-tree-model-filter-refilter (app-filter app))
                                    (gtk-tree-view-expand-all view)))
                              (listview-cursor-changed app)))

     (g-signal-connect search-entry "icon-release"
                             (lambda (entry pos event)
                               (declare (ignore event))
                               (when (eq pos :secondary)
                                 (setf (gtk-entry-text entry) "")
                                 (set-status app "View filter was reset.")
                                 (setf (gtk-tree-view-model view) (app-data app))
                                 (setf (gtk-tree-view-reorderable view) t)
                                 (gtk-tree-view-collapse-all view)
                                 (listview-cursor-changed app))))

      (g-signal-connect check-renderer "toggled"
         (lambda (renderer path)
           (declare (ignore renderer))
           (let* ((current-model (gtk-tree-view-model view))
                  (iter (gtk-tree-model-get-iter-from-string current-model path)))
             (when (and (eq current-model (app-filter app))
                        iter)
               (setf iter
                     (gtk-tree-model-filter-convert-iter-to-child-iter
                         (app-filter app)
                         iter)))
             (when iter
               (gtk-tree-model-get-value (app-data app)
                                     iter
                                     3
                                     (not (gtk-tree-model-get-value
                                              (app-data app)
                                              iter
                                              3)))))))

      (gtk-tree-view-column-set-cell-data-func
          col
          check-renderer
          (lambda (col check-renderer model iter)
            (declare (ignore col model))
            (if (gtk-toggle-action-get-active (gtk-action-group-get-action (app-actions-common app) "merge-mode"))
                ;; then
                (let ((current-model (gtk-tree-view-model view)))
                  (setf (gtk-cell-renderer-visible check-renderer)
                        (not (is-group (gtk-tree-model-get-value current-model iter 0))))
                  (setf (gtk-cell-renderer-toggle-active check-renderer)
                        (gtk-tree-model-get-value current-model iter 3)))
                ;; else
                (setf (gtk-cell-renderer-visible check-renderer) nil))))

     (setf (gtk-window-icon main-window)
           (gtk-widget-render-icon main-window "ps-pass-storage" :dialog ""))

     (g-signal-connect main-window "delete-event" (lambda-u (e-close app)))

      (g-signal-connect view "cursor-changed"
         (lambda (tree-view)
           (declare (ignore tree-view))
           (listview-cursor-changed app)))

     (g-signal-connect view "drag-motion"
                             (lambda (widget drag-context x y time)
                               (multiple-value-bind (path pos)
                                   (gtk-tree-view-get-dest-row-at-pos widget x y)

                                 (when (and path (or (eq pos :into-or-before) (eq pos :into-or-after)))
                                   (let* ((model (gtk-tree-view-model widget))
                                          (iter (gtk-tree-model-get-iter model path)))
                                     (if (is-group (gtk-tree-model-get-value model iter 0))
                                         (progn (gdk-drag-status drag-context :move time) nil)
                                         (progn (gdk-drag-status drag-context 0 time) t)))))))

      (g-signal-connect view "row-activated"
         (lambda (tree-view apath column)
           (declare (ignore tree-view apath column))
           (multiple-value-bind (iter path)
               (get-selected-iter app)
             (when iter
                   (let* ((data (app-data app))
                          (view view)
                          (entry (gtk-tree-model-get-value data iter 0)))
                     (if (is-group entry)
                         (if (gtk-tree-view-row-expanded view path)
                             (gtk-tree-view-collapse-row view path)
                             (gtk-tree-view-expand-row view path))
                         (cb-edit-entry app)))))))

     (g-signal-connect view "button-press-event"
                             (lambda (view event)
                               (when (= 3 (gdk-event-button-button event))
                                 (let ((path (gtk-tree-view-get-path-at-pos view
                                                                            (round (gdk-event-button-x event))
                                                                            (round (gdk-event-button-y event)))))
                                   (gtk-widget-grab-focus view)
                                   (when path
                                     (gtk-tree-view-set-cursor view path))
                                   (gtk-menu-popup (gtk-ui-manager-get-widget ui "/popup")
                                                   :button (gdk-event-button-button event)
                                                   :activate-time (gdk-event-button-time event)))
                                 t)))

     (g-signal-connect view "popup-menu"
                             (lambda (view)
                               (gtk-widget-grab-focus view)
                               (gtk-menu-popup (gtk-ui-manager-get-widget ui "/popup")
                                               :activate-time (gdk-event-get-time nil))
                               t))

     #+win32
     (progn
       (setf (gtk:about-dialog-global-url-hook)
             (lambda (dialog uri)
               (declare (ignore dialog))
               (win32-open-uri uri)))

       (gobject:connect-signal current-view "activate-link"
                               (lambda (label uri)
                                 (declare (ignore label))
                                 (win32-open-uri uri))))

      ;; Signal handler for the main window to handle the signal "destroy".
      (g-signal-connect main-window "destroy"
                        (lambda (widget)
                          (declare (ignore widget))
                          (leave-gtk-main)))

      (gtk-window-add-accel-group main-window
                                  (gtk-ui-manager-get-accel-group ui))

      (setf (app-main-window app) main-window)
      (setf (app-view app) view)
      (setf (app-column app) col)
      (setf (app-check-renderer app) check-renderer)
      (setf (app-current-icon app) current-icon)
      (setf (app-current-title app) current-title)
      (setf (app-current-description app) current-description)
      (setf (app-current-view app) current-view)
      (setf (app-statusbar app) statusbar)
      (setf (app-search-entry app) search-entry)

;      (on-start app)

      (let ((status-icon (create-status-icon app)))
        (gtk-container-add main-window content)
        (gtk-widget-show-all main-window)
        (on-start app)
        (set-filter-function app)
      )

;        (set-mode app)

;        (gtk-gtk-main-add-timeout 1
;                 (lambda ()
;                   (gdk-gdk-threads-enter)
;                   (on-start app)
;                   (gdk-gdk-threads-leave)
;                   nil))
;        (gtk-main)

;       (setf (gtk:status-icon-visible status-icon) nil)
;      )


)))

(defun main ()
  (within-main-loop
    (pass-storage))
  (join-gtk-main)
  (save-config))

(export 'main)

#|
(defun main-and-quit ()
  (main)
  #+sbcl(sb-ext:quit)
  #+clozure(ccl:quit))

(export 'main-and-quit)
|#

