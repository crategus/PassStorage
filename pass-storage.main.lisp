(in-package :pass-storage)

(defstruct app
  main-window
  data
  view
  current-icon
  current-title
  current-description
  current-view
  action-group
  filter
  statusbar

  filename
  password
  changed)

(defun set-status (app message)
  (let ((statusbar (app-statusbar app)))
    (gtk:statusbar-pop statusbar 0)
    (gtk:statusbar-push statusbar 0 message)))

(defun ensure-data-is-saved (app)
  (if (app-changed app)

      (case (ask-save (app-main-window app)
                      "Save changes before closing? If you don't save, changes will be permanently lost.")
        (:ok
         (cb-save app)
         t)
        (:reject
         t)
        (:cancel
         nil))

      t))

(defun e-close (app)
  (if (ensure-data-is-saved app)
      (progn
        (gtk:gtk-main-quit)
        (gtk:clipboard-clear (gtk:get-clipboard "CLIPBOARD"))
        nil)
      t))

(defun get-selected-iter (app)
  (let ((selection (gtk:tree-view-selection (app-view app))))
    (when selection

      (let ((current-model (gtk:tree-view-model (app-view app))))
        (cond

          ((eq current-model (app-data app))
           (let ((iter (gtk:tree-selection-selected selection)))
             (when iter
               (values iter
                       (gtk:tree-model-path (app-data app) iter)))))

          ((eq current-model (app-filter app))
           (let ((filter-iter (gtk:tree-selection-selected selection)))
             (when filter-iter
               (values (gtk:tree-model-filter-convert-iter-to-child-iter (app-filter app)
                                                                         filter-iter)
                       (gtk:tree-model-path (app-filter app) filter-iter))))))))))

(defun get-selected-group-iter (app)
  (let* ((data (app-data app))
         (iter (get-selected-iter app)))

    (loop
       while (and iter
                  (not (is-group (gtk:tree-model-value data iter 0))))
       do (setf iter (gtk:tree-model-iter-parent data iter)))

    iter))

(defun markup-escape-text (text)
  (with-output-to-string (str)
    (iter (for ch in-string text)
          (let ((code (char-code ch)))
            (case ch
              (#\& (format str "&amp;"))
              (#\< (format str "&lt;"))
              (#\> (format str "&gt;"))
              (#\' (format str "&apos;"))
              (#\" (format str "&quot;"))
              (t
               (if (or (<= 1 code 8)
                       (<= 11 code 12)
                       (<= 14 code 31)
                       (<= 127 code 132)
                       (<= 144 code 159))
                   (format str "&#x~X;" code)
                   (format str "~A" ch))))))))

(defun listview-cursor-changed (app)
  (let ((s (get-selected-iter app)))
    (loop
       for action in '("copy-name"
                       "copy-password"
                       "edit"
                       "delete")
       do
         (setf (gtk:action-sensitive (gtk:action-group-action (app-action-group app) action)) s))

    (let ((entry (and s (gtk:tree-model-value (app-data app) s 0))))
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
         do
           (setf (gtk:action-sensitive (gtk:action-group-action (app-action-group app) (format nil "convert-to-~(~A~)" class)))
                 (and s
                      (not (is-group entry))
                      (not (eql (find-class class) (class-of entry)))))))

    (if s
        (let ((entry (gtk:tree-store-value (app-data app) s 0)))
          (setf (gtk:image-stock (app-current-icon app))
                (entry-icon entry))

          (setf (gtk:label-label (app-current-title app))
                (format nil "<big><b>~A</b></big>"
                        (markup-escape-text (entry-name entry))))

          (setf (gtk:label-label (app-current-description app))
                (entry-description entry))

          (setf (gtk:label-label (app-current-view app))
                (with-output-to-string (str)
                  (iter (for (slot title) in (entry-slots entry))
                        (unless (or (eq slot 'name)
                                    (eq slot 'description))
                          (let ((safe-title (markup-escape-text title))
                                (safe-text (markup-escape-text (slot-value entry slot))))
                            (if (eq slot 'url)
                                (format str "<b>~A</b>: <a href='~A'>~A</a>~%" safe-title safe-text safe-text)
                                (format str "<b>~A</b>: ~A~%" safe-title safe-text))))))))
        ;; else
        (setf (gtk:image-stock (app-current-icon app)) ""
              (gtk:label-label (app-current-title app)) ""
              (gtk:label-label (app-current-description app)) ""
              (gtk:label-label (app-current-view app)) ""))))

(defun update-row (app iter entry)
  (let ((data (app-data app)))
    (setf (gtk:tree-store-value data iter 0) entry)
    (setf (gtk:tree-store-value data iter 1) (entry-name entry))
    (setf (gtk:tree-store-value data iter 2) (entry-icon entry))
    (gtk:tree-model-filter-refilter (app-filter app))))

(defun cb-add-item (app type)
  (let ((entry (make-instance type)))
    (when (edit-entry entry (app-main-window app) "Add")
      (let ((iter (gtk:tree-store-append (app-data app)
                                         (get-selected-group-iter app))))
        (update-row app iter entry)

        ;; select inserted entry
        (let* ((current-model (gtk:tree-view-model (app-view app)))
               (iter-to-select
                (cond
                  ((eq current-model (app-data app))
                   iter)
                  ((eq current-model (app-filter app))
                   (gtk:tree-model-filter-convert-child-iter-to-iter (app-filter app) iter)))))
          (when iter-to-select
            (let ((path-to-select (gtk:tree-model-path current-model iter-to-select)))
              (gtk:tree-view-expand-to-path (app-view app) path-to-select)
              (gtk:tree-view-set-cursor (app-view app) path-to-select))))

        (set-status app "New entry was added")
        (setf (app-changed app) t)))))

(defun cb-edit-entry (app)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter
      (let ((entry (gtk:tree-store-value data iter 0)))
        (when (edit-entry entry (app-main-window app) "Edit")
          (update-row app iter entry)
          (set-status app "Entry was changed")
          (setf (app-changed app) t))))))

(defun cb-convert-entry (app dest-class)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter
      (let ((entry (gtk:tree-store-value data iter 0)))
        (update-row app iter (copy-entry entry dest-class))
        (set-status app "Entry has changed type")
        (setf (app-changed app) t)
        (listview-cursor-changed app)))))

(defun cb-del-entry (app)
  (let ((iter (get-selected-iter app)))
    (when (and iter
               (ask (app-main-window app) "Do you really want to delete selected item?"))
      (let* ((data (app-data app)))
        (gtk:tree-store-remove data iter)
        (listview-cursor-changed app)
        (set-status app "Entry was deleted")
        (setf (app-changed app) t)))))

(defun load-data (app xml)
  (labels ((load-entry2 (elem parent-iter)
             (let ((type (intern (tag-get-attr elem :|type|) 'keyword))
                   (iter (gtk:tree-store-append (app-data app) parent-iter)))

               (update-row app
                           iter
                           (load-entry type elem))

               (when (eql type :|folder|)
                 (iter (for ch in (tag-children elem))
                       (parse ch iter)))))

           (parse (elem parent-iter)
             (when (is-tag elem)
               (cond
                 ;; toplevel
                 ((eq (tag-name elem) :|revelationdata|)

                  (iter (for ch in (tag-children elem))
                        (parse ch nil)))

                 ((eq (tag-name elem) :|entry|)

                  (load-entry2 elem parent-iter))))))

    (parse xml nil)))

(defmacro tree-foreach-collect (iter model parent-iter &body body)
  `(let ((,iter (if ,parent-iter
                    (gtk:tree-model-iter-first-child ,model ,parent-iter)
                    (gtk:tree-model-iter-first ,model))))
     (when ,iter
       (loop
          collect (progn ,@body)
          while (gtk:tree-model-iter-next ,model ,iter)))))

(defun save-data (app filename)
  (let ((data (app-data app)))

    (labels ((traverse (parent-iter)
               (tree-foreach-collect iter data parent-iter
                                     (let ((entry (gtk:tree-model-value data iter 0)))
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
    (gtk:tree-store-clear (app-data app))
    (setf (app-filename app) nil)
    (setf (app-password app) nil)
    (listview-cursor-changed app)
    (set-status app "New file was created")))

(defun open-file (app filename merge)
  (loop
     for password = (edit-object nil (app-main-window app) "Enter password" "ps-pass-storage"
                                 '((nil "Password" :entry :required :password)))

     while password

     do (handler-case
            (let ((xml (load-revelation-file filename (car password))))
	      (unless merge
		(gtk:tree-store-clear (app-data app)))
              (load-data app xml)
	      (unless merge
		(setf (app-filename app) filename)
		(setf (app-password app) (car password)))
              (setf (app-changed app) nil)
	      (if merge
		  (set-status app "File was merged")
		  (set-status app "File was opened"))
              (return-from open-file))
          (error (e)
            (declare (ignore e))
            (say-error (app-main-window app) "Can't open this file.")))))

(defun cb-open (app merge)
  (when (ensure-data-is-saved app)
    (let ((dlg (make-instance 'gtk:file-chooser-dialog
                              :action :open
                              :title (if merge "Merge file" "Open file")
                              :window-position :center-on-parent
                              :transient-for (app-main-window app))))

      (gtk:dialog-add-button dlg "gtk-cancel" :cancel)
      (gtk:dialog-add-button dlg "gtk-open" :ok)
      (gtk:set-dialog-alternative-button-order dlg (list :ok :cancel))
      (setf (gtk:dialog-default-response dlg) :ok)

      (when (std-dialog-run dlg)
        (open-file app (gtk:file-chooser-filename dlg) merge)))))

(defun cb-save-as (app)
  (let ((dlg (make-instance 'gtk:file-chooser-dialog
                            :action :save
                            :title "Save file"
                            :window-position :center-on-parent
                            :transient-for (app-main-window app))))

    (gtk:dialog-add-button dlg "gtk-cancel" :cancel)
    (gtk:dialog-add-button dlg "gtk-save" :ok)
    (gtk:set-dialog-alternative-button-order dlg (list :ok :cancel))
    (setf (gtk:dialog-default-response dlg) :ok)

    (when (std-dialog-run dlg)
      (setf (app-filename app) (gtk:file-chooser-filename dlg))
      (save-data app (gtk:file-chooser-filename dlg)))))

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
                 (search-in-secrets "Search in secrets (passwords)" :boolean))))

(defun cb-copy-name (app)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter

      (let* ((entry (gtk:tree-store-value data iter 0))
             (name (entry-get-name entry)))
        (when name

          (gtk:clipboard-set-text
           (gtk:get-clipboard "CLIPBOARD")
           name)
          (set-status app "Name was copied to clipboard"))))))

(defun cb-copy-password (app)
  (let ((data (app-data app))
        (iter (get-selected-iter app)))
    (when iter

      (let* ((entry (gtk:tree-store-value data iter 0))
             (password (entry-get-password entry)))
        (when password

          (gtk:clipboard-set-text
           (gtk:get-clipboard "CLIPBOARD")
           password)
          (set-status app "Secret (password) was copied to clipboard"))))))

(defun cb-about (app)
  (let ((dlg (make-instance 'gtk:about-dialog
                            :window-position :center-on-parent
                            :transient-for (app-main-window app)
                            :authors '("Andrey Kutejko <andy128k@gmail.com>")
                            :copyright "Copyright 2009, 2010, Andrey Kutejko"
                            :logo (gtk:widget-render-icon (app-main-window app) "ps-pass-storage" :dialog "")
                            :program-name "PassStorage"
                            :version "0.10.5.20"
                            :website "http://andy128k.github.com/PassStorage")))
    (gtk:dialog-run dlg)
    (gtk:object-destroy dlg)))

(defun make-menu (group &rest actions)
  (let ((menu (make-instance 'gtk:menu)))
    (iter (for action in actions)
          (if action
              (gtk:menu-shell-append menu (gtk:action-create-menu-item (gtk:action-group-action group action)))
              (gtk:menu-shell-append menu (make-instance 'gtk:menu-item :visible t))))
    menu))

(defun make-icon-set (paths)
  (let ((set (make-instance 'gtk:icon-set)))
    (iter (for path in paths)
          (let ((source (make-instance 'gtk:icon-source)))
            (setf (gtk:icon-source-filename source) (namestring path))
            (gtk:icon-set-add-source set source)))
    set))

(defmacro lambda-u (&body body)
  (let ((p (gensym)))
    `(lambda (&rest ,p)
       (declare (ignore ,p))
       (handler-case
           (progn ,@body)
         (error (c)
           ;; TODO: use parent-window
           (say-error nil (format nil "~A" c)))))))

(defmacro create-action (group action-params &optional accel func)
  `(let ((action (make-instance 'gtk:action ,@action-params)))
     (gtk:action-group-add-action ,group action :accelerator ,accel)
     ,(when func
            `(gobject:connect-signal action "activate" ,func))))

(defun entry-icon-by-class (class)
  (let ((dummy-entry (make-instance class)))
    (entry-icon dummy-entry)))

(defun entry-title-by-class (class)
  (let ((dummy-entry (make-instance class)))
    (entry-title dummy-entry)))

;; search
(defgeneric entry-satisfies (entry model iter text))

(defmethod entry-satisfies ((entry entry-group) model iter text)
  (or
   (entry-has-text entry
                   text
                   :look-at-secrets (config-search-in-secrets *config*))

   (iter (for i in-tree-model model children-of iter)
         (thereis (entry-satisfies (gtk:tree-store-value model i 0) model i text)))))

(defmethod entry-satisfies (entry model iter text)
  (entry-has-text entry
                  text
                  :look-at-secrets (config-search-in-secrets *config*)))

(defun location-prefix ()
  (let ((path (pathname-directory (directory-namestring (cl-binary-location:location)))))
    (if (and path
             (string-equal "bin" (car (last path))))
        (butlast path)
        path)))

(defun main ()

  (glib:random-set-seed (get-universal-time))

  (load-config)

  ;; stock icons
  (let ((icons (make-hash-table :test 'equal))
        (icons-directory (make-pathname
                          :defaults (directory-namestring (cl-binary-location:location))
                          :directory (append (location-prefix) '("share" "pixmaps" "pass-storage")))))
    ;; find all icons
    (fad:walk-directory
     icons-directory
     (lambda (fn)
       (push fn (gethash (pathname-name fn) icons))))

    ;; register stock icons
    (let ((factory (make-instance 'gtk:icon-factory)))
      (iter (for (icon-name files) in-hashtable icons)
            (gtk:icon-factory-add factory
                                  (concatenate 'string "ps-" icon-name)
                                  (make-icon-set files)))

      (gtk:icon-factory-add-default factory)))

  (let* ((data (make-instance 'gtk:tree-store :column-types '("GObject" "gchararray" "gchararray")))
         (app (make-app
               :data data
               :filter (make-instance 'gtk:tree-model-filter :child-model data)
               :action-group (make-instance 'gtk:action-group :name "action-group")))
         (ui (make-instance 'gtk:ui-manager)))

    (let ((action-group (app-action-group app)))
      (create-action action-group (:name "file-menu" :label "_File"))
      (create-action action-group (:name "new" :stock-id "gtk-new") "<Control>n" (lambda-u (cb-new app)))
      (create-action action-group (:name "open" :stock-id "gtk-open") "<Control>o" (lambda-u (cb-open app nil)))
      (create-action action-group (:name "merge" :label "_Merge") nil (lambda-u (cb-open app t)))
      (create-action action-group (:name "save" :stock-id "gtk-save") "<Control>s" (lambda-u (cb-save app)))
      (create-action action-group (:name "save-as" :stock-id "gtk-save-as") nil (lambda-u (cb-save-as app)))
      (create-action action-group (:name "quit" :stock-id "gtk-quit") "<Control>q" (lambda-u (e-close app)))

      (create-action action-group (:name "edit-menu" :label "_Edit"))
      (create-action action-group (:name "find" :label "_Find") "<Control>f")
      (create-action action-group (:name "copy-name" :label "Copy _name" :sensitive nil) "<Control>c" (lambda-u (cb-copy-name app)))
      (create-action action-group (:name "copy-password" :label "Copy pass_word" :sensitive nil) "<Control><Shift>c" (lambda-u (cb-copy-password app)))
      (create-action action-group (:name "change-password" :label "Change _password") nil (lambda-u (cb-change-password app)))
      (create-action action-group (:name "preferences" :stock-id "gtk-preferences") nil (lambda-u (cb-preferences app)))

      (create-action action-group (:name "entry-menu" :label "E_ntry"))

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
         do
           (let ((class class1))
             (create-action action-group
                            (:name (format nil "add-~(~A~)" class)
                                   :stock-id (entry-icon-by-class class)
                                   :label (format nil "Add ~A" (entry-title-by-class class)))
                            nil
                            (lambda-u (cb-add-item app class)))))

      (create-action action-group (:name "edit" :stock-id "gtk-edit" :sensitive nil) nil (lambda-u (cb-edit-entry app)))
      (create-action action-group (:name "convert" :label "Convert"))

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
             (create-action action-group
                            (:name (format nil "convert-to-~(~A~)" class)
                                   :stock-id (entry-icon-by-class class)
                                   :label (format nil "to ~A" (entry-title-by-class class))
                                   :sensitive nil)
                            nil
                            (lambda-u (cb-convert-entry app class)))))

      (create-action action-group (:name "delete" :stock-id "gtk-delete" :sensitive nil) nil (lambda-u (cb-del-entry app)))

      (create-action action-group (:name "help-menu" :label "_Help"))
      (create-action action-group (:name "about" :stock-id "gtk-about") nil (lambda-u (cb-about app)))

      (gtk:ui-manager-insert-action-group ui action-group 0))

    (gtk:ui-manager-add-ui-from-string ui
                                       "<ui>
  <menubar name='menubar'>
    <menu action='file-menu'>
      <menuitem action='new'/>
      <menuitem action='open'/>
      <menuitem action='merge'/>
      <menuitem action='save'/>
      <menuitem action='save-as'/>
      <separator/>
      <menuitem action='quit'/>
    </menu>
    <menu action='edit-menu'>
      <menuitem action='find'/>
      <separator/>
      <menuitem action='copy-name'/>
      <menuitem action='copy-password'/>
      <separator/>
      <menuitem action='change-password'/>
      <separator/>
      <menuitem action='preferences'/>
    </menu>
    <menu action='entry-menu'>
      <menuitem action='add-entry-group'/>
      <separator/>
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
    </menu>
    <menu action='help-menu'>
      <menuitem action='about'/>
    </menu>
  </menubar>
  <toolbar name='toolbar'>
    <toolitem action='edit'/>
    <toolitem action='delete'/>
  </toolbar>
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

    (gtk:let-ui

     (gtk:gtk-window
      :var main-window
      :title "PassStorage"
      :window-position :center
      :default-width 600
      :default-height 450
      (gtk:v-box

       (:expr (gtk:ui-manager-widget ui "/menubar"))
       :expand nil
       :position 0

       (:expr (gtk:ui-manager-widget ui "/toolbar"))
       :expand nil
       :position 1

       (gtk:h-box
        :border-width 4
        :spacing 8
        (gtk:label :label "Find:")
        :expand nil
        (gtk:entry
         :var search-entry)
        :expand nil)
       :expand nil
       :position 2

       (gtk:h-paned
        (gtk:scrolled-window
         :can-focus t
         :hscrollbar-policy :automatic
         :vscrollbar-policy :automatic
         :shadow-type :in
         (gtk:tree-view
          :var view
          :can-focus t
          :model (app-data app)
          :headers-visible nil
          :reorderable t
          :search-column 1
	  (gtk:tree-view-column
	   :var col
	   :sizing :autosize
	   (gtk:cell-renderer-pixbuf
	    :stock-size 1)
	   :expand nil
	   :attribute ("stock-id" 2)
           (gtk:cell-renderer-text)
	   :expand t
	   :attribute ("text" 1))))
	(gtk:v-box
         :width-request 40
         (gtk:image
          :yalign 1.0
          :var current-icon)
         (gtk:label
          :use-markup t
          :var current-title)
         :expand nil
         (gtk:label
          :ypad 10
          :var current-description)
         :expand nil
         (gtk:label
          :use-markup t
          :yalign 0.0
          :var current-view))
        :resize nil)
       :position 3

       (gtk:statusbar
        :var statusbar
        :has-resize-grip t)
       :expand nil
       :position 4))

     (gtk:toolbar-insert (gtk:ui-manager-widget ui "/toolbar")
                         (make-instance 'gtk:menu-tool-button
                                        :stock-id "gtk-add"
                                        :label "Add entry"
                                        :related-action (gtk:action-group-action (app-action-group app) "add-entry-generic")
                                        :menu (apply #'make-menu
                                                     (list
                                                      (app-action-group app)
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
                                                      "add-entry-website")))
			 0)

     (gtk:tree-model-filter-set-visible-function (app-filter app)
                                                 (lambda (model iter)
                                                   (when iter
                                                     (let ((entry (gtk:tree-store-value model iter 0)))
                                                       (when entry
                                                         (entry-satisfies entry model iter (gtk:entry-text search-entry)))))))

     (gobject:connect-signal (gtk:action-group-action (app-action-group app) "find") "activate"
                             (lambda-u
                              (gtk:widget-grab-focus search-entry)))

     (gobject:connect-signal search-entry "changed"
                             (lambda-u
                              (if (string= "" (gtk:entry-text search-entry))
                                  (progn
                                    (set-status app "View filter was reset.")
                                    (setf (gtk:tree-view-model (app-view app)) (app-data app))
                                    (setf (gtk:tree-view-reorderable (app-view app)) t)
                                    (gtk:tree-view-collapse-all (app-view app)))
                                  (progn
                                    (set-status app "View is filtered.")
                                    (setf (gtk:tree-view-model (app-view app)) (app-filter app))
                                    (setf (gtk:tree-view-reorderable (app-view app)) nil)
                                    (gtk:tree-model-filter-refilter (app-filter app))
                                    (gtk:tree-view-expand-all (app-view app))))
			      (listview-cursor-changed app)))

     (setf (gtk:gtk-window-icon main-window)
	   (gtk:widget-render-icon main-window "ps-pass-storage" :dialog ""))

     (setf (app-main-window app) main-window)
     (setf (app-view app) view)
     (setf (app-current-icon app) current-icon)
     (setf (app-current-title app) current-title)
     (setf (app-current-description app) current-description)
     (setf (app-current-view app) current-view)
     (setf (app-statusbar app) statusbar)

     (gtk:widget-grab-focus search-entry))

    (gobject:connect-signal (app-main-window app) "delete-event" (lambda-u (e-close app)))

    (gobject:connect-signal (app-view app) "cursor-changed" (lambda-u (listview-cursor-changed app)))
    (gobject:connect-signal (app-view app) "drag-motion"
                            (lambda (widget drag-context x y time)
                              (multiple-value-bind (path pos)
                                  (gtk:tree-view-get-dest-row-at-pos widget x y)

                                (when (and path (or (eq pos :into-or-before) (eq pos :into-or-after)))
                                  (let* ((model (gtk:tree-view-model widget))
                                         (iter (gtk:tree-model-iter-by-path model path)))

                                    (if (is-group (gtk:tree-store-value model iter 0))
                                        (progn (gdk:gdk-drag-status drag-context :move time) nil)
                                        (progn (gdk:gdk-drag-status drag-context 0 time) t)))))))

    (gobject:connect-signal (app-view app) "row-activated"
                            (lambda-u
                             (multiple-value-bind (iter path)
                                 (get-selected-iter app)

                               (when iter

                                 (let* ((data (app-data app))
                                        (view (app-view app))
                                        (entry (gtk:tree-store-value data iter 0)))

                                   (if (is-group entry)
                                       (if (gtk:tree-view-row-expanded-p view path)
                                           (gtk:tree-view-collapse-row view path)
                                           (gtk:tree-view-expand-row view path))
                                       (cb-edit-entry app)))))))

    (gobject:connect-signal (app-view app) "button-press-event"
                            (lambda (view event)
                              (when (= 3 (gdk:event-button-button event))
                                (let ((path (gtk:tree-view-get-path-at-pos view
                                                                           (round (gdk:event-button-x event))
                                                                           (round (gdk:event-button-y event)))))
                                  (gtk:widget-grab-focus view)
                                  (when path
                                    (gtk:tree-view-set-cursor view path))
                                  (gtk:menu-popup (gtk:ui-manager-widget ui "/popup")
                                                  :button (gdk:event-button-button event)
                                                  :activate-time (gdk:event-button-time event)))
                                t)))

    (gobject:connect-signal (app-view app) "popup-menu"
                            (lambda (view)
			      (gtk:widget-grab-focus view)
			      (gtk:menu-popup (gtk:ui-manager-widget ui "/popup")
					      :activate-time (gdk:event-get-time nil))
			      t))

    (gtk:gtk-window-add-accel-group (app-main-window app) (gtk:ui-manager-accel-group ui))

    (gtk:widget-show (app-main-window app))

    (let ((default-file (or
                         (first (cli-options))
                         (config-default-file *config*))))
      (when (and default-file (probe-file default-file))
        (gtk:gtk-main-add-timeout 1
                                  (lambda ()
                                    (gdk:gdk-threads-enter)
                                    (open-file app default-file nil)
                                    (gdk:gdk-threads-leave)
                                    nil))))

    (gtk:gtk-main)

    (save-config)))

(export 'main)

(defun main-and-quit ()
  (main)
  #+sbcl(sb-ext:quit)
  #+clozure(ccl:quit))

(export 'main-and-quit)

