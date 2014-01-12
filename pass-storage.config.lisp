(in-package :pass-storage)

(defstruct config
  default-file
  search-in-secrets
  show-secrets-on-preview)

(defvar *config* nil)

(defun config-path ()
  (pathname (g-build-filename (g-get-user-config-dir) "PassStorage.conf")))

(defun load-config ()
  (let ((*package* (find-package :pass-storage)))
    (with-open-stream (stream (open (config-path) :direction :input
                                                  :if-does-not-exist nil))
      (setf *config*
            (if stream
                (read stream)
                (make-config))))))

(defun save-config ()
  (with-open-stream (stream (open (config-path) :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create))
    (format stream "~S" *config*)))

