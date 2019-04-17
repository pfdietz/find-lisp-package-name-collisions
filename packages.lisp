;-*- Mode:     Lisp -*-

(in-package :cl-user)

;;;
;;; Simple utility to find package name and nickname collisions
;;; between different quicklisp projects, or between projects and builtin
;;; packages of various lisp implementations.
;;;
;;; Use: (find-and-print-collisions <quicklisp dist /software dir> <output file name>)
;;;
;;; Example: (find-and-print-collisions "~/quicklisp/dists/quicklisp/software" "collisions")
;;;
;;; Because this does not (and cannot) load all the projects, it may not be able to
;;; read all the lisp or asd files without error, so it may miss some packages.
;;;

(defparameter *standard-packages*
  '("COMMON-LISP-USER" "CL-USER" "COMMON-LISP" "CL" "KEYWORD"))

(defparameter *sbcl-packages*
  '("SB-LOOP" "SB-IMPL" "SB-DI" "SB-SEQUENCE" "SEQUENCE" "SB-C"
    "SB-X86-64-ASM" "SB-VM" "SB-FORMAT" "SB-DISASSEM" "SB-EXT" "SB-PROFILE"
    "SB-REGALLOC" "SB-EVAL" "SB-ALIEN-INTERNALS" "SB-UNIX" "SB-THREAD" "SB-DEBUG"
    "SB-ASSEM" "SB-SYS" "SB-GRAY" "SB-APROF" "SB-ALIEN" "SB-FASL" "SB-UNICODE"
    "SB-MOP" "SB-PRETTY" "SB-INT" "SB-KERNEL" "SB-WALKER"
    "SB-PCL" "SB-BIGNUM"))

(defparameter *clisp-packages*
  '("GSTREAM" "GRAY" "I18N" "SOCKET" "SCREEN" "CUSTOM" "EXT"
    "CLOS" "MOP" "CS-COMMON-LISP-USER" "CS-CL-USER" "CS-USER"
    "CS-COMMON-LISP" "CS-CL" "CS-LISP" "CHARSET" "SYSTEM" "SYS"
    "USER" "LISP" "EXPORTING" "POSIX" "OS" "REGEXP"))

(defparameter *abcl-packages*
  '("SEQUENCE" "PRECOMPILER" "PRE" "XP" "FORMAT" "THREADS" "LISP" "JAVA" "PROFILER"
    "PROF" "LOOP" "JVM" "EXTENSIONS" "EXT" "TOP-LEVEL" "TPL" "MOP" "SYSTEM"
    "SYS"))

(defparameter *ecl-packages*
  '("WALKER" "ECL-CDB" "FFI" "GRAY" "MP" "MULTIPROCESSING"
    "CLOS" "MOP" "C" "COMPILER" "SI" "SYS" "SYSTEM" "EXT"))

(defparameter *ccl-packages*
  '("OPENMCL-SOCKET" "OPENMCL-MOP" "ANSI-LOOP" "SwinkIO/0"
    "SWINK" "INSPECTOR" "ARCH" "X86" "X8632" "GRAY" "CCL"
    "X8664" "TARGET" "X86-LINUX64" "OS"))

(defparameter *cmucl-packages*
  '("PCL" "XREF" "PROFILE" "INSPECT" "CLOS-MOP" "MOP"
    "UNICODE" "HEMLOCK" "ED" "WIRE" "NEW-ASSEM"
    "ANSI-LOOP" "XLIB" "MULTIPROCESSING" "MP" "DFIXNUM"
    "DISASSEM" "FORMAT" "HEMLOCK-INTERNALS" "HI" "PRETTY-PRINT" "PP"
    "BIGNUM" "C-CALL" "STREAM" "WALKER" "ALIEN" "CONDITIONS" "EVAL"
    "FWRAPPERS" "INTL" "DEBUG" "ALIEN-INTERNALS" "C" "COMPILER"
    "OLD-C" "SYSTEM" "SYS" "UNIX" "X86" "VM" "DEBUG-INTERNALS"
    "DI" "KERNEL" "EXTENSIONS" "EXT" "LISP"))

;;; Find the package names defined in quicklisp systems, looking for collisions

(defun read-exprs-from-stream (s)
  "Read exprs from stream until all are done or there is an irrecoverable error.  Return the list of exprs."
  (assert (streamp s))
  (let ((forms nil)
        (*read-eval* nil))
    (handler-case
        ;; Skip over forms that cause read errors
        (loop for x = (handler-case (read s nil s) (reader-error () nil))
           until (eql x s)
           do (when x (push x forms)))
      (error () nil))
    (nreverse forms)))

(defun read-exprs-from-file (fn)
  (with-open-file (s fn :direction :input)
    (read-exprs-from-stream s)))

(defun package-defs-in-exprs (list)
  (loop for x in list
     when (and (consp x) (eql (car x) 'defpackage))
     append (package-names-defined-in-defpackage-form x)))

(defun proper-listp (x)
  (loop while (consp x) do (pop x))
  (null x))
  
(defun package-names-defined-in-defpackage-form (dpf)
  (assert (proper-listp dpf))
  (assert (eql (car dpf) 'defpackage))
  (let ((result nil))
    (flet ((%collect (s) (push (string s) result)))
      (%collect (cadr dpf))
      (loop for option in (cddr dpf)
         when (and (consp option)
                   (eql (car option) :nicknames))
         do (mapc #'%collect (cdr option))))
    result))

(defun package-names-defined-in-file (file)
  (let ((exprs (read-exprs-from-file file)))
    (package-defs-in-exprs exprs)))

(defun lisp-files-under-dir (dir)
  (let* ((exts '("lisp" "lsp" "cl" "asd"))
         (directories (loop for ext in exts
                         collect (format nil "~A/**/*.~A"
                                         dir ext)))
         (files (reduce #'append (mapcar #'directory directories)
                        :initial-value nil)))
    files))

(defun package-names-defined-in-files (dir &key impl-packages)
  ;; Must normalize DIR in case it had symbolic links
  (setf dir (namestring (car (directory dir))))
  (setf dir (string-right-trim "/" dir))
  (assert (not (string= dir "")))
  (let ((files (lisp-files-under-dir dir))
        (table (make-hash-table :test 'equal)))
    ;; TABLE maps package names to a list of pathnames or keywords
    ;; representing lisp implementations.  When IMPL-PACKAGES is true,
    ;; populate the table with the standard packages and those
    ;; defined in any of several implementations.
    (when impl-packages
      (flet ((%r (impl-packages impl-name)
               (assert (keywordp impl-name))
               (dolist (p impl-packages)
                 (push impl-name (gethash p table)))))
        (%r *standard-packages* :ansi-cl)
        (%r *sbcl-packages* :sbcl)
        (%r *clisp-packages* :clisp)
        (%r *abcl-packages* :abcl)
        (%r *ecl-packages* :ecl)
        (%r *ccl-packages* :ccl)
        (%r *cmucl-packages* :cmucl)))
    (loop for file in files
       for package-names = (package-names-defined-in-file file)
       do (loop for p in package-names
             do (push file (gethash p table))))
    (let ((collisions nil)
          (dir-len (1+ (length dir))))
      ;; Now find all collisions
      (maphash
       (lambda (package-name files-or-impls)
         (when (and (find-if #'pathnamep files-or-impls)
                    (> (length files-or-impls) 1))
           ;; Strip off the directory from the file names
           (let ((reduced-file-names
                  (mapcar (lambda (pathname-or-impl)
                            (if (pathnamep pathname-or-impl)
                                (subseq (namestring pathname-or-impl) dir-len)
                                pathname-or-impl))
                          files-or-impls)))
             (push (list package-name reduced-file-names) collisions))))
       table)
      (setf collisions (sort collisions #'string< :key #'car))
      collisions)))

(defun root-dir (str)
  (if (stringp str)
      (let ((pos (position #\/ str)))
        (if pos
            (subseq str 0 pos)
            str))
      str))

(defun filter-collisions (collisions)
  "Remove collisions that are all in the same ql system,
   and not in any implementation"
  (loop for collision in collisions
       when
       (let* ((roots (mapcar #'root-dir (cadr collision)))
              (unique-roots (remove-duplicates roots :test #'equal)))
         (> (length unique-roots) 1))
       collect collision))

(defun print-collisions-to-file (cols filename)
  (with-open-file (s filename :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (loop for (pn locs) in cols
       do (format s "~A:~%" pn)
       do (dolist (fn-or-impl locs)
            (format s "  ~A~%" fn-or-impl)))))

(defun find-and-print-collisions (dir output-file)
  (print-collisions-to-file
   (filter-collisions
    (package-names-defined-in-files dir))
   output-file))
