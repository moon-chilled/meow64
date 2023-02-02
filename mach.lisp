(cl:in-package #:meow64)

(defmacro define-some-structure ((structure maker) &rest fields)
  (loop with extra-slots = (mapcar 'car (remove-if-not (lambda (y) (eq :extra (cadr y))) fields))
        for (name orepr obytes . irepr) in (remove-if (lambda (y) (eq :extra (cadr y))) fields)
        for integerp = (member orepr '(:signed :unsigned))
        for stringp = (eq orepr :string)
        for constp = (and (consp irepr) (eq :const (car irepr)))
        for binder = (gensym (symbol-name name))
        for grab-bytes = `(prog1
                              ,(if integerp
                                   `(+ ,@(loop for j below obytes 
                                               collect `(ash (aref array (+ i ,j))
                                                             (ash ,j 3))))
                                   `(make-array
                                     ,obytes
                                     :element-type 'character
                                     :initial-contents (list ,@(loop for j below obytes
                                                                     collect `(code-char
                                                                               (aref array (+ i ,j)))))))
                            (incf i ,obytes))
        for translate-bytes = (ecase orepr
                                ((:unsigned :string) grab-bytes)
                                (:signed  `(let ((tmp ,grab-bytes))
                                            (logior tmp (- (mask-field ,(byte 1 (1- (ash obytes 3))) tmp))))))
        unless constp
          collect name into fields
        collect `(,binder ,translate-bytes) into bindings
        if constp collect `(equal ,binder ,(cadr irepr)) into assertions
        else collect (cond
                       ((null irepr) binder)
                       ((eq :enum (car irepr))
                        `(cond
                           ,@(loop for (v k) on (cdr irepr) by 'cddr
                                   collect `((equal ,binder ,k) ,v))
                           (t (error "unrecognised enumeration value ~a for ~a" ,binder ',name))))
                       (t (error "unrecognised irepr")))
               into initialisers
        finally
           (return
             `(progn
                (defstruct ,structure ,@fields ,@extra-slots)
                (defun ,maker (array &optional (i 0))
                  (let ,bindings
                    (assert (and ,@assertions))
                    (values
                     (,(intern (format nil "MAKE-~a" structure))
                      ,@(loop for f in fields for i in initialisers
                              collect (intern (symbol-name f) :keyword)
                              collect i))
                     i)))))))

(macrolet ((define-header (struct parse magic &rest extra)
             `(define-some-structure (,struct ,parse)
                (magic :unsigned 4 :const ,magic)
                ;; declared as signed in header but meh
                (cputype :unsigned 4 :enum :x86 7
                                           :amd64 #x01000007
                                           :arm64 #x0100000c)
                ;; ditto
                ;; this is actually a bitmask thingy kinda
                (cpusubtype :unsigned 4)
                ;; don't care about others yet
                (filetype :unsigned 4 :enum :executable 2
                                            :dylib 6)
                (ncmds :unsigned 4)
                (sizeofcmds :unsigned 4)
                (flags :unsigned 4)
                ,@extra)))
  (define-header mach-header-32 parse-mach-header-32 #xfeedface)
  (define-header mach-header-64 parse-mach-header-64 #xfeedfacf (reserved :unsigned 4)))
                  
(define-some-structure (load-command-header parse-load-command-header)
  (cmd :unsigned 4 :enum :segment            #x00000001
                         :symtab             #x00000002
                         :symseg             #x00000003
                         :thread             #x00000004
                         :unixthread         #x00000005
                         :loadfvmlib         #x00000006
                         :idfvmlib           #x00000007
                         :ident              #x00000008
                         :fvmfile            #x00000009
                         :prepage            #x0000000a
                         :dysymtab           #x0000000b
                         :load-dylib         #x0000000c
                         :id-dylib           #x0000000d
                         :load-dylinker      #x0000000e
                         :id-dylinker        #x0000000f
                         :prebound-dylib     #x00000010
                         :routines           #x00000011
                         :uuid               #x0000001b
                         :rpath              #x8000001c
                         :dyld-info-only     #x80000022
                         :version-min-macosx #x00000024
                         :function-starts    #x00000026
                         :main               #x80000028
                         :data-in-code       #x00000029
                         :source-version     #x0000002a
                         )
  (cmdsize :unsigned 4))

(macrolet ((define-segment-command (struct parse size)
             `(define-some-structure (,struct ,parse)
                (segname :string 16)
                (vmaddr :unsigned ,size)
                (vmsize :unsigned ,size)
                (fileoff :unsigned ,size)
                (filesize :unsigned ,size)
                ;; bitmask: 124 rwx.  (Why aren't r and x swapped?  idfk.)  Again, declared as signed in source
                (maxprot :unsigned 4)
                (initprot :unsigned 4)
                (nsects :unsigned 4)
                (flags :unsigned 4)
                (sections :extra))))
  (define-segment-command segment-command-32 parse-segment-command-32 4)
  (define-segment-command segment-command-64 parse-segment-command-64 8))

(macrolet ((define-section (struct parse size)
             `(define-some-structure (,struct ,parse)
                (sectname :string 16)
                (segname :string 16)
                (addr :unsigned ,size)
                (size :unsigned ,size)
                (offset :unsigned 4)
                (align :unsigned 4)
                (reloff :unsigned 4)
                (nreloc :unsigned 4)
                (type :unsigned 1 :enum :regular 0
                                        :zerofill 1
                                        :cstring-literals 2
                                        :4byte-literals 3
                                        :8byte-literals 4
                                        :literal-pointers 5
                                        :non-lazy-symbol-pointers 6
                                        :lazy-symbol-pointers 7
                                        :symbol-stubs 8
                                        :mod-init-func-pointers 9
                                        :mod-term-func-pointers 10
                                        :coalesced 11
                                        :gb-zerofill 12
                                        :interposing 13
                                        :16byte-literals 14
                                        :dtrace-dof 15
                                        :lazy-dylib-symbol-pointers 16
                                        :tls-regular 17
                                        :tls-zerofill 18
                                        :tls-local-variables 19
                                        :tls-local-variables-pointers 20
                                        :tls-init-function-pointers 21
                                        :init-func-offsets 22)
                ;; bitfield
                (attributes :unsigned 3)
                (reserved1 :unsigned 4)
                (reserved2 :unsigned 4)
                ,@(if (= size 8) '((reserved3 :unsigned 4)) ())
                (data :extra))))
  (define-section section-32 parse-section-32 4)
  (define-section section-64 parse-section-64 8))

(define-some-structure (macho-dylib parse-macho-dylib)
  (offset :unsigned 4)
  (timestamp :unsigned 4)
  (current-version :unsigned 4)
  (compatibility-version :unsigned 4)
  (path-name :extra))

(define-some-structure (macho-entry-point-command parse-macho-entry-point-command)
  (entryoff :unsigned 8)
  (stacksize :unsigned 8))

(define-some-structure (macho-dylinker-command parse-macho-dylinker-command)
  (offset :unsigned 4)
  (name :extra))

(defun parse-segment-32 (array i)
  (loop with (segment-header i) = (multiple-value-list (parse-segment-command-32 array i))
        repeat (segment-command-32-nsects segment-header)
        for (sect ni) = (multiple-value-list (parse-section-32 array i))
        do (setf i ni)
        collect sect into sects
        finally (setf (segment-command-32-sections segment-header) sects)
                (return segment-header)))

(defun read-nul-terminated-string (array i)
  (loop for j from i until (zerop (aref array j))
        finally (return (map 'string 'code-char (subseq array i j)))))

(defun parse-structure-with-names (struct-constructor array i &rest fs)
  (loop with res = (funcall struct-constructor array i)
        for (get-offset set-string) on fs by 'cddr
        ;; offset is from the start of load command, but i starts right after it; it is 8 bytes, so correct for that
        do (funcall set-string (read-nul-terminated-string array (+ (funcall get-offset res) i -8)) res)
        finally (return res)))

(defstruct macho32-file segments loaded-dylibs entry-point dylinker)

(defun parse-macho-32 (array)
  (multiple-value-bind (header i) (parse-mach-header-32 array)
    (loop repeat (mach-header-32-ncmds header)
          with entry-point and dylinker
          for (lch ni) = (multiple-value-list (parse-load-command-header array i))
          if (eq :segment (load-command-header-cmd lch))
            collect (parse-segment-32 array ni) into segments
          else if (eq :load-dylib (load-command-header-cmd lch))
            collect (parse-structure-with-names 'parse-macho-dylib array ni 'macho-dylib-offset #'(setf macho-dylib-path-name)) into loaded-dylibs
          else if (eq :load-dylinker (load-command-header-cmd lch))
            do (assert (null dylinker))
               (setf dylinker (parse-structure-with-names 'parse-macho-dylinker-command array ni 'macho-dylinker-command-offset #'(setf macho-dylinker-command-name)))
          else if (eq :main (load-command-header-cmd lch))
            do (assert (null entry-point))
               (setf entry-point (parse-macho-entry-point-command array ni))
          else
            collect (load-command-header-cmd lch) into extras
          do (incf i (load-command-header-cmdsize lch))
          finally (return (values (make-macho32-file :segments segments
                                                     :loaded-dylibs loaded-dylibs
                                                     :entry-point entry-point
                                                     :dylinker dylinker)
                                  extras)))))
    
