(cl:in-package #:meow64-macho)

;; https://github.com/aidansteele/osx-abi-macho-file-format-reference
;; http://networkpx.blogspot.com/2009/09/about-lcdyldinfoonly-command.html
;; and various apple sources/headers

(defmacro define-some-structure (structure &rest fields &aux (maker (intern (format nil "PARSE-~a" structure))))
  (loop with extra-slots = (mapcar 'car (remove-if-not (lambda (y) (eq :extra (cadr y))) fields))
        for (name orepr obytes . irepr) in (remove-if (lambda (y) (eq :extra (cadr y))) fields)
        for integerp = (member orepr '(:signed :unsigned))
        for stringp = (eq orepr :string)
        for constp = (and (consp irepr) (eq :const (car irepr)))
        for binder = (gensym (symbol-name name))
        for grab-bytes =(if integerp
                            `(+ ,@(loop for j below obytes 
                                        collect `(ash (aref array (+ i ,(+ j struct-size)))
                                                      ,(ash j 3))))
                            `(make-array
                              ,obytes
                              :element-type 'character
                              :initial-contents (list ,@(loop for j below obytes
                                                              collect `(code-char
                                                                        (aref array (+ i ,(+ j struct-size))))))))
        for translate-bytes = (ecase orepr
                                ((:unsigned :string) grab-bytes)
                                (:signed `(let ((tmp ,grab-bytes))
                                            (logior tmp (- (mask-field ',(byte 1 (1- (ash obytes 3))) tmp))))))
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
        sum obytes into struct-size
        finally
           (return
             `(progn
                (defstruct ,structure ,@fields ,@extra-slots)
                (declaim (ftype (function ((simple-array (unsigned-byte 8) *) &optional (and fixnum (integer 0))) (values ,structure fixnum)) ,maker))
                (defun ,maker (array &optional (i 0))
                  (declare (type (and fixnum (integer 0)) i))
                  (assert (< i (- (length array) ,struct-size)))
                  (let ,bindings
                    (assert (and ,@assertions))
                    (values
                     (,(intern (format nil "MAKE-~a" structure))
                      ,@(loop for f in fields for i in initialisers
                              collect (intern (symbol-name f) :keyword)
                              collect i))
                     (+ i ,struct-size))))))))

(macrolet ((define-header (struct magic &rest extra)
             `(define-some-structure ,struct
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
  (define-header header-32 #xfeedface)
  (define-header header-64 #xfeedfacf (reserved :unsigned 4)))
                  
(define-some-structure load-command-header
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

(define-some-structure dyld-info
  . #.(loop for s in '(rebase bind weak-bind lazy-bind export)
            collect `(,(intern (format nil "~a-OFF" s)) :unsigned 4)
            collect `(,(intern (format nil "~a-SIZE" s)) :unsigned 4)
            collect `(,s :extra)))

(macrolet ((define-segment-command (struct size)
             `(define-some-structure ,struct
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
  (define-segment-command segment-command-32 4)
  (define-segment-command segment-command-64 8))

(macrolet ((define-section (struct size)
             `(define-some-structure ,struct
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
  (define-section section-32 4)
  (define-section section-64 8))

(define-some-structure dylib-command
  (offset :unsigned 4)
  (timestamp :unsigned 4)
  (current-version :unsigned 4)
  (compatibility-version :unsigned 4)
  (path-name :extra))

(define-some-structure entry-point-command
  (entryoff :unsigned 8)
  (stacksize :unsigned 8))

(define-some-structure dylinker-command
  (offset :unsigned 4)
  (name :extra))

(define-some-structure symtab-command
  (symoff :unsigned 4)
  (nsyms :unsigned 4)
  (stroff :unsigned 4)
  (strsize :unsigned 4)
  (syms :extra))

(define-some-structure symtab-entry-32
  (offset :unsigned 4)
  (name :extra)
  (type :unsigned 1)
  (sect :unsigned 1) ; 0 => no sect; starts at 1 else
  (desc :signed 2) ; ???
  (value :unsigned 4))

#|
(define-some-structure dysymtab-command
  . #.(loop for s in '(ilocalsym nlocalsym iextdefsym nextdefsym iundefsym nundefsym tocoff ntocmodtaboff nmodtab extrefsymoff nextrefsyms indirectsymoff nindirectsyms extreloff nextrel localreloff)
            collect `(,s :unsigned 4)))
|#

(defun parse-segment-32 (array i)
  (loop with (segment-header i) = (multiple-value-list (parse-segment-command-32 array i))
        repeat (segment-command-32-nsects segment-header)
        for (sect ni) = (multiple-value-list (parse-section-32 array i))
        do (setf i ni)
        collect sect into sects
        finally (setf (segment-command-32-sections segment-header) sects)
                (return segment-header)))

(defun parse-symtab-32 (array i)
  (loop with symtab = (parse-symtab-command array i)
        with i = (symtab-command-symoff symtab)
        repeat (symtab-command-nsyms symtab)
        for (entry ni) = (multiple-value-list (parse-symtab-entry-32 array i))
        collect entry into entries
        do (setf i ni
                 (symtab-entry-32-name entry) (read-nul-terminated-string array (+ (symtab-entry-32-offset entry) (symtab-command-stroff symtab))))
        finally (setf (symtab-command-syms symtab) entries)
                (return symtab)))

(defun read-nul-terminated-string (array i &optional (f (constantly nil)))
  (loop for j from i until (zerop (aref array j))
        finally (funcall f j)
                (return (map 'string 'code-char (subseq array i j)))))

(defun parse-structure-with-names (struct-constructor array i &rest fs)
  (loop with res = (funcall struct-constructor array i)
        for (get-offset set-string) on fs by 'cddr
        ;; offset is from the start of load command, but our i starts right after it; it is 8 bytes, so correct for that
        do (funcall set-string (read-nul-terminated-string array (+ (funcall get-offset res) i -8)) res)
        finally (return res)))

(defun parse-dyld-information (array i)
  (labels ((read-uleb128 (array i &optional (f (constantly nil)))
             (loop for ni = i then (1+ ni)
                   for shift = 0 then (+ 7 shift)
                   for res = (logand #x7f (aref array ni)) then (+ res (ash (logand #x7f (aref array ni)) shift))
                   while (logtest (aref array ni) #x80)
                   finally (funcall f ni)
                           (return res)))
           (parse-binding-info (subseq)
             (loop for i = 0 then (1+ i)
                   with update-i = (lambda (ni) (setf i ni))
                   while (< i (length subseq))
                   for byte = (aref subseq i)
                   for op = (ldb (byte 4 4) byte)
                   for imm = (ldb (byte 4 0) byte)
                   if (= 0 op) collect '(:done)
                   else if (= 1 op) collect `(:set-dylib-ordinal ,imm)
                   else if (= 4 op) collect `(:set-symbol-trailing-flags ,(read-nul-terminated-string subseq (1+ i) update-i)
                                                                         ,@(if (logtest imm 1) '(:weak-import) ())
                                                                         ,@(if (logtest imm 8) '(:non-weak-definition) ()))
                   else if (= 5 op) collect `(:set-type ,(ecase imm
                                                           (1 :pointer)
                                                           (2 :text-absolute32)
                                                           (3 :text-pcrel32)))
                   ;; todo sleb128 set addend
                   else if (= 6 op) (read-uleb128 subseq (1+ i) update-i)
                   else if (= 7 op) collect `(:set-segment-and-offset ,imm ,(read-uleb128 subseq (1+ i) update-i))
                   else if (= 9 op) collect '(:do-bind)
                          else do (error "bad op ~a" op)))
           (parse-rebase-info (subseq)
             (loop for i = 0 then (1+ i)
                   with update-i = (lambda (ni) (setf i ni))
                   while (< i (length subseq))
                   for byte = (aref subseq i)
                   for op = (ldb (byte 4 4) byte)
                   for imm = (ldb (byte 4 0) byte)
                   if (= 0 op) collect '(:done)
                   else if (= 1 op) collect `(:set-type ,(ecase imm
                                                           (1 :pointer)
                                                           (2 :text-absolute32)
                                                           (3 :text-pcrel32)))
                   else if (= 2 op) collect `(:set-segment-and-offset ,imm ,(read-uleb128 subseq (1+ i) update-i))
                   else if (= 5 op) collect `(:do-rebase ,imm)
                   ;; I have absolutely no idea what the +4 is for, but dyldinfo does it...
                   else if (= 7 op) collect `(:do-rebase-add-addr ,(+ 4 (read-uleb128 subseq (1+ i) update-i)))))
           (parse-exports (subseq &optional (i 0) (prefix ""))
             (loop with terminal-size = (aref subseq i)
                   ;; address is supposed to get added to a base address (which...?  Text's?)
                   with terminal = (if (zerop terminal-size)
                                       nil
                                       (let* (ti
                                              (flags (read-uleb128 subseq (1+ i) (lambda (ni) (setf ti (1+ ni)))))
                                              (address (read-uleb128 subseq ti)))
                                         (list prefix address flags)))
                               
                   with childi = (+ i terminal-size 2)
                   with nchild = (progn (format t "i ~a nc ~a~%" (1- childi) (aref subseq (1- childi))) (aref subseq (1- childi)))
                   repeat nchild
                   for nsuffix = (read-nul-terminated-string subseq childi (lambda (ni) (setf childi (1+ ni))))
                   for ci = (read-uleb128 subseq childi (lambda (ni) (setf childi (1+ ni))))
                   append (parse-exports subseq ci (concatenate 'string prefix nsuffix)) into res
                   finally (return (if terminal (cons terminal res) res)))))
                   
    (let ((info (parse-dyld-info array i)))
      (loop for target in '(bind lazy-bind weak-bind rebase export)
            for off = (intern (format nil "~a-OFF" target)) and size = (intern (format nil "~a-SIZE" target))
            do (setf (slot-value info target) (funcall
                                               (cond
                                                 ((eq target 'rebase) #'parse-rebase-info)
                                                 ((eq target 'export) #'parse-exports)
                                                 (t #'parse-binding-info))
                                               (subseq array (slot-value info off)
                                                       (+ (slot-value info off) (slot-value info size))))))
      info)))

(defstruct macho32-file segments symtabs loaded-dylibs entry-point dyld-info dylinker)

(defun parse-macho-32 (array)
  (multiple-value-bind (header i) (parse-header-32 array)
    (loop repeat (header-32-ncmds header)
          with entry-point and dylinker and dyld-info
          for (lch ni) = (multiple-value-list (parse-load-command-header array i))
          for cmd = (load-command-header-cmd lch)
          if (eq :segment cmd)
            collect (parse-segment-32 array ni) into segments
          else if (eq :load-dylib cmd)
            collect (parse-structure-with-names 'parse-dylib-command array ni 'dylib-command-offset #'(setf dylib-command-path-name)) into loaded-dylibs
          else if (eq :symtab cmd)
            collect (parse-symtab-32 array ni) into symtabs
          else if (eq :load-dylinker cmd)
            do (assert (null dylinker)) ;there can only be one!
               (setf dylinker (parse-structure-with-names 'parse-dylinker-command array ni 'dylinker-command-offset #'(setf dylinker-command-name)))
          else if (eq :main cmd)
            do (assert (null entry-point))
               (setf entry-point (parse-entry-point-command array ni))
          else if (eq :dyld-info-only cmd)
            do (assert (null dyld-info))
               (setf dyld-info (parse-dyld-information array ni))
          else
            collect (load-command-header-cmd lch) into extras
          do (incf i (load-command-header-cmdsize lch))
          finally (return (values (make-macho32-file :segments segments
                                                     :symtabs symtabs
                                                     :loaded-dylibs loaded-dylibs
                                                     :entry-point entry-point
                                                     :dyld-info dyld-info
                                                     :dylinker dylinker)
                                  extras)))))
    
