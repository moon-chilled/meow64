(cl:defpackage #:meow64-macho
  (:use #:cl)
  (:export #:parse-macho-32
           #:macho32-file #:macho32-file-segments #:macho32-file-symtabs #:macho32-file-loaded-dylibs #:macho32-file-entry-point #:macho32-file-dylinker))
           
(cl:defpackage #:meow64
  (:use #:cl)
  (:local-nicknames (#:macho #:meow64-macho)))
