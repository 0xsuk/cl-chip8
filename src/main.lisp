(in-package :chip8)

(deftype int4 () '(unsinged-byte 4)) ; 0 to 15
(deftype int8 () '(unsigned-byte 8))
(deftype int12 () '(unsigned-byte 12))
(deftype int16 () '(unsigned-byte 16))

(defconstant +memory-size+ (* 1024 4))

; in function call, type should be quoted to prevent evaluation (ex: (make-array 16 :element-type 'int8))
; in slot argument, type should not be quoted because that is how defstruct treats argument (ex: (running t :type boolean))
(defstruct chip
  (running t :type boolean)
  (registers (make-array 16 :element-type 'int8)
   :type (simple-array int8 (16))
   :read-only t ; only change the *element* of the array, not the array itself
   )
  (index 0 :type int16)
  (program-counter #x200 :type int12) ; #x200 is where the ROM data eventually gets loaded into the CHIP-8 memory.
  (stack (make-array 16 :element-type 'int12 :fill-pointer 0)
   :type (vector int12 16) ; more complex than simple-array, can have :fill-pointer
   :read-only t
   )
  (memory (make-array +memory-size+ :element-type 'int8)
   :type (simple-array int8 (#.+memory-size+))
   :read-only t)
  (loaded-rom nil :type (or null string))
  )

(defun-inline chip-flag (chip)
  (aref (chip-registers chip) #xF))

(defun-inline (setf chip-flag) (new-value chip)
  (setf (aref (chip-registers chip) #xF) new-value))

(define-with-macro chip
  running
  memory
  stack
  registers
  index
  program-counter
  flag
  loaded-rom)

(defun reset (chip)
  (with-chip (chip)
    (fill memory 0)
    (fill registers 0)
    (replace memory (alexandria:read-file-into-byte-vector loaded-rom)
             :start1 #x200)
    (setf running t
          program-counter #x200
          (fill-pointer stack) 0))
  (values))

(defun load-rom (chip filename)
  (setf (chip-loaded-rom chip) filename)
  (reset chip))

(defparameter *c* nil)

(defun run (rom-filename)
  (let ((chip (make-chip)))
    (setf *c* chip)
    (load-rom chip rom-filename)
    (run-cpu chip)
    ))

(defconstant +cycles-per-second+ 500)
(defconstant +cycles-before-sleep+ 10)

(defun run-cpu (chip)
  (iterate
    (while (chip-running chip))
    (emulate-cycle chip)
    (for tick :every-nth +cycles-before-sleep+ :do
         (sleep (/ +cycles-before-sleep+ +cycles-per-second+)))))

(defun-inline chop (size integer)
  "truncates integer to given size of bits."
  (ldb (byte size 0) integer)) ; (byte <how many bits> <position of least significant bit>)

(defun-inline cat-bytes (high-order low-order)
  "concatinate two bytes."
  (dpb high-order (byte 8 8) low-order))
;; In the context of the dpb function in Common Lisp, "deposit" means to replace a specific byte in an integer with a new byte.

;; When you "deposit" a byte into an integer, you're not adding the byte to the integer in the sense of performing an arithmetic addition. Instead, you're replacing a specific set of bits (a "byte") within the binary representation of the integer with a new set of bits.

;; For example, if you have a 16-bit integer that's represented as #x1234 (which is 0001 0010 0011 0100 in binary), and you deposit the byte #xAB (which is 1010 1011 in binary) into the second byte of the integer, you get a new integer #xAB34 (which is 1010 1011 0011 0100 in binary). The second byte of the original integer (the 12 in 1234) has been replaced with AB.

;; So, "deposit" in this context is a bit like "insert" or "replace", but it's used specifically to talk about replacing a byte within an integer.

(defun emulate-cycle (chip)
  (with-chip (chip)
    (let ((instruction (cat-bytes (aref memory program-counter) ; all instructions are two bytes long, including their arguments.
                                  (aref memory (1+ program-counter)))))
      (zapf program-counter (chop 12 (+ % 2))) ; why 2? if program counter goes to the final, it wraps arounds to 0 because of chop. we could signal an error though
      (dispatch-instruction chip instruction)
      )))

(defun dispatch-instruction (chip instruction)
  (macrolet ((call (name) `(,name chip instruction)))
    (ecase (logand #xF000 instruction)
      (#x0000 (ecase instruction
                (#x00E0 (call op-cls))
                (#x00EE (call op-ret))))
      (#x1000 (call op-jp-imm))
      (#x2000 (call op-call))
      (#x3000 (call op-se-reg-imm))
      (#x4000 (call op-sne-reg-imm))
      (#x5000 (ecase (logand #x000F instruction)
                (#x0 (call op-se-reg-reg))))
      (#x6000 (call op-ld-reg<imm))
      (#x7000 (call op-add-reg<imm))
      (#x8000 (ecase (logand #x000F instruction)
                (#x0 (call op-ld-reg<reg))
                (#x1 (call op-or))
                (#x2 (call op-and))
                (#x3 (call op-xor))
                (#x4 (call op-add-reg<reg))
                (#x5 (call op-sub-reg<reg))
                (#x6 (call op-shr))
                (#x7 (call op-subn-reg<reg))
                (#xE (call op-shl))))
      (#x9000 (ecase (logand #x000F instruction)
                (#x0 (call op-sne-reg-reg))))
      (#xA000 (call op-ld-i<imm))
      (#xB000 (call op-jp-imm+reg))
      (#xC000 (call op-rand))
      (#xD000 (call op-draw))
      (#xE000 (ecase (logand #x00FF instruction)
                (#x9E (call op-skp))
                (#xA1 (call op-sknp))))
      (#xF000 (ecase (logand #x00FF instruction)
                (#x07 (call op-ld-reg<dt))
                (#x0A (call op-ld-reg<key))
                (#x15 (call op-ld-dt<reg))
                (#x18 (call op-ld-st<reg))
                (#x1E (call op-add-index<reg))
                (#x29 (call op-ld-font<vx))
                (#x33 (call op-ld-bcd<vx))
                (#x55 (call op-ld-mem<regs))
                (#x65 (call op-ld-regs<mem)))))))

(defmacro define-instruction (name argument-list &body body)
  `(progn
     (declaim (ftype (function (chip int16) null) ,name))
     (defun ,name (chip instruction)
       (declare (ignorable instruction))
       (with-chip (chip)
         (macrolet ((register (index)
                      `(aref registers ,index)))
           (let ,(parse-instruction-argument-bindings argument-list)
             ,@body)))
       nil)))

(defun parse-instruction-argument-bindings (argument-list)
  (flet ((normalize-arg (arg)
           (destructuring-bind (symbol &optional (nibbles 1))
               (ensure-list arg)
             (list symbol nibbles))))
    (iterate
      (for (symbol nibbles) :in (mapcar #'normalize-arg argument-list))
      (for position :first 3 :then (- position nibbles))
      (when (not (eql symbol '_))
        (collect `(,symbol (ldb (byte ,(* nibbles 4)
                                      ,(* position 4))
                                instruction))))
      )))

(define-instruction op-rand (_ r (mask 2)) ; RND
  "generate random number"
  (setf (register r)
        (logand (random 256) mask)))

(define-instruction op-jp-imm (_ (target 3)) ; JP addr
  (setf program-counter target))

(define-instruction op-jp-imm+reg (_ (target 3))
  (setf program-counter (chop 12 (+ target (register 0))))) ; JP V0 + addr

(define-instruction op-call (_ (target 3)) ; CALL addr
  (vector-push program-counter stack)
  (setf program-counter target))

(define-instruction op-ret () ; RET
  (setf program-counter (vector-pop stack)))

(defun-inline digit (position integer &optional (base 10))
  "take `position`th digit in the integer"
  (-<> integer
       (floor <> (expt base position))
       (mod <> base)))

(define-instruction op-ld-bcd<vx (_ r _ _) ; LD B, Vx
  (let ((number (register r)))
    (setf (aref memory (+ index 0)) (digit 2 number)
          (aref memory (+ index 1)) (digit 1 number)
          (aref memory (+ index 2)) (digit 0 number)))
  )

(defun-inline +_8 (x y)
  (let ((result (+ x y)))
    (values (chop 8 result)
            (if (> result 255) 1 0)))) ; carry

(defun-inline -_8 (x y)
  (let ((result (- x y)))
    (values (chop 8 result)
            (if (> x y) 1 0)))) ; not borrow

(define-instruction op-add-reg<reg (_ rx ry) ; ADD Vx, Vy (8-bit)
  (setf (values (register rx) flag)
        (+_8 (register rx) (register ry))))

(define-instruction op-sub-reg<reg (_ rx ry) ; ADD Vx, Vy (8-bit)
  (setf (values (register rx) flag)
        (-_8 (register rx) (register ry))))

(define-instruction op-subn-reg<reg (_ rx ry)
  (setf (values (register rx) flag)
        (-_8 (register ry) (register rx))))

(define-instruction op-add-reg<imm (_ r (immediate 2)) ;; ADD Vx, Imm
  ;; For some weird reason the ADD immediate op doesn't set the flag
  (zapf (register r) (+_8 % immediate)))

(define-instruction op-add-index<reg (_ r)
  (zapf index (chop 16 (+ % (register r)))))

(defun-inline get-bit (position integer)
  (ldb (byte 1 position) integer))

(defun-inline >>_8 (v)
  (values (ash v -1)
          (get-bit 0 v)))

(defun-inline <<_8 (v)
  (values (chop 8 (ash v 1))
          (get-bit 7 v)))

(define-instruction op-shr (_ r) ; SHR
  (setf (values (register r) flag)
        (>>_8 (register r))))

(define-instruction op-shl (_ r) ; SHL
  (setf (values (register r) flag)
        (<<_8 (register r))))

(defmacro macro-map (lambda-list items &rest body)
  (with-gensyms (macro)
    `(macrolet ((,macro ,(ensure-list lambda-list) ,@body))
       ,@(iterate (for item :in items)
           (collect `(,macro ,@(ensure-list item)))))))

(macro-map ;; AND/OR/XOR
           (NAME    OP)
           ((op-and logand)
            (op-or  logior)
            (op-xor logxor))
           `(define-instruction ,name (_ destination source _)
              (zapf (register destination) (,op % (register source)))))

(macro-map ; SE/SNE
           (NAME            TEST X-ARG  X-FORM        Y-ARG         Y-FORM)
           ((op-se-reg-imm  =    (r 1)  (register r)  (immediate 2) immediate)
            (op-sne-reg-imm not= (r 1)  (register r)  (immediate 2) immediate)
            (op-se-reg-reg  =    (rx 1) (register rx) (ry 1)        (register ry))
            (op-sne-reg-reg not= (rx 1) (register rx) (ry 1)        (register ry)))
           `(define-instruction ,name)
           )
