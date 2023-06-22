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
