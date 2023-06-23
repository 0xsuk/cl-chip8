(asdf:defsystem :cl-chip8
  :name "chip8"
  :description "Simple Chip-8 emulator."
  :author "Steve Losh"
  :depends-on (
               :cl-arrows
               :losh
               :iterate
               )
  :serial t
  :components ((:file "package")
               (:module "src" :serial t
								:components ((:file "main"))))
  )
