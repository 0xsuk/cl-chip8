(pushnew (uiop:getcwd) ql:*local-project-directories*)
(ql:quickload :cl-chip8)
(asdf:load-system :cl-chip8)
