(pushnew (uiop:getcwd) ql:*local-project-directories*)
(ql:quickload :jockey)
(asdf:load-system :jockey)
