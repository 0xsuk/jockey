(pushnew (uiop:getcwd) ql:*local-project-directories*)
(asdf:load-system :jockey)
