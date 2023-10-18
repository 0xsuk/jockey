(asdf:defsystem :jockey
  :name "jockey"
  :author "0xsuk"
  :depends-on (:cffi
               :suk)
  :serial t
  :components (
               (:file "package")
               (:file "cffi")
               (:file "jockey"))
  )
