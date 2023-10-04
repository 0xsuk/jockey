(asdf:defsystem :jockey
  :name "jockey"
  :author "0xsuk"
  :depends-on (:cffi)
  :serial t
  :components (
               (:file "package")
               (:file "jack")
               (:file "jockey"))
  )
