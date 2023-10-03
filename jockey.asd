(asdf:defsystem :jockey
  :name "jockey"
  :author "0xsuk"
  :depends-on (:cffi)
  :serial t
  :components (
               (:file "jack")
               (:file "jockey"))
  )
