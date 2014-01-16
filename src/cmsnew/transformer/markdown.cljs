(ns cmsnew.transformer.markdown)
;; markdown parsing
;; this requires showdown.js to be available

(let [conv-class (.-converter js/Showdown)
      converter (conv-class.)]
  (defn markdown-to-html [markdown-txt]
    (.makeHtml converter markdown-txt)))
