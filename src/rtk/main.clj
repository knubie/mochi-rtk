(ns rtk.main
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.data.csv :as csv]
    [cognitect.transit :as transit]
    [nano-id.core :as nano-id]))


(defn make-id
  "Creates an 8 character longer alphanumeric ID.
  *Note*: that IDs cannot begin with an underscore."
  []
  (let [alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        custom-id (nano-id/custom alphabet 8)]
    (keyword (custom-id))))


(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (map keyword) ;; Drop if you want string keys instead
            repeat)
    (rest csv-data)))


(def export-data
  {:version 2
   :templates [{:id :z0MaQ6x5
                :name "RTK"
                :content "## <sub><sup>`<< ID >>`</sup></sub> << Keyword >>\n---\n<< Components >>\n\n<< Story >>\n---\n# <span style=\"font-family: EPSON-KYOKASHO; font-size: 4em;\"><< Kanji >></span>"
                :fields {:pDrlbE4x {:id :pDrlbE4x
                                    :name "ID"}
                         :name {:id :name
                                :name "Keyword"}
                         :THwgyfYd {:id :THwgyfYd
                                    :name "Components"}
                         :5DxonkEs {:id :5DxonkEs
                                    :name "Story"}
                         :vVM4vfPZ {:id :vVM4vfPZ
                                    :name "Kanji"}}}]
   :decks [{:id :Jmpfbg0L
            :name "Remembering the Kanji"
            :template-id :z0MaQ6x5
            :cards []}]})

(defn rtk-data
  ; "Looks a bit like this:
  ; {:kanji \"好\",
  ; :id_6th_ed "103",
  ; :keyword_6th_ed \"fond\",
  ; :components \"woman; child\"}
  ; (Some keys not included."
  []
  (with-open [reader (io/reader
                       ; "sample.csv"
                       "heisig-kanjis.csv")]
    (csv-data->maps
      (doall
        (csv/read-csv reader)))))

(defn trim-data
  "Remove unnecessary data, plus assigns a unique ID to each kanji."
  [data]
  (map
    #(-> %
       (select-keys [:kanji :id_6th_ed :keyword_6th_ed :components])
       (assoc :id (make-id)))
    data))


(defn by-keyword
  "Keys the list of kanji by its 'keyword' (not clojure keyword)."
  [data]
  (reduce
    (fn [m row]
      (assoc m (:keyword_6th_ed row) row))
    {} data))

; {
;   "~:content": "",
;   "~:name": "Hesitate",
;   "~:deck-id": "~:Jmpfbg0L",
;   "~:fields": {
;                "~:name": {
;                           "~:id": "~:name",
;                           "~:value": "Hesitate"}
;                ,
;                "~:5DxonkEs": {
;                               "~:id": "~:5DxonkEs",
;                               "~:value": ""}
;                ,
;                "~:vVM4vfPZ": {
;                               "~:id": "~:vVM4vfPZ",
;                               "~:value": "躊"}
;                ,
;                "~:pDrlbE4x": {
;                               "~:id": "~:pDrlbE4x",
;                               "~:value": "2997"}}
              
;   ,
;   "~:pos": "U",
;   "~:references": {
;                    "~#set": []}
;   ,
;   "~:reverse-reviews": [],
;   "~:id": "~:lgCypCNj",
;   "~:reviews": [],
;   "~:new?": false,
;   "~:template-id": "~:z0MaQ6x5"}

(def deck-id :Jmpfbg0L)
(def template-id :z0MaQ6x5)
(def field-components-id :THwgyfYd)
(def field-story-id :5DxonkEs)
(def field-kanji-id :vVM4vfPZ)
(def field-number-id :pDrlbE4x)


(defn -main [& _args]
  ; (println (rtk-data))
  (let [data (-> (rtk-data) (trim-data))
        indexed (by-keyword data)
        cards
        (map
          (fn [row]
            (let [components (->> (str/split (:components row) #";\ ?")
                                  (filter seq))
                                  ; (map #(let [card (get indexed %)]
                                  ;         (if card
                                  ;           (str "[[" (name (:id card)) "]]")
                                  ;           %))))
                  child-comps (->> (str/split (:components row) #";\ ?")
                                  (filter seq)
                                  (map #(let [card (get indexed %)]
                                          (if card
                                            (->> (str/split (:components card) #";\ ?")
                                                 (filter seq))
                                            [])))
                                  (flatten))
                  components (->> components
                                  (remove (fn [comp]
                                            ;; is this comp a member of any of the other comps comps?
                                            (some #(= comp %) child-comps)))
                                  (map #(let [card (get indexed %)]
                                          (if card
                                            (str "[[" (name (:id card)) "]]")
                                            %))))]
              ;; For each component, check if it is a component of any of the other components.
              {:content ""
               :name (:keyword_6th_ed row)
               :deck-id deck-id
               :id (:id row)
               :template-id template-id
               :fields {field-number-id {:id field-number-id, :value (:id_6th_ed row)}
                        :name {:id :name, :value (:keyword_6th_ed row)}
                        field-components-id {:id field-components-id, :value (str/join " " components)}
                        field-story-id {:id field-story-id, :value ""}
                        field-kanji-id {:id field-kanji-id, :value (:kanji row)}}}))
                  
          data)]

    (with-open [out (io/output-stream "data.json")]
      (transit/write (transit/writer out :json-verbose)
                     (assoc-in export-data [:decks 0 :cards] cards)))))
    ; (println
    ;   (transit/write writer
    ;     (assoc-in export-data [:decks 0 :cards] cards)))))
    ; (println data)))
