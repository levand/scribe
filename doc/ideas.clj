                                        ; Template Usage

;; create a basic template?

(<template> myTemplate {:autoescape false :private true}
            ...)

(<template> myTemplate {:autoescape true}
           "<html>"
           (..other commands..)
           "</html>")

(<template> myTemplate {:autoescape true}
            (hiccup/html [:html [:head [:title "$foo"]]]))



[:html [:head [:title foo]]]

[:html [:head [:title "$foo"]]]

"<html><head><title>$foo</title></head></html>"

---------------------------

;; Hiccup style

(defn layout [title body]
  (hiccup/html [:html [:head [:title title]]
                [:body body]]))

(defn post [^String title ^String content]
  [:div {:id "post"}
   [:h2 title]
    [:br]
   [:p content]])

(layout "Foo" (post "hello" "body text"))


---------------------

(scr/template IndexPage
              [:html
               [:head [:title $title]]
               [:body
                (scr/call Header)
                (scr/call Body)]])

(scr/template PostPage
              [:html
               [:head [:title $title]]
               [:body
                (scr/call Header)
                (scr/call Body)]])

(scr/template CommentPage
              [:html
               [:head [:title $title]]
               [:body
                (scr/call Header)
                (scr/call Body)]])

(scr/template Header
              [:div {:id "header"}
               [:h1 $title]
               [:i "some cheeky subtitle"]])

(scr/template Body
              [:div {:id "post"}
               [:h2 $title]
               [:br]
               [:p $content]])

(def all-pages (scribe/from-namespace my-ns))


(scr/render IndexPage {:title "My Title"
                       :content "My Content"})

--------------------------------------

[:IndexPage [:Header ...] [:Body ...]]

;; Don't like macro noise
(defmacro MyGenericLayout [inner]
                `[:html
                  [:head [:title $title]]
                  [:body ~@inner]])

;; Better
(make-layout MyGenericLayout [inner]
             [:html
              [:head [:title $title]]
              [:body inner]])

(scr/template Page1
              (MyGenericLayout
               [:div {:id $div_id}
                (scr/call Body)]))

[MyGenericLayout [:div {:id}]]
[Body [:div {:id}]]

(MyGenericLayout (Page1 {:div_id "foo"}))

(scr/template Page2
              [:html
               [:head [:title $title]]
               [:body
                (scr/call Header)
                (scr/call Body)]])

(scr/template Page3
              [:html
               [:head [:title $title]]
               [:body
                (scr/call Header)
                (scr/call Body)]])


(scr/template
 Page1
 {:parent MyGenericLayout
  :inner
  [:div {:id $div_id}
   (scr/call Body)]})

(defn surround-template-with-parent [tmpl]
  )








(defn myTemplate [foo]
  [:html [:head [:title foo]]])


;; literal helper functions?

;; html5 plugin/helper? for layout?

;; mixing with hiccup?


(template-cmd )