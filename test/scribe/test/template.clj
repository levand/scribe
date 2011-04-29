(ns scribe.test.template
  (:use clojure.test)
  (:use scribe.template :reload))

(deftest t-namespace-cmd
  (let [cmd (namespace-cmd "foo.bar")]
    (is (= "\n{namespace foo.bar}"
           (render cmd)))))

(deftest t-extract-params
  (let [extract-params (ns-resolve 'scribe.template 'extract-params)]
    (is (= #{"foo" "bar_fight" "baz"}
           (extract-params "$foo.test.var $bar_fight||$baz.biq")))))

(deftest t-print-cmd
  (let [cmd (print-cmd "$foo.bar" :id :escapeHtml)]
    (is (= "{$foo.bar |id |escapeHtml}"
           (render cmd)))
    (is (= #{"foo"} (params cmd)))))

(deftest t-template-cmd
  (let [tpl-cmd  (template-cmd "foo" {:private "true"}
                               "<html>{}" (print-cmd "$bar.baz" :escapeHtml)
                               "</html>")]
    (is (= "\n\n/**\n * @bar bar\n */\n{template .foo private=\"true\"}\n<html>{lb}{rb}{$bar.baz |escapeHtml}</html>\n{/template}\n"
           (render tpl-cmd)))
    (is (= #{"bar"} (params tpl-cmd)))))

(deftest t-literal-cmd
  (let [cmd (literal-cmd "Foo bar baz")]
    (is (= "{literal}Foo bar baz{/literal}" (render cmd)))))

(deftest t-msg-cmd
  (let [cmd (msg-cmd
             {:desc "This is a description" :meaning "noun"}
             "Check out this fancy message")]
    (is (= "{msg desc=\"This is a description\" meaning=\"noun\"}Check out this fancy message{/msg}")))
  (let [cmd (msg-cmd {:desc "desc" :meaning "noun"}
                     "<html>" (print-cmd "$foobar") "</html>")]
    (is (= "{msg desc=\"desc\" meaning=\"noun\"}<html>{$foobar}</html>{/msg}"
           (render cmd)))
    (is (= #{"foobar"}
           (params cmd)))))

(deftest t-if-cmd
  (let [cmd (if-cmd "$bar == $foo" "true"
                    (elseif-cmd "$baz == $qux") "false"
                    (else-cmd) "other")]
    (is (= "{if $bar == $foo}true{elseif $baz == $qux}false{else}other{/if}"
           (render cmd)))
    (is (= #{"foo" "bar" "baz" "qux"}
           (params cmd)))))

(deftest t-switch-cmd
  (let [cmd (switch-cmd "$foo"
                        (case-cmd 1 2 3) "few"
                        (case-cmd "4" "5" "6") "some"
                        (case-cmd "$bar") "other"
                        (default-cmd) "default")]
    (is (= "{switch $foo}{case 1, 2, 3}few{case 4, 5, 6}some{case $bar}other{default}default{/switch}"
           (render cmd)))
    (is (= #{"foo" "bar"}
           (params cmd)))))

(deftest t-foreach-cmd)

(deftest t-for-cmd)