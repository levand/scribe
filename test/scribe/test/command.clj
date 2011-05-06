(ns scribe.test.command
  (:use clojure.test)
  (:use scribe.command :reload))

(deftest t-namespace-cmd
  (let [cmd (namespace-cmd "foo.bar")]
    (is (= "\n{namespace foo.bar}"
           (render cmd)))))

(deftest t-extract-params
  (let [extract-params (ns-resolve 'scribe.command 'extract-params)]
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

(deftest t-foreach-cmd
  (let [cmd (foreach-cmd "$foo" "$foobar"
                         "content1"
                         (ifempty-cmd)
                         "content2")
        cmd2 (foreach-cmd "$foo" "$foobar"
                          (call-cmd ".name" "$foo"))]
    (is (= "{foreach $foo in $foobar}content1{ifempty}content2{/foreach}"
           (render cmd)))
    (is (= #{"foobar"} (params cmd)))
    (is (= #{"foobar"} (params cmd2)))))


(deftest t-for-cmd
  (let [cmd1 (for-cmd "$foo" [10] "foo")
        cmd2 (for-cmd "$foo" [5 10] "foo")
        cmd3 (for-cmd "$foo" [1 10 2] "foo")
        cmd4 (for-cmd "$foo" [10] (if-cmd "$foo == $bar == $baz" "childs"))]
    (is (= "{for $foo in range(10)}foo{/for}" (render cmd1)))
    (is (= "{for $foo in range(5, 10)}foo{/for}" (render cmd2)))
    (is (= "{for $foo in range(1, 10, 2)}foo{/for}" (render cmd3)))
    (is (= #{"foo"}) (params cmd1))
    (is (= #{"bar" "baz"} (params cmd4)))))

(deftest t-call-cmd
  (let [cmd1 (call-cmd ".myCall" "all")
        cmd2 (call-cmd ".myCallWithKids" "$baz"
                       (param-cmd "aparam" "$foo")
                       (param-cmd "bparam" "$bar"))]
    (is (= "{call .myCall data=\"all\"/}"
           (render cmd1)))
    (is (= "{call .myCallWithKids data=\"$baz\"}{param aparam: $foo/}{param bparam: $bar/}{/call}"
           (render cmd2)))
    (is (= #{"foo" "bar" "baz"}
           (params cmd2)))))

(deftest t-param-cmd
  (let [cmd1 (param-cmd "fooparam" nil "foobar" "bazbuzz")]
    (is (= "{param fooparam}foobarbazbuzz{/param}" (render cmd1)))))

(deftest t-css-cmd
  (let [cmd1 (css-cmd "command_text")]
    (is (= "{css command_text}" (render cmd1)))))
