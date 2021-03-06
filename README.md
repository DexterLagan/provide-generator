# provide-generator
Racket Provide Generator

Generates a pretty provide form for Racket modules.

<h2>How to compile:</h2>
compile to standalone binary using Racket 6.11 or newer.

<h2>How to use:</h2>
1) Copy the contents of your module's source into the clipboard;<br>
2) Launch provide-generator;<br>
3) Paste the newly generated provide form into your module.

<h2>Sample Output:</h2>

<pre><code>(provide all-but-last                 ; (all-but-last l)
         close-parenthesis            ; (close-parenthesis pre-indentation l post-indentation)
         composex                     ; (composex stx) [MACRO]
         generate-provide-form        ; (generate-provide-form stx-lines indentation)
         generate-provide-line        ; (generate-provide-line stx-line indentation)
         get-clipboard                ; (get-clipboard)
         get-function-macro-name      ; (get-function-macro-name stx-line)
         get-length-of-longest-string ; (get-length-of-longest-string l)
         get-stx-lines                ; (get-stx-lines s)
         println                      ; (println s)
         set-clipboard                ; (set-clipboard s)
         stx->str)                    ; (stx->str stx)</code></pre>
