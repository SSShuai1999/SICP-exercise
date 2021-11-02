#lang racket

; 练习 1.1

10 ; -> 10

(+ 5 3 4) ; -> 12

(- 9 1) ; -> 8

(/ 6 2) ; 3

(+ (* 2 4) (- 4 6)) ; 6

(define a 3) ; 声明了 a -> 3

(define b (+ a 1)) ; 声明了 b 为 4

(+ a b (* a b)) ; -> 19

(= a b) ; #f

(if (and (> b a) (< b (* a b)))
    b
    a)  ; -> 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; -> 16

(+ 2 (if (> b a) b a)) ; -> 6

(* (cond ([> a b] a)
         ([< a b] b)
         (else -1))
   (+ a 1)) ; -> 16


; 练习 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; 练习 1.3

; 第一种实现;
(define (max-sum1 a b c)
   (cond ((and [> a c] [> b c] (+ a b)))
         ((and [> a b] [> c b] (+ a c)))
         (else (+ b c))))

(display "1.3 第一种实现: \n")
(= (max-sum1 1 2 3) 5)
(= (max-sum1 1 3 2) 5)
(= (max-sum1 3 2 1) 5)
(= (max-sum1 3 4 4) 8)

; 第二种实现
(define (bigger x y)
  (if (> x y) x y))

(define (smaller x y)
  (if (< x y) x y))

(define (max-sum2 x y z)
  (+ (bigger (smaller x y) z)
     (bigger x y)))

(display "1.3 第二种实现: \n")
(= (max-sum2 1 2 3) 5)
(= (max-sum2 1 3 2) 5)
(= (max-sum2 3 2 1) 5)
(= (max-sum2 3 4 4) 8)

; 练习 1.4
; 定义 a-plus-abs-b 过程，参数为 a、b，如果 b 大于 0 结果就是 a + b 的值，否则就是 a - b 的值.

; 练习 1.5
; 我们之前学了两种求值方法。分别是「正则序求值」和「应用序求值」。下面的代码会在这不同的求值方法中有不同的体现：

; (define (p) (p))
; (define (test x y)
;   (if (= x 0)
;        0
;        y))

; (test 0 (p))

; 应用序:
; 如果是在「应用序」法则里面，我们会看到报错。这是因为应用序是采用「先求值参数而后应用」的求值模型。
; 在计算 (test 0 (p)) 表达式的时候，应用序会调用 (p) 过程, 由于这个过程会一直调用自身，陷入无限循环。所以导致出错。

; 正则序:
; 而正则序则不同，正则序采用「完全展开而后归约」的求值模型。
; 我们可以把它理解为惰性的。只有用到它的时候，它才开始计算。
; 而我们在计算 (test 0 (p)) 表达式的时候，由于在过程体中，走到 (= x 0) 表达式的时候，就会计算出 0 的结果。
; 所以根本不会计算到 (p)，当然也就不会出现问题了。

; 牛顿法求平方根的实例
(define (square x)
  (* x x))

(define (improve guess x)
  (/ (+ (/ x guess) guess) 2))

(define (good-enough? guess x)
   (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)             ; 猜测的结果够好吗?
      guess                              ; 够好了
      (sqrt-iter (improve guess x) x)))  ; 否则我们把 guess 的值进行优化(规则是:它们商之和的平均数作为新的猜测值),再次进行检查  

(define (sqrt x)
  (sqrt-iter 1.0 x))

; 练习 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; 用 Eva 的 new-if 重写了一遍
(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

; Eva 写的 "新 if" 看起来不错！但是，经过重写后的函数与之前的机制有了本质的差别。
; 解释器可能会对这段函数进行抱怨，原因是递归的太深了！
; 为什么会这样呢？
; 根据本书 P12 所说: "if 是条件表达式的一种受限形式，适用于分情况分析中只有两个情况的需要。
; 一般形式为: (if <predicate> <consequent> <alternative>)"。
; if 是一种受限形式？是的，受限的方式为：惰性求值，如果 <predicate> 为 #t ,那么就只求值 <consequent>，否则求值 <alternative>。
; 如果我们用了 new-if 进行改造，那么改造后就是一个普通的函数，也就失去了受限。

; 举个例子：
; (if #t (display "hello") (display "world")) -> logs `hello`
; (new-if #t (display "hello") (display "world")) -> logs `helloworld`

; 看出差别了吗？
; 在 if 表达式中，只有一个表达式被求值。而 new-if 则是两个都被求值。
; 所以当我们使用 new-if 重构后，由于会对所有表达式进行应用序求值，递归嵌套深度就会不断增加。
; ↓ ↓ ↓ ↓ ↓ ↓
; (new-if (good-enough? guess x)
;         guess
;         (sqrt-iter (improve guess x) x)))   -> 这个表达式会一直进行求值

; 练习 1.7
; 当输入 (sqrt 999999999999999999999999) 的时候，程序陷入了死循环
; 当输入 (sqrt 0.000000001) :
; 显示答案是: 0.03125001065624928 
; 正确答案是: 0.000031622776601683795
; 我们可以发现，不管是特别大的数还是特别小的数，当前这个程序都无法胜任这个工作。
; 原因出现在 good-enough? 函数身上。

; (define (good-enough? guess x)
;    (< (abs (- (square guess) x)) 0.001))

; 进行改造后：
(define (good-enough2? old-guess new-guess)
   (> 0.00001
      (/ (abs (- new-guess old-guess))
         old-guess)))

(define (sqrt-iter3 guess x)
  (if (good-enough2? guess (improve guess x))             
      (improve guess x)                       
      (sqrt-iter3 (improve guess x) x)))

(define (sqrt3 x)
  (sqrt-iter3 1.0 x))

; 练习 1.8
(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* guess 2)) 3))

(define (cube-root-iter guess x)
  (if (good-enough2? guess (cube-improve guess x))             
      (cube-improve guess x)                       
      (cube-root-iter (cube-improve guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))
