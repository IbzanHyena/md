cur=. (4!:3$0) {::~ 4!:4<'y'
dir=. 0 {:: fpathname cur
target=. dir,'md.ijs'

load target

contents=. (1!:1) 3
stdout markdown contents
exit''
