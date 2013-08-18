sr     = 44100
kr     = 2205
ksmps  = 20
nchnls = 2

instr 1
;  ifrq = cpspch(p5)
  ifrq = p5
  
  kenv linseg 0, .01, p4, p3-.02, p4, .01, 0

  a0     oscil 8,   0.5    * ifrq,  1
  a1     oscil 8,   1      * ifrq,  1
  a2     oscil 8,   2      * ifrq,  1
  a3     oscil 8,   2.9966 * ifrq,  1
  a4     oscil 8,   4      * ifrq,  1
  a5     oscil 3,   5.9932 * ifrq,  1
  a6     oscil 2,   8      * ifrq,  1
  a7     oscil 1,  10.0794 * ifrq,  1
  a8     oscil 1,  11.9864 * ifrq,  1
  a9     oscil 4,  16      * ifrq,  1

  aorgan = kenv* (a0+a1+a2+a3+a4+a5+a6+a7+a8+a9)

  outs aorgan, aorgan
endin
